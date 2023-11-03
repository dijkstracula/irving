use std::{collections::BTreeMap, fmt::Write};

use crate::{
    ast::{
        actions,
        declarations::{self, MapDecl},
        expressions, logic, span, statements,
    },
    typechecker::sorts::IvySort,
    visitor::{ast::Visitable, *},
};

use super::{pprint::PrettyPrinter, ExtractResult};

pub struct Extractor<W>
where
    W: Write,
{
    pub pp: PrettyPrinter<W>,

    /// We need to remember whether we are traversing `after init` as we emit different
    /// predicates for actions in that case.
    pub emitting_after_init: bool,

    /// In order to deduplicate tokens that in Ivy reside in different nonoverlapping
    /// scopes, we remember
    pub emitted_relations_by_name: BTreeMap<expressions::Token, usize>,
    pub emitted_relations_by_decl: BTreeMap<MapDecl, usize>,
    pub emitted_symbols: BTreeMap<expressions::Token, BTreeMap<expressions::Sort, usize>>,
    pub num_properties: usize,
}

impl Extractor<String> {
    pub fn new() -> Self {
        Self {
            pp: PrettyPrinter::new(),
            emitting_after_init: false,
            emitted_relations_by_name: BTreeMap::new(),
            emitted_relations_by_decl: BTreeMap::new(),
            emitted_symbols: BTreeMap::new(),
            num_properties: 0,
        }
    }
}

impl<W> Extractor<W>
where
    W: Write,
{
    pub fn write_separated<U>(&mut self, us: &mut Vec<U>, sep: &str) -> ExtractResult<Vec<U>>
    where
        U: Visitable<(), std::fmt::Error>,
    {
        for (i, u) in us.iter_mut().enumerate() {
            if i > 0 {
                self.pp.write_str(sep)?;
            }
            u.visit(self)?;
        }
        Ok(ControlMut::Produce(()))
    }

    fn lvar(sym: &str, id: &usize) -> String {
        if id == &0 {
            format!("{sym}")
        } else {
            format!("{sym}{id}")
        }
    }

    fn array_sort(id: &usize) -> String {
        if id == &0 {
            format!("Arr")
        } else {
            format!("Arr{id}")
        }
    }

    fn const_fun(id: &usize) -> String {
        if id == &0 {
            format!("Const")
        } else {
            format!("Const{id}")
        }
    }

    fn read_fun(id: &usize) -> String {
        if id == &0 {
            format!("Read")
        } else {
            format!("Read{id}")
        }
    }

    fn write_fun(id: &usize) -> String {
        if id == &0 {
            format!("Write")
        } else {
            format!("Write{id}")
        }
    }

    fn next_sort(sort: &String) -> String {
        format!("{sort}_next")
    }

    pub fn emit_unique_symbol(
        &mut self,
        name: &mut expressions::Token,
        sort: &mut expressions::Sort,
    ) -> Result<usize, std::fmt::Error> {
        if !self.emitted_symbols.contains_key(name) {
            self.emitted_symbols.insert(name.clone(), BTreeMap::new());
        }

        let sorts = self.emitted_symbols.get_mut(name).unwrap();

        let id = if !sorts.contains_key(sort) {
            let id = sorts.len();
            sorts.insert(sort.clone(), id);

            let lvar = Self::lvar(name, &id);

            self.pp
                .write_fmt(format_args!("(declare-fun {} () ", lvar))?;
            sort.visit(self)?.modifying(sort);
            self.pp.write_str(")\n")?;

            self.pp
                .write_fmt(format_args!("(declare-fun {} () ", Self::next_sort(&lvar)))?;
            sort.visit(self)?.modifying(sort);
            self.pp.write_str(")\n")?;

            self.pp
                .write_fmt(format_args!("(define-fun .{} () ", lvar))?;
            sort.visit(self)?.modifying(sort);
            self.pp.write_fmt(format_args!(
                "(! {} :next {})",
                lvar,
                Self::next_sort(&lvar)
            ))?;
            self.pp.write_str(")\n")?;
            self.pp.write_str("\n")?;

            id
        } else {
            sorts.get(sort).unwrap().clone()
        };

        Ok(id)
    }
}

impl<W> ast::Visitor<(), std::fmt::Error> for Extractor<W>
where
    W: Write,
{
    // Top-levels

    fn finish_prog(
        &mut self,
        _ast: &mut crate::ast::toplevels::Prog,
    ) -> VisitorResult<(), std::fmt::Error, crate::ast::toplevels::Prog> {
        // Guessing you want to put the trans-conditions stuff here once you've accumulated all the values up.
        Ok(ControlMut::Produce(()))
    }

    // Statements

    fn action_seq(&mut self, ast: &mut Vec<actions::Action>) -> ExtractResult<statements::Stmt> {
        self.pp.write_str("(and ")?;

        for (i, a) in ast.iter_mut().enumerate() {
            if i > 0 {
                self.pp.write_str("\n")?;
            }
            a.visit(self)?;
        }
        self.pp.write_str(")")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_action_decl(
        &mut self,
        _span: &span::Span,
        name: &mut expressions::Token,
        ast: &mut declarations::ActionDecl,
    ) -> VisitorResult<(), std::fmt::Error, declarations::Decl> {
        for p in &mut ast.params {
            self.emit_unique_symbol(&mut p.name, &mut p.decl)?;
        }

        self.pp.write_str("(declare-fun ")?;
        name.visit(self)?.modifying(name);
        self.pp.write_str("() Bool)\n")?;

        self.pp.write_str("(define-fun .")?;
        name.visit(self)?.modifying(name);
        self.pp.write_str("() Bool (! ")?;
        name.visit(self)?.modifying(name);
        self.pp.write_str(": action 0)")?;
        self.pp.write_str(")\n\n")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_app(&mut self, ast: &mut expressions::AppExpr) -> ExtractResult<expressions::Expr> {
        ast.func.visit(self)?;

        self.pp.write_str("(")?;
        self.write_separated(&mut ast.args, ", ")?;
        self.pp.write_str(")")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_logical_app(
        &mut self,
        ast: &mut logic::LogicApp,
    ) -> VisitorResult<(), std::fmt::Error, logic::Fmla> {
        ast.func.visit(self)?;

        self.pp.write_str("(")?;
        self.write_separated(&mut ast.args, ", ")?;
        self.pp.write_str(")")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_after_decl(
        &mut self,
        _span: &span::Span,
        ast: &mut declarations::ActionMixinDecl,
    ) -> VisitorResult<(), std::fmt::Error, declarations::Decl> {
        if ast.name.last() == Some(&String::from("init")) {
            // Note to Cole: since assignments work differently inside init
            // (e.g. (= thing 0) versus (= thing_next (+ thing 1)) or whatever)
            // we'll want to remember in the AST traversal whether we're emitting
            // the init block or not.
            self.emitting_after_init = true;
            self.pp
                .write_str("(define-fun init-conditions () Bool (!\n")?;
        } else {
            // You'll have to handle the other case...
        }
        Ok(ControlMut::Produce(()))
    }

    fn finish_after_decl(
        &mut self,
        _span: &span::Span,
        ast: &mut declarations::ActionMixinDecl,
        _n: (),
        _p: Option<Vec<()>>,
        _r: Option<()>,
        _b: Vec<()>,
    ) -> VisitorResult<(), std::fmt::Error, declarations::Decl> {
        if ast.name.last() == Some(&String::from("init")) {
            self.pp.write_str(") :init true)\n\n")?;
            self.emitting_after_init = false;
        }
        Ok(ControlMut::Produce(()))
    }

    fn begin_assign(
        &mut self,
        _span: &span::Span,
        ast: &mut actions::AssignAction,
    ) -> VisitorResult<(), std::fmt::Error, actions::Action> {
        if self.emitting_after_init {
            self.pp.write_str("(= ")?;
            match &mut ast.lhs {
                // Assigning to a map is represented as an assignment in the AST.
                expressions::Expr::App {
                    expr:
                        expressions::AppExpr {
                            func_sort: expressions::Sort::Resolved(IvySort::Map(_, _)),
                            ..
                        },
                    ..
                } => {
                    todo!()
                }
                expressions::Expr::ProgramSymbol { sym, .. } => self.pp.write_str(&sym.name)?,
                _ => todo!(),
            }
            self.pp.write_str(" ")?;
            ast.rhs.visit(self)?.modifying(&mut ast.rhs);
            self.pp.write_str(")")?;
        }
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_assign_logical(
        &mut self,
        _span: &span::Span,
        ast: &mut actions::AssignLogicalAction,
    ) -> VisitorResult<(), std::fmt::Error, actions::Action> {
        if self.emitting_after_init {
            self.pp.write_str("(= ")?;
            match &mut ast.lhs {
                // Assigning to a map is represented as an assignment in the AST.
                logic::Fmla::App {
                    app: logic::LogicApp { func, .. },
                    ..
                } => {
                    // XXX: This is probably not 100% right...
                    func.visit(self)?.modifying(func);
                    self.pp.write_str(" ")?;

                    // Assume that we always want to initialize it with ConstArr 0...
                    // This also assumes the LHS is basically of the form RelationName(X, ...)
                    let func_sym = match func.as_ref() {
                        logic::Fmla::ProgramSymbol { sym, .. } => sym.clone(),
                        _ => panic!("LHS of a logical assign is too complicated"),
                    };
                    let id = self.emitted_relations_by_name.get(&func_sym.name).unwrap();
                    self.pp
                        .write_fmt(format_args!("({} 0)", Self::const_fun(id)))?;
                }
                fmla => fmla.visit(self)?.modifying(fmla),
            }
            self.pp.write_str(" ")?;
            ast.rhs.visit(self)?.modifying(&mut ast.rhs);
            self.pp.write_str(")")?;
        }
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_logical_binop(
        &mut self,
        ast: &mut logic::LogicBinOp,
    ) -> VisitorResult<(), std::fmt::Error, logic::Fmla> {
        self.pp.write_str("(")?;
        self.verb(&mut ast.op)?.modifying(&mut ast.op);
        self.pp.write_str(" ")?;
        ast.lhs.visit(self)?.modifying(&mut ast.lhs);
        self.pp.write_str(" ")?;
        ast.rhs.visit(self)?.modifying(&mut ast.rhs);
        self.pp.write_str(")")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_forall(
        &mut self,
        ast: &mut logic::Forall,
    ) -> VisitorResult<(), std::fmt::Error, logic::Fmla> {
        self.pp.write_str("(forall (")?;
        for (i, var) in &mut ast.vars.iter_mut().enumerate() {
            if i > 0 {
                self.pp.write_str(" ")?;
            }
            var.name.visit(self)?.modifying(&mut var.name);
        }
        self.pp.write_str(") ")?;
        ast.fmla.visit(self)?.modifying(&mut ast.fmla);
        self.pp.write_str(")")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_unary_op(
        &mut self,
        op: &mut expressions::Verb,
        rhs: &mut expressions::Expr,
    ) -> VisitorResult<(), std::fmt::Error, expressions::Expr> {
        self.pp.write_str("(")?;
        self.verb(op)?.modifying(op);
        self.pp.write_str(" ")?;
        rhs.visit(self)?.modifying(rhs);
        self.pp.write_str(")")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_logical_unary_op(
        &mut self,
        op: &mut expressions::Verb,
        rhs: &mut logic::Fmla,
    ) -> VisitorResult<(), std::fmt::Error, logic::Fmla> {
        self.pp.write_str("(")?;
        self.verb(op)?.modifying(op);
        self.pp.write_str(" ")?;
        rhs.visit(self)?.modifying(rhs);
        self.pp.write_str(")")?;
        Ok(ControlMut::SkipSiblings(()))
    }
    // Declarations

    fn begin_export_decl(
        &mut self,
        ast: &mut declarations::ExportDecl,
    ) -> VisitorResult<(), std::fmt::Error, declarations::Decl> {
        match ast {
            declarations::ExportDecl::Action(_) => Ok(ControlMut::Produce(())),
            declarations::ExportDecl::ForwardRef(_) => Ok(ControlMut::SkipSiblings(())),
        }
    }

    fn begin_invariant_decl(
        &mut self,
        ast: &mut logic::Fmla,
    ) -> VisitorResult<(), std::fmt::Error, declarations::Decl> {
        // We need to emit all the quantified variables if they have not yet been introduced.
        match ast {
            logic::Fmla::Forall {
                fmla: logic::Forall { vars, .. },
                ..
            }
            | logic::Fmla::Exists {
                fmla: logic::Exists { vars, .. },
                ..
            } => {
                for quant in vars {
                    self.emit_unique_symbol(&mut quant.name, &mut quant.decl)?;
                }
            }
            _ => (),
        }

        self.num_properties += 1;
        self.pp.write_fmt(format_args!(
            "(define-fun property{} () Bool !(\n",
            self.num_properties
        ))?;
        Ok(ControlMut::Produce(()))
    }

    fn finish_invariant_decl(
        &mut self,
        _ast: &mut logic::Fmla,
    ) -> VisitorResult<(), std::fmt::Error, declarations::Decl> {
        self.pp.write_str(") :invar-property 0))")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_map_decl(
        &mut self,
        _span: &span::Span,
        name: &mut expressions::Token,
        ast: &mut declarations::MapDecl,
    ) -> VisitorResult<(), std::fmt::Error, declarations::Decl> {
        assert!(ast.domain.len() == 1);
        let mut dom = &mut ast.domain.get_mut(0).unwrap().decl.clone();
        let mut range = ast.range.clone();

        if !self.emitted_relations_by_decl.contains_key(ast) {
            let new_id = self.emitted_relations_by_decl.len();
            self.emitted_relations_by_decl.insert(ast.clone(), new_id);

            let array_sort = Self::array_sort(&new_id);

            self.pp
                .write_fmt(format_args!("(declare-sort {} 0)\n", array_sort))?;
            self.pp.write_fmt(format_args!(
                "(declare-fun {} (Int) {})\n",
                Self::const_fun(&new_id),
                array_sort
            ))?;

            self.pp.write_fmt(format_args!(
                "(declare-fun {} ({} ",
                Self::read_fun(&new_id),
                array_sort,
            ))?;
            self.sort(&mut dom)?.modifying(&mut dom);
            self.pp.write_str(") ")?;
            self.sort(&mut range)?.modifying(&mut range);
            self.pp.write_str(")\n")?;

            self.pp.write_fmt(format_args!(
                "(declare-fun {} ({} ",
                Self::write_fun(&new_id),
                array_sort,
            ))?;
            self.sort(&mut dom)?.modifying(&mut dom);
            self.pp.write_str(" ")?;
            self.sort(&mut range)?.modifying(&mut range);
            self.pp.write_fmt(format_args!(") {})\n", array_sort,))?;
            self.pp.write_str("\n")?;
        }
        let id = self.emitted_relations_by_decl.get(ast).unwrap();
        let array_sort = Self::array_sort(id);

        self.pp
            .write_fmt(format_args!("(declare-fun {} () {})\n", name, array_sort))?;
        self.pp.write_fmt(format_args!(
            "(declare-fun {} () {})\n",
            Self::next_sort(name),
            array_sort
        ))?;
        self.pp.write_fmt(format_args!(
            "(define-fun .{} () (! {} :next {}))\n",
            name,
            name,
            Self::next_sort(name)
        ))?;
        self.pp.write_str("\n")?;
        self.emitted_relations_by_name
            .insert(name.clone(), id.clone());

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_typedecl(
        &mut self,
        _span: &span::Span,
        _name: &mut expressions::Token,
        _ast: &mut expressions::Sort,
    ) -> VisitorResult<(), std::fmt::Error, declarations::Decl> {
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_vardecl(
        &mut self,
        _span: &span::Span,
        name: &mut expressions::Token,
        sort: &mut expressions::Sort,
    ) -> VisitorResult<(), std::fmt::Error, declarations::Decl> {
        let _ = self.emit_unique_symbol(name, sort)?;
        //self.pp.write_str(&Self::lvar(name, &id))?;
        Ok(ControlMut::SkipSiblings(()))
    }

    // Terminals

    fn boolean(&mut self, b: &mut bool) -> VisitorResult<(), std::fmt::Error, bool> {
        if *b {
            self.pp.write_str("true")?;
        } else {
            self.pp.write_str("false")?;
        }
        Ok(ControlMut::Produce(()))
    }
    fn sort(
        &mut self,
        s: &mut expressions::Sort,
    ) -> VisitorResult<(), std::fmt::Error, expressions::Sort> {
        match s {
            expressions::Sort::ToBeInferred => self.pp.write_str("Int")?,
            expressions::Sort::Annotated(_ident) => todo!(),
            expressions::Sort::Resolved(is) => match is {
                crate::typechecker::sorts::IvySort::Uninterpreted => self.pp.write_str("Int")?,
                crate::typechecker::sorts::IvySort::This => todo!(),
                crate::typechecker::sorts::IvySort::Unit => todo!(),
                crate::typechecker::sorts::IvySort::Top => todo!(),
                crate::typechecker::sorts::IvySort::Bool => self.pp.write_str("Bool")?,
                crate::typechecker::sorts::IvySort::Number => self.pp.write_str("Int")?,
                crate::typechecker::sorts::IvySort::BitVec(_) => todo!(),
                crate::typechecker::sorts::IvySort::Vector(_) => todo!(),
                crate::typechecker::sorts::IvySort::BoundedSequence(_, _) => todo!(),
                crate::typechecker::sorts::IvySort::Enum(_) => todo!(),
                crate::typechecker::sorts::IvySort::Action(_, _, _, _) => todo!(),
                crate::typechecker::sorts::IvySort::Map(_, _) => todo!(),
                crate::typechecker::sorts::IvySort::Class(_) => todo!(),
                crate::typechecker::sorts::IvySort::Module(_) => todo!(),
                crate::typechecker::sorts::IvySort::Object(_) => todo!(),
                crate::typechecker::sorts::IvySort::SortVar(_) => self.pp.write_str("Int")?,
            },
        }
        Ok(ControlMut::Produce(()))
    }

    fn param(
        &mut self,
        p: &mut expressions::Symbol,
    ) -> VisitorResult<(), std::fmt::Error, expressions::Symbol> {
        let id = self.emit_unique_symbol(&mut p.name, &mut p.decl)?;
        self.pp.write_str(&Self::lvar(&p.name, &id))?;

        Ok(ControlMut::Produce(()))
    }

    fn symbol(
        &mut self,
        _span: &span::Span,
        p: &mut expressions::Symbol,
    ) -> VisitorResult<(), std::fmt::Error, expressions::Symbol> {
        self.pp.write_str(&p.name)?;
        Ok(ControlMut::Produce(()))
    }

    fn verb(
        &mut self,
        v: &mut expressions::Verb,
    ) -> VisitorResult<(), std::fmt::Error, expressions::Verb> {
        self.pp.write_str(match v {
            expressions::Verb::Iff => "<->",
            expressions::Verb::Or => "|",
            expressions::Verb::And => "&",
            expressions::Verb::Lt => "<",
            expressions::Verb::Le => "<=",
            expressions::Verb::Gt => ">",
            expressions::Verb::Ge => ">=",
            expressions::Verb::Equals => "=",
            expressions::Verb::Notequals => "~=",
            expressions::Verb::Not => "not",
            expressions::Verb::Arrow => "=>",
            expressions::Verb::Plus => "+",
            expressions::Verb::Minus => "-",
            expressions::Verb::Times => "*",
            expressions::Verb::Div => "/",
            expressions::Verb::Dot => ".",
        })?;
        Ok(ControlMut::Produce(()))
    }

    fn token(
        &mut self,
        token: &mut expressions::Token,
    ) -> VisitorResult<(), std::fmt::Error, expressions::Token> {
        self.pp.write_str(token)?;
        Ok(ControlMut::Produce(()))
    }
}
