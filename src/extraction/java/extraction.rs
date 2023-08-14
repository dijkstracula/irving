use crate::{
    ast::{
        declarations::{self, Binding},
        span::Span, logic,
    },
    extraction::{java::extraction::expressions::Token, ExtractResult}, passes::quantifier_bounds::QuantBounds,
};
use std::{collections::BTreeMap, fmt::Write};

use thiserror::Error;

use crate::{
    ast::{
        actions,
        expressions::{self, IndexExpr, Verb},
        statements, toplevels,
    },
    extraction::pprint::PrettyPrinter,
    visitor::ast::{Visitable, Visitor},
};

use crate::visitor::*;

use super::types::JavaType;

pub struct Extractor<W>
where
    W: Write,
{
    pub pp: PrettyPrinter<W>,
    type_aliases: BTreeMap<Token, JavaType>,
}

impl Extractor<String> {
    pub fn new() -> Self {
        Self {
            pp: PrettyPrinter::new(),
            type_aliases: BTreeMap::new(),
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

    pub fn write_paramlist(
        &mut self,
        us: &mut expressions::ParamList,
        sep: &str,
    ) -> ExtractResult<Vec<expressions::Symbol>> {
        for (i, u) in us.iter_mut().enumerate() {
            if i > 0 {
                self.pp.write_str(sep)?;
            }
            self.param(u)?;
        }
        Ok(ControlMut::Produce(()))
    }

    fn write_lambda(
        &mut self,
        params: &mut expressions::ParamList,
        ret: &mut Option<expressions::Symbol>,
        body: &mut Vec<statements::Stmt>,
    ) -> ExtractResult<Vec<statements::Stmt>> {
        self.pp.write_str("(")?;
        self.write_paramlist(params, ", ")?;
        self.pp.write_str(") -> {\n")?;

        // The first declaration needs to define the return value.
        ret.as_mut().map(|ret| {
            let mut retdecl =
                declarations::Decl::Var {
                    span: Span::Optimized, /* We're just using this to walk the binding, so it doesn't matter */
                    decl: Binding::from(ret.name.clone(), ret.decl.clone())
                };
            retdecl.visit(self).unwrap();
            self.pp.write_str(";\n").unwrap();
        });

        for stmt in body {
            stmt.visit(self)?.modifying(stmt);
            self.pp.write_str(";\n")?;
        }

        // The final declaration needs to wrap the return value in a Right<U>.
        // (For void-producing actions, U=j.l.Void, the only inhabitant of which
        // is `null`.)
        match ret {
            None => self.pp.write_str("return Either.right(null);\n")?,
            Some(ret) => self
                .pp
                .write_fmt(format_args!("return Either.right({});\n", ret.name))?,
        }

        self.pp.write_str("})")?;

        Ok(ControlMut::Produce(()))
    }

    fn jtype_from_sort(s: &mut expressions::Sort) -> JavaType {
        match s {
            expressions::Sort::ToBeInferred => todo!(),
            expressions::Sort::Annotated(_) => todo!(),
            expressions::Sort::Resolved(ivysort) => {
                let j: JavaType = ivysort.clone().into(); // XXX: poor choices lead to this clone.a
                j
            }
        }
    }
}

impl<W> ast::Visitor<(), std::fmt::Error> for Extractor<W>
where
    W: Write,
{
    fn begin_prog(&mut self, ast: &mut toplevels::Prog) -> ExtractResult<toplevels::Prog> {
        let imports = include_str!("templates/imports.txt");
        self.pp.write_str(imports)?;
        self.pp.write_str("\n\n")?;

        for decl in &mut ast.top {
            decl.visit(self)?.modifying(decl);
            self.pp.write_str("\n")?;
        }
        Ok(ControlMut::SkipSiblings(()))
    }

    fn action_seq(&mut self, ast: &mut Vec<actions::Action>) -> ExtractResult<statements::Stmt> {
        for (i, a) in ast.iter_mut().enumerate() {
            if i > 0 {
                self.pp.write_str(";\n")?;
            }
            a.visit(self)?;
        }
        Ok(ControlMut::Produce(()))
    }

    fn begin_if(&mut self, ast: &mut statements::If) -> ExtractResult<statements::Stmt> {
        self.pp.write_str("if (")?;
        ast.tst.visit(self)?;
        self.pp.write_str(") {\n")?;

        self.write_separated(&mut ast.thn, ";\n")?;

        if let Some(stmts) = &mut ast.els {
            self.pp.write_str("} else {\n")?;
            for stmt in stmts {
                stmt.visit(self)?.modifying(stmt);
                self.pp.write_str(";\n")?;
            }
        }
        self.pp.write_str("}\n")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_while(&mut self, ast: &mut statements::While) -> ExtractResult<statements::Stmt> {
        self.pp.write_str("while ")?;
        ast.test.visit(self)?;

        self.pp.write_str(" {\n")?;
        for stmt in &mut ast.doit {
            stmt.visit(self)?.modifying(stmt);
            self.pp.write_str(";\n")?;
        }
        self.pp.write_str("}\n")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    // Declarations

    fn begin_action_decl(
        &mut self,
        _span: &Span,
        name: &mut Token,
        ast: &mut declarations::ActionDecl,
    ) -> ExtractResult<declarations::Decl> {
        let arity = ast.params.len();
        let mut ret = ast.ret.clone().or(Some(expressions::Symbol {
            name: "_void".into(),
            decl: expressions::Sort::Resolved(crate::typechecker::sorts::IvySort::Unit),
        }));

        self.pp
            .write_fmt(format_args!("protected Action{}<", arity))?;
        let mut sorts: Vec<expressions::Sort> = ast
            .params
            .iter_mut()
            .chain(ret.iter_mut())
            .map(|sym| sym.decl.clone())
            .collect::<_>();
        self.write_separated(&mut sorts, ", ")?;
        self.pp
            .write_fmt(format_args!("> {name} = new Action{arity}<>("))?;

        match &mut ast.body {
            None => {
                self.pp.write_str(")")?;
            }
            Some(ref mut body) => {
                self.write_lambda(&mut ast.params, &mut ast.ret, body)?;
            }
        }

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_after_decl(
        &mut self,
        _span: &Span,
        ast: &mut declarations::ActionMixinDecl,
    ) -> ExtractResult<declarations::Decl> {
        // We have a special case for `after init`: since this is being emitted
        // as part of the constructor, just inline the mixin's body here.
        let init_name = vec!["init".to_owned()];
        if ast.name == init_name {
            for inner in &mut ast.body {
                inner.visit(self)?.modifying(inner);
            }
            return Ok(ControlMut::SkipSiblings(()));
        }

        // Otherwise, emit the callback to the action instance variable.
        ast.name.visit(self)?.modifying(&mut ast.name);
        self.pp.write_str(".onAfter((")?;

        if let Some(params) = &mut ast.params {
            self.write_paramlist(params, ", ")?;
        }
        self.pp.write_str(") -> {\n")?;
        for stmt in &mut ast.body {
            stmt.visit(self)?.modifying(stmt);
            self.pp.write_str(";\n")?;
        }
        self.pp.write_str("})")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_alias_decl(
        &mut self,
        sym: &mut expressions::Token,
        sort: &mut expressions::Sort,
    ) -> ExtractResult<declarations::Decl> {
        let j: JavaType = (sort as &expressions::Sort).into(); // XXX: poor choices lead to this clone.a
        self.type_aliases.insert(sym.clone(), j);
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_before_decl(
        &mut self,
        ast: &mut declarations::ActionMixinDecl,
    ) -> ExtractResult<declarations::Decl> {
        ast.name.visit(self)?.modifying(&mut ast.name);
        self.pp.write_str(".addBefore((")?;

        if let Some(params) = &mut ast.params {
            self.write_paramlist(params, ",")?;
            // XXX: also the return value needs to be bound.
        }
        self.pp.write_str(") -> {\n")?;
        for stmt in &mut ast.body {
            stmt.visit(self)?.modifying(stmt);
            self.pp.write_str(";\n")?;
        }
        self.pp.write_str("})")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_implement_decl(
        &mut self,
        ast: &mut declarations::ActionMixinDecl,
    ) -> ExtractResult<declarations::Decl> {
        ast.name.visit(self)?.modifying(&mut ast.name);
        self.pp.write_str(".on((")?;

        self.write_lambda(ast.params.as_mut().unwrap(), &mut ast.ret, &mut ast.body)?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_import_decl(
        &mut self,
        ast: &mut declarations::ImportDecl,
    ) -> ExtractResult<declarations::Decl> {
        self.pp.write_str("private void ")?;
        self.pp.write_fmt(format_args!("{}(", ast.name))?;

        for param in &mut ast.params {
            self.param(param)?.modifying(param);
        }
        self.pp.write_str(") {\n")?;

        self.pp.write_str("System.out.println(\"")?;
        self.pp.write_str("\");")?;

        self.pp.write_str("\n}")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_instance_decl(
        &mut self,
        name: &mut Token,
        ast: &mut declarations::InstanceDecl,
    ) -> ExtractResult<declarations::Decl> {
        self.pp.write_fmt(format_args!("class {} extends ", name))?;
        ast.sort.visit(self)?.modifying(&mut ast.sort);
        if !ast.args.is_empty() {
            self.pp.write_str("<")?;

            // maybe slightly confusing: because we parameterise modules on their sorts,
            // the `id` field contains the identifier for sort, not the `sort` field.
            let mut sorts = ast.args.iter().map(|a| a.name.clone()).collect::<Vec<_>>();
            self.write_separated(&mut sorts, ", ")?;
            self.pp.write_str(">")?;
        }

        self.pp.write_str(" {\n}")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_interpret_decl(
        &mut self,
        _name: &mut Token,
        _sort: &mut expressions::Sort,
    ) -> ExtractResult<declarations::Decl> {
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_invariant_decl(
        &mut self,
        ast: &mut crate::ast::logic::Fmla,
    ) -> ExtractResult<declarations::Decl> {
        self.pp.write_str("addConjecture(")?;

        match ast {
            crate::ast::logic::Fmla::Forall(_) => todo!(),
            crate::ast::logic::Fmla::Exists(_) => todo!(),
            crate::ast::logic::Fmla::Pred(expr) => {
                self.pp.write_str("() -> ")?;
                expr.visit(self)?.modifying(expr);
            }
        }
        self.pp.write_str(")")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_typedecl(
        &mut self,
        _span: &Span,
        name: &mut Token,
        s: &mut expressions::Sort,
    ) -> ExtractResult<declarations::Decl> {
        if let expressions::Sort::ToBeInferred = s {
            return Ok(ControlMut::SkipSiblings(()));
        }
        self.type_aliases
            .insert(name.clone(), (s as &expressions::Sort).into());
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_vardecl(
        &mut self,
        _span: &Span,
        name: &mut Token,
        sort: &mut expressions::Sort,
    ) -> ExtractResult<declarations::Decl> {
        self.pp
            .write_fmt(format_args!("{} ", Self::jtype_from_sort(sort).as_jval()))?;
        name.visit(self)?.modifying(name);
        Ok(ControlMut::SkipSiblings(()))
    }

    // Expressions

    fn begin_app(&mut self, ast: &mut expressions::AppExpr) -> ExtractResult<expressions::Expr> {
        ast.func.visit(self)?;

        self.pp.write_str("(")?;
        self.write_separated(&mut ast.args, ", ")?;
        self.pp.write_str(")")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_assign(
        &mut self,
        _span: &Span,
        ast: &mut actions::AssignAction,
    ) -> ExtractResult<actions::Action> {
        ast.lhs.visit(self)?;
        self.pp.write_str(" = ")?;
        ast.rhs.visit(self)?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_binop(&mut self, ast: &mut expressions::BinOp) -> ExtractResult<expressions::Expr> {
        ast.lhs.visit(self)?;

        let op_str = match ast.op {
            Verb::Plus => "+",
            Verb::Minus => "-",
            Verb::Times => "*",
            Verb::Div => "/",
            Verb::Dot => ".",

            Verb::Equals => "==",
            Verb::Notequals => "!=",
            Verb::Lt => "<",
            Verb::Le => "<=",
            Verb::Gt => ">",
            Verb::Ge => ">=",

            Verb::And => "&&",
            Verb::Or => "||",
            _ => {
                eprintln!("{:?}", ast.op);
                unimplemented!()
            }
        };
        match ast.op {
            Verb::Dot => {
                self.pp.write_str(op_str)?;
            }
            _ => {
                self.pp.write_fmt(format_args!(" {} ", op_str))?;
            }
        }
        ast.rhs.visit(self)?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_call(&mut self, ast: &mut expressions::AppExpr) -> ExtractResult<actions::Action> {
        self.begin_app(ast)?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_ensure(&mut self, _ast: &mut actions::EnsureAction) -> ExtractResult<actions::Action> {
        self.pp.write_str("ensureThat(")?;
        Ok(ControlMut::Produce(()))
    }
    fn finish_ensure(
        &mut self,
        _ast: &mut actions::EnsureAction,
        _pred_t: (),
    ) -> ExtractResult<actions::Action> {
        self.pp.write_str(")")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_field_access(
        &mut self,
        lhs: &mut expressions::Expr,
        rhs: &mut expressions::Symbol,
    ) -> ExtractResult<expressions::Expr> {
        lhs.visit(self)?;
        self.pp.write_fmt(format_args!(".{}", rhs.name))?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_forall(&mut self, ast: &mut logic::Forall) -> VisitorResult<(), std::fmt::Error, logic::Fmla> {
        let mut qb = QuantBounds::new_forall();

        // XXX: unfortunate that we duplicate QuantBounds::begin_forall() here.
        for var in &ast.vars {
            qb.bounds.insert(var.name.clone(), vec![]);
        }
        ast.fmla.visit(&mut qb)?.modifying(&mut ast.fmla);

        for var in &ast.vars {
            let bounds = qb.bounds.get(&var.name).unwrap();
        }

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_index(&mut self, expr: &mut IndexExpr) -> ExtractResult<expressions::Expr> {
        expr.lhs.visit(self)?;
        self.pp.write_str("[")?;
        expr.idx.visit(self)?;
        self.pp.write_str("]")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_object_decl(
        &mut self,
        name: &mut Token,
        ast: &mut declarations::ObjectDecl,
    ) -> ExtractResult<declarations::Decl> {
        self.pp.write_fmt(format_args!("class IvyObj_{name}"))?;
        self.pp.write_str(" {\n")?;

        for decl in ast.actions() {
            decl.visit(self)?.modifying(decl);
            self.pp.write_str(";\n")?;
        }
        self.pp.write_str("\n")?;

        for decl in ast.vars() {
            self.pp.write_str("private ")?;
            decl.visit(self)?.modifying(decl);
            self.pp.write_str(";\n")?;
        }
        self.pp.write_str("\n")?;

        // Constructor
        self.pp.write_fmt(format_args!("public IvyObj_{name}("))?;
        self.write_paramlist(&mut ast.params, ", ")?;
        self.pp.write_fmt(format_args!(") {{\n"))?;

        for param in &ast.params {
            self.pp
                .write_fmt(format_args!("this.{} = {};\n", param.name, param.name))?;
        }

        self.pp.write_str("\n")?;
        for decl in ast
            .body
            .iter_mut()
            .filter(|d| d.name_for_binding().is_none())
        {
            decl.visit(self)?.modifying(decl);
            self.pp.write_str(";")?;
            //self.pp.write_fmt(format_args!(" // {:?}", decl))?;
            self.pp.write_str("\n")?;
        }

        self.pp.write_str("\n} //cstr \n")?;

        for decl in ast.subobjects() {
            decl.visit(self)?.modifying(decl);
            self.pp.write_str("\n")?;
        }

        self.pp.write_str("\n")?;

        self.pp.write_str("\n}\n")?;

        self.pp.write_fmt(format_args!(
            "IvyObj_{name} {name} = new IvyObj_{name}();\n"
        ))?;

        Ok(ControlMut::SkipSiblings(()))
    }

    // Terminals

    fn boolean(&mut self, b: &mut bool) -> ExtractResult<bool> {
        if *b {
            self.pp.write_str("true")?;
        } else {
            self.pp.write_str("false")?;
        }
        Ok(ControlMut::Produce(()))
    }

    fn identifier(&mut self, i: &mut expressions::Ident) -> ExtractResult<expressions::Ident> {
        self.write_separated(i, ".")?;
        Ok(ControlMut::Produce(()))
    }

    fn number(&mut self, _span: &Span, n: &mut i64) -> ExtractResult<i64> {
        self.pp.write_str(&n.to_string())?;
        Ok(ControlMut::Produce(()))
    }

    fn param(&mut self, p: &mut expressions::Symbol) -> ExtractResult<expressions::Symbol> {
        self.sort(&mut p.decl)?;
        self.pp.write_str(" ")?;
        self.pp.write_str(&p.name)?;
        Ok(ControlMut::Produce(()))
    }

    fn sort(&mut self, s: &mut expressions::Sort) -> ExtractResult<expressions::Sort> {
        match s {
            expressions::Sort::ToBeInferred => (),
            expressions::Sort::Annotated(ident) => {
                ident.visit(self)?.modifying(ident);
            }
            expressions::Sort::Resolved(ivysort) => {
                let j: JavaType = ivysort.clone().into(); // XXX: poor choices lead to this clone.a
                self.pp.write_str(j.as_jref().as_str())?;
            }
        }
        Ok(ControlMut::Produce(()))
    }

    fn token(&mut self, s: &mut expressions::Token) -> ExtractResult<expressions::Token> {
        let alias = self.type_aliases.get_mut(s);
        match alias {
            None => self.pp.write_str(s)?,
            Some(jsort) => {
                // TODO: the reference type is safe but it'd be nice to
                // avoid boxing: can we safely drop to the value type?
                self.pp.write_str(jsort.as_jref().as_str())?;
            }
        }
        Ok(ControlMut::Produce(()))
    }

    fn this(&mut self) -> ExtractResult<expressions::Expr> {
        self.pp.write_str("this")?;
        Ok(ControlMut::Produce(()))
    }
}

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum JExtractionError {
    #[error("Symbol {0:?} failed to have a type inferred (did the typechecking pass run?)")]
    UnresolvedType(expressions::Token),
}
