use std::fmt::Write;

use crate::{
    ast::{
        actions,
        declarations::{self, Binding},
        expressions::{self, AnnotatedSymbol, IndexExpr, Sort, Symbol, Verb},
        logic, statements, toplevels,
    },
    typechecker::sorts::IvySort,
    visitor::ast::Visitor,
};

use crate::visitor::ast::Visitable;

use crate::visitor::*;

use super::pprint::PrettyPrinter;

pub struct Extractor<W>
where
    W: Write,
{
    pub pp: PrettyPrinter<W>,
}

impl Extractor<String> {
    pub fn new() -> Self {
        Self {
            pp: PrettyPrinter::new(),
        }
    }
}

impl<W> Extractor<W>
where
    W: Write,
{
    pub fn write_separated<U>(&mut self, us: &mut Vec<U>, sep: &str) -> VisitorResult<(), Vec<U>>
    where
        U: Visitable<()>,
    {
        for (i, u) in us.into_iter().enumerate() {
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
    ) -> VisitorResult<(), Vec<AnnotatedSymbol>> {
        for (i, u) in us.into_iter().enumerate() {
            if i > 0 {
                self.pp.write_str(sep)?;
            }
            self.param(u)?;
        }
        Ok(ControlMut::Produce(()))
    }
}

impl<W> ast::Visitor<()> for Extractor<W>
where
    W: Write,
{
    fn begin_prog(&mut self, p: &mut toplevels::Prog) -> VisitorResult<(), toplevels::Prog> {
        self.pp.write_fmt(format_args!(
            "#lang ivy{}.{}\n\n",
            p.major_version, p.minor_version
        ))?;

        for decl in &mut p.top {
            decl.visit(self)?.modifying(decl)?;
            self.pp.write_str("\n")?;
        }
        Ok(ControlMut::SkipSiblings(()))
    }

    // Statements

    fn action_seq(
        &mut self,
        ast: &mut Vec<actions::Action>,
    ) -> VisitorResult<(), statements::Stmt> {
        for (i, a) in ast.iter_mut().enumerate() {
            if i > 0 {
                self.pp.write_str(";\n")?;
            }
            a.visit(self)?;
        }
        Ok(ControlMut::Produce(()))
    }

    fn begin_if(&mut self, ast: &mut statements::If) -> VisitorResult<(), statements::Stmt> {
        self.pp.write_str("if ")?;
        ast.tst.visit(self)?;
        self.pp.write_str(" {\n")?;

        self.write_separated(&mut ast.thn, ";\n")?;

        if let Some(stmts) = &mut ast.els {
            self.pp.write_str("} else {\n")?;
            self.write_separated(stmts, ";\n")?;
        }
        self.pp.write_str("}\n")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_while(&mut self, ast: &mut statements::While) -> VisitorResult<(), statements::Stmt> {
        self.pp.write_str("while ")?;
        ast.test.visit(self)?;

        self.pp.write_str(" {\n")?;
        self.write_separated(&mut ast.doit, ";\n")?;
        self.pp.write_str("}\n")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    // Actions

    fn begin_assert(
        &mut self,
        _ast: &mut actions::AssertAction,
    ) -> VisitorResult<(), actions::Action> {
        self.pp.write_str("assert ")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_assign(
        &mut self,
        ast: &mut actions::AssignAction,
    ) -> VisitorResult<(), actions::Action> {
        ast.lhs.visit(self)?;
        self.pp.write_str(" := ")?;
        ast.rhs.visit(self)?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_assume(
        &mut self,
        _ast: &mut actions::AssumeAction,
    ) -> VisitorResult<(), actions::Action> {
        self.pp.write_str("assume ")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_ensure(
        &mut self,
        _ast: &mut actions::EnsureAction,
    ) -> VisitorResult<(), actions::Action> {
        self.pp.write_str("ensure ")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_requires(
        &mut self,
        _ast: &mut actions::RequiresAction,
    ) -> VisitorResult<(), actions::Action> {
        self.pp.write_str("require ")?;
        Ok(ControlMut::Produce(()))
    }

    // Declarations

    fn begin_action_decl(
        &mut self,
        name: &mut Symbol,
        ast: &mut declarations::ActionDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        self.pp.write_fmt(format_args!("action {}", name))?;
        if ast.params.len() > 0 {
            self.pp.write_str("(")?;
            self.write_paramlist(&mut ast.params, ", ")?;
            self.pp.write_str(")")?;
        }

        if let Some(ret) = &mut ast.ret {
            self.pp.write_str(" returns(")?;
            self.param(ret)?.modifying(ret)?;
            self.pp.write_str(")")?;
        }
        if let Some(stmts) = &mut ast.body {
            self.pp.write_str(" = {\n")?;
            self.write_separated(stmts, ";\n")?;
            self.pp.write_str("\n}\n")?;
        }

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_after_decl(
        &mut self,
        ast: &mut declarations::AfterDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        self.pp.write_str("after ")?;
        self.identifier(&mut ast.name)?;

        if let Some(params) = &mut ast.params {
            if params.len() > 0 {
                self.pp.write_str("(")?;
                self.write_paramlist(params, ", ")?;
                self.pp.write_str(")")?;
            }
        }

        if let Some(ret) = &mut ast.ret {
            self.pp.write_str(" returns(")?;
            self.param(ret)?.modifying(ret)?;
            self.pp.write_str(")")?;
        }

        self.pp.write_str(" {\n")?;
        self.write_separated(&mut ast.body, ";\n")?;
        self.pp.write_str("\n}\n")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_alias_decl(
        &mut self,
        sym: &mut expressions::Symbol,
        s: &mut expressions::Sort,
    ) -> VisitorResult<(), declarations::Decl> {
        self.pp.write_fmt(format_args!("alias {} = ", sym))?;
        self.sort(s)?.modifying(s)?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_attribute_decl(
        &mut self,
        _ast: &mut expressions::Expr,
    ) -> VisitorResult<(), declarations::Decl> {
        self.pp.write_str("attribute ")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_axiom_decl(
        &mut self,
        _ast: &mut logic::Fmla,
    ) -> VisitorResult<(), declarations::Decl> {
        self.pp.write_str("axiom ")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_before_decl(
        &mut self,
        ast: &mut declarations::BeforeDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        self.pp.write_str("before ")?;
        self.identifier(&mut ast.name)?;

        if let Some(params) = &mut ast.params {
            if params.len() > 0 {
                self.pp.write_str("(")?;
                self.write_paramlist(params, ", ")?;
                self.pp.write_str(")")?;
            }
        }

        self.pp.write_str(" {\n")?;
        self.write_separated(&mut ast.body, ";\n")?;
        self.pp.write_str("\n}\n")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_export_decl(
        &mut self,
        _ast: &mut declarations::ExportDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        self.pp.write_str("export ")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_common_decl(
        &mut self,
        _ast: &mut Vec<declarations::Decl>,
    ) -> VisitorResult<(), declarations::Decl> {
        self.pp.write_str("common {")?;
        Ok(ControlMut::Produce(()))
    }
    fn finish_common_decl(
        &mut self,
        _ast: &mut Vec<declarations::Decl>,
    ) -> VisitorResult<(), declarations::Decl> {
        self.pp.write_str("}")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_global_decl(
        &mut self,
        ast: &mut Vec<declarations::Decl>,
    ) -> VisitorResult<(), declarations::Decl> {
        self.pp.write_str("global {\n")?;
        for decl in ast {
            decl.visit(self)?.modifying(decl)?;
            self.pp.write_str("\n")?;
        }
        self.pp.write_str("}")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_instance_decl(
        &mut self,
        name: &mut Symbol,
        ast: &mut declarations::InstanceDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        self.pp.write_str("instance ")?;
        name.visit(self)?.modifying(name)?;
        self.pp.write_str(" = ")?;
        ast.sort.visit(self)?.modifying(&mut ast.sort)?;
        if !ast.args.is_empty() {
            self.pp.write_str("(")?;
            ast.args.visit(self)?.modifying(&mut ast.args)?;
            self.pp.write_str(")")?;
        }
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_implement_decl(
        &mut self,
        ast: &mut declarations::ImplementDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        self.pp.write_str("implement ")?;
        self.identifier(&mut ast.name)?;

        if let Some(params) = &mut ast.params {
            self.pp.write_str("(")?;
            self.write_paramlist(params, ", ")?;
            self.pp.write_str(")")?;
        }

        if let Some(ret) = &mut ast.ret {
            self.pp.write_str(" returns(")?;
            self.param(ret)?.modifying(ret)?;
            self.pp.write_str(")")?;
        }

        if let Some(stmts) = &mut ast.body {
            self.pp.write_str(" {\n")?;
            self.write_separated(stmts, "\n")?;
            self.pp.write_str("\n}\n")?;
        }

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_import_decl(
        &mut self,
        ast: &mut declarations::ImportDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        self.pp
            .write_fmt(format_args!("import action {}(", ast.name))?;
        Ok(ControlMut::Produce(()))
    }
    fn finish_import_decl(
        &mut self,
        _ast: &mut declarations::ImportDecl,
        _n: (),
        _p: Vec<()>,
    ) -> VisitorResult<(), declarations::Decl> {
        self.pp.write_str(")")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_include_decl(
        &mut self,
        _ast: &mut expressions::Symbol,
    ) -> VisitorResult<(), declarations::Decl> {
        self.pp.write_str("include ")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_invariant_decl(
        &mut self,
        _ast: &mut logic::Fmla,
    ) -> VisitorResult<(), declarations::Decl> {
        self.pp.write_str("invariant ")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_process_decl(
        &mut self,
        name: &mut Symbol,
        inst: &mut declarations::ObjectDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        self.pp.write_fmt(format_args!("isolate {}", name))?;
        if inst.params.len() > 0 {
            self.pp.write_str("(")?;
            inst.params.visit(self)?;
            self.pp.write_str(")")?;
        }
        self.pp.write_str(" {\n")?;
        self.write_separated(&mut inst.body, "\n")?;
        self.pp.write_str("\n}\n")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_module_decl(
        &mut self,
        name: &mut Symbol,
        module: &mut declarations::ModuleDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        self.pp.write_fmt(format_args!("isolate {}", name))?;

        if module.sortsyms.len() > 0 {
            self.pp.write_str("(")?;
            module.sortsyms.visit(self)?;
            self.pp.write_str(")")?;
        }
        self.pp.write_str(" {\n")?;
        self.write_separated(&mut module.body, "\n")?;
        self.pp.write_str("\n}\n")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_object_decl(
        &mut self,
        name: &mut Symbol,
        ast: &mut declarations::ObjectDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        self.pp.write_fmt(format_args!("object {}", name))?;

        if ast.params.len() > 0 {
            self.pp.write_str("(")?;
            ast.params.visit(self)?;
            self.pp.write_str(")")?;
        }
        self.pp.write_str(" {\n")?;
        self.write_separated(&mut ast.body, "\n")?;
        self.pp.write_str("}\n")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_relation(
        &mut self,
        name: &mut Symbol,
        ast: &mut declarations::Relation,
    ) -> VisitorResult<(), declarations::Decl> {
        self.pp.write_fmt(format_args!("relation {}(", name))?;
        self.write_paramlist(&mut ast.params, ", ")?;
        self.pp.write_str(")")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_typedecl(
        &mut self,
        name: &mut Symbol,
        sort: &mut Sort,
    ) -> VisitorResult<(), declarations::Decl> {
        self.pp.write_fmt(format_args!("type {}", name))?;

        match sort {
            Sort::ToBeInferred => (),
            Sort::Annotated(_) | Sort::Resolved(_) => {
                self.pp.write_str(" = ")?;
                self.sort(sort)?.modifying(sort)?;
            }
        }
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_vardecl(
        &mut self,
        name: &mut Symbol,
        sort: &mut Sort,
    ) -> VisitorResult<(), declarations::Decl> {
        self.pp.write_fmt(format_args!("var {}", name))?;
        match sort {
            expressions::Sort::ToBeInferred => (),
            expressions::Sort::Annotated(_) | expressions::Sort::Resolved(_) => {
                self.pp.write_str(": ")?;
                self.sort(sort)?.modifying(sort)?;
            }
        };
        Ok(ControlMut::SkipSiblings(()))
    }

    // Quantifieds

    fn begin_forall(&mut self, fmla: &mut logic::Forall) -> VisitorResult<(), logic::Fmla> {
        self.pp.write_str("forall ")?;
        self.write_paramlist(&mut fmla.vars, ", ")?;
        self.pp.write_str(" . ")?;
        fmla.fmla.visit(self)?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_exists(&mut self, fmla: &mut logic::Exists) -> VisitorResult<(), logic::Fmla> {
        self.pp.write_str("exists ")?;
        self.write_paramlist(&mut fmla.vars, ", ")?;
        self.pp.write_str(" . ")?;
        fmla.fmla.visit(self)?;
        Ok(ControlMut::SkipSiblings(()))
    }

    // Expressions

    fn begin_app(
        &mut self,
        ast: &mut expressions::AppExpr,
    ) -> VisitorResult<(), expressions::Expr> {
        ast.func.visit(self)?;

        self.pp.write_str("(")?;
        self.write_separated(&mut ast.args, ", ")?;
        self.pp.write_str(")")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_binop(
        &mut self,
        ast: &mut expressions::BinOp,
    ) -> VisitorResult<(), expressions::Expr> {
        ast.lhs.visit(self)?;

        let op_str = match ast.op {
            Verb::Plus => "+",
            Verb::Minus => "-",
            Verb::Times => "*",
            Verb::Div => "/",
            Verb::Dot => ".",

            Verb::Equals => "=",
            Verb::Notequals => "~=",
            Verb::Lt => "<",
            Verb::Le => "<=",
            Verb::Gt => ">",
            Verb::Ge => ">=",

            Verb::Arrow => "->",

            Verb::And => "&",
            Verb::Or => "|",
            _ => {
                eprintln!("Uh oh!: {:?}", ast.op);
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

    fn begin_call(&mut self, ast: &mut expressions::AppExpr) -> VisitorResult<(), actions::Action> {
        self.begin_app(ast)?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_field_access(
        &mut self,
        lhs: &mut expressions::Expr,
        rhs: &mut expressions::AnnotatedSymbol,
    ) -> VisitorResult<(), expressions::Expr> {
        lhs.visit(self)?;
        self.pp.write_str(".")?;
        self.annotated_symbol(rhs)?.modifying(rhs)?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_index(&mut self, expr: &mut IndexExpr) -> VisitorResult<(), expressions::Expr> {
        expr.lhs.visit(self)?;
        self.pp.write_str("[")?;
        expr.idx.visit(self)?;
        self.pp.write_str("]")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_unary_op(
        &mut self,
        op: &mut Verb,
        rhs: &mut expressions::Expr,
    ) -> VisitorResult<(), expressions::Expr> {
        match op {
            Verb::Not => {
                self.pp.write_str("~")?;
                if let expressions::Expr::BinOp(_) = rhs {
                    self.pp.write_str("(")?;
                    rhs.visit(self)?.modifying(rhs)?;
                    self.pp.write_str(")")?;
                    Ok(ControlMut::SkipSiblings(()))
                } else {
                    Ok(ControlMut::Produce(()))
                }
            }
            _ => unimplemented!(),
        }
    }

    // Terminals

    fn annotated_symbol(
        &mut self,
        p: &mut expressions::AnnotatedSymbol,
    ) -> VisitorResult<(), expressions::AnnotatedSymbol> {
        p.id.visit(self)?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn boolean(&mut self, b: &mut bool) -> VisitorResult<(), bool> {
        if *b {
            self.pp.write_str("true")?;
        } else {
            self.pp.write_str("false")?;
        }
        Ok(ControlMut::Produce(()))
    }

    fn identifier(&mut self, i: &mut expressions::Ident) -> VisitorResult<(), expressions::Ident> {
        self.write_separated(i, ".")?;
        Ok(ControlMut::Produce(()))
    }

    fn number(&mut self, n: &mut i64) -> VisitorResult<(), i64> {
        self.pp.write_str(&n.to_string())?;
        Ok(ControlMut::Produce(()))
    }

    fn param(
        &mut self,
        p: &mut expressions::AnnotatedSymbol,
    ) -> VisitorResult<(), expressions::AnnotatedSymbol> {
        p.id.visit(self)?;

        match &mut p.sort {
            expressions::Sort::ToBeInferred => (),
            expressions::Sort::Annotated(_) | expressions::Sort::Resolved(_) => {
                self.pp.write_str(":")?;
                self.sort(&mut p.sort)?;
            }
        }
        Ok(ControlMut::SkipSiblings(()))
    }

    fn sort(&mut self, s: &mut Sort) -> VisitorResult<(), Sort> {
        match s {
            Sort::ToBeInferred => todo!(),
            Sort::Annotated(ident) => {
                self.identifier(ident)?;
            }
            Sort::Resolved(ivysort) => match ivysort {
                // These are inferred, usually, I suppose.
                IvySort::BitVec(width) => {
                    self.pp.write_fmt(format_args!("bv[{}]", width))?;
                }
                IvySort::Range(min, max) => {
                    self.pp.write_str("{")?;
                    min.visit(self)?;
                    self.pp.write_str("..")?;
                    max.visit(self)?;
                    self.pp.write_str("}")?;
                }
                IvySort::Enum(_) => {
                    self.pp.write_str(" = {")?;
                    //self.write_separated(branches, ", ")?;
                    self.pp.write_str(" }")?;
                }
                IvySort::Subclass(s) => {
                    self.pp.write_fmt(format_args!("of {}", s))?;
                }
                IvySort::Process(_proc) => {
                    self.pp.write_str("{\n")?;
                    self.pp.write_str("implementation {\n")?;
                    self.pp.write_str("}\n")?;
                    self.pp.write_str("specification {\n")?;
                    self.pp.write_str("common {\n")?;
                    self.pp.write_str("}\n")?;
                    self.pp.write_str("}\n")?;
                    self.pp.write_str("}")?;
                }
                IvySort::Uninterpreted => {}
                IvySort::Module(m) => {
                    self.pp.write_str(&m.name)?;
                }
                _ => {
                    println!("Uh oh! {:?}", s);
                    self.pp.write_str("???")?;
                }
            },
        };

        Ok(ControlMut::Produce(()))
    }

    fn symbol(&mut self, s: &mut expressions::Symbol) -> VisitorResult<(), expressions::Symbol> {
        self.pp.write_str(s)?;
        Ok(ControlMut::Produce(()))
    }

    fn this(&mut self) -> VisitorResult<(), expressions::Expr> {
        self.pp.write_str("this")?;
        Ok(ControlMut::Produce(()))
    }
}
