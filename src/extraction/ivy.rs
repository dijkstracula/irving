use std::fmt::Write;

use crate::{
    ast::{
        actions,
        declarations::{self, Binding},
        expressions::{self, IndexExpr, Sort, Symbol, Token, Verb},
        logic,
        span::Span,
        statements, toplevels,
    },
    typechecker::sorts::IvySort,
    visitor::ast::Visitor,
};

use crate::visitor::ast::Visitable;

use crate::visitor::*;

use super::{pprint::PrettyPrinter, ExtractResult};

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
    ) -> ExtractResult<Vec<Symbol>> {
        for (i, u) in us.iter_mut().enumerate() {
            if i > 0 {
                self.pp.write_str(sep)?;
            }
            self.param(u)?;
        }
        Ok(ControlMut::Produce(()))
    }
}

impl<W> ast::Visitor<(), std::fmt::Error> for Extractor<W>
where
    W: Write,
{
    fn begin_prog(&mut self, p: &mut toplevels::Prog) -> ExtractResult<toplevels::Prog> {
        self.pp.write_fmt(format_args!(
            "#lang ivy{}.{}\n\n",
            p.major_version, p.minor_version
        ))?;

        let _ = p.top.body.visit(self)?.modifying(&mut p.top.body);
        Ok(ControlMut::SkipSiblings(()))
    }

    // Statements

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
        self.pp.write_str("if ")?;
        ast.tst.visit(self)?;
        self.pp.write_str(" {\n")?;

        for stmt in &mut ast.thn {
            stmt.visit(self)?.modifying(stmt);
            self.pp.write_str(";\n")?;
        }

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
        self.write_separated(&mut ast.doit, ";\n")?;
        self.pp.write_str("}\n")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    // Actions

    fn begin_assert(&mut self, _ast: &mut actions::AssertAction) -> ExtractResult<actions::Action> {
        self.pp.write_str("assert ")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_assign_logical(
        &mut self,
        _span: &Span,
        ast: &mut actions::AssignLogicalAction,
    ) -> VisitorResult<(), std::fmt::Error, actions::Action> {
        ast.lhs.visit(self)?;
        self.pp.write_str(" := ")?;
        ast.rhs.visit(self)?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_assign(
        &mut self,
        _span: &Span,
        ast: &mut actions::AssignAction,
    ) -> ExtractResult<actions::Action> {
        ast.lhs.visit(self)?;
        self.pp.write_str(" := ")?;
        ast.rhs.visit(self)?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_assume(&mut self, _ast: &mut actions::AssumeAction) -> ExtractResult<actions::Action> {
        self.pp.write_str("assume ")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_ensure(&mut self, _ast: &mut actions::EnsureAction) -> ExtractResult<actions::Action> {
        self.pp.write_str("ensure ")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_requires(
        &mut self,
        _ast: &mut actions::RequiresAction,
    ) -> ExtractResult<actions::Action> {
        self.pp.write_str("require ")?;
        Ok(ControlMut::Produce(()))
    }

    // Declarations

    fn begin_action_decl(
        &mut self,
        _span: &Span,
        name: &mut Token,
        ast: &mut declarations::ActionDecl,
    ) -> ExtractResult<declarations::Decl> {
        self.pp.write_fmt(format_args!("action {}", name))?;
        if !ast.params.is_empty() {
            self.pp.write_str("(")?;
            self.write_paramlist(&mut ast.params, ", ")?;
            self.pp.write_str(")")?;
        }

        if let Some(ret) = &mut ast.ret {
            self.pp.write_str(" returns(")?;
            self.param(ret)?.modifying(ret);
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
        _span: &Span,
        ast: &mut declarations::ActionMixinDecl,
    ) -> ExtractResult<declarations::Decl> {
        self.pp.write_str("after ")?;
        self.identifier(&mut ast.name)?;

        if let Some(params) = &mut ast.params {
            if !params.is_empty() {
                self.pp.write_str("(")?;
                self.write_paramlist(params, ", ")?;
                self.pp.write_str(")")?;
            }
        }

        if let Some(ret) = &mut ast.ret {
            self.pp.write_str(" returns(")?;
            self.param(ret)?.modifying(ret);
            self.pp.write_str(")")?;
        }

        self.pp.write_str(" {\n")?;
        self.write_separated(&mut ast.body, ";\n")?;
        self.pp.write_str("\n}\n")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_alias_decl(
        &mut self,
        sym: &mut expressions::Token,
        s: &mut expressions::Sort,
    ) -> ExtractResult<declarations::Decl> {
        self.pp.write_fmt(format_args!("alias {} = ", sym))?;
        self.sort(s)?.modifying(s);
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_attribute_decl(
        &mut self,
        _ast: &mut expressions::Expr,
    ) -> ExtractResult<declarations::Decl> {
        self.pp.write_str("attribute ")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_axiom_decl(&mut self, _ast: &mut logic::Fmla) -> ExtractResult<declarations::Decl> {
        self.pp.write_str("axiom ")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_before_decl(
        &mut self,
        ast: &mut declarations::ActionMixinDecl,
    ) -> ExtractResult<declarations::Decl> {
        self.pp.write_str("before ")?;
        self.identifier(&mut ast.name)?;

        if let Some(params) = &mut ast.params {
            if !params.is_empty() {
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

    fn begin_class_decl(
        &mut self,
        _span: &Span,
        name: &mut Token,
        ast: &mut declarations::ClassDecl,
    ) -> VisitorResult<(), std::fmt::Error, declarations::Decl> {
        match &ast.parent {
            None => self.pp.write_fmt(format_args!("class {} = {{\n", name))?,
            Some(parent) => self
                .pp
                .write_fmt(format_args!("subclass {} of {} = {{\n", name, parent))?,
        };

        for Binding { name, decl, .. } in &mut ast.fields {
            self.pp.write_fmt(format_args!("field {}: ", name))?;
            decl.visit(self)?.modifying(decl);
            self.pp.write_str("\n")?;
        }
        self.pp.write_str("\n")?;
        for Binding { name, decl, span } in &mut ast.actions {
            self.begin_action_decl(span, name, decl)?;
            // In this case, begin_action_decl returns SkipSiblings so there
            // is nothing else we need to do.
        }
        self.pp.write_str("\n}\n")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_export_decl(
        &mut self,
        _ast: &mut declarations::ExportDecl,
    ) -> ExtractResult<declarations::Decl> {
        self.pp.write_str("export ")?;
        Ok(ControlMut::Produce(()))
    }

    fn finish_export_decl(
        &mut self,
        _ast: &mut declarations::ExportDecl,
    ) -> VisitorResult<(), std::fmt::Error, declarations::Decl> {
        self.pp.write_str("\n")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_common_decl(
        &mut self,
        _ast: &mut Vec<declarations::Decl>,
    ) -> ExtractResult<declarations::Decl> {
        self.pp.write_str("common {")?;
        Ok(ControlMut::Produce(()))
    }
    fn finish_common_decl(
        &mut self,
        _ast: &mut Vec<declarations::Decl>,
    ) -> ExtractResult<declarations::Decl> {
        self.pp.write_str("}")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_global_decl(
        &mut self,
        ast: &mut Vec<declarations::Decl>,
    ) -> ExtractResult<declarations::Decl> {
        self.pp.write_str("global {\n")?;
        for decl in ast {
            decl.visit(self)?.modifying(decl);
            self.pp.write_str("\n")?;
        }
        self.pp.write_str("}")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_instance_decl(
        &mut self,
        name: &mut Token,
        ast: &mut declarations::InstanceDecl,
    ) -> ExtractResult<declarations::Decl> {
        self.pp.write_str("instance ")?;
        name.visit(self)?.modifying(name);
        self.pp.write_str(" = ")?;
        ast.sort.visit(self)?.modifying(&mut ast.sort);
        if !ast.args.is_empty() {
            self.pp.write_str("(")?;
            ast.args.visit(self)?.modifying(&mut ast.args);
            self.pp.write_str(")")?;
        }
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_implement_decl(
        &mut self,
        ast: &mut declarations::ActionMixinDecl,
    ) -> ExtractResult<declarations::Decl> {
        self.pp.write_str("implement ")?;
        self.identifier(&mut ast.name)?;

        if let Some(params) = &mut ast.params {
            self.pp.write_str("(")?;
            self.write_paramlist(params, ", ")?;
            self.pp.write_str(")")?;
        }

        if let Some(ret) = &mut ast.ret {
            self.pp.write_str(" returns(")?;
            self.param(ret)?.modifying(ret);
            self.pp.write_str(")")?;
        }

        self.pp.write_str(" {\n")?;
        self.write_separated(&mut ast.body, "\n")?;
        self.pp.write_str("\n}\n")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_import_decl(
        &mut self,
        ast: &mut declarations::ImportDecl,
    ) -> ExtractResult<declarations::Decl> {
        self.pp
            .write_fmt(format_args!("import action {}(", ast.name))?;
        self.pp.write_str(")")?;
        Ok(ControlMut::Produce(()))
    }
    fn finish_import_decl(
        &mut self,
        _span: &Span,
        _ast: &mut declarations::ImportDecl,
        _n: (),
        _p: Vec<()>,
    ) -> ExtractResult<declarations::Decl> {
        self.pp.write_str(")")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_include_decl(&mut self, _ast: &mut expressions::Token) -> ExtractResult<Token> {
        self.pp.write_str("include ")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_interpret_decl(
        &mut self,
        name: &mut Token,
        sort: &mut Sort,
    ) -> ExtractResult<declarations::Decl> {
        self.pp.write_fmt(format_args!("interpret {name} -> "))?;
        sort.visit(self)?.modifying(sort);
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_invariant_decl(
        &mut self,
        _ast: &mut logic::Fmla,
    ) -> ExtractResult<declarations::Decl> {
        self.pp.write_str("invariant ")?;
        Ok(ControlMut::Produce(()))
    }
    fn finish_invariant_decl(
        &mut self,
        _ast: &mut logic::Fmla,
    ) -> VisitorResult<(), std::fmt::Error, declarations::Decl> {
        self.pp.write_str("\n")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_map_decl(
        &mut self,
        _span: &Span,
        name: &mut Token,
        ast: &mut declarations::MapDecl,
    ) -> VisitorResult<(), std::fmt::Error, declarations::Decl> {
        self.pp.write_fmt(format_args!("var {} = (", name))?;
        self.write_paramlist(&mut ast.domain, ",")?;
        self.pp.write_str(")\n")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_module_decl(
        &mut self,
        _span: &Span,
        name: &mut Token,
        module: &mut declarations::ModuleDecl,
    ) -> ExtractResult<declarations::Decl> {
        self.pp.write_fmt(format_args!("isolate {}", name))?;

        if !module.sortsyms.is_empty() {
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
        _span: &Span,
        name: &mut Token,
        ast: &mut declarations::ObjectDecl,
    ) -> ExtractResult<declarations::Decl> {
        self.pp.write_fmt(format_args!("object {}", name))?;

        if !ast.params.is_empty() {
            self.pp.write_str("(")?;
            ast.params.visit(self)?;
            self.pp.write_str(")")?;
        }
        self.pp.write_str(" {\n")?;
        for decl in &mut ast.body {
            decl.visit(self)?.modifying(decl);
            self.pp.write_str("\n")?;
        }
        self.pp.write_str("}\n")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_typedecl(
        &mut self,
        _span: &Span,
        name: &mut Token,
        sort: &mut Sort,
    ) -> ExtractResult<declarations::Decl> {
        self.pp.write_fmt(format_args!("type {}", name))?;

        match sort {
            Sort::ToBeInferred | Sort::Resolved(IvySort::SortVar(_)) => (),
            Sort::Annotated(_) | Sort::Resolved(_) => {
                self.pp.write_str(" = ")?;
                self.sort(sort)?.modifying(sort);
            }
        }
        self.pp.write_str("\n")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_vardecl(
        &mut self,
        _span: &Span,
        name: &mut Token,
        sort: &mut Sort,
    ) -> ExtractResult<declarations::Decl> {
        self.pp.write_fmt(format_args!("var {}", name))?;
        match sort {
            expressions::Sort::ToBeInferred | expressions::Sort::Resolved(IvySort::SortVar(_)) => {
                ()
            }
            expressions::Sort::Annotated(_) | expressions::Sort::Resolved(_) => {
                self.pp.write_str(": ")?;
                self.sort(sort)?.modifying(sort);
            }
        };
        self.pp.write_str("\n")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    // Quantifieds

    fn begin_forall(&mut self, fmla: &mut logic::Forall) -> ExtractResult<logic::Fmla> {
        self.pp.write_str("forall ")?;
        self.write_paramlist(&mut fmla.vars, ", ")?;
        self.pp.write_str(" . ")?;
        fmla.fmla.visit(self)?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_exists(&mut self, fmla: &mut logic::Exists) -> ExtractResult<logic::Fmla> {
        self.pp.write_str("exists ")?;
        self.write_paramlist(&mut fmla.vars, ", ")?;
        self.pp.write_str(" . ")?;
        fmla.fmla.visit(self)?;
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

    fn begin_logical_binop(
        &mut self,
        ast: &mut logic::LogicBinOp,
    ) -> VisitorResult<(), std::fmt::Error, logic::Fmla> {
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

            Verb::And => "&",
            Verb::Or => "|",
            Verb::Arrow => "->",
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

    fn begin_logical_field_access(
        &mut self,
        lhs: &mut logic::Fmla,
        rhs: &mut Symbol,
    ) -> VisitorResult<(), std::fmt::Error, logic::Fmla> {
        lhs.visit(self)?;
        self.pp.write_str(".")?;
        self.symbol(lhs.span(), rhs)?.modifying(rhs);
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

    fn begin_binop(&mut self, ast: &mut expressions::BinOp) -> ExtractResult<expressions::Expr> {
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

    fn begin_call(&mut self, ast: &mut expressions::AppExpr) -> ExtractResult<actions::Action> {
        self.begin_app(ast)?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_field_access(
        &mut self,
        lhs: &mut expressions::Expr,
        rhs: &mut expressions::Symbol,
    ) -> ExtractResult<expressions::Expr> {
        lhs.visit(self)?;
        self.pp.write_str(".")?;
        self.symbol(lhs.span(), rhs)?.modifying(rhs);
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_index(&mut self, expr: &mut IndexExpr) -> ExtractResult<expressions::Expr> {
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
    ) -> ExtractResult<expressions::Expr> {
        match op {
            Verb::Not => {
                self.pp.write_str("~")?;
                if let expressions::Expr::BinOp { .. } = rhs {
                    self.pp.write_str("(")?;
                    rhs.visit(self)?.modifying(rhs);
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

    fn symbol(
        &mut self,
        _span: &Span,
        p: &mut expressions::Symbol,
    ) -> ExtractResult<expressions::Symbol> {
        p.name.visit(self)?;

        Ok(ControlMut::SkipSiblings(()))
    }

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
        p.name.visit(self)?;

        match &mut p.decl {
            expressions::Sort::ToBeInferred | expressions::Sort::Resolved(IvySort::SortVar(_)) => {
                // In these cases, it's still uninterpreted, I think.
                self.pp.write_str(":???")?;
            }
            expressions::Sort::Annotated(_) | expressions::Sort::Resolved(_) => {
                self.pp.write_str(":")?;
                self.sort(&mut p.decl)?;
            }
        }
        Ok(ControlMut::SkipSiblings(()))
    }

    fn sort(&mut self, s: &mut Sort) -> ExtractResult<Sort> {
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
                IvySort::Bool => {
                    self.pp.write_str("bool")?;
                }
                IvySort::Range(min, max) => {
                    self.pp.write_str("{")?;
                    self.number(&Span::Todo, min)?;
                    self.pp.write_str("..")?;
                    self.number(&Span::Todo, max)?;
                    self.pp.write_str("}")?;
                }
                IvySort::Enum(_) => {
                    self.pp.write_str(" = {")?;
                    //self.write_separated(branches, ", ")?;
                    self.pp.write_str(" }")?;
                }
                IvySort::Number => {
                    self.pp.write_str("nat")?;
                }
                IvySort::Object(_proc) => {
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
                    self.pp.write_fmt(format_args!("???{:?}???", s))?;
                }
            },
        };

        Ok(ControlMut::Produce(()))
    }

    fn token(&mut self, s: &mut expressions::Token) -> ExtractResult<expressions::Token> {
        self.pp.write_str(s)?;
        Ok(ControlMut::Produce(()))
    }

    fn this(&mut self) -> ExtractResult<expressions::Expr> {
        self.pp.write_str("this")?;
        Ok(ControlMut::Produce(()))
    }
}
