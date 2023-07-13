use std::fmt::Write;

use crate::{
    ast::{
        actions,
        expressions::{self, IndexExpr, Verb},
        statements, toplevels,
    },
    extraction::pprint::PrettyPrinter,
    visitor::visitor::Visitable,
};

use crate::visitor::*;

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
}

impl<W> Visitor<()> for Extractor<W>
where
    W: Write,
{
    fn begin_prog(&mut self, _ast: &mut toplevels::Prog) -> VisitorResult<(), toplevels::Prog> {
        let imports = include_str!("templates/imports.txt");
        self.pp.write_str(imports)?;

        self.pp.write_str("\npublic class Protocol {{")?;
        Ok(ControlMut::Produce(()))
    }
    fn finish_prog(&mut self, _ast: &mut toplevels::Prog) -> VisitorResult<(), toplevels::Prog> {
        self.pp.write_str("\n}")?;
        Ok(ControlMut::Produce(()))
    }

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
        self.pp.write_str("if (")?;
        ast.tst.visit(self)?;
        self.pp.write_str(") {\n")?;

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
            Verb::Lt => "<",
            Verb::Le => "<=",
            Verb::Gt => ">",
            Verb::Ge => ">=",

            Verb::Arrow => "->",

            Verb::And => "&",
            Verb::Or => "&",
            _ => {
                unimplemented!()
            }
        };
        self.pp.write_fmt(format_args!(" {} ", op_str))?;
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
        self.pp.write_fmt(format_args!(".{}", rhs.id))?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_index(&mut self, expr: &mut IndexExpr) -> VisitorResult<(), expressions::Expr> {
        expr.lhs.visit(self)?;
        self.pp.write_str("[")?;
        expr.idx.visit(self)?;
        self.pp.write_str("]")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    // Terminals

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
        Ok(ControlMut::Produce(()))
    }

    fn sort(&mut self, _s: &mut expressions::Sort) -> VisitorResult<(), expressions::Sort> {
        todo!()
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
