use crate::{
    ast::declarations::{self, ActionDecl, Binding, ExportDecl},
    extraction::java::extraction::expressions::Symbol,
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
    type_aliases: BTreeMap<Symbol, expressions::Sort>,
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
    ) -> VisitorResult<(), Vec<expressions::AnnotatedSymbol>> {
        for (i, u) in us.into_iter().enumerate() {
            if i > 0 {
                self.pp.write_str(sep)?;
            }
            self.param(u)?;
        }
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

impl<W> ast::Visitor<()> for Extractor<W>
where
    W: Write,
{
    fn begin_prog(&mut self, ast: &mut toplevels::Prog) -> VisitorResult<(), toplevels::Prog> {
        let imports = include_str!("templates/imports.txt");
        self.pp.write_str(imports)?;
        self.pp.write_str("\n\n")?;

        for decl in &mut ast.top {
            decl.visit(self)?.modifying(decl)?;
            self.pp.write_str("\n")?;
        }
        Ok(ControlMut::SkipSiblings(()))
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
        for stmt in &mut ast.doit {
            stmt.visit(self)?.modifying(stmt)?;
            self.pp.write_str(";\n")?;
        }
        self.pp.write_str("}\n")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    // Declarations

    fn begin_action_decl(
        &mut self,
        name: &mut Symbol,
        ast: &mut declarations::ActionDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        name.visit(self)?.modifying(name)?;
        self.pp.write_str(".on((")?;

        self.write_paramlist(&mut ast.params, ",")?;
        self.pp.write_str(") -> {\n")?;
        for stmt in &mut ast.body {
            stmt.visit(self)?.modifying(stmt)?;
            self.pp.write_str(";\n")?;
        }
        self.pp.write_str("})")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_after_decl(
        &mut self,
        ast: &mut declarations::AfterDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        ast.name.visit(self)?.modifying(&mut ast.name)?;
        self.pp.write_str(".onAfter((")?;

        if let Some(params) = &mut ast.params {
            self.write_paramlist(params, ",")?;
            // XXX: also the return value needs to be bound.
        }
        self.pp.write_str(") -> {\n")?;
        for stmt in &mut ast.body {
            stmt.visit(self)?.modifying(stmt)?;
            self.pp.write_str(";\n")?;
        }
        self.pp.write_str("})")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_alias_decl(
        &mut self,
        sym: &mut expressions::Symbol,
        e: &mut expressions::Sort,
    ) -> VisitorResult<(), declarations::Decl> {
        self.type_aliases.insert(sym.clone(), e.clone());
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_before_decl(
        &mut self,
        ast: &mut declarations::BeforeDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        ast.name.visit(self)?.modifying(&mut ast.name)?;
        self.pp.write_str(".addBefore((")?;

        if let Some(params) = &mut ast.params {
            self.write_paramlist(params, ",")?;
            // XXX: also the return value needs to be bound.
        }
        self.pp.write_str(") -> {\n")?;
        for stmt in &mut ast.body {
            stmt.visit(self)?.modifying(stmt)?;
            self.pp.write_str(";\n")?;
        }
        self.pp.write_str("})")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_implement_decl(
        &mut self,
        ast: &mut declarations::ImplementDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        ast.name.visit(self)?.modifying(&mut ast.name)?;
        self.pp.write_str(".on((")?;

        match &mut ast.params {
            None => unreachable!(),
            Some(params) => self.write_paramlist(params, ",")?,
        };

        self.pp.write_str(") -> {\n")?;
        for stmt in &mut ast.body {
            stmt.visit(self)?.modifying(stmt)?;
            self.pp.write_str(";\n")?;
        }
        self.pp.write_str("})")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_import_decl(
        &mut self,
        ast: &mut declarations::ImportDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        self.pp.write_str("private void ")?;
        self.pp.write_fmt(format_args!("{}(", ast.name))?;

        for param in &mut ast.params {
            self.param(param)?.modifying(param)?;
        }
        self.pp.write_str(") {\n")?;

        self.pp.write_str("System.out.println(\"")?;
        self.pp.write_str("\");")?;

        self.pp.write_str("\n}")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_instance_decl(
        &mut self,
        name: &mut Symbol,
        ast: &mut declarations::InstanceDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        self.pp.write_fmt(format_args!("class {} extends ", name))?;
        ast.sort.visit(self)?.modifying(&mut ast.sort)?;

        if !ast.args.is_empty() {
            self.pp.write_str("<")?;

            // maybe slightly confusing: because we parameterise modules on their sorts,
            // the `id` field contains the identifier for sort, not the `sort` field.
            let mut sorts = ast.args.iter().map(|a| a.id.clone()).collect::<Vec<_>>();
            self.write_separated(&mut sorts, ",")?;
            self.pp.write_str(">")?;
        }

        Ok(ControlMut::SkipSiblings(()))
    }
    fn begin_isolate_decl(
        &mut self,
        name: &mut expressions::Symbol,
        ast: &mut declarations::IsolateDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        self.pp
            .write_fmt(format_args!("public class {name} {{\n\n"))?;

        // fields
        for f in &mut ast.fields {
            f.visit(self)?.modifying(f)?;
            self.pp.write_str("\n")?;
        }

        // Action object declarations
        for decl in &mut ast.body {
            match decl {
                declarations::Decl::Export(ExportDecl::Action(Binding {
                    name,
                    decl: ActionDecl { params, ret, body },
                }))
                | declarations::Decl::Action(Binding {
                    name,
                    decl: ActionDecl { params, ret, body },
                }) => {
                    let mut sorts = params.iter().map(|a| a.sort.clone()).collect::<Vec<_>>();
                    if let Some(ret) = ret {
                        sorts.push(ret.sort.clone());
                    }
                    self.pp
                        .write_fmt(format_args!("public Action{}<", sorts.len()))?;
                    self.write_separated(&mut sorts, ",")?;
                    self.pp
                        .write_fmt(format_args!("> {name} = new Action{}<>();\n", sorts.len()))?;
                }
                _ => (),
            }
        }

        // Constructor
        self.pp.write_fmt(format_args!("public {name}("))?;
        self.write_paramlist(&mut ast.params, ", ")?;
        self.pp.write_fmt(format_args!(") {{\n"))?;

        for param in &ast.params {
            self.pp
                .write_fmt(format_args!("this.{} = {};\n", param.id, param.id))?;
        }

        self.pp.write_str("\n")?;
        for decl in ast
            .body
            .iter_mut()
            .filter(|d| d.name_for_binding().is_none())
        {
            decl.visit(self)?.modifying(decl)?;
            self.pp.write_str("\n")?;
        }
        self.pp.write_str("}\n\n")?;

        // Declarations
        for decl in &mut ast.body {
            decl.visit(self)?.modifying(decl)?;
            self.pp.write_str("\n")?;
        }
        self.pp.write_str("}\n\n")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_typedecl(
        &mut self,
        name: &mut Symbol,
        s: &mut expressions::Sort,
    ) -> VisitorResult<(), declarations::Decl> {
        self.type_aliases.insert(name.clone(), s.clone());
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_vardecl(
        &mut self,
        name: &mut Symbol,
        sort: &mut expressions::Sort,
    ) -> VisitorResult<(), declarations::Decl> {
        self.pp.write_fmt(format_args!(
            "private {} ",
            Self::jtype_from_sort(sort).as_jval()
        ))?;
        name.visit(self)?.modifying(name)?;
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

    fn begin_assign(
        &mut self,
        ast: &mut actions::AssignAction,
    ) -> VisitorResult<(), actions::Action> {
        ast.lhs.visit(self)?;
        self.pp.write_str(" = ")?;
        ast.rhs.visit(self)?;

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

    fn begin_object_decl(
        &mut self,
        name: &mut Symbol,
        ast: &mut declarations::ObjectDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        self.pp
            .write_fmt(format_args!("Object {name} = new Object"))?;
        self.pp.write_str(" {\n")?;
        self.write_separated(&mut ast.body, "\n")?;
        self.pp.write_str("\n}")?;
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
        self.sort(&mut p.sort)?;
        self.pp.write_str(" ")?;
        self.pp.write_str(&p.id)?;
        Ok(ControlMut::Produce(()))
    }

    fn sort(&mut self, s: &mut expressions::Sort) -> VisitorResult<(), expressions::Sort> {
        match s {
            expressions::Sort::ToBeInferred => (),
            expressions::Sort::Annotated(ident) => {
                ident.visit(self)?.modifying(ident)?;
            }
            expressions::Sort::Resolved(ivysort) => {
                let j: JavaType = ivysort.clone().into(); // XXX: poor choices lead to this clone.a
                self.pp.write_str(j.as_jref().as_str())?;
            }
        }
        Ok(ControlMut::Produce(()))
    }

    fn symbol(&mut self, s: &mut expressions::Symbol) -> VisitorResult<(), expressions::Symbol> {
        let alias = self.type_aliases.get_mut(s);
        match alias {
            None => self.pp.write_str(s)?,
            Some(sort) => {
                let mut s2 = sort.clone();
                self.sort(&mut s2)?;
            }
        }
        Ok(ControlMut::Produce(()))
    }

    fn this(&mut self) -> VisitorResult<(), expressions::Expr> {
        self.pp.write_str("this")?;
        Ok(ControlMut::Produce(()))
    }
}

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum JExtractionError {
    #[error("Symbol {0:?} failed to have a type inferred (did the typechecking pass run?)")]
    UnresolvedType(expressions::Symbol),
}
