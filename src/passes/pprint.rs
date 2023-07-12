use core::fmt::Result;
use std::fmt::Write;

use crate::{
    ast::{
        actions, declarations,
        expressions::{self, IndexExpr, Verb},
        logic, statements, toplevels,
    },
    typechecker::sorts::IvySort,
    visitor::visitor::Visitable,
};

use crate::visitor::*;

pub struct PrettyPrinter<W>
where
    W: Write,
{
    pub out: W,
    indent: usize,
    curr_line_is_indented: bool,
}

impl<W> PrettyPrinter<W>
where
    W: Write,
{
    fn write_separated<U>(&mut self, us: &mut Vec<U>, sep: &str) -> VisitorResult<(), Vec<U>>
    where
        U: Visitable<()>,
    {
        for (i, u) in us.into_iter().enumerate() {
            if i > 0 {
                self.write_str(sep)?;
            }
            u.visit(self)?;
        }
        Ok(ControlMut::Produce(()))
    }
}

impl PrettyPrinter<String> {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self {
            out: String::new(),
            indent: 0,
            curr_line_is_indented: false,
        }
    }
}

impl<W: Write> Write for PrettyPrinter<W> {
    fn write_str(&mut self, s: &str) -> Result {
        // TODO: can I do this without allocation?
        let lines = s.split("\n").enumerate();

        for (i, line) in lines {
            if i > 0 {
                self.out.write_str("\n")?;
                self.curr_line_is_indented = false;
            }

            println!("{i} \"{line}\" {}", self.indent);
            self.indent -= line.matches("}").count();
            self.indent -= line.matches(")").count();
            if line.len() > 0 && !self.curr_line_is_indented {
                let indent = std::iter::repeat(" ")
                    .take(4 * self.indent)
                    .collect::<String>();
                self.out.write_str(&indent)?;
                self.curr_line_is_indented = true;
            }
            self.out.write_str(line)?;

            self.indent += line.matches("{").count();
            self.indent += line.matches("(").count();
        }
        Ok(())
    }

    fn write_fmt(&mut self, args: std::fmt::Arguments<'_>) -> Result {
        // XXX: ugh.
        let mut buf = String::new();
        buf.write_fmt(args)?;
        self.write_str(&buf)
    }
}

impl<W: Write> Visitor<()> for PrettyPrinter<W> {
    fn begin_prog(&mut self, p: &mut toplevels::Prog) -> VisitorResult<(), toplevels::Prog> {
        self.write_fmt(format_args!(
            "#lang ivy{}.{}\n\n",
            p.major_version, p.minor_version
        ))?;
        for top in &mut p.top {
            top.visit(self)?.modifying(top)?;
            self.write_str("\n")?;
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
                self.write_str(";\n")?;
            }
            a.visit(self)?;
        }
        Ok(ControlMut::Produce(()))
    }

    fn begin_if(&mut self, ast: &mut statements::If) -> VisitorResult<(), statements::Stmt> {
        self.write_str("if ")?;
        ast.tst.visit(self)?;
        self.write_str(" {\n")?;

        self.write_separated(&mut ast.thn, ";\n")?;

        if let Some(stmts) = &mut ast.els {
            self.write_str("} else {\n")?;
            self.write_separated(stmts, ";\n")?;
        }
        self.write_str("\n}\n")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_while(&mut self, ast: &mut statements::While) -> VisitorResult<(), statements::Stmt> {
        self.write_str("while ")?;
        ast.test.visit(self)?;

        self.write_str(" {\n")?;
        self.write_separated(&mut ast.doit, ";\n")?;
        self.write_str("}\n")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    // Actions

    fn begin_assert(
        &mut self,
        _ast: &mut actions::AssertAction,
    ) -> VisitorResult<(), actions::Action> {
        self.write_str("assert ")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_assign(
        &mut self,
        ast: &mut actions::AssignAction,
    ) -> VisitorResult<(), actions::Action> {
        println!("{:?}", ast);
        ast.lhs.visit(self)?;
        self.write_str(" := ")?;
        ast.rhs.visit(self)?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_assume(
        &mut self,
        _ast: &mut actions::AssumeAction,
    ) -> VisitorResult<(), actions::Action> {
        self.write_str("assume ")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_ensure(
        &mut self,
        _ast: &mut actions::EnsureAction,
    ) -> VisitorResult<(), actions::Action> {
        self.write_str("ensure ")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_requires(
        &mut self,
        _ast: &mut actions::RequiresAction,
    ) -> VisitorResult<(), actions::Action> {
        self.write_str("require ")?;
        Ok(ControlMut::Produce(()))
    }

    // Declarations

    fn begin_action_decl(
        &mut self,
        ast: &mut declarations::ActionDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        self.write_fmt(format_args!("action {}", ast.name))?;
        if ast.params.len() > 0 {
            self.write_str("(")?;
            self.write_separated(&mut ast.params, ", ")?;
            self.write_str(")")?;
        }

        if let Some(ret) = &mut ast.ret {
            self.write_str(" returns(")?;
            ret.visit(self)?;
            self.write_str(")")?;
        }
        if let Some(stmts) = &mut ast.body {
            self.write_str(" = {\n")?;
            println!("{:?}", stmts);
            self.write_separated(stmts, ";\n")?;
            self.write_str("\n}\n")?;
        }

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_after_decl(
        &mut self,
        ast: &mut declarations::AfterDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        self.write_str("after ")?;
        self.identifier(&mut ast.name)?;

        if let Some(params) = &mut ast.params {
            if params.len() > 0 {
                self.write_str("(")?;
                self.write_separated(params, ", ")?;
                self.write_str(")")?;
            }
        }

        if let Some(ret) = &mut ast.ret {
            self.write_str(" returns(")?;
            ret.visit(self)?;
            self.write_str(")")?;
        }

        self.write_str(" {\n")?;
        self.write_separated(&mut ast.body, ";\n")?;
        self.write_str("\n}\n")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_alias_decl(
        &mut self,
        sym: &mut expressions::Symbol,
        e: &mut expressions::Expr,
    ) -> VisitorResult<(), declarations::Decl> {
        self.write_fmt(format_args!("alias {} = ", sym))?;
        e.visit(self)?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_attribute_decl(
        &mut self,
        _ast: &mut expressions::Expr,
    ) -> VisitorResult<(), declarations::Decl> {
        self.write_str("attribute ")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_axiom_decl(
        &mut self,
        _ast: &mut logic::Fmla,
    ) -> VisitorResult<(), declarations::Decl> {
        self.write_str("axiom ")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_before_decl(
        &mut self,
        ast: &mut declarations::BeforeDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        self.write_str("before ")?;
        self.identifier(&mut ast.name)?;

        if let Some(params) = &mut ast.params {
            if params.len() > 0 {
                self.write_str("(")?;
                self.write_separated(params, ", ")?;
                self.write_str(")")?;
            }
        }

        self.write_str(" {\n")?;
        self.write_separated(&mut ast.body, ";\n")?;
        self.write_str("\n}\n")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_export_decl(
        &mut self,
        _ast: &mut declarations::ExportDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        self.write_str("export ")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_common_decl(
        &mut self,
        _ast: &mut Vec<declarations::Decl>,
    ) -> VisitorResult<(), declarations::Decl> {
        self.write_str("common {")?;
        Ok(ControlMut::Produce(()))
    }
    fn finish_common_decl(
        &mut self,
        _ast: &mut Vec<declarations::Decl>,
    ) -> VisitorResult<(), declarations::Decl> {
        self.write_str("}")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_global_decl(
        &mut self,
        ast: &mut Vec<declarations::Decl>,
    ) -> VisitorResult<(), declarations::Decl> {
        self.write_str("global {\n")?;
        for decl in ast {
            decl.visit(self)?.modifying(decl)?;
            self.write_str("\n")?;
        }
        self.write_str("}")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_instance_decl(
        &mut self,
        ast: &mut declarations::InstanceDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        self.write_str("instance ")?;
        ast.name.visit(self)?.modifying(&mut ast.name)?;
        self.write_str(" = ")?;
        ast.sort.visit(self)?.modifying(&mut ast.sort)?;
        if !ast.args.is_empty() {
            self.write_str("(")?;
            ast.args.visit(self)?.modifying(&mut ast.args)?;
            self.write_str(")")?;
        }
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_implement_decl(
        &mut self,
        ast: &mut declarations::ImplementDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        self.write_str("implement ")?;
        self.identifier(&mut ast.name)?;

        if let Some(params) = &mut ast.params {
            self.write_str("(")?;
            self.write_separated(params, ", ")?;
            self.write_str(")")?;
        }

        if let Some(ret) = &mut ast.ret {
            self.write_str(" returns(")?;
            ret.visit(self)?;
            self.write_str(")")?;
        }

        if let Some(stmts) = &mut ast.body {
            self.write_str(" {\n")?;
            self.write_separated(stmts, "\n")?;
            self.write_str("\n}\n")?;
        }

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_import_decl(
        &mut self,
        ast: &mut declarations::ImportDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        self.write_fmt(format_args!("import action {}(", ast.name))?;
        Ok(ControlMut::Produce(()))
    }
    fn finish_import_decl(
        &mut self,
        _ast: &mut declarations::ImportDecl,
        _n: (),
        _p: Vec<()>,
    ) -> VisitorResult<(), declarations::Decl> {
        self.write_str(")")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_implementation_decl(
        &mut self,
        ast: &mut Vec<declarations::Decl>,
    ) -> VisitorResult<(), declarations::Decl> {
        self.write_str("specification {\n")?;
        self.write_separated(ast, "\n")?;
        self.write_str("\n}")?;
        Ok(ControlMut::SkipSiblings(()))
    }
    fn finish_implementation_decl(
        &mut self,
        _ast: &mut Vec<declarations::Decl>,
    ) -> VisitorResult<(), declarations::Decl> {
        self.write_str("}")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_include_decl(
        &mut self,
        _ast: &mut expressions::Symbol,
    ) -> VisitorResult<(), declarations::Decl> {
        self.write_str("include ")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_invariant_decl(
        &mut self,
        _ast: &mut logic::Fmla,
    ) -> VisitorResult<(), declarations::Decl> {
        self.write_str("invariant ")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_isolate_decl(
        &mut self,
        inst: &mut declarations::IsolateDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        self.write_fmt(format_args!("isolate {}", inst.name))?;
        if inst.params.len() > 0 {
            self.write_str("(")?;
            inst.params.visit(self)?;
            self.write_str(")")?;
        }
        self.write_str(" {\n")?;
        self.write_separated(&mut inst.body, "\n")?;
        self.write_str("\n}\n")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_module_decl(
        &mut self,
        module: &mut declarations::ModuleDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        self.write_fmt(format_args!("isolate {}", module.name))?;

        if module.sortsyms.len() > 0 {
            self.write_str("(")?;
            module.sortsyms.visit(self)?;
            self.write_str(")")?;
        }
        self.write_str(" {\n")?;
        self.write_separated(&mut module.body, "\n")?;
        self.write_str("\n}\n")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_normalized_isolate_decl(
        &mut self,
        module: &mut declarations::NormalizedIsolateDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        self.write_fmt(format_args!("process {}", module.name))?;

        if module.params.len() > 0 {
            self.write_str("(")?;
            module.params.visit(self)?;
            self.write_str(")")?;
        }
        self.write_str(" {\n")?;

        if module.impl_decls.len() > 0 {
            self.write_str("implementation {\n")?;
            module.impl_decls.visit(self)?;
            self.write_str("\n}\n")?;
        }
        if module.spec_decls.len() > 0 {
            self.write_str("specification {\n")?;
            module.spec_decls.visit(self)?;
            self.write_str("\n}\n")?;
        }
        if module.common_spec_decls.len() > 0 && module.common_impl_decls.len() > 0 {
            self.write_str("common {\n")?;
            if module.impl_decls.len() > 0 {
                self.write_str("implementation {\n")?;
                module.impl_decls.visit(self)?;
                self.write_str("\n}\n")?;
            }
            if module.spec_decls.len() > 0 {
                self.write_str("specification {\n")?;
                module.spec_decls.visit(self)?;
                self.write_str("\n}\n")?;
            }
            self.write_str("\n}\n")?;
        }

        self.write_str("\n}\n")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_object_decl(
        &mut self,
        ast: &mut declarations::ObjectDecl,
    ) -> VisitorResult<(), declarations::Decl> {
        self.write_fmt(format_args!("object {}", ast.name))?;

        if ast.params.len() > 0 {
            self.write_str("(")?;
            ast.params.visit(self)?;
            self.write_str(")")?;
        }
        self.write_str(" {\n")?;
        self.write_separated(&mut ast.body, "\n")?;
        self.write_str("}\n")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_specification(
        &mut self,
        ast: &mut Vec<declarations::Decl>,
    ) -> VisitorResult<(), declarations::Decl> {
        self.write_str("specification {\n")?;
        self.write_separated(ast, "\n")?;
        self.write_str("\n}")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_relation(
        &mut self,
        ast: &mut declarations::Relation,
    ) -> VisitorResult<(), declarations::Decl> {
        self.write_fmt(format_args!("relation {}(", ast.name))?;
        self.write_separated(&mut ast.params, ", ")?;
        self.write_str(")")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_typedecl(
        &mut self,
        ast: &mut expressions::Type,
    ) -> VisitorResult<(), declarations::Decl> {
        self.write_str("type ")?;
        match &ast.ident {
            expressions::TypeName::Name(n) => {
                self.write_str(n)?;
            }
            expressions::TypeName::This => {
                self.write_str("this")?;
            }
        }
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_vardecl(
        &mut self,
        term: &mut expressions::Term,
    ) -> VisitorResult<(), declarations::Decl> {
        self.write_fmt(format_args!("var {}", term.id))?;
        if let Some(sort) = &mut term.sort {
            self.write_str(": ")?;
            self.identifier(sort)?;
        }
        Ok(ControlMut::SkipSiblings(()))
    }

    // Quantifieds

    fn begin_forall(&mut self, fmla: &mut logic::Forall) -> VisitorResult<(), logic::Fmla> {
        self.write_str("forall ")?;
        self.write_separated(&mut fmla.vars, ", ")?;
        self.write_str(" . ")?;
        fmla.fmla.visit(self)?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_exists(&mut self, fmla: &mut logic::Exists) -> VisitorResult<(), logic::Fmla> {
        self.write_str("exists ")?;
        self.write_separated(&mut fmla.vars, ", ")?;
        self.write_str(" . ")?;
        fmla.fmla.visit(self)?;
        Ok(ControlMut::SkipSiblings(()))
    }
    // Expressions

    fn begin_app(
        &mut self,
        ast: &mut expressions::AppExpr,
    ) -> VisitorResult<(), expressions::Expr> {
        ast.func.visit(self)?;

        self.write_str("(")?;
        self.write_separated(&mut ast.args, ", ")?;
        self.write_str(")")?;
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
            Verb::Or => "&",
            _ => {
                println!("Uh oh!: {:?}", ast.op);
                unimplemented!()
            }
        };
        self.write_fmt(format_args!(" {} ", op_str))?;
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
        rhs: &mut expressions::Symbol,
    ) -> VisitorResult<(), expressions::Expr> {
        lhs.visit(self)?;
        self.write_str(".")?;
        rhs.visit(self)?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_index(&mut self, expr: &mut IndexExpr) -> VisitorResult<(), expressions::Expr> {
        expr.lhs.visit(self)?;
        self.write_str("[")?;
        expr.idx.visit(self)?;
        self.write_str("]")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    // Terminals

    fn boolean(&mut self, b: &mut bool) -> VisitorResult<(), bool> {
        if *b {
            self.write_str("true")?;
        } else {
            self.write_str("false")?;
        }
        Ok(ControlMut::Produce(()))
    }

    fn identifier(&mut self, i: &mut expressions::Ident) -> VisitorResult<(), expressions::Ident> {
        self.write_separated(i, ".")?;
        Ok(ControlMut::Produce(()))
    }

    fn number(&mut self, n: &mut i64) -> VisitorResult<(), i64> {
        self.write_str(&n.to_string())?;
        Ok(ControlMut::Produce(()))
    }

    fn param(&mut self, p: &mut expressions::Param) -> VisitorResult<(), expressions::Param> {
        p.id.visit(self)?;

        if let Some(sort) = &mut p.sort {
            self.write_str(": ")?;
            self.identifier(sort)?;
        }
        Ok(ControlMut::SkipSiblings(()))
    }

    fn sort(&mut self, s: &mut IvySort) -> VisitorResult<(), IvySort> {
        match s {
            // These are inferred, usually, I suppose.
            IvySort::Range(min, max) => {
                self.write_str(" = {")?;
                min.visit(self)?;
                self.write_str("..")?;
                max.visit(self)?;
                self.write_str("}")?;
            }
            IvySort::Enum(_) => {
                self.write_str(" = {")?;
                //self.write_separated(branches, ", ")?;
                self.write_str(" }")?;
            }
            IvySort::Subclass(s) => {
                self.write_fmt(format_args!(" of {}", s))?;
            }
            IvySort::Process(_proc) => {
                self.write_str(" = {\n")?;
                self.write_str("implementation {\n")?;
                self.write_str("}\n")?;
                self.write_str("specification {\n")?;
                self.write_str("common {\n")?;
                self.write_str("}\n")?;
                self.write_str("}\n")?;
                self.write_str("}")?;
            }
            IvySort::Uninterpreted => {}
            _ => todo!(),
        }
        Ok(ControlMut::Produce(()))
    }

    fn symbol(&mut self, s: &mut expressions::Symbol) -> VisitorResult<(), expressions::Symbol> {
        self.write_str(s)?;
        Ok(ControlMut::Produce(()))
    }

    fn this(&mut self) -> VisitorResult<(), expressions::Expr> {
        self.write_str("this")?;
        Ok(ControlMut::Produce(()))
    }
}
