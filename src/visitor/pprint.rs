#![allow(dead_code)]
#![allow(unused_variables)]

use std::fmt;
use std::fmt::{Error, Write};

use crate::ast::actions::*;
use crate::ast::declarations::*;
use crate::ast::expressions::*;
use crate::ast::logic::*;
use crate::ast::statements::*;
use crate::ast::toplevels::*;
use crate::visitor::visitor::Visitor;

use super::control::Control::Continue;
use super::control::VisitorResult;

pub struct PrettyPrinter<W> where W: Write {
    pub out: W,
    indent: usize,
    curr_line_is_indented: bool
}

impl PrettyPrinter<String> {
    pub fn new() -> Self {
        PrettyPrinter { out: String::new(), indent: 0, curr_line_is_indented: false}
    }
}

impl <W> PrettyPrinter<W> where W: Write {

    fn write_comma_separated<FU, U>(&mut self, params: &mut Vec<U>, f: FU) -> VisitorResult<Vec<U>, Error>
        where
    FU: FnMut(&mut Self, &mut U) -> VisitorResult<U, Error> {
        self.visit_vec_interleaved(params, f, |pp| pp.write_str(", "))
    }

    fn write_seminl_separated<FU, U>(&mut self, params: &mut Vec<U>, f: FU) -> VisitorResult<Vec<U>, Error>
        where
    FU: FnMut(&mut Self, &mut U) -> VisitorResult<U, Error> {
        self.visit_vec_interleaved(params, f, |pp| pp.write_str(";\n"))
    }
}

impl <W: Write> Write for PrettyPrinter<W> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        // TODO: can I do this without allocation?
        let lines = s.split("\n").enumerate().collect::<Vec<_>>();

        for (i, line) in &lines {
            self.indent -= line.matches("}").count();
            self.indent -= line.matches(")").count();
            if line.len() > 0 && !self.curr_line_is_indented {
                let indent = std::iter::repeat(" ").take(4 * self.indent).collect::<String>();
                self.out.write_str(&indent)?;
                self.curr_line_is_indented = true;
            }
            self.out.write_str(line)?;

            self.indent += line.matches("{").count();
            self.indent += line.matches("(").count();
            if *i < lines.len() - 1 {
                self.out.write_str("\n")?;
                self.curr_line_is_indented = false;
            }
        }
        Ok(())
    }
}

impl <W: Write> Visitor<Error> for PrettyPrinter<W> {
    fn visit_prog(&mut self, p: &mut Prog) -> VisitorResult<Prog, Error> {
        self.write_fmt(format_args!("#lang ivy{}.{}\n", p.major_version, p.minor_version))?;
        self.visit_isolate(&mut p.top)?;
        Ok(Continue)
    }

    fn visit_if(&mut self, p: &mut If) -> VisitorResult<Stmt, Error> {
        self.write_str("if ")?;
        self.visit_expr(&mut p.tst)?;
        self.write_str(" {\n")?;

        for stmt in &mut p.thn {
            self.visit_stmt(stmt)?;
            self.write_str(";\n")?;
        }
        match &mut p.els {
            None => (),
            Some(stmts) => {
                self.write_str("} else {")?;
                for stmt in stmts {
                    self.visit_stmt(stmt)?;
                    self.write_str(";\n")?;
                }
                self.write_str("}\n")?;
            }
        }
        self.write_str("}\n")?;
        Ok(Continue)
    }

    fn visit_while(&mut self, p: &mut While) -> VisitorResult<Stmt, Error> {
        self.write_str("while ")?;
        self.visit_expr(&mut p.test)?;
        self.write_str(" {\n")?;
        for stmt in &mut p.doit {
            self.visit_stmt(stmt)?;
            self.write_str(";\n")?;
        }
        self.write_str("}\n")?;
        Ok(Continue)
    }

    fn visit_action_sequence(&mut self, actions: &mut Vec<Action>) -> VisitorResult<Stmt, Error> {
        for a in actions {
            self.visit_action(a)?;
            self.write_str(";\n")?;
        }
        Ok(Continue)
    }

    fn visit_assert(&mut self, a: &mut AssertAction) -> VisitorResult<Action, Error> {
        self.write_str("assert ")?;
        self.visit_expr(&mut a.pred)?;
        Ok(Continue)
    }

    fn visit_assign(&mut self, a: &mut AssignAction) -> VisitorResult<Action, Error> {
        self.visit_expr(&mut a.lhs)?;
        self.write_str(" := ")?;
        self.visit_expr(&mut a.rhs)?;
        Ok(Continue)
    }

    fn visit_assume(&mut self, a: &mut AssumeAction) -> VisitorResult<Action, Error> {
        self.write_str("assume ")?;
        self.visit_expr(&mut a.pred)?;
        Ok(Continue)
    }

    fn visit_ensure(&mut self, e: &mut EnsureAction) -> VisitorResult<Action, Error> {
        self.write_str("ensure ")?;
        self.visit_formula(&mut e.pred)?;
        Ok(Continue)
    }

    fn visit_requires(&mut self, e: &mut RequiresAction) -> VisitorResult<Action, Error> {
        self.write_str("requires ")?;
        self.visit_formula(&mut e.pred)?;
        Ok(Continue)
    }

    fn visit_action_decl(&mut self, action: &mut ActionDecl) -> VisitorResult<Decl, Error> {
        self.write_str("action ")?;
        self.write_str(&action.name.join("."))?;
        self.write_str("(")?;
        self.write_comma_separated(&mut action.params, |pp, p| pp.visit_param(p))?;
        self.write_str(")")?;

        if let Some(ret) = &mut action.ret {
            self.write_str(" returns(")?;
            self.visit_param(ret)?;
            self.write_str(")")?;
        }
        if let Some(stmts) = &mut action.body {
            self.write_str(" {\n")?;
            self.write_seminl_separated(stmts, |pp, d| pp.visit_stmt(d))?;
            self.write_str("}\n")?;
        }
        Ok(Continue)
    }

    fn visit_after(&mut self, action: &mut AfterDecl) -> VisitorResult<Decl, Error> {
        self.write_str("after ")?;
        self.write_str(&action.name.join("."))?;
        self.write_str("(")?;
        if let Some(params) = &mut action.params {
            self.write_comma_separated(params, |pp, p| pp.visit_param(p))?;
        }
        self.write_str(")")?;

        if let Some(ret) = &mut action.ret {
            self.write_str(" returns(")?;
            self.visit_param(ret)?;
            self.write_str(")")?;
        }

        self.write_str(" {\n")?;
        self.write_seminl_separated(&mut action.body, |pp, d| pp.visit_stmt(d))?;
        self.write_str("}\n")?;
        Ok(Continue)
    }

    fn visit_alias(&mut self, name: &Symbol, val: &mut Expr) -> VisitorResult<Decl, Error> {
        self.write_fmt(format_args!("alias {} = ", name))?;
        self.visit_expr(val)?;
        Ok(Continue)
    }

    fn visit_axiom(&mut self, axiom: &mut Fmla) -> VisitorResult<Decl, Error> {
        self.write_str("axiom ")?;
        self.visit_formula(axiom)?;
        Ok(Continue)
    }

    fn visit_before(&mut self, action: &mut BeforeDecl) -> VisitorResult<Decl, Error> {
        self.write_str("before ")?;
        self.write_str(&action.name.join("."))?;
        self.write_str("(")?;
        if let Some(params) = &mut action.params {
            self.write_comma_separated(params, |pp, p| pp.visit_param(p))?;
        }
        self.write_str(") {\n")?;
        self.write_seminl_separated(&mut action.body, |pp, d| pp.visit_stmt(d))?;
        self.write_str("}\n")?;
        Ok(Continue)
    }

    fn visit_export(&mut self, action: &mut ExportDecl) -> VisitorResult<Decl, Error> {
        self.write_str("import ")?;
        match action {
            ExportDecl::Action(a) => self.visit_action_decl(a),
            ExportDecl::ForwardRef(r) => { self.write_str(&r.join(".")); Ok(Continue) }
        }
    }

    fn visit_function(&mut self, fun: &mut FunctionDecl) -> VisitorResult<Decl, Error> {
        todo!()
    }

    fn visit_globals(&mut self, defs: &mut Vec<Decl>) -> VisitorResult<Decl, Error> {
        self.write_str("global {")?;
        self.write_seminl_separated(defs, |pp, def| pp.visit_decl(def))?;
        self.write_str("}")?;
        Ok(Continue)
    }

    fn visit_import(&mut self, action: &mut ImportDecl) -> VisitorResult<Decl, Error> {
        self.write_fmt(format_args!("import action {}(", action.name))?;
        self.write_comma_separated(&mut action.params, |pp, p| pp.visit_param(p))?;
        self.write_str(")")?;
        Ok(Continue)
    }

    fn visit_include(&mut self, module: &mut Symbol) -> VisitorResult<Decl, Error> {
        self.write_fmt(format_args!("include {}", module))?;
        Ok(Continue)
    }

    fn visit_instance(&mut self, inst: &mut InstanceDecl) -> VisitorResult<Decl, Error> {
        self.write_fmt(format_args!("instance {} : {}(", &inst.name.join("."), &inst.sort.join(".")))?;
        self.write_comma_separated(&mut inst.args, |pp, p| pp.visit_param(p))?;
        self.write_str(")")?;
        Ok(Continue)
    }

    fn visit_isolate(&mut self, inst: &mut IsolateDecl) -> VisitorResult<Decl, Error> {
        self.out.write_fmt(format_args!("isolate {}", inst.name))?;
        if inst.params.len() > 0 {
            self.write_str("(")?;
            self.write_comma_separated(&mut inst.params, |pp, p| pp.visit_param(p))?;
            self.write_str(")")?;
        }
        self.write_str(" {\n")?;
        for decl in &mut inst.body {
            self.visit_decl(decl)?;
            self.write_str("\n")?;
        }
        self.write_str("}\n")?;
        Ok(Continue)
    }

    fn visit_invariant(&mut self, inv: &mut Fmla) -> VisitorResult<Decl, Error> {
        self.write_str("invariant ")?;
        self.visit_formula(inv)?;
        Ok(Continue)
    }

    fn visit_module(&mut self, module: &mut ModuleDecl) -> VisitorResult<Decl, Error> {
        self.write_fmt(format_args!("module {}(", &module.name.join(".")))?;
        self.write_comma_separated(&mut module.params, |pp, p| pp.visit_param(p))?;
        self.write_str(")")?;
        Ok(Continue)
    }

    fn visit_object(&mut self, obj: &mut ObjectDecl) -> VisitorResult<Decl, Error> {
        self.write_fmt(format_args!("object {}", obj.name.join(".")))?;
        if obj.params.len() > 0 {
            self.write_str("(")?;
            self.write_comma_separated(&mut obj.params, |pp, p| pp.visit_param(p))?;
            self.write_str(")")?;
        }

        self.write_str(" = {\n")?;
        self.write_seminl_separated(&mut obj.body, |pp, d| pp.visit_decl(d))?;
        self.write_str("}")?;
        Ok(Continue)
    }

    fn visit_relation(&mut self, obj: &mut Relation) -> VisitorResult<Decl, Error> {
        self.write_fmt(format_args!("relation {}", obj.name.join(".")))?;
        self.write_str("(")?;
        self.write_comma_separated(&mut obj.params, |pp, p| pp.visit_param(p))?;
        self.write_str(")")?;
        Ok(Continue)
    }

    fn visit_vardecl(&mut self, term: &mut Term) -> VisitorResult<Decl, Error> {
        self.write_fmt(format_args!("var {}", term.id.join(".")))?;
        if let Some(sort) = &term.sort {
            self.write_fmt(format_args!(": {}", sort.join(".")))?;
        }
        Ok(Continue)
    }

    fn visit_typedecl(&mut self, name: &TypeName, sort: &mut Sort) -> VisitorResult<Decl, Error> {
        self.write_str("type ")?;
        match name {
            TypeName::Name(n) => { self.write_str(n)?; }
            TypeName::This => { self.write_str("this")?; }
        }

        match sort {
            Sort::Range(min, max) => { 
                self.write_str(" = {")?;
                self.visit_expr(min)?;
                self.write_str("..")?;
                self.visit_expr(max)?;
                self.write_str("}")?;
            },
            Sort::Enum(branches) => {
                self.write_str(" = {")?;
                self.write_comma_separated(branches, |pp, e| pp.visit_symbol(e))?;
                self.write_str(" }")?;
            }
            Sort::Subclass(s) => { self.write_fmt(format_args!(" of {}", s))?; }
            Sort::Uninterpreted => {},
        }
        Ok(Continue)
    }

    // Expressions

    fn visit_app(&mut self, a: &mut AppExpr) -> VisitorResult<Expr, Error> {
        self.visit_expr(a.func.as_mut())?;

        self.write_str("(")?;
        self.write_comma_separated(&mut a.args, |pp, e| pp.visit_expr(e))?;
        self.write_str(")")?;
        Ok(Continue)
    }

    fn visit_binop(&mut self, lhs: &mut Expr, op: &Verb, rhs: &mut Expr) -> VisitorResult<Expr, Error> {
        let op_str = match op {
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
                println!("{:?}", op);
                unimplemented!()
            }
        };

        self.visit_expr(lhs)?;
        match op {
            Verb::Dot => {
                self.out.write_fmt(format_args!("{}", op_str))?;
            },
            _ => {
                self.out.write_fmt(format_args!(" {} ", op_str))?;
            }
        }
        self.visit_expr(rhs)?;
        Ok(Continue)
    }

    fn visit_boolean(&mut self, b: &mut bool) -> VisitorResult<Expr, Error> {
        if *b {
            self.write_str("true")?;
        } else {
            self.write_str("false")?;
        }
        Ok(Continue)
    }

    fn visit_formula(&mut self, fmla: &mut Fmla) -> VisitorResult<Fmla, Error> {
        let (quant, mut vars, mut fmla) = match fmla {
            Fmla::Exists(Exists { vars, fmla }) =>
                ("exists ", vars, fmla),
            Fmla::Forall(Forall { vars, fmla}) => 
                ("forall ", vars, fmla),
            Fmla::Pred(e) => { 
                self.visit_expr(e);
                return Ok(Continue)
            }
        };

        self.out.write_str(quant)?;
        self.write_comma_separated(&mut vars, |pp, v| pp.visit_param(v))?;
        self.write_str(" . ")?;
        self.visit_formula(&mut fmla)?;
        Ok(Continue)
    }

    fn visit_identifier(&mut self, ident: &mut Ident) -> VisitorResult<Expr, Error> {
        let s = ident.join(".");
        self.write_str(&s)?;
        Ok(Continue)
    }

    fn visit_index(&mut self, idx: &mut IndexExpr) -> VisitorResult<Expr, Error> {
        self.visit_expr(&mut idx.lhs)?;
        self.write_str("[")?;
        self.visit_expr(&mut idx.idx)?;
        self.write_str("]")?;
        Ok(Continue)
    }

    fn visit_number(&mut self, n: &mut i64) -> VisitorResult<Expr, Error> {
        self.write_str(&n.to_string())?;
        Ok(Continue)
    }

    fn visit_param(&mut self, p: &mut Param) -> VisitorResult<Param, Error> {
        self.visit_symbol(&mut p.id)?;
        if let Some(sort) = &mut p.sort {
            self.write_str(":")?;
            self.visit_identifier(sort)?;
        }
        Ok(Continue)
    }
    fn visit_symbol(&mut self, sym: &mut Symbol) -> VisitorResult<Symbol, Error> {
        self.write_str(sym)?;
        Ok(Continue)
    }
    fn visit_unaryop(&mut self, op: &Verb, expr: &mut Expr) -> VisitorResult<Expr, Error> {
        let op = match op {
            Verb::Not => "!",
            Verb::Minus => "-",
            _ => unreachable!()
        };
        self.write_str(op)?;
        self.visit_expr(expr)?;
        Ok(Continue)
    }

    fn visit_term(&mut self, term: &mut Term) -> VisitorResult<Expr, Error> {
        self.visit_identifier(&mut term.id)?;
        if let Some(sort) = &mut term.sort {
            self.write_str(":")?;
            self.visit_identifier(sort)?;
        }
        Ok(Continue)
    }

    fn visit_this(&mut self) -> VisitorResult<Expr, Error> {
        self.write_str("this")?;
        Ok(Continue)
    }

    fn visit_call(&mut self, e: &mut AppExpr) -> VisitorResult<Expr, Error> {
        self.visit_expr(&mut e.func)?;

        for arg in &mut e.args {
            self.visit_expr(arg)?;
        }
        Ok(Continue)
    }
}
