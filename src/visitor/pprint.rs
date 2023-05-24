#![allow(dead_code)]
#![allow(unused_variables)]

use std::fmt::{Error, Write};

use crate::ast::actions::*;
use crate::ast::declarations::*;
use crate::ast::expressions::*;
use crate::ast::logic::*;
use crate::ast::statements::*;
use crate::ast::toplevels::*;
use crate::visitor::visitor::Visitor;

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

    fn write_comma_separated<FU, U>(&mut self, params: &mut Vec<U>, f: FU) -> std::fmt::Result
        where
    FU: FnMut(&mut Self, &mut U) -> std::fmt::Result {
        self.visit_vec_interleaved(params, f, |pp| pp.write_str(", "))
    }

    fn write_seminl_separated<FU, U>(&mut self, params: &mut Vec<U>, f: FU) -> std::fmt::Result
        where
    FU: FnMut(&mut Self, &mut U) -> std::fmt::Result {
        self.visit_vec_interleaved(params, f, |pp| pp.write_str(";\n"))
    }
}

impl <W: Write> Write for PrettyPrinter<W> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
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

impl <W: Write> Visitor<(), Error> for PrettyPrinter<W> {
    fn visit_prog(&mut self, p: &mut Prog) -> Result<(), Error> {
        self.write_fmt(format_args!("#lang ivy{}.{}\n", p.major_version, p.minor_version))?;
        self.visit_isolate(&mut p.top)
    }

    fn visit_if(&mut self, p: &mut If) -> Result<(), Error> {
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
        self.write_str("}\n")
    }

    fn visit_while(&mut self, p: &mut While) -> Result<(), Error> {
        self.write_str("while ")?;
        self.visit_expr(&mut p.test)?;
        self.write_str(" {\n")?;
        for stmt in &mut p.doit {
            self.visit_stmt(stmt)?;
            self.write_str(";\n")?;
        }
        self.write_str("}\n")
    }

    fn visit_action_sequence(&mut self, actions: &mut Vec<Action>) -> Result<(), Error> {
        for a in actions {
            self.visit_action(a)?;
            self.write_str(";\n")?;
        }
        Ok(())
    }

    fn visit_assert(&mut self, a: &mut AssertAction) -> Result<(), Error> {
        self.write_str("assert ")?;
        self.visit_expr(&mut a.pred)
    }

    fn visit_assign(&mut self, a: &mut AssignAction) -> Result<(), Error> {
        self.visit_expr(&mut a.lhs)?;
        self.write_str(" := ")?;
        self.visit_expr(&mut a.rhs)
    }

    fn visit_assume(&mut self, a: &mut AssumeAction) -> Result<(), Error> {
        self.write_str("assume ")?;
        self.visit_expr(&mut a.pred)
    }

    fn visit_ensure(&mut self, e: &mut EnsureAction) -> Result<(), Error> {
        self.write_str("ensure ")?;
        self.visit_formula(&mut e.pred)
    }

    fn visit_requires(&mut self, e: &mut RequiresAction) -> Result<(), Error> {
        self.write_str("requires ")?;
        self.visit_formula(&mut e.pred)
    }

    fn visit_action_decl(&mut self, action: &mut ActionDecl) -> Result<(), Error> {
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
        Ok(())
    }

    fn visit_after(&mut self, action: &mut AfterDecl) -> Result<(), Error> {
        self.write_str("after ")?;
        self.write_str(&action.name.join("."))?;
        self.write_str("(")?;
        match &mut action.params {
            None => (),
            Some(params) => self.write_comma_separated(params, |pp, p| pp.visit_param(p))?,
        }
        self.write_str(")")?;

        if let Some(ret) = &mut action.ret {
            self.write_str(" returns(")?;
            self.visit_param(ret)?;
            self.write_str(")")?;
        }

        self.write_str(" {\n")?;
        self.write_seminl_separated(&mut action.body, |pp, d| pp.visit_stmt(d))?;
        self.write_str("}\n")
    }

    fn visit_alias(&mut self, name: &Symbol, val: &mut Expr) -> Result<(), Error> {
        self.write_fmt(format_args!("alias {} = ", name))?;
        self.visit_expr(val)
    }

    fn visit_axiom(&mut self, axiom: &mut Fmla) -> Result<(), Error> {
        self.write_str("axiom ")?;
        self.visit_formula(axiom)
    }

    fn visit_before(&mut self, action: &mut BeforeDecl) -> Result<(), Error> {
        self.write_str("before ")?;
        self.write_str(&action.name.join("."))?;
        self.write_str("(")?;
        match &mut action.params {
            None => (),
            Some(params) => self.write_comma_separated(params, |pp, p| pp.visit_param(p))?,
        }
        self.write_str(") {\n")?;
        self.write_seminl_separated(&mut action.body, |pp, d| pp.visit_stmt(d))?;
        self.write_str("}\n")
    }

    fn visit_export(&mut self, action: &mut ExportDecl) -> Result<(), Error> {
        self.write_str("import ")?;
        match action {
            ExportDecl::Action(a) => self.visit_action_decl(a),
            ExportDecl::ForwardRef(r) => self.write_str(&r.join("."))
        }
    }

    fn visit_function(&mut self, fun: &mut FunctionDecl) -> Result<(), Error> {
        todo!()
    }

    fn visit_globals(&mut self, defs: &mut Vec<Decl>) -> Result<(), Error> {
        self.write_str("global {")?;
        self.write_seminl_separated(defs, |pp, def| pp.visit_decl(def))?;
        self.write_str("}")
    }

    fn visit_import(&mut self, action: &mut ImportDecl) -> Result<(), Error> {
        self.write_fmt(format_args!("import action {}(", action.name))?;
        self.write_comma_separated(&mut action.params, |pp, p| pp.visit_param(p))?;
        self.write_str(")")
    }

    fn visit_include(&mut self, module: &mut Symbol) -> Result<(), Error> {
        self.write_fmt(format_args!("include {}", module))
    }

    fn visit_instance(&mut self, inst: &mut InstanceDecl) -> Result<(), Error> {
        self.write_fmt(format_args!("instance {} : {}(", &inst.name.join("."), &inst.sort.join(".")))?;
        self.write_comma_separated(&mut inst.args, |pp, p| pp.visit_param(p))?;
        self.write_str(")")
    }

    fn visit_isolate(&mut self, inst: &mut IsolateDecl) -> Result<(), Error> {
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
        self.write_str("}\n")
    }

    fn visit_invariant(&mut self, inv: &mut Fmla) -> Result<(), Error> {
        self.write_str("invariant ")?;
        self.visit_formula(inv)
    }

    fn visit_module(&mut self, module: &mut ModuleDecl) -> Result<(), Error> {
        self.write_fmt(format_args!("module {}(", &module.name.join(".")))?;
        self.write_comma_separated(&mut module.params, |pp, p| pp.visit_param(p))?;
        self.write_str(")")
    }

    fn visit_object(&mut self, obj: &mut ObjectDecl) -> Result<(), Error> {
        self.write_fmt(format_args!("object {}", obj.name.join(".")))?;
        if obj.params.len() > 0 {
            self.write_str("(")?;
            self.write_comma_separated(&mut obj.params, |pp, p| pp.visit_param(p))?;
            self.write_str(")")?;
        }

        self.write_str(" = {\n")?;
        self.write_seminl_separated(&mut obj.body, |pp, d| pp.visit_decl(d))?;
        self.write_str("}")
    }

    fn visit_relation(&mut self, obj: &mut Relation) -> Result<(), Error> {
        self.write_fmt(format_args!("relation {}", obj.name.join(".")))?;
        self.write_str("(")?;
        self.write_comma_separated(&mut obj.params, |pp, p| pp.visit_param(p))?;
        self.write_str(")")
    }

    fn visit_vardecl(&mut self, term: &mut Term) -> Result<(), Error> {
        self.write_fmt(format_args!("var {}", term.id.join(".")))?;
        if let Some(sort) = &term.sort {
            self.write_fmt(format_args!(": {}", sort.join(".")))?;
        }
        Ok(())
    }

    fn visit_typedecl(&mut self, name: &TypeName, sort: &mut Sort) -> Result<(), Error> {
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
                self.write_comma_separated(branches, |pp, e| pp.write_str(e))?;
                self.write_str(" }")?;
            }
            Sort::Subclass(s) => { self.write_fmt(format_args!(" of {}", s))?; }
            Sort::Uninterpreted => {},
        }
        Ok(())
    }

    // Expressions

    fn visit_app(&mut self, a: &mut AppExpr) -> Result<(), std::fmt::Error> {
        self.visit_expr(a.func.as_mut())?;

        self.write_str("(")?;
        self.write_comma_separated(&mut a.args, |pp, e| pp.visit_expr(e))?;
        self.write_str(")")
    }

    fn visit_binop(&mut self, lhs: &mut Expr, op: &Verb, rhs: &mut Expr) -> Result<(), Error> {
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
        self.visit_expr(rhs)
    }

    fn visit_boolean(&mut self, b: &mut bool) -> Result<(), Error> {
        if *b {
            self.write_str("true")?;
        } else {
            self.write_str("false")?;
        }
        Ok(())
    }

    fn visit_formula(&mut self, fmla: &mut Fmla) -> Result<(), Error> {
        let (quant, mut vars, mut fmla) = match fmla {
            Fmla::Exists(Exists { vars, fmla }) =>
                ("exists ", vars, fmla),
            Fmla::Forall(Forall { vars, fmla}) => 
                ("forall ", vars, fmla),
            Fmla::Pred(e) => return self.visit_expr(e),
        };

        self.out.write_str(quant)?;
        self.write_comma_separated(&mut vars, |pp, v| pp.visit_param(v))?;
        self.write_str(" . ")?;
        self.visit_formula(&mut fmla)
    }

    fn visit_identifier(&mut self, ident: &mut Ident) -> Result<(), Error> {
        let s = ident.join(".");
        self.write_str(&s)
    }

    fn visit_index(&mut self, idx: &mut IndexExpr) -> Result<(), Error> {
        self.visit_expr(&mut idx.lhs)?;
        self.write_str("[")?;
        self.visit_expr(&mut idx.idx)?;
        self.write_str("]")
    }

    fn visit_number(&mut self, n: &mut i64) -> Result<(), Error> {
        self.write_str(&n.to_string())
    }

    fn visit_param(&mut self, p: &mut Param) -> Result<(), Error> {
        self.write_str(&p.id)?;
        match &mut p.sort {
            None => Ok(()),
            Some(sort) => {
                self.write_str(":")?;
                self.visit_identifier(sort)
            }
        }
    }

    fn visit_unaryop(&mut self, op: &Verb, expr: &mut Expr) -> Result<(), Error> {
        let op = match op {
            Verb::Not => "!",
            Verb::Minus => "-",
            _ => unreachable!()
        };
        self.write_str(op)?;
        self.visit_expr(expr)
    }

    fn visit_term(&mut self, term: &mut Term) -> Result<(), Error> {
        self.visit_identifier(&mut term.id)?;
        match &mut term.sort {
            None => Ok(()),
            Some(sort) => self.visit_identifier(sort)
        }
    }

    fn visit_this(&mut self) -> Result<(), Error> {
        self.write_str("this")
    }
}
