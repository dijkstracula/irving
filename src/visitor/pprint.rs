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

impl <W: Write> Write for PrettyPrinter<W> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        // TODO: can I do this without allocation?
        let lines = s.lines().enumerate().collect::<Vec<_>>();

        for (i, line) in &lines {
            if !self.curr_line_is_indented {
                let indent = std::iter::repeat(" ").take(self.indent).collect::<String>();
                self.out.write_str(&indent)?;
                self.curr_line_is_indented = true;
            }
            self.out.write_str(line)?;

            self.indent += line.matches("{").count();
            self.indent += line.matches("(").count();
            self.indent -= line.matches("}").count();
            self.indent -= line.matches(")").count();
            if *i < lines.len() - 1 {
                self.out.write_str("\n")?;
                self.curr_line_is_indented = false;
            }
        }
        Ok(())
    }
}

impl <W: Write> Visitor<(), Error> for PrettyPrinter<W> {
    fn visit_prog(&mut self, p: &Prog) -> Result<(), Error> {
        self.out.write_fmt(format_args!("#lang {}.{}", p.major_version, p.minor_version))?;
        self.visit_isolate(&p.top)
    }

    fn visit_if(&mut self, p: &If) -> Result<(), Error> {
        todo!()
    }

    fn visit_while(&mut self, p: &While) -> Result<(), Error> {
        todo!()
    }

    fn visit_action_sequence(&mut self, actions: &Vec<Action>) -> Result<(), Error> {
        todo!()
    }

    fn visit_assert(&mut self, a: &AssertAction) -> Result<(), Error> {
        todo!()
    }

    fn visit_assign(&mut self, a: &AssignAction) -> Result<(), Error> {
        todo!()
    }

    fn visit_assume(&mut self, a: &AssumeAction) -> Result<(), Error> {
        todo!()
    }

    fn visit_call(&mut self, e: &AppExpr) -> Result<(), Error> {
        todo!()
    }

    fn visit_ensure(&mut self, e: &EnsureAction) -> Result<(), Error> {
        todo!()
    }

    fn visit_requires(&mut self, e: &RequiresAction) -> Result<(), Error> {
        todo!()
    }

    fn visit_action_decl(&mut self, action: &ActionDecl) -> Result<(), Error> {
        todo!()
    }

    fn visit_after(&mut self, action: &AfterDecl) -> Result<(), Error> {
        todo!()
    }

    fn visit_alias(&mut self, name: &Symbol, val: &Expr) -> Result<(), Error> {
        todo!()
    }

    fn visit_axiom(&mut self, axiom: &Fmla) -> Result<(), Error> {
        todo!()
    }

    fn visit_before(&mut self, action: &BeforeDecl) -> Result<(), Error> {
        todo!()
    }

    fn visit_export(&mut self, action: &ExportDecl) -> Result<(), Error> {
        todo!()
    }

    fn visit_function(&mut self, fun: &FunctionDecl) -> Result<(), Error> {
        todo!()
    }

    fn visit_globals(&mut self, defs: &Vec<Decl>) -> Result<(), Error> {
        todo!()
    }

    fn visit_import(&mut self, action: &ImportDecl) -> Result<(), Error> {
        todo!()
    }

    fn visit_include(&mut self, module: &Symbol) -> Result<(), Error> {
        todo!()
    }

    fn visit_instance(&mut self, inst: &InstanceDecl) -> Result<(), Error> {
        todo!()
    }

    fn visit_isolate(&mut self, inst: &IsolateDecl) -> Result<(), Error> {
        self.out.write_fmt(format_args!("isolate {}", inst.name))?;
        if inst.params.len() > 0 {
            self.write_str("(")?;
            for param in &inst.params {
                self.visit_param(param)?;
            }
            self.write_str(")")?;
        }
        self.write_str("{")?;
        self.write_str("\n")?;
        for decl in &inst.body {
            self.visit_decl(decl)?;
            self.write_str("\n")?;
        }
        self.write_str("}")?;
        self.write_str("\n")
    }

    fn visit_invariant(&mut self, inv: &Fmla) -> Result<(), Error> {
        self.write_str("invariant ")?;
        self.visit_formula(inv)?;
        self.write_str(";")
    }

    fn visit_module(&mut self, module: &ModuleDecl) -> Result<(), Error> {
        todo!()
    }

    fn visit_object(&mut self, obj: &ObjectDecl) -> Result<(), Error> {
        todo!()
    }

    fn visit_relation(&mut self, obj: &Relation) -> Result<(), Error> {
        todo!()
    }

    fn visit_vardecl(&mut self, term: &Term) -> Result<(), Error> {
        todo!()
    }

    fn visit_typedecl(&mut self, name: &TypeName, sort: &Sort) -> Result<(), Error> {
        todo!()
    }

    // Expressions

    fn visit_app(&mut self, a: &AppExpr) -> Result<(), std::fmt::Error> {
        self.visit_expr(a.func.as_ref())?;

        self.write_str("(")?;
        let mut sep = false;
        for arg in &a.args {
            if sep {
                self.write_str(", ")?;
            } else {
                sep = true;
            }
            self.visit_expr(arg)?;
        }
        self.write_str(")")
    }

    fn visit_binop(&mut self, lhs: &Expr, op: &Verb, rhs: &Expr) -> Result<(), Error> {
        let op_str = match op {
            Verb::Plus => "+",
            Verb::Minus => "-",
            Verb::Times => "*",
            Verb::Div => "/",
            Verb::Dot => ".",

            Verb::Equals => "=",
            Verb::Arrow => "->",

            Verb::And => "&",
            _ => unimplemented!()
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

    fn visit_boolean(&mut self, b: bool) -> Result<(), Error> {
        if b {
            self.write_str("true")?;
        } else {
            self.write_str("false")?;
        }
        Ok(())
    }

    fn visit_formula(&mut self, fmla: &Fmla) -> Result<(), Error> {
        let (quant, vars, fmla) = match fmla {
            Fmla::Exists(Exists { vars, fmla }) =>
                ("exists ", vars, fmla),
            Fmla::Forall(Forall { vars, fmla}) => 
                ("forall ", vars, fmla),
            Fmla::Pred(e) => return self.visit_expr(e),
        };


        self.out.write_fmt(format_args!("{}", quant))?;
        let mut sep = false;
        for v in vars {
            if sep {
                self.write_str(", ")?;
            } else {
                sep = true;
            }
            self.visit_param(v)?;
        }

        self.write_str(" . ")?;
        self.visit_formula(&fmla)
    }

    fn visit_identifier(&mut self, ident: &Ident) -> Result<(), Error> {
        let s = ident.join(".");
        self.write_str(&s)
    }

    fn visit_index(&mut self, idx: &IndexExpr) -> Result<(), Error> {
        self.visit_expr(&idx.lhs)?;
        self.write_str("[")?;
        self.visit_expr(&idx.idx)?;
        self.write_str("]")
    }

    fn visit_number(&mut self, n: &i64) -> Result<(), Error> {
        self.write_str(&n.to_string())
    }

    fn visit_param(&mut self, p: &Param) -> Result<(), Error> {
        self.write_str(&p.id)?;
        match &p.sort {
            None => Ok(()),
            Some(sort) => {
                self.write_str(":")?;
                self.visit_identifier(&sort)
            }
        }
    }

    fn visit_unaryop(&mut self, op: &Verb, expr: &Expr) -> Result<(), Error> {
        let op = match op {
            Verb::Not => "!",
            Verb::Minus => "-",
            _ => unreachable!()
        };
        self.write_str(op)?;
        self.visit_expr(expr)
    }

    fn visit_term(&mut self, term: &Term) -> Result<(), Error> {
        self.visit_identifier(&term.id)?;
        match &term.sort {
            None => Ok(()),
            Some(sort) => self.visit_identifier(&sort)
        }
    }

    fn visit_this(&mut self) -> Result<(), Error> {
        self.write_str("this")
    }
}
