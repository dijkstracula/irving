#![allow(dead_code)]
#![allow(unused_variables)]

use std::fmt::{Error, Write};

use super::expressions::*;
use super::logic::*;
use super::visitor::{ExpressionVisitor, StatementVisitor};

pub struct PrettyPrinter<W> where W: Write {
    pub out: W,
}

impl PrettyPrinter<String> {
    pub fn new() -> Self {
        PrettyPrinter { out: String::new() }
    }
}

impl <W: Write> StatementVisitor<(), Error> for PrettyPrinter<W> {
    fn visit_prog(&mut self, p: &super::toplevels::Prog) -> Result<(), Error> {
        self.out.write_fmt(format_args!("#lang {}.{}", p.major_version, p.minor_version))?;
        self.visit_isolate(&p.top)
    }

    fn visit_if(&mut self, p: &super::statements::If) -> Result<(), Error> {
        todo!()
    }

    fn visit_while(&mut self, p: &super::statements::While) -> Result<(), Error> {
        todo!()
    }

    fn visit_action_sequence(&mut self, actions: &Vec<super::actions::Action>) -> Result<(), Error> {
        todo!()
    }

    fn visit_assert(&mut self, a: &super::actions::AssertAction) -> Result<(), Error> {
        todo!()
    }

    fn visit_assign(&mut self, a: &super::actions::AssignAction) -> Result<(), Error> {
        todo!()
    }

    fn visit_assume(&mut self, a: &super::actions::AssumeAction) -> Result<(), Error> {
        todo!()
    }

    fn visit_call(&mut self, e: &AppExpr) -> Result<(), Error> {
        todo!()
    }

    fn visit_ensure(&mut self, e: &super::actions::EnsureAction) -> Result<(), Error> {
        todo!()
    }

    fn visit_requires(&mut self, e: &super::actions::RequiresAction) -> Result<(), Error> {
        todo!()
    }

    fn visit_action_decl(&mut self, action: &super::declarations::ActionDecl) -> Result<(), Error> {
        todo!()
    }

    fn visit_after(&mut self, action: &super::declarations::AfterDecl) -> Result<(), Error> {
        todo!()
    }

    fn visit_alias(&mut self, name: &Symbol, val: &Expr) -> Result<(), Error> {
        todo!()
    }

    fn visit_axiom(&mut self, axiom: &Fmla) -> Result<(), Error> {
        todo!()
    }

    fn visit_before(&mut self, action: &super::declarations::BeforeDecl) -> Result<(), Error> {
        todo!()
    }

    fn visit_export(&mut self, action: &super::declarations::ExportDecl) -> Result<(), Error> {
        todo!()
    }

    fn visit_function(&mut self, fun: &super::declarations::FunctionDecl) -> Result<(), Error> {
        todo!()
    }

    fn visit_globals(&mut self, defs: &Vec<super::declarations::Decl>) -> Result<(), Error> {
        todo!()
    }

    fn visit_import(&mut self, action: &super::declarations::ImportDecl) -> Result<(), Error> {
        todo!()
    }

    fn visit_include(&mut self, module: &Symbol) -> Result<(), Error> {
        todo!()
    }

    fn visit_instance(&mut self, inst: &super::declarations::InstanceDecl) -> Result<(), Error> {
        todo!()
    }

    fn visit_isolate(&mut self, inst: &super::declarations::IsolateDecl) -> Result<(), Error> {
        self.out.write_fmt(format_args!("isolate {}", inst.name))?;
        if inst.params.len() > 0 {
            self.out.write_str("(")?;
            for param in &inst.params {
                self.visit_param(param)?;
            }
            self.out.write_str(")")?;
        }
        self.out.write_str("{")?;
        for decl in &inst.body {
        }
        self.out.write_str("}")
    }

    fn visit_invariant(&mut self, inv: &Fmla) -> Result<(), Error> {
        todo!()
    }

    fn visit_module(&mut self, module: &super::declarations::ModuleDecl) -> Result<(), Error> {
        todo!()
    }

    fn visit_object(&mut self, obj: &super::declarations::ObjectDecl) -> Result<(), Error> {
        todo!()
    }

    fn visit_relation(&mut self, obj: &super::declarations::Relation) -> Result<(), Error> {
        todo!()
    }

    fn visit_vardecl(&mut self, term: &Term) -> Result<(), Error> {
        todo!()
    }

    fn visit_typedecl(&mut self, name: &TypeName, sort: &super::declarations::Sort) -> Result<(), Error> {
        todo!()
    }
}

impl <W: Write> ExpressionVisitor<(), Error> for PrettyPrinter<W> {
    fn visit_app(&mut self, a: &AppExpr) -> Result<(), std::fmt::Error> {
        self.visit_expr(a.func.as_ref())?;

        self.out.write_str("(")?;
        let mut sep = false;
        for arg in &a.args {
            if sep {
                self.out.write_str(", ")?;
            } else {
                sep = true;
            }
            self.visit_expr(arg)?;
        }
        self.out.write_str(")")
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

    fn visit_boolean(&mut self, b: &bool) -> Result<(), Error> {
        todo!()
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
                self.out.write_str(", ")?;
            } else {
                sep = true;
            }
            self.visit_param(v)?;
        }

        self.out.write_str(" . ")?;
        self.visit_formula(&fmla)
    }

    fn visit_identifier(&mut self, ident: &Ident) -> Result<(), Error> {
        let s = ident.join(".");
        self.out.write_str(&s)
    }

    fn visit_index(&mut self, idx: &IndexExpr) -> Result<(), Error> {
        self.visit_expr(&idx.lhs)?;
        self.out.write_str("[")?;
        self.visit_expr(&idx.idx)?;
        self.out.write_str("]")
    }

    fn visit_number(&mut self, n: &i64) -> Result<(), Error> {
        self.out.write_str(&n.to_string())
    }

    fn visit_param(&mut self, p: &Param) -> Result<(), Error> {
        self.out.write_str(&p.id)?;
        match &p.sort {
            None => Ok(()),
            Some(sort) => {
                self.out.write_str(":")?;
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
        self.out.write_str(op)?;
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
        self.out.write_str("this")
    }
}
