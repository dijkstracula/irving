use crate::{ast::expressions::*, typechecker::Error};

use super::visitor_v2::{ControlV2, VisitorV2, VisitorV2Result};

/// Constant-folds arithmetic expressions.  This is not a useful optimization
/// for us but demonstrates how to use the new Visitor.
pub struct ConstantFold;

impl VisitorV2<Error> for ConstantFold {
    fn finish_binop(&mut self, expr: &mut BinOp) -> VisitorV2Result<Expr, Error> {
        match (expr.lhs.as_ref(), &expr.op, expr.rhs.as_ref()) {
            (Expr::Number(lhs), Verb::Plus, Expr::Number(rhs)) => {
                Ok(ControlV2::Change(Expr::Number(lhs + rhs)))
            }
            (Expr::Number(lhs), Verb::Minus, Expr::Number(rhs)) => {
                Ok(ControlV2::Change(Expr::Number(lhs - rhs)))
            }
            (Expr::Number(lhs), Verb::Times, Expr::Number(rhs)) => {
                Ok(ControlV2::Change(Expr::Number(lhs * rhs)))
            }
            (Expr::Number(lhs), Verb::Div, Expr::Number(rhs)) => {
                Ok(ControlV2::Change(Expr::Number(lhs / rhs)))
            }

            (lhs, Verb::Plus, Expr::Number(0)) => Ok(ControlV2::Change(lhs.clone())),

            _ => Ok(ControlV2::Continue),
        }
    }
}
