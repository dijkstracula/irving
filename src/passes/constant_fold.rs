use crate::ast::expressions::*;

use crate::visitor::*;

/// Constant-folds arithmetic expressions.  This is not a useful optimization
/// for us but demonstrates how to use the new Visitor.
pub struct ConstantFold;

impl Visitor<()> for ConstantFold {
    fn finish_binop(
        &mut self,
        expr: &mut BinOp,
        _lhs_ret: (),
        _op_ret: (),
        _rhs_ret: (),
    ) -> VisitorResult<(), Expr> {
        match (expr.lhs.as_ref(), &expr.op, expr.rhs.as_ref()) {
            (Expr::Number(lhs), Verb::Plus, Expr::Number(rhs)) => {
                Ok(ControlMut::Mutation(Expr::Number(lhs + rhs), ()))
            }
            (Expr::Number(lhs), Verb::Minus, Expr::Number(rhs)) => {
                Ok(ControlMut::Mutation(Expr::Number(lhs - rhs), ()))
            }
            (Expr::Number(lhs), Verb::Times, Expr::Number(rhs)) => {
                Ok(ControlMut::Mutation(Expr::Number(lhs * rhs), ()))
            }
            (Expr::Number(lhs), Verb::Div, Expr::Number(rhs)) => {
                Ok(ControlMut::Mutation(Expr::Number(lhs / rhs), ()))
            }

            (lhs, Verb::Plus, Expr::Number(0)) => Ok(ControlMut::Mutation(lhs.clone(), ())),

            _ => Ok(ControlMut::Produce(())),
        }
    }
}
