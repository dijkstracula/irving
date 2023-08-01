use crate::ast::expressions::*;

use crate::visitor::*;

/// Constant-folds arithmetic expressions.  This is not a useful optimization
/// for us but demonstrates how to use the new Visitor.
pub struct ConstantFold;

impl ast::Visitor<()> for ConstantFold {
    fn finish_binop(
        &mut self,
        expr: &mut BinOp,
        _lhs_ret: (),
        _op_ret: (),
        _rhs_ret: (),
    ) -> VisitorResult<(), ExprKind> {
        match (expr.lhs.as_ref(), &expr.op, expr.rhs.as_ref()) {
            (ExprKind::Number(lhs), Verb::Plus, ExprKind::Number(rhs)) => {
                Ok(ControlMut::Mutation(ExprKind::Number(lhs + rhs), ()))
            }
            (ExprKind::Number(lhs), Verb::Minus, ExprKind::Number(rhs)) => {
                Ok(ControlMut::Mutation(ExprKind::Number(lhs - rhs), ()))
            }
            (ExprKind::Number(lhs), Verb::Times, ExprKind::Number(rhs)) => {
                Ok(ControlMut::Mutation(ExprKind::Number(lhs * rhs), ()))
            }
            (ExprKind::Number(lhs), Verb::Div, ExprKind::Number(rhs)) => {
                Ok(ControlMut::Mutation(ExprKind::Number(lhs / rhs), ()))
            }

            (lhs, Verb::Plus, ExprKind::Number(0)) => Ok(ControlMut::Mutation(lhs.clone(), ())),

            _ => Ok(ControlMut::Produce(())),
        }
    }
}
