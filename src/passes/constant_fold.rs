use crate::ast::expressions::*;

use crate::ast::span::Span;
use crate::visitor::*;

/// Constant-folds arithmetic expressions.  This is not a useful optimization
/// for us but demonstrates how to use the new Visitor.
pub struct ConstantFold;

impl ast::Visitor<(), std::io::Error> for ConstantFold {
    fn finish_binop(
        &mut self,
        expr: &mut BinOp,
        _lhs_ret: (),
        _op_ret: (),
        _rhs_ret: (),
    ) -> VisitorResult<(), std::io::Error, Expr> {
        let span = Span::Optimized;

        match (expr.lhs.as_ref(), &expr.op, expr.rhs.as_ref()) {
            (Expr::Number { val: lhs, .. }, Verb::Plus, Expr::Number { val: rhs, .. }) => {
                Ok(ControlMut::Mutation(
                    Expr::Number {
                        span,
                        val: lhs + rhs,
                    },
                    (),
                ))
            }
            (Expr::Number { val: lhs, .. }, Verb::Minus, Expr::Number { val: rhs, .. }) => {
                Ok(ControlMut::Mutation(
                    Expr::Number {
                        span,
                        val: lhs - rhs,
                    },
                    (),
                ))
            }
            (Expr::Number { val: lhs, .. }, Verb::Times, Expr::Number { val: rhs, .. }) => {
                Ok(ControlMut::Mutation(
                    Expr::Number {
                        span,
                        val: lhs * rhs,
                    },
                    (),
                ))
            }
            (Expr::Number { val: lhs, .. }, Verb::Div, Expr::Number { val: rhs, .. }) => {
                Ok(ControlMut::Mutation(
                    Expr::Number {
                        span,
                        val: lhs / rhs,
                    },
                    (),
                ))
            }

            (lhs, Verb::Plus, Expr::Number { val: 0, .. }) => {
                Ok(ControlMut::Mutation(lhs.clone(), ()))
            }

            _ => Ok(ControlMut::Produce(())),
        }
    }
}
