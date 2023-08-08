use crate::ast::expressions::*;

use crate::ast::span::Span;
use crate::visitor::ast::Visitable;
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

        // Literals
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

            // left-associativity: (a * b) * c) == (a * (b * c))
            (
                lhs @ Expr::BinOp {
                    expr:
                        BinOp {
                            lhs: l_lhs,
                            op: lhs_op,
                            rhs: l_rhs,
                        },
                    ..
                },
                rhs_op,
                rhs @ Expr::Number { .. },
                ..,
            ) => {
                if lhs_op == rhs_op && lhs_op.is_arith() {
                    let mut new_binop = Expr::BinOp {
                        span: Span::Optimized,
                        expr: BinOp {
                            lhs: l_lhs.clone(),
                            op: *lhs_op,
                            rhs: Box::new(Expr::BinOp {
                                span: Span::Optimized,
                                expr: BinOp {
                                    lhs: l_rhs.clone(),
                                    op: *rhs_op,
                                    rhs: Box::new(rhs.clone()),
                                },
                            }),
                        },
                    };
                    new_binop.visit(self)?.modifying(&mut new_binop);

                    // + 1 because we compare against the lhs subtree
                    if new_binop.depth() < lhs.depth() + 1 {
                        Ok(ControlMut::Mutation(new_binop, ()))
                    } else {
                        Ok(ControlMut::Produce(()))
                    }
                } else {
                    Ok(ControlMut::Produce(()))
                }
            }

            _ => Ok(ControlMut::Produce(())),
        }
    }
}
