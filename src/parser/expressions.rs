use std::rc::Rc;

use pest::error::ErrorVariant;
use pest::iterators::Pairs;
use pest::pratt_parser::{Assoc, Op, PrattParser};
use pest_consume::Error;

use crate::ast::expressions::*;
use crate::ast::span::Span;
use crate::parser::ivy::*;

lazy_static::lazy_static! {
    // This ordering is taken from the precedence numberings
    // from ivy2/lang.ivy.
    static ref PRATT: PrattParser<Rule> =
    PrattParser::new()
        .op(Op::infix(Rule::ARROW, Assoc::Left))
        .op(Op::infix(Rule::COLON, Assoc::Left))

        .op(Op::infix(Rule::LT, Assoc::Left))
        .op(Op::infix(Rule::LE, Assoc::Left))
        .op(Op::infix(Rule::GT, Assoc::Left))
        .op(Op::infix(Rule::GE, Assoc::Left))
        .op(Op::infix(Rule::PLUS, Assoc::Left))
        .op(Op::infix(Rule::MINUS, Assoc::Left))
        .op(Op::infix(Rule::TIMES, Assoc::Left))
        .op(Op::infix(Rule::DIV, Assoc::Left))

        .op(Op::infix(Rule::OR, Assoc::Left))
        .op(Op::infix(Rule::AND, Assoc::Left))

        .op(Op::infix(Rule::EQ, Assoc::Left))
        .op(Op::infix(Rule::NEQ, Assoc::Left))

        .op(Op::infix(Rule::IFF, Assoc::Left))

        .op(Op::prefix(Rule::UMINUS))
        .op(Op::prefix(Rule::NOT))

        // Postfix
        .op(Op::postfix(Rule::fnapp_args))
        .op(Op::postfix(Rule::index))

        .op(Op::infix(Rule::DOT, Assoc::Left));

}

pub fn parse_rval(input: Rc<str>, pairs: Pairs<Rule>) -> Result<Expr> {
    PRATT
        .map_primary(|primary| {
            let res: Result<Expr> = Expr::expr_from_pair(input, primary);
            res
        })
        .map_prefix(|op, rhs| {
            let verb = match op.as_rule() {
                Rule::UMINUS => Verb::Minus,
                Rule::NOT => Verb::Not,
                _ => {
                    return Err(Error::new_from_span(
                        ErrorVariant::CustomError {
                            message: "Expected unary op".into(),
                        },
                        op.as_span(),
                    ));
                }
            };
            let rhs = rhs?;

            let op_span = Span::from_pest(input, op.as_span());
            //let rhs_span = rhs.0

            Ok(Expr { 

                span: op_span, /* XXX: not right */
                expr:  ExprKind::UnaryOp {
                    op: verb,
                    expr: Box::new(rhs)
                },
            })
        })
        .map_infix(|lhs, op, rhs| {
            let verb = match op.as_rule() {
                Rule::DOT => Verb::Dot,
                Rule::AND => Verb::And,
                Rule::OR => Verb::Or,
                Rule::ARROW => Verb::Arrow,
                Rule::GT => Verb::Gt,
                Rule::GE => Verb::Ge,
                Rule::LT => Verb::Lt,
                Rule::LE => Verb::Le,
                Rule::EQ => Verb::Equals,
                Rule::NEQ => Verb::Notequals,

                Rule::PLUS => Verb::Plus,
                Rule::MINUS => Verb::Minus,
                Rule::TIMES => Verb::Times,
                Rule::DIV => Verb::Div,
                _ => unreachable!("Unexpected binary op"),
            };

            if verb == Verb::Dot {
                let field = match rhs? {
                    ExprKind::ProgramSymbol(field) => field,
                    _ => {
                        return Err(Error::new_from_span(
                            ErrorVariant::CustomError {
                                message: "invalid field access expression".into(),
                            },
                            op.as_span(),
                        ));
                    }
                };
                Ok(ExprKind::FieldAccess(FieldAccess {
                    record: Box::new(lhs?),
                    field,
                }))
            } else {
                Ok(ExprKind::BinOp(BinOp {
                    lhs: Box::new(lhs?),
                    op: verb,
                    rhs: Box::new(rhs?),
                }))
            }
        })
        .map_postfix(|lhs, op| match op.as_rule() {
            Rule::fnapp_args => {
                let results = op
                    .into_inner()
                    .map(|e| parse_rval(e.into_inner()))
                    .collect::<Vec<Result<_>>>();
                let args = results.into_iter().collect::<Result<Vec<_>>>()?;
                Ok(ExprKind::App(AppExpr {
                    func: Box::new(lhs?),
                    args,
                }))
            }
            Rule::index => {
                let idx = parse_rval(op.into_inner());
                Ok(ExprKind::Index(IndexExpr {
                    lhs: Box::new(lhs?),
                    idx: Box::new(idx?),
                }))
            }
            _ => unimplemented!(),
        })
        .parse(pairs)
}
