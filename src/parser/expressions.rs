use std::rc::Rc;

use pest::error::ErrorVariant;
use pest::iterators::Pairs;
use pest::pratt_parser::{Assoc, Op, PrattParser};
use pest_consume::Error;

use crate::ast::expressions::*;
use crate::ast::span::Span;
use crate::parser::ivy::*;

lazy_static::lazy_static! {
    pub static ref PRATT: PrattParser<Rule> =
    PrattParser::new()
        .op(Op::infix(Rule::ARROW, Assoc::Left))
        .op(Op::infix(Rule::COLON, Assoc::Left))

        .op(Op::infix(Rule::PLUS, Assoc::Left) | Op::infix(Rule::MINUS, Assoc::Left))
        .op(Op::infix(Rule::TIMES, Assoc::Left) | Op::infix(Rule::DIV, Assoc::Left))

        .op(Op::infix(Rule::OR, Assoc::Left) | Op::infix(Rule::AND, Assoc::Left))

        .op(Op::infix(Rule::LT, Assoc::Left) | Op::infix(Rule::LE, Assoc::Left) |
            Op::infix(Rule::GT, Assoc::Left) | Op::infix(Rule::GE, Assoc::Left) |
            Op::infix(Rule::EQ, Assoc::Left) | Op::infix(Rule::NEQ, Assoc::Left))

        .op(Op::infix(Rule::IFF, Assoc::Left))

        .op(Op::prefix(Rule::UMINUS))
        .op(Op::prefix(Rule::NOT))

        // Postfix
        .op(Op::postfix(Rule::fnapp_args))
        .op(Op::postfix(Rule::index))

        .op(Op::infix(Rule::DOT, Assoc::Left));

}

pub fn parse_rval(input: Rc<ParserState>, pairs: Pairs<Rule>) -> Result<Expr> {
    PRATT
        .map_primary(|primary| {
            let span = Span::from_pest(Rc::clone(&input), &primary.as_span());
            match primary.as_rule() {
                Rule::THIS => Ok(Expr::This(span)),
                Rule::PROGTOK => Ok(Expr::ProgramSymbol {
                    span: span.clone(),
                    sym: Symbol::from(primary.as_str(), Sort::ToBeInferred, span),
                }),
                Rule::boollit => {
                    let val = match primary.as_str() {
                        "true" => true,
                        "false" => false,
                        _ => unreachable!(),
                    };
                    Ok(Expr::Boolean { span, val })
                }
                Rule::number => {
                    let val: i64 = primary.as_str().parse().unwrap();
                    Ok(Expr::Number { span, val })
                }
                Rule::rval => parse_rval(Rc::clone(&input), primary.into_inner()),
                x => Err(Error::new_from_span(
                    ErrorVariant::CustomError {
                        message: format!("Expected expression, got {x:?}"),
                    },
                    primary.as_span(),
                )),
            }
        })
        .map_prefix(|op, rhs| {
            let verb = match op.as_rule() {
                Rule::UMINUS => Verb::Minus,
                Rule::NOT => Verb::Not,
                x => {
                    return Err(Error::new_from_span(
                        ErrorVariant::CustomError {
                            message: format!("Expected prefix operator, got {x:?}"),
                        },
                        op.as_span(),
                    ))
                }
            };
            let rhs = rhs?;
            let span = Span::merge(
                &Span::from_pest(Rc::clone(&input), &op.as_span()),
                rhs.span(),
            );
            Ok(Expr::UnaryOp {
                span,
                op: verb,
                expr: Box::new(rhs),
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
                x => {
                    return Err(Error::new_from_span(
                        ErrorVariant::CustomError {
                            message: format!("Expected logical operator, got {x:?}"),
                        },
                        op.as_span(),
                    ))
                }
            };

            if verb == Verb::Dot {
                let (rhs_span, field) = match rhs? {
                    Expr::ProgramSymbol { span, sym } => (span, sym),
                    x => {
                        return Err(Error::new_from_span(
                            ErrorVariant::CustomError {
                                message: format!("invalid field {x:?}"),
                            },
                            op.as_span(),
                        ));
                    }
                };
                let span = Span::merge(
                    &Span::from_pest(Rc::clone(&input), &op.as_span()),
                    &rhs_span,
                );
                Ok(Expr::FieldAccess {
                    span,
                    expr: FieldAccess {
                        record: Box::new(lhs?),
                        field,
                    },
                })
            } else {
                let lhs = lhs?;
                let rhs = rhs?;
                let span = Span::merge(lhs.span(), rhs.span());
                Ok(Expr::BinOp {
                    span,
                    expr: BinOp {
                        lhs: Box::new(lhs),
                        op: verb,
                        rhs: Box::new(rhs),
                    },
                })
            }
        })
        .map_postfix(|lhs, op| {
            let lhs = lhs?;
            let op_span = op.as_span();
            match op.as_rule() {
                Rule::fnapp_args => {
                    let results = op
                        .into_inner()
                        .map(|e| parse_rval(Rc::clone(&input), e.into_inner()))
                        .collect::<Vec<Result<_>>>();
                    let args = results.into_iter().collect::<Result<Vec<_>>>()?;

                    let span =
                        Span::merge(lhs.span(), &Span::from_pest(Rc::clone(&input), &op_span));
                    Ok(Expr::App {
                        span,
                        expr: AppExpr {
                            func: Box::new(lhs),
                            func_sort: Sort::ToBeInferred,
                            args,
                        },
                    })
                }
                Rule::index => {
                    let idx = parse_rval(Rc::clone(&input), op.into_inner())?;
                    let span = Span::merge(lhs.span(), idx.span());
                    Ok(Expr::Index {
                        span,
                        expr: IndexExpr {
                            lhs: Box::new(lhs),
                            idx: Box::new(idx),
                        },
                    })
                }
                x => {
                    return Err(Error::new_from_span(
                        ErrorVariant::CustomError {
                            message: format!("Expected postfix operator, got {x:?}"),
                        },
                        op.as_span(),
                    ))
                }
            }
        })
        .parse(pairs)
}
