use std::rc::Rc;

use pest::error::ErrorVariant;
use pest::iterators::{Pair, Pairs};
use pest::pratt_parser::{Assoc, Op, PrattParser};
use pest_consume::Error;

use crate::ast::declarations::Binding;
use crate::ast::logic::Fmla;
use crate::ast::span::Span;
use crate::ast::{expressions::*, logic};
use crate::parser::ivy::*;

lazy_static::lazy_static! {
    static ref PRATT: PrattParser<Rule> =
    PrattParser::new()
        .op(Op::infix(Rule::ARROW, Assoc::Right))
        .op(Op::infix(Rule::COLON, Assoc::Left))

        .op(Op::infix(Rule::PLUS, Assoc::Left) | Op::infix(Rule::MINUS, Assoc::Left))
        .op(Op::infix(Rule::TIMES, Assoc::Left) | Op::infix(Rule::DIV, Assoc::Left))

        .op(Op::infix(Rule::OR, Assoc::Left) | Op::infix(Rule::AND, Assoc::Left))

        .op(Op::infix(Rule::LT, Assoc::Left) | Op::infix(Rule::LE, Assoc::Left) |
            Op::infix(Rule::GT, Assoc::Left) | Op::infix(Rule::GE, Assoc::Left) |
            Op::infix(Rule::EQ, Assoc::Left) | Op::infix(Rule::NEQ, Assoc::Left))

        .op(Op::infix(Rule::IFF, Assoc::Left))

        // Prefix
        .op(Op::prefix(Rule::UMINUS))
        .op(Op::prefix(Rule::NOT))

        // Postfix
        .op(Op::postfix(Rule::log_app_args))

        .op(Op::infix(Rule::DOT, Assoc::Left));
}

// TODO: this should be something other than a Symbol.
pub fn parse_lsym(input: Rc<str>, primary: Pair<'_, Rule>) -> Result<Symbol> {
    let span = Span::from_pest(input, &primary.as_span());
    let mut pairs = primary.into_inner();
    let name = pairs.next().unwrap().as_str().to_owned();
    let sort = pairs.next().map(|s| vec![s.as_str().to_owned()]);

    match sort {
        None => Ok(Symbol::from(name, Sort::ToBeInferred, span)),
        Some(sort) => Ok(Symbol::from(name, Sort::Annotated(sort), span)),
    }
}

pub fn parse_log_term(input: Rc<str>, pairs: Pairs<Rule>) -> Result<Fmla> {
    PRATT
        .map_primary(|primary| {
            let span = Span::from_pest(Rc::clone(&input), &primary.as_span());
            match primary.as_rule() {
                Rule::relation_lval => {
                    let mut pairs = primary.into_inner();
                    let name = pairs.next().unwrap().as_str().to_owned();
                    let args = pairs
                        .next()
                        .unwrap()
                        .into_inner()
                        .map(|e| parse_log_term(Rc::clone(&input), e.into_inner()))
                        .collect::<Result<Vec<_>>>()?;

                    Ok(Fmla::App {
                        span: span.clone(),
                        app: logic::LogicApp {
                            func: Box::new(Fmla::ProgramSymbol {
                                span: span.clone(), //XXX: not really right, this is the whole application's span
                                sym: Binding::from(name, Sort::ToBeInferred, span),
                            }),
                            args,
                        },
                    })
                },
                Rule::boollit => {
                    let val = match primary.as_str() {
                        "true" => true,
                        "false" => false,
                        _ => unreachable!(),
                    };
                    Ok(Fmla::Boolean { span, val })
                }
                Rule::number => {
                    let val: i64 = primary.as_str().parse().unwrap();
                    Ok(Fmla::Number { span, val })
                },
                Rule::logicsym => Ok(Fmla::LogicSymbol {
                    span,
                    sym: parse_lsym(Rc::clone(&input), primary)?
                }),
                Rule::PROGTOK => Ok(Fmla::ProgramSymbol {
                    span: span.clone(),
                    sym: Symbol::from(primary.as_str(), Sort::ToBeInferred, span),
                }),
                Rule::log_term => parse_log_term(Rc::clone(&input), primary.into_inner()),
                Rule::fmla => {
                    let node = Node::new_with_user_data(primary, Rc::clone(&input));
                    IvyParser::fmla(node)
                },
                Rule::forall => {
                    let node = Node::new_with_user_data(primary, Rc::clone(&input));
                    let fmla = IvyParser::forall(node)?;
                    Ok(Fmla::Forall { span, fmla })
                }
                Rule::exists => {
                    let node = Node::new_with_user_data(primary, Rc::clone(&input));
                    let fmla = IvyParser::exists(node)?;
                    Ok(Fmla::Exists { span, fmla })
                }
                x => Err(Error::new_from_span(
                    ErrorVariant::CustomError {
                        message: format!("Expected formula expression, got {x:?}"),
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
                            message: format!("Expected unary operator, got {x:?}"),
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
            Ok(Fmla::UnaryOp {
                span,
                op: verb,
                fmla: Box::new(rhs),
            })
        })
        .map_infix(|lhs, op, rhs| {
            let verb = match op.as_rule() {
                Rule::AND => Verb::And,
                Rule::OR => Verb::Or,
                Rule::IFF => Verb::Iff,
                Rule::ARROW => Verb::Arrow,
                Rule::GT => Verb::Gt,
                Rule::GE => Verb::Ge,
                Rule::LT => Verb::Lt,
                Rule::LE => Verb::Le,
                Rule::EQ => Verb::Equals,
                Rule::NEQ => Verb::Notequals,
                Rule::DOT => Verb::Dot,

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

            // Lift a Dot binary operation into a field access.
            if verb == Verb::Dot {
                let (rhs_span, field) = match rhs? {
                    Fmla::ProgramSymbol { span, sym } => (span, sym),
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

                let lhs = lhs?;
                if lhs.is_quantified() {
                    return Err(Error::new_from_span(
                            ErrorVariant::CustomError {
                                message: format!("Can't apply `.` operator to quantified formula {lhs:?}"),
                            },
                            op.as_span().clone()
                        )
                    );
                }

                Ok(Fmla::FieldAccess {
                    span,
                    fmla: logic::FieldAccess {
                        record: Box::new(lhs),
                        field,
                    },
                })
            } else {
                let lhs = lhs?;
                let rhs = rhs?;
                let span = Span::merge(lhs.span(), rhs.span());
                Ok(Fmla::BinOp {
                    span,
                    binop: logic::LogicBinOp {
                        lhs: Box::new(lhs),
                        op: verb,
                        rhs: Box::new(rhs),
                    },
                })
            }
        })
        .map_postfix(|lhs, op| {
            let pest_span = op.as_span();
            let op_span = Span::from_pest(Rc::clone(&input), &op.as_span());
            match op.as_rule() {
                Rule::log_app_args => {
                    let results = op
                        .into_inner()
                        .map(|e| parse_log_term(Rc::clone(&input), e.into_inner()))
                        .collect::<Vec<Result<_>>>();
                    let args = results.into_iter().collect::<Result<Vec<_>>>()?;

                    let lhs = lhs?;
                    if lhs.is_quantified() {
                        return Err(Error::new_from_span(
                                ErrorVariant::CustomError {
                                    message: format!("Can't perform function application on arbitrary formula {lhs:?}"),
                                },
                                pest_span,
                            ))
                    }

                    let span = Span::merge(lhs.span(), &op_span);
                    Ok(Fmla::App {
                        span,
                        app: logic::LogicApp {
                            func: Box::new(lhs),
                            args,
                        },
                    })
                }
                x => {
                    return Err(Error::new_from_span(
                        ErrorVariant::CustomError {
                            message: format!("Expected logical operator, got {x:?}"),
                        },
                        op.as_span(),
                    ))
                }
            }
        })
        .parse(pairs)
}
