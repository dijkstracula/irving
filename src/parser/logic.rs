use std::rc::Rc;

use pest::error::ErrorVariant;
use pest::iterators::{Pair, Pairs};
use pest::pratt_parser::{Assoc, Op, PrattParser};
use pest_consume::Error;

use crate::ast::declarations::Binding;
use crate::ast::expressions::*;
use crate::ast::span::Span;
use crate::parser::ivy::*;

lazy_static::lazy_static! {
    static ref PRATT: PrattParser<Rule> =
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

        // Prefix
        .op(Op::prefix(Rule::UMINUS))
        .op(Op::prefix(Rule::NOT))

        // Postfix
        // XXX: it is unfortunate how close the program expression's
        // Pratt parser is to this one.
        .op(Op::postfix(Rule::log_app_args))

        .op(Op::infix(Rule::DOT, Assoc::Left));
}

// TODO: this should be something other than a Symbol.
pub fn parse_lsym(_input: Rc<str>, primary: Pair<'_, Rule>) -> Result<Symbol> {
    // TODO: we need a separate AST node for logicvars.
    let mut pairs = primary.into_inner();
    let name = pairs.next().unwrap().as_str().to_owned();
    let sort = pairs.next().map(|s| vec![s.as_str().to_owned()]);
    match sort {
        None => Ok(Symbol::from(name, Sort::ToBeInferred)),
        Some(sort) => Ok(Symbol::from(name, Sort::Annotated(sort))),
    }
}

pub fn parse_log_term(input: Rc<str>, pairs: Pairs<Rule>) -> Result<Expr> {
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

                    Ok(Expr::App {
                        span: span.clone(),
                        expr: AppExpr {
                            func: Box::new(Expr::ProgramSymbol {
                                span: span.clone(), //XXX: not really right
                                sym: Binding::from(name, Sort::ToBeInferred),
                            }),
                            args,
                        },
                    })
                }
                Rule::logicsym => Ok(Expr::LogicSymbol {
                    span: span,
                    sym: parse_lsym(Rc::clone(&input), primary)?,
                }),
                Rule::PROGTOK => {
                    let name = primary.as_str().to_owned();
                    Ok(Expr::ProgramSymbol {
                        span,
                        sym: Binding::from(name, Sort::ToBeInferred),
                    })
                }
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
                Rule::log_term => parse_log_term(Rc::clone(&input), primary.into_inner()),
                x => Err(Error::new_from_span(
                    ErrorVariant::CustomError {
                        message: format!("Expected formula, got {x:?}"),
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
                            message: format!("Expected logical operator, got {x:?}"),
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
                x => {
                    return Err(Error::new_from_span(
                        ErrorVariant::CustomError {
                            message: format!("Expected logical operator, got {x:?}"),
                        },
                        op.as_span(),
                    ))
                }
            };

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
        })
        .map_postfix(|lhs, op| {
            let op_span = Span::from_pest(Rc::clone(&input), &op.as_span());
            match op.as_rule() {
                Rule::log_app_args => {
                    let results = op
                        .into_inner()
                        .map(|e| parse_log_term(Rc::clone(&input), e.into_inner()))
                        .collect::<Vec<Result<_>>>();
                    let args = results.into_iter().collect::<Result<Vec<_>>>()?;

                    let lhs = lhs?;
                    let span = Span::merge(lhs.span(), &op_span);
                    Ok(Expr::App {
                        span,
                        expr: AppExpr {
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
