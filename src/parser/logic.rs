use pest::iterators::Pairs;
use pest::pratt_parser::{Assoc, Op, PrattParser};

use crate::ast::expressions::*;
use crate::parser::ivy::*;

lazy_static::lazy_static! {
    // This ordering is taken from the precedence numberings
    // from ivy2/lang.ivy.
    static ref PRATT: PrattParser<Rule> =
    PrattParser::new()
        .op(Op::infix(Rule::DOT, Assoc::Left))
        .op(Op::infix(Rule::COLON, Assoc::Left))

        .op(Op::infix(Rule::LT, Assoc::Left))
        .op(Op::infix(Rule::LE, Assoc::Left))
        .op(Op::infix(Rule::GT, Assoc::Left))
        .op(Op::infix(Rule::GE, Assoc::Left))

        .op(Op::infix(Rule::EQ, Assoc::Left))
        .op(Op::infix(Rule::NEQ, Assoc::Left))

        .op(Op::infix(Rule::IFF, Assoc::Left))
        .op(Op::infix(Rule::ARROW, Assoc::Left))
        .op(Op::infix(Rule::OR, Assoc::Left))
        .op(Op::infix(Rule::AND, Assoc::Left))

        // Prefix
        .op(Op::prefix(Rule::NOT))

        // Postfix
        .op(Op::postfix(Rule::log_app_args));
}

pub fn parse_log_term(pairs: Pairs<Rule>) -> Result<Expr> {
    PRATT
        .map_primary(|primary| match primary.as_rule() {
            Rule::logicvar => {
                let mut pairs = primary.into_inner();
                let id = pairs.next().unwrap().as_str().to_owned();
                let sort = pairs.next().map(|s| vec![s.as_str().to_owned()]);
                match sort {
                    // TODO: wondering if either return path should just be a Term.
                    // TODO: we need a separate AST node for logicvars.
                    None => Ok(Expr::Symbol(Symbol {
                        id,
                        sort: Sort::ToBeInferred,
                    })),
                    Some(sort) => Ok(Expr::Symbol(Symbol {
                        id,
                        sort: Sort::Annotated(sort),
                    })),
                }
            }
            Rule::symbol => Ok(Expr::Symbol(Symbol {
                id: primary.as_str().to_owned(),
                sort: Sort::ToBeInferred,
            })),
            Rule::boollit => {
                let val = match primary.as_str() {
                    "true" => true,
                    "false" => false,
                    _ => unreachable!(),
                };
                Ok(Expr::Boolean(val))
            }
            Rule::number => {
                let val: i64 = primary.as_str().parse().unwrap();
                Ok(Expr::Number(val))
            }
            Rule::log_term => parse_log_term(primary.into_inner()),
            _ => unreachable!("parse_log_term expected primary, found {:?}", primary),
        })
        .map_prefix(|op, rhs| {
            let verb = match op.as_rule() {
                Rule::UMINUS => Verb::Minus,
                Rule::NOT => Verb::Not,
                _ => unreachable!("Unexpected unary op"),
            };
            Ok(Expr::UnaryOp {
                op: verb,
                expr: Box::new(rhs?),
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
                _ => unimplemented!(),
            };

            Ok(Expr::BinOp(BinOp {
                lhs: Box::new(lhs?),
                op: verb,
                rhs: Box::new(rhs?),
            }))
        })
        .map_postfix(|lhs, op| match op.as_rule() {
            Rule::log_app_args => {
                let results = op
                    .into_inner()
                    .map(|e| parse_log_term(e.into_inner()))
                    .collect::<Vec<Result<_>>>();
                let args = results.into_iter().collect::<Result<Vec<_>>>()?;
                Ok(Expr::App(AppExpr {
                    func: Box::new(lhs?),
                    args,
                }))
            }
            _ => unimplemented!(),
        })
        .parse(pairs)
}
