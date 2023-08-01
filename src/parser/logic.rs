use pest::iterators::{Pair, Pairs};
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

// TODO: this should be something other than a Symbol.
pub fn parse_lsym(primary: Pair<'_, Rule>) -> Result<Symbol> {
    // TODO: we need a separate AST node for logicvars.
    let mut pairs = primary.into_inner();
    let id = pairs.next().unwrap().as_str().to_owned();
    let sort = pairs.next().map(|s| vec![s.as_str().to_owned()]);
    match sort {
        None => Ok(Symbol {
            id,
            sort: Sort::ToBeInferred,
        }),
        Some(sort) => Ok(Symbol {
            id,
            sort: Sort::Annotated(sort),
        }),
    }
}

pub fn parse_log_term(pairs: Pairs<Rule>) -> Result<ExprKind> {
    PRATT
        .map_primary(|primary| match primary.as_rule() {
            Rule::relation_lval => {
                let mut pairs = primary.into_inner();
                let name = pairs.next().unwrap().as_str().to_owned();
                let args = pairs
                    .next()
                    .unwrap()
                    .into_inner()
                    .map(|e| parse_log_term(e.into_inner()))
                    .collect::<Result<Vec<_>>>()?;

                Ok(ExprKind::App(AppExpr {
                    func: Box::new(ExprKind::inferred_progsym(name)),
                    args,
                }))
            }
            Rule::logicsym => Ok(ExprKind::ProgramSymbol(parse_lsym(primary)?)),
            Rule::PROGTOK => {
                let tok = primary.as_str().to_owned();
                Ok(ExprKind::inferred_progsym(tok))
            }
            Rule::boollit => {
                let val = match primary.as_str() {
                    "true" => true,
                    "false" => false,
                    _ => unreachable!(),
                };
                Ok(ExprKind::Boolean(val))
            }
            Rule::number => {
                let val: i64 = primary.as_str().parse().unwrap();
                Ok(ExprKind::Number(val))
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
            Ok(ExprKind::UnaryOp {
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

            Ok(ExprKind::BinOp(BinOp {
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
                Ok(ExprKind::App(AppExpr {
                    func: Box::new(lhs?),
                    args,
                }))
            }
            _ => unimplemented!(),
        })
        .parse(pairs)
}
