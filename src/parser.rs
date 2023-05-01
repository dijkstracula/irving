use pest::{pratt_parser::{Assoc, Op, PrattParser}, iterators::Pairs};
use pest_consume::{Parser, Error, match_nodes};
use crate::ast;

// include the grammar file so that Cargo knows to rebuild this file on grammar
// changes (c.f. the Calyx frontend compiler)
const _LEXER: &str = include_str!("grammars/lexer.pest");
const _GRAMMAR: &str = include_str!("grammars/syntax.pest");

#[derive(Parser)]
#[grammar = "grammars/lexer.pest"]
#[grammar = "grammars/syntax.pest"]
pub struct IvyParser;


lazy_static::lazy_static! {
    // This ordering is taken from the precedence numberings
    // from ivy2/lang.ivy.
    static ref PRATT: PrattParser<Rule> =
    PrattParser::new()
        .op(Op::infix(Rule::DOT, Assoc::Left))
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

        .op(Op::infix(Rule::EQ, Assoc::Left))
        .op(Op::infix(Rule::NEQ, Assoc::Left))
        .op(Op::infix(Rule::ISA, Assoc::Left))
        
        .op(Op::infix(Rule::IFF, Assoc::Left))
        .op(Op::infix(Rule::OR, Assoc::Left))
        .op(Op::infix(Rule::AND, Assoc::Left))

        .op(Op::infix(Rule::COMMA, Assoc::Left));
}

fn parse_expr(pairs: Pairs<Rule>) -> Result<ast::Expr> {
    PRATT
    .map_primary(|primary| match primary.as_rule() {
        Rule::symbol => {
            let name = primary.as_str();
            Ok(ast::Expr::Symbol(name.to_owned()))
        },
        Rule::number => {
            let val: i64 = primary.as_str().parse::<>().unwrap();
            Ok(ast::Expr::Number(val))
        }

        Rule::unary_expr => {
            parse_expr(primary.into_inner())
        }
        Rule::expr => {
            parse_expr(primary.into_inner())
        }
        _ => unreachable!("parse_expr expected primary, found {:?}", primary),
    })
    .map_infix(|lhs, op, rhs| {
        let verb = match op.as_rule() {
            Rule::DOT => ast::Verb::Dot,
            _ => unimplemented!()
        };

        Ok(ast::Expr::BinOp { 
            lhs: Box::new(lhs?), 
            op: verb, 
            rhs: Box::new(rhs?) })
    })
    .parse(pairs)
}

pub type Result<T> = std::result::Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>; //TODO: consume a UserData thing rather than ()


#[pest_consume::parser]
#[allow(dead_code)]
impl IvyParser {

    // Terminals

    fn EOI(_input: Node) -> Result<()> {
        Ok(())
    }

    fn number(input: Node) -> Result<i64> {
        input.as_str()
            .parse::<i64>()
            .map_err(|_| input.error("Expected number"))
    }

    fn symbol(input: Node) -> Result<String> {
        Ok(input.as_str().to_owned())
    }

    // Utils

    // Exprs

    pub fn expr(input: Node) -> Result<ast::Expr> {
        let pairs = input.as_pair().to_owned().into_inner();
        parse_expr(pairs)
    }

    // Decls

    pub fn param(input: Node) -> Result<ast::Param> {
        match_nodes!(
        input.into_children();
        [symbol(name), symbol(typ)] => {
            Ok(ast::Param {name, typ})
        })
    }

    pub fn paramlist(input: Node) -> Result<Vec<ast::Param>> {
        match_nodes!(
        input.into_children();
        [param(params)..] => {
            Ok(params.collect())
        })
    }

    pub fn decl_ret(input: Node) -> Result<Option<ast::Param>> {
        match_nodes!(
        input.into_children();
        [param(ret)] => Ok(Some(ret)) )
    }

    pub fn decl_sig(input: Node) -> Result<ast::DeclSig> {
        match_nodes!(
        input.into_children();
        [symbol(name), paramlist(params)] => {
            Ok(ast::DeclSig { name, params})
        })
    }


    pub fn block(input: Node) -> Result<Vec<ast::Decl>> {
        match_nodes!(
        input.into_children();
        [decl(decls)..] => {
            Ok(decls.collect())
        })
    }

    pub fn action_decl(input: Node) -> Result<ast::Action> {
        match_nodes!(
        input.into_children();
            [decl_sig(ast::DeclSig{name, params}), decl_ret(ret), block(body)] => Ok(
                ast::Action{name, kind: ast::ActionKind::Internal, params, ret, body: Some(body)}
            ),
            [decl_sig(ast::DeclSig{name, params}), block(body)] => Ok(
                ast::Action{name, kind: ast::ActionKind::Internal, params, ret: None, body: Some(body)}
            ),
            [decl_sig(ast::DeclSig{name, params})] => Ok(
                ast::Action{name, kind: ast::ActionKind::Internal, params, ret: None, body: None}
            ),
        )
    }

    pub fn module_decl(input: Node) -> Result<ast::Module> {
        match_nodes!(
        input.into_children();
        [decl_sig(ast::DeclSig{name, params}), block(body)] => Ok(
            ast::Module{name, params, body}
        ))
    }

    pub fn decl(input: Node) -> Result<ast::Decl> {
        match_nodes!(
        input.into_children();
        [action_decl(decl)] => Ok(ast::Decl::Action(decl)),
        [module_decl(decl)] => Ok(ast::Decl::Module(decl))
        )
    }

    pub fn langver(input: Node) -> Result<(u8, u8)> {
        match_nodes!(
        input.clone().into_children(); // XXX: avoid the clone?
        [number(major)] => {
            if major >= 255 {
                Err(input.error(format!("Invalid major version number {:?}", major)))
            } else {
                Ok((major as u8, 0))
            }
        },
        [number(major), number(minor)] => {
            if major >= 255 {
                Err(input.error(format!("Invalid major version number {:?}.{:?}", major, minor)))
            } else if minor >= 255 { 
                Err(input.error(format!("Invalid minor version number {:?}.{:?}", major, minor)))
            } else {
                Ok((major as u8, minor as u8))
            }
        })
    }

    pub fn prog(input: Node) -> Result<ast::Prog> {
        match_nodes!(
        input.into_children();
        [langver((major, minor)) /* Todo: decl */, EOI(())] => {
            Ok(ast::Prog { major_version: major, minor_version: minor, decls: vec!() })
        })
    }
}