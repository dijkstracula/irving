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
        Rule::simple_expr => {
            parse_expr(primary.into_inner())
        }
        _ => unreachable!("parse_expr expected primary, found {:?}", primary),
    })
    .map_infix(|lhs, op, rhs| {
        let verb = match op.as_rule() {
            Rule::DOT => ast::Verb::Dot,
            Rule::LT => ast::Verb::Lt,
            Rule::EQ => ast::Verb::Equals,
            Rule::PLUS => ast::Verb::Plus,
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

    pub fn simple_expr(input: Node) -> Result<ast::Expr> {
        let pairs = input.as_pair().to_owned().into_inner();
        parse_expr(pairs)
    }

    pub fn expr(input: Node) -> Result<ast::Expr> {
        match_nodes!(
        input.into_children();
        // Ergonomically this is weird.  I feel like there has to be a better way of
        // bridging the world of the Pratt parser and the PEG parser...
        [expr(func), fnapp_args(args)] => {
           Ok(ast::Expr::App(
            ast::AppExpr{func: Box::new(func), args}))
        },
        [simple_expr(func), fnapp_args(args)] => {
           Ok(ast::Expr::App(
            ast::AppExpr{func: Box::new(func), args}))
        },
        [simple_expr(e)] => Ok(e),
        [expr(e)] => Ok(e))
    }

    pub fn fnapp_args(input: Node) -> Result<Vec<ast::Expr>> {
        match_nodes!(
        input.into_children();
        [expr(args)..] => {
            Ok(args.collect())
        })
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
        },
        [symbol(name)] => {
            Ok(ast::DeclSig { name, params: vec!()})
        })
    }

    pub fn decl_block(input: Node) -> Result<Vec<ast::Decl>> {
        match_nodes!(
        input.into_children();
        [decl(decls)..] => {
            Ok(decls.collect())
        })
    }

    pub fn action_decl(input: Node) -> Result<ast::ActionDecl> {
        match_nodes!(
        input.into_children();
            [decl_sig(ast::DeclSig{name, params}), decl_ret(ret), decl_block(body)] => Ok(
                ast::ActionDecl{name, kind: ast::ActionKind::Internal, params, ret, body: Some(body)}
            ),
            [decl_sig(ast::DeclSig{name, params}), decl_block(body)] => Ok(
                ast::ActionDecl{name, kind: ast::ActionKind::Internal, params, ret: None, body: Some(body)}
            ),
            [decl_sig(ast::DeclSig{name, params})] => Ok(
                ast::ActionDecl{name, kind: ast::ActionKind::Internal, params, ret: None, body: None}
            ),
        )
    }

    pub fn function_decl(input: Node) -> Result<ast::Function> {
        match_nodes!(
        input.into_children();
            [decl_sig(ast::DeclSig{name, params}), symbol(ret)] => Ok(
                ast::Function{name, params, ret}
            ),
        )
    }

    pub fn module_decl(input: Node) -> Result<ast::Module> {
        match_nodes!(
        input.into_children();
        [decl_sig(ast::DeclSig{name, params}), decl_block(body)] => Ok(
            ast::Module{name, params, body}
        ))
    }

    pub fn relation_decl(input: Node) -> Result<ast::Relation> {
        match_nodes!(
        input.into_children();
            [decl_sig(ast::DeclSig{name, params})] => Ok(
                ast::Relation{name, params}
            ),
        )
    }

    pub fn type_decl(input: Node) -> Result<ast::Type> {
        match_nodes!(
        input.into_children();
        [symbol(sort), symbol(supr)] => Ok(
            ast::Type {sort: sort, supr: Some(supr) }
        ),
        [symbol(sort)] => Ok(
            ast::Type {sort: sort, supr: None }
        ))
    }


    pub fn var_decl(input: Node) -> Result<ast::Var> {
        match_nodes!(
        input.into_children();
        [symbol(name), symbol(typ)] => Ok(
            ast::Var {name, typ: Some(typ)}
        ),
        [symbol(name)] => Ok(
            ast::Var {name, typ: None}
        ))
    }

    pub fn decl(input: Node) -> Result<ast::Decl> {
        match_nodes!(
        input.into_children();
        [action_decl(decl)]   => Ok(ast::Decl::Action(decl)),
        [function_decl(decl)] => Ok(ast::Decl::Function(decl)),
        [module_decl(decl)]   => Ok(ast::Decl::Module(decl)),
        [relation_decl(decl)] => Ok(ast::Decl::Relation(decl)),
        [type_decl(decl)]     => Ok(ast::Decl::Type(decl)),
        [var_decl(decl)]      => Ok(ast::Decl::Var(decl)),
        [stmt(stmt)]          => Ok(ast::Decl::Stmt(stmt))
        )
    }

    // Actions

    pub fn action(input: Node) -> Result<ast::Action> {
        match_nodes!(
        input.into_children();
        [assign_action(action)] => Ok(ast::Action::Assign(action)))
    }

    pub fn actions(input: Node) -> Result<Vec<ast::Action>> {
        match_nodes!(
        input.into_children();
        [action(actions)..] => Ok(actions.collect()))
    }

    pub fn assign_action(input: Node) -> Result<ast::AssignAction> {
        match_nodes!(
        input.into_children();
        [param(ast::Param{name, typ:_}), expr(rhs)] => Ok(
            ast::AssignAction{lhs: ast::Expr::Symbol(name), rhs}
        ),
        [expr(lhs), expr(rhs)] => Ok(
            // TODO: need a ::new() function that returns
            // some sort of error if lhs isn't a valid lval.
            ast::AssignAction{lhs, rhs}
        ),
        )
    }

    // Statements
    pub fn stmt_block(input: Node) -> Result<Vec<ast::Stmt>> {
        match_nodes!(
        input.into_children();
        [stmt(stmts)..] => Ok(stmts.collect())
        )
    }

    pub fn if_stmt(input: Node) -> Result<ast::If> {
        match_nodes!(
        input.into_children();
        [expr(tst), stmt_block(thens)] => Ok(
            ast::If{tst, thn: thens, els: None}
        ),
        [expr(tst), stmt_block(thens), stmt_block(elses)] => Ok(
            ast::If{tst, thn: thens, els: Some(elses)}
        ),
        )
    }

    pub fn while_stmt(input: Node) -> Result<ast::While> {
        match_nodes!(
        input.into_children();
        [expr(test), stmt_block(stmts)] => Ok(
            ast::While{test, doit: stmts}
        ),
        )
    }

    pub fn stmt(input: Node) -> Result<ast::Stmt> {
        match_nodes!(
        input.into_children();
        [actions(actions)] => Ok(ast::Stmt::CompoundActions(actions)),
        [if_stmt(stmt)]     => Ok(ast::Stmt::If(stmt)),
        [while_stmt(stmt)]  => Ok(ast::Stmt::While(stmt)),
        )
    }

    // Toplevels

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
        [langver((major, minor)), decl(decls).., EOI(())] => {
            Ok(ast::Prog { 
                major_version: major, 
                minor_version: minor,
                decls: decls.collect() })
        })
    }
}