use pest::{pratt_parser::{Assoc, Op, PrattParser}, iterators::Pairs};
use pest_consume::{Parser, Error, match_nodes};
use crate::ast::{self};

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

        .op(Op::infix(Rule::COMMA, Assoc::Left))

        .op(Op::prefix(Rule::UMINUS))
        .op(Op::prefix(Rule::NOT))


        // Postfix
        // TODO: array indexing
        .op(Op::postfix(Rule::fnapp_args));
}

fn parse_expr(pairs: Pairs<Rule>) -> Result<ast::Expr> {
    PRATT
    .map_primary(|primary| match primary.as_rule() {
        Rule::term => {
            let mut pairs = primary.into_inner();
            let id = pairs.next()
                .map(|p| p.into_inner()
                    .map(|s| s.as_str().to_owned()).collect::<Vec<_>>())
                .unwrap();
            let sort = pairs.next()
                .map(|p| p.into_inner()
                    .map(|s| s.as_str().to_owned()).collect::<Vec<_>>());
            match sort {
                // TODO: wondering if either return path should just be a Term.
                None    => Ok(ast::Expr::Identifier(id)),
                Some(_) => Ok(ast::Expr::Term(ast::Term{ id, sort }))
            }
        }
        Rule::ident => {
            let results = primary
                .into_inner()
                .map(|p| p.as_str().to_owned())
                .collect::<Vec<_>>();
            Ok(ast::Expr::Identifier(results))
        }
        Rule::number => {
            let val: i64 = primary.as_str().parse::<>().unwrap();
            Ok(ast::Expr::Number(val))
        }
        Rule::simple_expr => {
            parse_expr(primary.into_inner())
        }
        _ => unreachable!("parse_expr expected primary, found {:?}", primary),
    })

    .map_prefix(|op, rhs| {
        let verb = match op.as_rule() {
            Rule::UMINUS => ast::Verb::Minus,
            Rule::NOT    => ast::Verb::Not,
            _ => unreachable!("Unexpected unary op")
        };
        Ok(ast::Expr::UnaryOp { op: verb, expr: Box::new(rhs?) })
    })

    .map_infix(|lhs, op, rhs| {
        let verb = match op.as_rule() {
            Rule::DOT => ast::Verb::Dot,
            Rule::AND => ast::Verb::And,
            Rule::OR => ast::Verb::Or,
            Rule::ARROW => ast::Verb::Arrow,
            Rule::GT => ast::Verb::Gt,
            Rule::GE => ast::Verb::Ge,
            Rule::LT => ast::Verb::Lt,
            Rule::LE => ast::Verb::Le,
            Rule::EQ => ast::Verb::Equals,
            Rule::NEQ => ast::Verb::Notequals,

            Rule::PLUS => ast::Verb::Plus,
            Rule::MINUS => ast::Verb::Minus,
            _ => unimplemented!()
        };

        Ok(ast::Expr::BinOp { 
            lhs: Box::new(lhs?), 
            op: verb, 
            rhs: Box::new(rhs?) 
        })
    })

    .map_postfix(|lhs, op| match op.as_rule() {
        Rule::fnapp_args => {
            let results = op.into_inner()
                .map(|e| parse_expr(e.into_inner()))
                .collect::<Vec<Result<_>>>();
            let args = results.into_iter()
                .collect::<Result<Vec<_>>>()?;
            Ok(ast::Expr::App(ast::AppExpr { func: Box::new(lhs?), args }))
        },
        _ => unimplemented!()
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
    
    fn param(input: Node) -> Result<ast::Param> {
        match_nodes!(
        input.into_children();
        [symbol(id), symbol(sort)] => Ok(ast::Param {id, sort: Some(sort) }),
        [symbol(id)] => Ok(ast::Param {id, sort: None })
        )
    }

    pub fn paramlist(input: Node) -> Result<Vec<ast::Param>> {
        match_nodes!(
        input.into_children();
        [param(params)..] => {
            Ok(params.collect())
        })
    }

    fn ident(input: Node) -> Result<Vec<String>> {
        match_nodes!(
        input.into_children();
        [symbol(qualifiers)..] => Ok(qualifiers.collect()),
        )
    }

    fn term(input: Node) -> Result<ast::Term> {
        match_nodes!(
        input.into_children();
        [ident(id), ident(sort)] => Ok(ast::Term {id, sort: Some(sort) }),
        [ident(id)] => Ok(ast::Term {id, sort: None })
        )
    }

    pub fn termlist(input: Node) -> Result<Vec<ast::Term>> {
        match_nodes!(
        input.into_children();
        [term(terms)..] => {
            Ok(terms.collect())
        })
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
        [simple_expr(e)] => Ok(e),
        [exists(e)] => Ok(ast::Expr::Formula(e)),
        [forall(e)] => Ok(ast::Expr::Formula(e)),
        )
    }

    pub fn fnapp_args(input: Node) -> Result<Vec<ast::Expr>> {
        match_nodes!(
        input.into_children();
        [expr(args)..] => {
            Ok(args.collect())
        })
    }

    pub fn forall(input: Node) -> Result<ast::Formula> {
        match_nodes!(
        input.into_children();
        [paramlist(params), expr(e)] => {
            Ok(ast::Formula::Forall { params, expr: Box::new(e)})
        })
    }

    pub fn exists(input: Node) -> Result<ast::Formula> {
        match_nodes!(
        input.into_children();
        [paramlist(params), expr(e)] => {
            Ok(ast::Formula::Exists { params, expr: Box::new(e)})
        })
    }

    // Decls

    pub fn decl_ret(input: Node) -> Result<Option<ast::Param>> {
        match_nodes!(
        input.into_children();
        [param(ret)] => Ok(Some(ret)))
    }

    pub fn decl_sig(input: Node) -> Result<ast::DeclSig> {
        match_nodes!(
        input.into_children();
        [ident(name), paramlist(params)] => {
            Ok(ast::DeclSig { name, params})
        },
        [ident(name)] => {
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

    pub fn axiom_decl(input: Node) -> Result<ast::Expr> {
        match_nodes!(
        input.into_children();
        [expr(e)] => Ok(e)
        )
    }

    pub fn after_decl(input: Node) -> Result<ast::AfterDecl> {
        match_nodes!(
        input.into_children();
        [decl_sig(ast::DeclSig{name, params}), decl_ret(ret), decl_block(body)] => Ok(
            ast::AfterDecl { name, params: Some(params), ret: ret, body}
        ),
        [decl_sig(ast::DeclSig{name, params}), decl_block(body)] => Ok(
            ast::AfterDecl { name, params: Some(params), ret: None, body}
        ),
        [ident(name), decl_block(body)] => Ok(
            ast::AfterDecl { name, params: None, ret: None, body}
        ),
        )
    }

    pub fn export_decl(input: Node) -> Result<ast::ExportDecl> {
        match_nodes!(
        input.into_children();
            [ident(name)] => Ok(
                ast::ExportDecl::ForwardRef(name)
            ),
            [action_decl(decl)] => Ok(
                ast::ExportDecl::Action(decl)
            ),
        )
    }
    pub fn function_decl(input: Node) -> Result<ast::FunctionDecl> {
        match_nodes!(
        input.into_children();
            [decl_sig(ast::DeclSig{name, params}), symbol(ret)] => Ok(
                ast::FunctionDecl{name, params, ret}
            ),
        )
    }

    pub fn invariant_decl(input: Node) -> Result<ast::Expr> {
        match_nodes!(
        input.into_children();
        [expr(e)] => Ok(e)
        )
    }

    pub fn instance_decl(input: Node) -> Result<ast::InstanceDecl> {
        match_nodes!(
        input.into_children();
        [ident(name), decl_sig(ast::DeclSig{name: sort, params: sort_args})] => 
            Ok(ast::InstanceDecl{name, sort, args: sort_args})
        )
    }


    pub fn module_decl(input: Node) -> Result<ast::ModuleDecl> {
        match_nodes!(
        input.into_children();
        [decl_sig(ast::DeclSig{name, params}), decl_block(body)] => Ok(
            ast::ModuleDecl{name, params, body}
        ))
    }

    pub fn object_decl(input: Node) -> Result<ast::ObjectDecl> {
        match_nodes!(
        input.into_children();
        [decl_sig(ast::DeclSig{name, params}), decl_block(body)] => Ok(
            ast::ObjectDecl{name, params, body}
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


    pub fn var_decl(input: Node) -> Result<ast::Term> {
        match_nodes!(
        input.into_children();
        [term(term)] => Ok(term))
    }

    pub fn decl(input: Node) -> Result<ast::Decl> {
        match_nodes!(
        input.into_children();
        [action_decl(decl)]   => Ok(ast::Decl::Action(decl)),
        [after_decl(decl)]    => Ok(ast::Decl::AfterAction(decl)),
        [axiom_decl(fmla)]    => Ok(ast::Decl::Axiom(fmla)),
        [export_decl(fmla)]   => Ok(ast::Decl::Export(fmla)),
        [function_decl(decl)] => Ok(ast::Decl::Function(decl)),
        [invariant_decl(fmla)] => Ok(ast::Decl::Invariant(fmla)),
        [instance_decl(decl)] => Ok(ast::Decl::Instance(decl)),
        [module_decl(decl)]   => Ok(ast::Decl::Module(decl)),
        [object_decl(decl)]   => Ok(ast::Decl::Object(decl)),
        [relation_decl(decl)] => Ok(ast::Decl::Relation(decl)),
        [type_decl(decl)]     => Ok(ast::Decl::Type(decl)),
        [var_decl(decl)]      => Ok(ast::Decl::Var(decl)),
        [stmt(stmts)..]       => Ok(ast::Decl::Stmts(stmts.collect()))
        )
    }

    // Actions

    pub fn action(input: Node) -> Result<ast::Action> {
        match_nodes!(
        input.into_children();
        [assert_action(action)] => Ok(ast::Action::Assert(action)),
        [assign_action(action)] => Ok(ast::Action::Assign(action)),
        [assume_action(action)] => Ok(ast::Action::Assume(action)),
        [ensure_action(action)] => Ok(ast::Action::Ensure(action)),
        [requires_action(action)] => Ok(ast::Action::Requires(action))
        )
    }

    pub fn actions(input: Node) -> Result<Vec<ast::Action>> {
        match_nodes!(
        input.into_children();
        [action(actions)..] => Ok(actions.collect()))
    }

    pub fn assign_action(input: Node) -> Result<ast::AssignAction> {
        match_nodes!(
        input.into_children();
        [var_decl(ast::Term{id, sort}), expr(rhs)] => Ok(
            ast::AssignAction{lhs: ast::Expr::Term(ast::Term{id, sort}), rhs}
        ),
        [expr(lhs), expr(rhs)] => Ok(
            // TODO: need a ::new() function that returns
            // some sort of error if lhs isn't a valid lval.
            ast::AssignAction{lhs, rhs}
        ),
        )
    }

    pub fn assert_action(input: Node) -> Result<ast::AssertAction> {
        match_nodes!(
        input.into_children();
        [expr(pred)] => Ok(ast::AssertAction{pred}),
        )
    }

    pub fn assume_action(input: Node) -> Result<ast::AssumeAction> {
        match_nodes!(
        input.into_children();
        [expr(pred)] => Ok(ast::AssumeAction{pred}),
        )
    }

    pub fn ensure_action(input: Node) -> Result<ast::EnsureAction> {
        match_nodes!(
        input.into_children();
        [expr(pred)] => Ok(ast::EnsureAction{pred}),
        )
    }

    pub fn requires_action(input: Node) -> Result<ast::RequiresAction> {
        match_nodes!(
        input.into_children();
        [expr(pred)] => Ok(ast::RequiresAction{pred}),
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