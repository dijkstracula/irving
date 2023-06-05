use pest::error::ErrorVariant;
use pest_consume::{match_nodes, Error, Parser};

use crate::ast::actions::*;
use crate::ast::declarations::*;
use crate::ast::expressions::*;
use crate::ast::logic::*;
use crate::ast::statements::*;
use crate::ast::toplevels::*;

use crate::parser::expressions::parse_expr;
use crate::typechecker::sorts::IvySort;

use super::logic::parse_log_term;

// include the grammar file so that Cargo knows to rebuild this file on grammar
// changes (c.f. the Calyx frontend compiler)
const _LEXER: &str = include_str!("./grammars/lexer.pest");
const _GRAMMAR: &str = include_str!("grammars/syntax.pest");
const _LOGIC: &str = include_str!("grammars/logic.pest");

#[derive(Parser)]
#[grammar = "parser/grammars/lexer.pest"]
#[grammar = "parser/grammars/syntax.pest"]
#[grammar = "parser/grammars/logic.pest"]
pub struct IvyParser;

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
        input
            .as_str()
            .parse::<i64>()
            .map_err(|_| input.error("Expected number"))
    }

    fn symbol(input: Node) -> Result<String> {
        Ok(input.as_str().to_owned())
    }

    fn LOGICVAR(input: Node) -> Result<String> {
        Ok(input.as_str().to_owned())
    }

    fn param(input: Node) -> Result<Param> {
        match_nodes!(
        input.into_children();
        [symbol(id), ident(sort)] => Ok(Param {id, sort: Some(sort) }),
        [symbol(id)] => Ok(Param {id, sort: None })
        )
    }

    pub fn paramlist(input: Node) -> Result<Vec<Param>> {
        match_nodes!(
        input.into_children();
        [param(params)..] => {
            Ok(params.collect())
        })
    }

    fn ident(input: Node) -> Result<Vec<String>> {
        match_nodes!(
        input.into_children();
        // TODO: This is wrong: we need Ident to be an enum of qualifiers or the "this" keyword.
        [symbol(qualifiers)..] => Ok(qualifiers.collect()),
        [THIS(_)] => Ok(vec!("this".into())),
        )
    }

    fn term(input: Node) -> Result<Term> {
        match_nodes!(
        input.into_children();
        [ident(id), ident(sort)] => Ok(Term {id, sort: Some(sort) }),
        [ident(id)] => Ok(Term {id, sort: None })
        )
    }

    pub fn termlist(input: Node) -> Result<Vec<Term>> {
        match_nodes!(
        input.into_children();
        [term(terms)..] => {
            Ok(terms.collect())
        })
    }

    fn THIS(input: Node) -> Result<Expr> {
        Ok(Expr::This)
    }

    // Utils

    // Formulas

    fn logicvar(input: Node) -> Result<Param> {
        match_nodes!(
        input.into_children();
        [LOGICVAR(id), symbol(sort)] => Ok(Param {id, sort: Some(vec!(sort)) }),
        [LOGICVAR(id)]               => Ok(Param {id, sort: None })
        )
    }

    pub fn log_term(input: Node) -> Result<Expr> {
        let pairs = input.as_pair().to_owned().into_inner();
        parse_log_term(pairs)
    }

    pub fn forall(input: Node) -> Result<Forall> {
        match_nodes!(
        input.into_children();
        [logicvar(vars).., fmla(f)] => {
            Ok(Forall { vars: vars.collect(), fmla: Box::new(f)})
        })
    }

    pub fn exists(input: Node) -> Result<Exists> {
        match_nodes!(
        input.into_children();
        [logicvar(vars).., fmla(f)] => {
            Ok(Exists { vars: vars.collect(), fmla: Box::new(f)})
        })
    }

    pub fn fmla(input: Node) -> Result<Fmla> {
        match_nodes!(
        input.into_children();
        [exists(e)]     => Ok(Fmla::Exists(e)),
        [forall(e)]     => Ok(Fmla::Forall(e)),
        [log_term(e)]   => Ok(Fmla::Pred(e)),
        )
    }

    // Exprs

    pub fn expr(input: Node) -> Result<Expr> {
        let pairs = input.as_pair().to_owned().into_inner();
        parse_expr(pairs)
    }

    pub fn fnapp_args(input: Node) -> Result<Vec<Expr>> {
        match_nodes!(
        input.into_children();
        [expr(args)..] => {
            Ok(args.collect())
        })
    }

    // Decls

    pub fn decl_ret(input: Node) -> Result<DeclRet> {
        match_nodes!(
        input.into_children();
        [param(ret)] => Ok(Some(ret)))
    }

    pub fn decl_sig(input: Node) -> Result<DeclSig> {
        match_nodes!(
        input.into_children();
        [ident(name), paramlist(params)] => {
            Ok(DeclSig { name, params})
        },
        [ident(name)] => {
            Ok(DeclSig { name, params: vec!()})
        })
    }

    pub fn decl_block(input: Node) -> Result<Vec<Decl>> {
        match_nodes!(
        input.into_children();
        [decl(decls)..] => {
            Ok(decls.collect())
        })
    }

    pub fn action_decl(input: Node) -> Result<ActionDecl> {
        let span = input.as_pair().as_span(); // Irritating!

        let ret = match_nodes!(
        input.into_children();
            [decl_sig(DeclSig{name, params}), decl_ret(ret), stmt_block(body)] => Ok(
                ActionDecl{name, params, ret, body: Some(body)}
            ),
            [decl_sig(DeclSig{name, params}), stmt_block(body)] => Ok(
                ActionDecl{name, params, ret: None, body: Some(body)}
            ),
            [decl_sig(DeclSig{name, params})] => Ok(
                ActionDecl{name, params, ret: None, body: None}
            ),
        );
        ret.and_then(|action| {
            // TODO: having a QualifiedDeclSig vs a DeclSig with just a Symbol as a name would simplify this.
            if action.name.len() > 1 {
                Err(Error::new_from_span(
                    ErrorVariant::<Rule>::CustomError {
                        message: "Need an unqualified action name".into(),
                    },
                    span,
                ))
            } else {
                Ok(action)
            }
        })
    }

    pub fn alias_decl(input: Node) -> Result<(Symbol, Expr)> {
        match_nodes!(
        input.into_children();
        [symbol(lhs), expr(rhs)] => Ok((lhs, rhs)))
    }

    pub fn attribute_decl(input: Node) -> Result<Expr> {
        match_nodes!(
        input.into_children();
        [expr(e)] => Ok(e)
        )
    }

    pub fn axiom_decl(input: Node) -> Result<Fmla> {
        match_nodes!(
        input.into_children();
        [fmla(e)] => Ok(e)
        )
    }

    pub fn after_decl(input: Node) -> Result<AfterDecl> {
        match_nodes!(
        input.into_children();
        [decl_sig(DeclSig{name, params}), decl_ret(ret), stmt_block(body)] => Ok(
            AfterDecl { name, params: Some(params), ret: ret, body}
        ),
        [decl_sig(DeclSig{name, params}), stmt_block(body)] => Ok(
            AfterDecl { name, params: Some(params), ret: None, body}
        ),
        [ident(name), stmt_block(body)] => Ok(
            AfterDecl { name, params: None, ret: None, body}
        ),
        )
    }

    pub fn common_decl(input: Node) -> Result<Vec<Decl>> {
        match_nodes!(
        input.into_children();
            [decl_block(decls)] => Ok(decls)
        )
    }

    pub fn enum_decl(input: Node) -> Result<Vec<Symbol>> {
        match_nodes!(
        input.into_children();
            [symbol(cstrs)..] => Ok(cstrs.collect())
        )
    }

    pub fn export_decl(input: Node) -> Result<ExportDecl> {
        match_nodes!(
        input.into_children();
            [symbol(name)] => Ok(
                ExportDecl::ForwardRef(name)
            ),
            [action_decl(decl)] => Ok(
                ExportDecl::Action(decl)
            ),
        )
    }

    pub fn function_decl(input: Node) -> Result<FunctionDecl> {
        match_nodes!(
        input.into_children();
            [decl_sig(DeclSig{name, params}), symbol(ret)] => Ok(
                FunctionDecl{name, params, ret}
            ),
        )
    }

    pub fn global_decl(input: Node) -> Result<Vec<Decl>> {
        match_nodes!(
        input.into_children();
            [decl_block(decls)] => Ok(decls)
        )
    }

    pub fn implement_decl(input: Node) -> Result<ActionDecl> {
        // XXX: Looks like `handle_before_after` in ivy_parser.py just treats
        // implement like defining an action, modulo internal name mangling.
        match_nodes!(
        input.into_children();
            [decl_sig(DeclSig{name, params}), decl_ret(ret), stmt_block(body)] => Ok(
                ActionDecl{name, params, ret, body: Some(body)}
            ),
            [decl_sig(DeclSig{name, params}), stmt_block(body)] => Ok(
                ActionDecl{name, params, ret: None, body: Some(body)}
            ),
        )
    }

    pub fn implementation_decl(input: Node) -> Result<Vec<Decl>> {
        match_nodes!(
        input.into_children();
            [decl_block(decls)] => Ok(decls)
        )
    }

    pub fn import_decl(input: Node) -> Result<ImportDecl> {
        let span = input.as_pair().as_span(); // Irritating!

        match_nodes!(
        input.into_children();
            [decl_sig(DeclSig{mut name, params})] => {
                if name.len() > 1 {
                    Err(Error::new_from_span(
                        ErrorVariant::<Rule>::CustomError { message: "Need an unqualified isolate name".into() },
                        span))
                } else {
                    Ok(ImportDecl{name: name.pop().unwrap(), params})
                }
            }
        )
    }

    pub fn include_decl(input: Node) -> Result<Symbol> {
        match_nodes!(
        input.into_children();
            [symbol(module)] => Ok(module)
        )
    }

    pub fn instance_decl(input: Node) -> Result<InstanceDecl> {
        match_nodes!(
        input.into_children();
        [symbol(name), decl_sig(DeclSig{name: sort, params: sort_args})] =>
            Ok(InstanceDecl{name, sort, args: sort_args})
        )
    }

    pub fn invariant_decl(input: Node) -> Result<Fmla> {
        match_nodes!(
        input.into_children();
        [fmla(e)] => Ok(e)
        )
    }

    pub fn extract_decl(input: Node) -> Result<IsolateDecl> {
        let span = input.as_pair().as_span(); // Irritating!

        match_nodes!(
        input.into_children();
        [decl_sig(DeclSig{mut name, params}), decl_block(body)] => {
            if name.len() > 1 {
                Err(Error::new_from_span(
                    ErrorVariant::<Rule>::CustomError { message: "Need an unqualified isolate name".into() },
                    span))
            } else {
                Ok(IsolateDecl{name: name.pop().unwrap(), params, body})
            }
        })
    }

    pub fn module_decl(input: Node) -> Result<ModuleDecl> {
        match_nodes!(
        input.into_children();
        [decl_sig(DeclSig{name, params}), decl_block(body)] => Ok(
            ModuleDecl{name, params, body}
        ))
    }

    pub fn object_decl(input: Node) -> Result<ObjectDecl> {
        match_nodes!(
        input.into_children();
        [decl_sig(DeclSig{name, params}), decl_block(body)] => Ok(
            ObjectDecl{name, params, body}
        ))
    }

    pub fn range_decl(input: Node) -> Result<(Expr, Expr)> {
        match_nodes!(
        input.into_children();
            [expr(lo), expr(hi)] => Ok((lo, hi)),
        )
    }

    pub fn relation_decl(input: Node) -> Result<Relation> {
        match_nodes!(
        input.into_children();
            [decl_sig(DeclSig{name, params})] => Ok(
                Relation{name, params}
            ),
        )
    }

    pub fn specification_decl(input: Node) -> Result<Vec<Decl>> {
        match_nodes!(
        input.into_children();
            [decl_block(decls)] => Ok(decls)
        )
    }

    pub fn type_decl(input: Node) -> Result<Type> {
        match_nodes!(
        input.into_children();
        [type_name(ident), enum_decl(cstrs)] => Ok(
            Type {ident, sort: IvySort::Enum(cstrs) }
        ),
        [type_name(ident), range_decl((lo, hi))] => Ok(
            Type {ident, sort: IvySort::Range(Box::new(lo), Box::new(hi)) }
        ),
        [type_name(ident), symbol(supr)] => Ok(
            Type {ident, sort: IvySort::Subclass(supr) }
        ),
        [type_name(ident)] => Ok(
            Type {ident, sort: IvySort::Uninterpreted }
        ))
    }

    pub fn type_name(input: Node) -> Result<TypeName> {
        match_nodes!(
        input.into_children();
        [symbol(sym)] => Ok(TypeName::Name(sym)),
        [THIS(_)] => Ok(TypeName::This),
        )
    }

    pub fn var_decl(input: Node) -> Result<Term> {
        match_nodes!(
        input.into_children();
        [term(term)] => Ok(term))
    }

    pub fn decl(input: Node) -> Result<Decl> {
        match_nodes!(
        input.into_children();
        [action_decl(decl)]   => Ok(Decl::Action(decl)),
        [after_decl(decl)]    => Ok(Decl::AfterAction(decl)),
        [alias_decl((l,r))]   => Ok(Decl::Alias(l, r)),
        [attribute_decl(expr)] => Ok(Decl::Attribute(expr)),
        [axiom_decl(fmla)]    => Ok(Decl::Axiom(fmla)),
        [common_decl(decls)] => Ok(Decl::Common(decls)),
        [export_decl(fmla)]   => Ok(Decl::Export(fmla)),
        [extract_decl(decl)]  => Ok(Decl::Isolate(decl)),
        [global_decl(decls)]  => Ok(Decl::Globals(decls)),
        [function_decl(decl)] => Ok(Decl::Function(decl)),
        [implement_decl(decl)] => Ok(Decl::Action(decl)),
        [implementation_decl(decls)] => Ok(Decl::Implementation(decls)),
        [import_decl(decl)]    => Ok(Decl::Import(decl)),
        [include_decl(module)] => Ok(Decl::Include(module)),
        [invariant_decl(fmla)] => Ok(Decl::Invariant(fmla)),
        [instance_decl(decl)] => Ok(Decl::Instance(decl)),
        [module_decl(decl)]   => Ok(Decl::Module(decl)),
        [object_decl(decl)]   => Ok(Decl::Object(decl)),
        [relation_decl(decl)] => Ok(Decl::Relation(decl)),
        [specification_decl(decls)] => Ok(Decl::Specification(decls)),
        [type_decl(decl)]     => Ok(Decl::Type(decl)),
        [var_decl(decl)]      => Ok(Decl::Var(decl)),
        [stmt(stmts)..]       => Ok(Decl::Stmts(stmts.collect()))
        )
    }

    // Actions

    pub fn action(input: Node) -> Result<Action> {
        match_nodes!(
        input.into_children();
        [assert_action(action)] => Ok(Action::Assert(action)),
        [assign_action(action)] => Ok(Action::Assign(action)),
        [assume_action(action)] => Ok(Action::Assume(action)),
        [call_action(expr)]     => Ok(Action::Call(expr)),
        [ensure_action(action)] => Ok(Action::Ensure(action)),
        [requires_action(action)] => Ok(Action::Requires(action))
        )
    }

    pub fn actions(input: Node) -> Result<Vec<Action>> {
        match_nodes!(
        input.into_children();
        [action(actions)..] => Ok(actions.collect()))
    }

    pub fn assign_action(input: Node) -> Result<AssignAction> {
        match_nodes!(
        input.into_children();
        [var_decl(Term{id, sort}), expr(rhs)] => Ok(
            AssignAction{lhs: Expr::Term(Term{id, sort}), rhs}
        ),
        [expr(lhs), expr(rhs)] => Ok(
            // TODO: need a ::new() function that returns
            // some sort of error if lhs isn't a valid lval.
            AssignAction{lhs, rhs}
        ),
        )
    }

    pub fn assert_action(input: Node) -> Result<AssertAction> {
        match_nodes!(
        input.into_children();
        [expr(pred)] => Ok(AssertAction{pred}),
        )
    }

    pub fn assume_action(input: Node) -> Result<AssumeAction> {
        match_nodes!(
        input.into_children();
        [expr(pred)] => Ok(AssumeAction{pred}),
        )
    }

    pub fn call_action(input: Node) -> Result<AppExpr> {
        let span = input.as_pair().as_span(); // Irritating!

        match_nodes!(
        input.into_children();
        [expr(call)] => match call {
            Expr::App(call) => Ok(call),
            _ => Err(Error::new_from_span(ErrorVariant::<Rule>::CustomError { message: format!("Expected function call, got {:?}", call) }, span))
        })
    }

    pub fn ensure_action(input: Node) -> Result<EnsureAction> {
        match_nodes!(
        input.into_children();
        [fmla(pred)] => Ok(EnsureAction{pred}),
        )
    }

    pub fn requires_action(input: Node) -> Result<RequiresAction> {
        match_nodes!(
        input.into_children();
        [fmla(pred)] => Ok(RequiresAction{pred}),
        )
    }

    // Statements

    pub fn stmt_block(input: Node) -> Result<Vec<Stmt>> {
        match_nodes!(
        input.into_children();
        [stmt(stmts)..] => Ok(stmts.collect())
        )
    }

    pub fn if_stmt(input: Node) -> Result<If> {
        match_nodes!(
        input.into_children();
        [expr(tst), stmt_block(thens)] => Ok(
            If{tst, thn: thens, els: None}
        ),
        [expr(tst), stmt_block(thens), stmt_block(elses)] => Ok(
            If{tst, thn: thens, els: Some(elses)}
        ),
        )
    }

    pub fn while_stmt(input: Node) -> Result<While> {
        match_nodes!(
        input.into_children();
        [expr(test), stmt_block(stmts)] => Ok(
            While{test, doit: stmts}
        ),
        )
    }

    pub fn stmt(input: Node) -> Result<Stmt> {
        match_nodes!(
        input.into_children();
        [actions(actions)]  => Ok(Stmt::ActionSequence(actions)),
        [if_stmt(stmt)]     => Ok(Stmt::If(stmt)),
        [while_stmt(stmt)]  => Ok(Stmt::While(stmt)),
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

    pub fn prog(input: Node) -> Result<Prog> {
        match_nodes!(
        input.into_children();
        [langver((major, minor)), decl(decls).., EOI(())] => {
            Ok(Prog {
                major_version: major,
                minor_version: minor,
                top: IsolateDecl{
                    name: "this".into(),
                    params: vec!(),
                    body: decls.collect()
                }})
        })
    }
}
