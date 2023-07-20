use pest::error::ErrorVariant;
use pest_consume::{match_nodes, Error, Parser};

use crate::ast::actions::*;
use crate::ast::declarations::*;
use crate::ast::expressions;
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

    fn param(input: Node) -> Result<Symbol> {
        match_nodes!(
        input.into_children();
        [symbol(id), ident(sort)] => Ok(Symbol { id, sort: Sort::Annotated(sort) }),
        [symbol(id)] => Ok(Symbol {id, sort: Sort::ToBeInferred })
        )
    }

    pub fn paramlist(input: Node) -> Result<ParamList> {
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

    fn THIS(input: Node) -> Result<Expr> {
        Ok(Expr::This)
    }

    // Utils

    // Formulas

    fn logicvar(input: Node) -> Result<Symbol> {
        match_nodes!(
        input.into_children();
        [LOGICVAR(id), symbol(sort)] => Ok(Symbol {id, sort: Sort::Annotated(vec!(sort)) }),
        [LOGICVAR(id)]               => Ok(Symbol {id, sort: Sort::ToBeInferred })
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

    pub fn builtin_type(input: Node) -> Result<expressions::Sort> {
        match_nodes!(
        input.into_children();
        [bv_decl(width)] => Ok(Sort::Resolved(IvySort::BitVec(width))),
        [enum_decl(cstrs)] => Ok(Sort::Resolved(IvySort::Enum(cstrs))),
        [range_decl((lo, hi))] => Ok(Sort::Resolved(IvySort::Range(Box::new(lo), Box::new(hi)))),
        [symbol(supr)] => Ok(Sort::Resolved(IvySort::Subclass(supr))),
        [_THIS] => Ok(Sort::Resolved(IvySort::This)),
        )
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
        [symbol(name), paramlist(params)] => {
            Ok(DeclSig { name, params})
        },
        [symbol(name)] => {
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

    pub fn mod_sig(input: Node) -> Result<ModSig> {
        match_nodes!(
        input.into_children();
        [symbol(name), symbol(sortsyms)..] => {
            Ok(ModSig{name, sortsyms: sortsyms.collect()})
        })
    }

    // Mixins and Actions

    pub fn mixin_sig(input: Node) -> Result<MixinSig> {
        match_nodes!(
        input.into_children();
        [ident(name), paramlist(params)] => {
            Ok(MixinSig { name, params: Some(params)})
        },
        [ident(name)] => {
            Ok(MixinSig { name, params: None})
        })
    }

    pub fn action_decl(input: Node) -> Result<Binding<ActionDecl>> {
        match_nodes!(
        input.into_children();
            [decl_sig(DeclSig{name, params}), decl_ret(ret), stmt_block(body)] => Ok(
                Binding::from(name, ActionDecl{params, ret, body: Some(body)})
            ),
            [decl_sig(DeclSig{name, params}), stmt_block(body)] => Ok(
                Binding::from(name, ActionDecl{params, ret: None, body: Some(body)})
            ),
            [decl_sig(DeclSig{name, params}), decl_ret(ret)] => Ok(
                Binding::from(name, ActionDecl{params, ret, body: None})
            ),
            [decl_sig(DeclSig{name, params})] => Ok(
                Binding::from(name, ActionDecl{params, ret: None, body: None})
            ),
        )
    }

    pub fn alias_decl(input: Node) -> Result<Binding<expressions::Sort>> {
        match_nodes!(
        input.into_children();
        [symbol(lhs), builtin_type(rhs)] => Ok(Binding::from(lhs, rhs)),
        )
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

    pub fn after_decl(input: Node) -> Result<ActionMixinDecl> {
        match_nodes!(
        input.into_children();
        [mixin_sig(MixinSig{name, params}), decl_ret(ret), stmt_block(body)] => Ok(
            ActionMixinDecl { name, params, ret, body}
        ),
        [mixin_sig(MixinSig{name, params}), stmt_block(body)] => Ok(
            ActionMixinDecl { name, params, ret: None, body}
        ),
        [ident(name), stmt_block(body)] => Ok(
            ActionMixinDecl { name, params: None, ret: None, body}
        ),
        )
    }

    pub fn before_decl(input: Node) -> Result<ActionMixinDecl> {
        match_nodes!(
        input.into_children();
        [mixin_sig(MixinSig{name, params}), stmt_block(body)] => Ok(
            ActionMixinDecl { name, params, body, ret: None}
        ),
        [ident(name), stmt_block(body)] => Ok(
            ActionMixinDecl { name, params: None, body, ret: None}
        ),
        )
    }

    pub fn bv_decl(input: Node) -> Result<u8> {
        let span = input.as_pair().as_span(); // Irritating!

        match_nodes!(
        input.into_children();
        [number(width)] => {
            u8::try_from(width).map_err(|_|
                Error::new_from_span(ErrorVariant::<Rule>::CustomError { message:
                    format!("Bit vector width {} too large", width) }, span))
        }
        )
    }

    pub fn common_decl(input: Node) -> Result<Vec<Decl>> {
        match_nodes!(
        input.into_children();
            [decl_block(decls)] => Ok(decls)
        )
    }

    pub fn enum_decl(input: Node) -> Result<Vec<Token>> {
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
            [action_decl(binding)] => Ok(
                ExportDecl::Action(binding)
            ),
        )
    }

    pub fn function_decl(input: Node) -> Result<Binding<FunctionDecl>> {
        match_nodes!(
        input.into_children();
            [decl_sig(DeclSig{name, params}), symbol(ret)] => Ok(
                Binding::from(name, FunctionDecl { params, ret })
            ),
        )
    }

    pub fn global_decl(input: Node) -> Result<Vec<Decl>> {
        match_nodes!(
        input.into_children();
            [decl_block(decls)] => Ok(decls)
        )
    }

    pub fn implement_action_decl(input: Node) -> Result<ImplementDecl> {
        // XXX: Looks like `handle_before_after` in ivy_parser.py just treats
        // implement like defining an action, modulo internal name mangling.
        match_nodes!(
        input.into_children();
            [mixin_sig(MixinSig{name, params}), decl_ret(ret), stmt_block(body)] => Ok(
                ImplementDecl{name, params, ret, body: Some(body)}
            ),
            [mixin_sig(MixinSig{name, params}), stmt_block(body)] => Ok(
                ImplementDecl{name, params, ret: None, body: Some(body)}
            ),
        )
    }

    pub fn implementation_decl(input: Node) -> Result<ObjectDecl> {
        match_nodes!(
        input.into_children();
            [decl_block(decls)] => Ok(ObjectDecl { params: vec!(), fields: vec!(), body: decls })
        )
    }

    pub fn import_decl(input: Node) -> Result<ImportDecl> {
        match_nodes!(
        input.into_children();
            [decl_sig(DeclSig{name, params})] => Ok(ImportDecl{name, params})
        )
    }

    pub fn include_decl(input: Node) -> Result<Token> {
        match_nodes!(
        input.into_children();
            [symbol(module)] => Ok(module)
        )
    }

    pub fn instance_decl(input: Node) -> Result<Binding<InstanceDecl>> {
        match_nodes!(
        input.into_children();
        [symbol(name), mixin_sig(MixinSig{name: sort, params: sort_args})] =>
            Ok(Binding::from(name, InstanceDecl{sort, args: sort_args.unwrap_or_default()}))
        )
    }

    pub fn interpret_decl(input: Node) -> Result<InterpretDecl> {
        match_nodes!(
        input.into_children();
        [symbol(name), builtin_type(sort)] => Ok(InterpretDecl { name, sort } ),
        [symbol(name), ident(rhs)] => Ok(InterpretDecl{name, sort: Sort::Annotated(rhs)}),
        [symbol(name)] => Ok(InterpretDecl{name, sort: Sort::Resolved(IvySort::Uninterpreted)})
        )
    }

    pub fn invariant_decl(input: Node) -> Result<Fmla> {
        match_nodes!(
        input.into_children();
        [fmla(e)] => Ok(e)
        )
    }

    pub fn process_decl(input: Node) -> Result<Binding<ObjectDecl>> {
        match_nodes!(
        input.into_children();
        [decl_sig(DeclSig{name, params}), decl_block(body)] => {
            Ok(Binding::from(name, ObjectDecl{
                params,
                fields: vec!(),
                body
                })
            )
        })
    }

    pub fn module_decl(input: Node) -> Result<Binding<ModuleDecl>> {
        match_nodes!(
        input.into_children();
        [mod_sig(ModSig{name, sortsyms}), decl_block(body)] => Ok(
            Binding::from(name, ModuleDecl{sortsyms, body})))
    }

    pub fn object_decl(input: Node) -> Result<Binding<ObjectDecl>> {
        match_nodes!(
        input.into_children();
        [symbol(name), decl_block(body)] => Ok(
            Binding::from(name, ObjectDecl{params: vec!(), fields: vec!(), body})))
    }

    pub fn range_decl(input: Node) -> Result<(Expr, Expr)> {
        match_nodes!(
        input.into_children();
            [expr(lo), expr(hi)] => Ok((lo, hi)),
        )
    }

    pub fn relation_decl(input: Node) -> Result<Binding<Relation>> {
        match_nodes!(
        input.into_children();
            [decl_sig(DeclSig{name, params})] => Ok(
                Binding::from(name, Relation{params})
            ),
        )
    }

    pub fn specification_decl(input: Node) -> Result<ObjectDecl> {
        match_nodes!(
        input.into_children();
            [decl_block(decls)] => Ok(ObjectDecl { params: vec![], fields: vec!(), body: decls })
        )
    }

    pub fn type_decl(input: Node) -> Result<Binding<Sort>> {
        match_nodes!(
        input.into_children();
        [symbol(sym), builtin_type(resolved)] => Ok(Binding::from(sym, resolved)),
        [symbol(lhs), symbol(rhs)] => Ok(Binding::from(lhs, Sort::Resolved(IvySort::Subclass(rhs)))),
        [symbol(sym)] => Ok(Binding::from(sym, Sort::ToBeInferred)),
        [symbol(sym)] => {
            Ok(Binding::from(sym, Sort::ToBeInferred))
        },
        [_THIS] => Ok(Binding::from("this".into(), Sort::Resolved(IvySort::This))),
        )
    }

    pub fn var_decl(input: Node) -> Result<Binding<Sort>> {
        match_nodes!(
        input.into_children();
        [param(Symbol { id, sort })] => Ok(Binding::from(id, sort)))
    }

    pub fn decl(input: Node) -> Result<Decl> {
        match_nodes!(
        input.into_children();
        [action_decl(binding)]   => Ok(Decl::Action(binding)),
        [after_decl(decl)]    => Ok(Decl::AfterAction(decl)),
        [alias_decl(binding)]   => Ok(Decl::Alias(binding)),
        [attribute_decl(expr)] => Ok(Decl::Attribute(expr)),
        [axiom_decl(fmla)]    => Ok(Decl::Axiom(fmla)),
        [before_decl(decl)]    => Ok(Decl::BeforeAction(decl)),
        [common_decl(decls)] => Ok(Decl::Common(decls)),
        [export_decl(fmla)]   => Ok(Decl::Export(fmla)),
        [global_decl(decls)]  => Ok(Decl::Globals(decls)),
        [function_decl(binding)] => Ok(Decl::Function(binding)),
        [implement_action_decl(binding)] => Ok(Decl::Implement(binding)),
        [implementation_decl(decl)] => Ok(Decl::Object(Binding { name: "impl".into(), decl })),
        [import_decl(decl)]    => Ok(Decl::Import(decl)),
        [include_decl(module)] => Ok(Decl::Include(module)),
        [invariant_decl(fmla)] => Ok(Decl::Invariant(fmla)),
        [instance_decl(binding)] => Ok(Decl::Instance(binding)),
        [interpret_decl(decl)] => Ok(Decl::Interpret(decl)),
        [module_decl(decl)]   => Ok(Decl::Module(decl)),
        [object_decl(binding)]   => Ok(Decl::Object(binding)),
        [process_decl(binding)]   => Ok(Decl::Object(binding)),
        [relation_decl(binding)] => Ok(Decl::Relation(binding)),
        [specification_decl(decl)] => Ok(Decl::Object(Binding { name: "spec".into(), decl })),
        [type_decl(binding)]     => Ok(Decl::Type(binding)),
        [var_decl(binding)]      => Ok(Decl::Var(binding)),
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
        [var_decl(Binding{name, decl}), expr(rhs)] => Ok(
            AssignAction{lhs: Expr::Symbol(Symbol{id: name, sort: decl}), rhs}
        ),
        [expr(lhs), expr(rhs)] => match lhs {
            Expr::App(_) | Expr::FieldAccess(_) | Expr::Index(_) | Expr::Symbol(_) | Expr::This => Ok(AssignAction{lhs, rhs}),
            _ => todo!(),
        },
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
        [var_decl(binding)]  => Ok(Stmt::VarDecl(binding)),
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
        [langver((major, minor)), decl(decls).., EOI(())] => Ok(
            Prog {
                major_version: major,
                minor_version: minor,
                top: decls.collect::<Vec<_>>()
            })
        )
    }
}
