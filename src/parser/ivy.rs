use std::path::PathBuf;
use std::rc::Rc;

use pest::error::ErrorVariant;
use pest_consume::{match_nodes, Error, Parser};

use crate::ast::actions::*;
use crate::ast::declarations::*;
use crate::ast::expressions;
use crate::ast::expressions::*;
use crate::ast::logic::*;
use crate::ast::span::SourceSpan;
use crate::ast::span::Span;
use crate::ast::statements::*;
use crate::ast::toplevels::*;

use crate::parser::expressions::parse_rval;
use crate::typechecker::sorts::IvySort;

use super::logic::parse_log_term;

const _LEXER: &str = include_str!("./grammars/lexer.pest");
const _GRAMMAR: &str = include_str!("grammars/syntax.pest");
const _LOGIC: &str = include_str!("grammars/logic.pest");

#[derive(Parser)]
#[grammar = "parser/grammars/lexer.pest"]
#[grammar = "parser/grammars/syntax.pest"]
#[grammar = "parser/grammars/logic.pest"]
pub struct IvyParser;

#[derive(Clone, Eq, PartialEq, PartialOrd, Ord, Debug)]
pub struct ParserState {
    pub file_name: PathBuf,
    pub file_text: Rc<String>,
}

impl ParserState {
    pub fn new<P, S>(file_name: P, file_text: S) -> Self
    where
        P: Into<PathBuf>,
        S: Into<String>,
    {
        Self {
            file_name: file_name.into(),
            file_text: Rc::from(file_text.into()),
        }
    }
}

pub type ParseError = Error<Rule>;
pub type Result<T> = std::result::Result<T, ParseError>;
pub type Node<'i> = pest_consume::Node<'i, Rule, Rc<ParserState>>;

fn pest_err(message: String, irving_span: Span) -> Option<ParseError> {
    if let Span::Source(SourceSpan { input, start, end }) = irving_span {
        let span = pest::Span::new(&input.file_text, start, end).unwrap();
        Some(Error::new_from_span(
            ErrorVariant::<Rule>::CustomError { message },
            span,
        ))
    } else {
        None
    }
}

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

    fn PROGTOK(input: Node) -> Result<String> {
        Ok(input.as_str().to_owned())
    }

    fn LOGICTOK(input: Node) -> Result<String> {
        Ok(input.as_str().to_owned())
    }

    fn param(input: Node) -> Result<Symbol> {
        let span = Span::from_node(&input);
        match_nodes!(
        input.into_children();
        [PROGTOK(name), ident(sort)] => Ok(Symbol::from(name, Sort::Annotated(sort), span)),
        [PROGTOK(name)] => Ok(Symbol::from(name, Sort::ToBeInferred, span))
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
        [PROGTOK(qualifiers)..] => Ok(qualifiers.collect()),
        [THIS(_)] => Ok(vec!("this".into())),
        )
    }

    fn THIS(input: Node) -> Result<Expr> {
        Ok(Expr::This(Span::from_pest(
            Rc::clone(input.user_data()),
            &input.as_span(),
        )))
    }

    // Utils

    // Formulas

    fn logicsym(input: Node) -> Result<Symbol> {
        let span = Span::from_node(&input);
        match_nodes!(
        input.into_children();
        [LOGICTOK(name), PROGTOK(sort)] => Ok(Symbol::from(name, Sort::Annotated(vec!(sort)), span)),
        [LOGICTOK(name)]                => Ok(Symbol::from(name, Sort::ToBeInferred, span))
        )
    }

    pub fn log_term(input: Node) -> Result<Fmla> {
        let pairs = input.as_pair().to_owned().into_inner();
        parse_log_term(Rc::clone(input.user_data()), pairs)
    }

    pub fn forall(input: Node) -> Result<Forall> {
        match_nodes!(
        input.into_children();
        [logicsym(vars).., fmla(f)] => {
            Ok(Forall { vars: vars.collect(), fmla: Box::new(f)})
        })
    }

    pub fn exists(input: Node) -> Result<Exists> {
        match_nodes!(
        input.into_children();
        [logicsym(vars).., fmla(f)] => {
            Ok(Exists { vars: vars.collect(), fmla: Box::new(f)})
        })
    }

    pub fn fmla(input: Node) -> Result<Fmla> {
        let span = Span::from_node(&input);

        match_nodes!(
        input.into_children();
        [exists(fmla)]     => Ok(Fmla::Exists{ span, fmla }),
        [forall(fmla)]     => Ok(Fmla::Forall{ span, fmla }),
        [log_term(fmla)]   => Ok(fmla),
        )
    }

    // Exprs

    // Lvals

    pub fn lval(input: Node) -> Result<Expr> {
        let span = input.as_pair().as_span(); // Irritating!

        match_nodes!(
        input.into_children();
        //[relation_lval(lval)] => Ok(lval),
        [rval(lval)] => match lval {
            Expr::App{..} |
            Expr::FieldAccess{..} |
            Expr::Index{..} |
            Expr::ProgramSymbol { .. } => Ok(lval),
            // Can it be This? Expr::This => todo!(),
            _ => Err(
                Error::new_from_span(ErrorVariant::<Rule>::CustomError { message:
                    format!("invalid lvalue: {:?}", lval) }, span))
        }
        )
    }

    pub fn lparamlist(input: Node) -> Result<Vec<Symbol>> {
        match_nodes!(
        input.into_children();
        [logicsym(args)..] => {
            Ok(args.collect())
        })
    }

    pub fn log_app_args(input: Node) -> Result<Vec<Fmla>> {
        match_nodes!(
        input.into_children();
        [log_term(args)..] => {
            Ok(args.collect())
        })
    }

    // Rvals

    pub fn rval(input: Node) -> Result<Expr> {
        let pairs = input.as_pair().to_owned().into_inner();
        parse_rval(input.user_data().clone(), pairs)
    }

    pub fn builtin_type(input: Node) -> Result<expressions::Sort> {
        match_nodes!(
        input.into_children();
        [bv_decl(width)] => Ok(Sort::Resolved(IvySort::BitVec(width))),
        [enum_decl(cstrs)] => Ok(Sort::Resolved(IvySort::Enum(cstrs))),
        [range_decl((lo, hi))] => Ok(Sort::Resolved(IvySort::BoundedSequence(lo, hi))),
        [PROGTOK(_)] => todo!(),
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
        [PROGTOK(name), paramlist(params)] => {
            Ok(DeclSig { name, params})
        },
        [PROGTOK(name)] => {
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
        let span = Span::from_node(&input);
        match_nodes!(
        input.into_children();
        [PROGTOK(name), PROGTOK(sortsyms)..] => {
            let sortsyms = sortsyms.into_iter()
                .map(|sym| Binding::from(sym, Sort::ToBeInferred, span.clone()))
                .collect();
            Ok(ModSig{name, sortsyms})
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
        let span = Span::from_node(&input);

        match_nodes!(
        input.into_children();
            [decl_sig(DeclSig{name, params}), decl_ret(ret), stmt_block(body)] => Ok(
                Binding::from(name, ActionDecl{params, ret, body: Some(body)}, span)
            ),
            [decl_sig(DeclSig{name, params}), stmt_block(body)] => Ok(
                Binding::from(name, ActionDecl{params, ret: None, body: Some(body)}, span)
            ),
            [decl_sig(DeclSig{name, params}), decl_ret(ret)] => Ok(
                Binding::from(name, ActionDecl{params, ret, body: None}, span)
            ),
            [decl_sig(DeclSig{name, params})] => Ok(
                Binding::from(name, ActionDecl{params, ret: None, body: None}, span)
            ),
        )
    }

    pub fn alias_decl(input: Node) -> Result<Binding<expressions::Sort>> {
        let span = Span::from_node(&input);

        match_nodes!(
        input.into_children();
        [PROGTOK(lhs), builtin_type(rhs)] => Ok(Binding::from(lhs, rhs, span))
        )
    }

    pub fn attribute_decl(input: Node) -> Result<(Span, Expr, Option<Expr>)> {
        let span = Span::from_node(&input);
        match_nodes!(
        input.into_children();
        [rval(lhs)] => Ok((span, lhs, None)),
        [rval(lhs), rval(rhs)] => Ok((span, lhs, Some(rhs)))
        )
    }

    pub fn axiom_decl(input: Node) -> Result<(Span, Fmla)> {
        let span = Span::from_node(&input);
        match_nodes!(
        input.into_children();
        [fmla(f)] => Ok((span, f))
        )
    }

    pub fn after_decl(input: Node) -> Result<(Span, ActionMixinDecl)> {
        let span = Span::from_node(&input);

        match_nodes!(
        input.into_children();
        [mixin_sig(MixinSig{name, params}), decl_ret(ret), stmt_block(body)] => Ok(
            (span, ActionMixinDecl { name, params, ret, body})
        ),
        [mixin_sig(MixinSig{name, params}), stmt_block(body)] => Ok(
            (span, ActionMixinDecl { name, params, ret: None, body})
        ),
        [ident(name), stmt_block(body)] => Ok(
            (span, ActionMixinDecl { name, params: None, ret: None, body})
        ),
        )
    }

    pub fn before_decl(input: Node) -> Result<(Span, ActionMixinDecl)> {
        let span = Span::from_node(&input);

        match_nodes!(
        input.into_children();
        [mixin_sig(MixinSig{name, params}), stmt_block(body)] => Ok(
            (span, ActionMixinDecl { name, params, body, ret: None})
        ),
        [ident(name), stmt_block(body)] => Ok(
            (span, ActionMixinDecl { name, params: None, body, ret: None})
        ),
        )
    }

    pub fn bv_decl(input: Node) -> Result<u8> {
        let span = input.as_pair().as_span(); // Irritating!

        match_nodes!(
        input.into_children();
        [number(width)] => {
            u8::try_from(width).map_err(|_|
                Error::new_from_span(ErrorVariant::<Rule>::CustomError {
                    message: format!("Bit vector width {} too large", width) },
                    span))
        }
        )
    }

    pub fn class_decl(input: Node) -> Result<Binding<ClassDecl>> {
        let span = Span::from_node(&input);
        let parent: Option<Token> = None;

        match_nodes!(
        input.into_children();
        [PROGTOK(name), class_slot_block(slots)] => {
            let mut fields: Vec<Binding<Sort>> = vec!();
            let mut actions: Vec<Binding<ActionDecl>> = vec!();

            for slot in slots {
                match slot {
                    ClassSlot::Field(b) => {
                        if b.decl == Sort::ToBeInferred {
                            return Err(pest_err("Field type must be declared".into(), b.span).unwrap());
                        }
                        fields.push(b);
                    },
                    ClassSlot::Action(b) => actions.push(b),
                }
            }

            Ok(Binding::from(name, ClassDecl {
                parent, fields, actions
            }, span))
        })
    }

    pub fn class_slot_block(input: Node) -> Result<Vec<ClassSlot>> {
        match_nodes!(
        input.into_children();
        [class_slot_decl(decls)..] => Ok(decls.collect::<Vec<_>>())
        )
    }

    pub fn class_slot_decl(input: Node) -> Result<ClassSlot> {
        match_nodes!(
        input.into_children();
        [param(sym)] => Ok(ClassSlot::Field(sym)),
        [action_decl(binding)] => Ok(ClassSlot::Action(binding))
        )
    }

    pub fn common_decl(input: Node) -> Result<(Span, Vec<Decl>)> {
        let span = Span::from_node(&input);
        match_nodes!(
        input.into_children();
            [decl_block(decls)] => Ok((span, decls))
        )
    }

    pub fn enum_decl(input: Node) -> Result<Vec<Token>> {
        match_nodes!(
        input.into_children();
            [PROGTOK(cstrs)..] => Ok(cstrs.collect())
        )
    }

    pub fn export_decl(input: Node) -> Result<(Span, ExportDecl)> {
        let span = Span::from_node(&input);
        match_nodes!(
        input.into_children();
            [PROGTOK(name)] => Ok(
                (span, ExportDecl::ForwardRef(name))
            ),
            [action_decl(binding)] => Ok(
                (span, ExportDecl::Action(binding))
            ),
        )
    }

    pub fn function_decl(input: Node) -> Result<Binding<FunctionDecl>> {
        let span = Span::from_node(&input);
        match_nodes!(
        input.into_children();
            [PROGTOK(name), lparamlist(_), ] =>
                Err(pest_err(format!("Function {name} missing both body and range sort"), span).unwrap()),
            [PROGTOK(name), lparamlist(params), ident(ret)] =>
                Ok(Binding::from(name, FunctionDecl {params, ret: Some(ret), body: None}, span)),
            [PROGTOK(name), lparamlist(params), fmla(body) ] =>
                Ok(Binding::from(name, FunctionDecl {params, ret: None, body: Some(body)}, span)),
            [PROGTOK(name), lparamlist(params), ident(ret), fmla(body) ] =>
                Ok(Binding::from(name, FunctionDecl {params, ret: Some(ret), body: Some(body)}, span))
        )
    }

    pub fn global_decl(input: Node) -> Result<Vec<Decl>> {
        match_nodes!(
        input.into_children();
            [decl_block(decls)] => Ok(decls)
        )
    }

    pub fn implement_action_decl(input: Node) -> Result<(Span, ActionMixinDecl)> {
        let span = Span::from_node(&input);
        // XXX: Looks like `handle_before_after` in ivy_parser.py just treats
        // implement like defining an action, modulo internal name mangling.
        match_nodes!(
        input.into_children();
            [mixin_sig(MixinSig{name, params}), decl_ret(ret), stmt_block(body)] => Ok(
                (span, ActionMixinDecl{name, params, ret, body})
            ),
            [mixin_sig(MixinSig{name, params}), stmt_block(body)] => Ok(
                (span, ActionMixinDecl{name, params, ret: None, body})
            ),
        )
    }

    pub fn implementation_decl(input: Node) -> Result<(Span, ObjectDecl)> {
        let span = Span::from_node(&input);
        match_nodes!(
        input.into_children();
            [decl_block(decls)] => Ok((span, ObjectDecl { params: vec!(), body: decls }))
        )
    }

    pub fn import_decl(input: Node) -> Result<(Span, ImportDecl)> {
        let span = Span::from_node(&input);

        match_nodes!(
        input.into_children();
            [decl_sig(DeclSig{name, params})] => Ok((span, ImportDecl{name, params}))
        )
    }

    pub fn includes(input: Node) -> Result<Vec<IncludeDecl>> {
        match_nodes!(
        input.into_children();
            [include_decl(decls)..] => Ok(decls.collect::<Vec<_>>())
        )
    }

    pub fn include_decl(input: Node) -> Result<IncludeDecl> {
        let span = Span::from_node(&input);
        match_nodes!(
        input.into_children();
            [PROGTOK(name)] => Ok(IncludeDecl { span, name })
        )
    }

    pub fn instance_decl(input: Node) -> Result<Binding<InstanceDecl>> {
        let span = Span::from_node(&input);
        match_nodes!(
        input.into_children();
        [PROGTOK(name), mixin_sig(MixinSig{name: sort, params: sort_args})] =>
            Ok(Binding::from(name, InstanceDecl{sort, args: sort_args.unwrap_or_default()}, span))
        )
    }

    pub fn interpret_decl(input: Node) -> Result<(Span, InterpretDecl)> {
        let span = Span::from_node(&input);
        match_nodes!(
        input.into_children();
        [PROGTOK(name), builtin_type(sort)] => Ok((span, InterpretDecl { name, sort } )),
        [PROGTOK(name), ident(rhs)] => Ok((span, InterpretDecl{name, sort: Sort::Annotated(rhs)})),
        [PROGTOK(name)] => Ok((span, InterpretDecl{name, sort: Sort::Resolved(IvySort::Uninterpreted)}))
        )
    }

    pub fn invariant_decl(input: Node) -> Result<Binding<Fmla>> {
        let span = Span::from_node(&input);
        let txt = input.as_span().as_str();

        match_nodes!(
        input.into_children();
        [fmla(f)] => {
            Ok(Binding::from(txt.to_owned(), f, span))
        },
        [PROGTOK(name), fmla(f)] => Ok(Binding::from(name, f, span))
        )
    }

    pub fn process_decl(input: Node) -> Result<Binding<ObjectDecl>> {
        let span = Span::from_node(&input);

        match_nodes!(
        input.into_children();
        [decl_sig(DeclSig{name, params}), decl_block(body)] => {
            Ok(Binding::from(name, ObjectDecl{
                params,
                body
                }, span))
        })
    }

    pub fn module_decl(input: Node) -> Result<Binding<ModuleDecl>> {
        let span = Span::from_node(&input);

        match_nodes!(
        input.into_children();
        [mod_sig(ModSig{name, sortsyms}), decl_block(body)] => {
            Ok(Binding::from(name, ModuleDecl{sortsyms, body}, span))
        })
    }

    pub fn object_decl(input: Node) -> Result<Binding<ObjectDecl>> {
        let span = Span::from_node(&input);

        match_nodes!(
        input.into_children();
        [PROGTOK(name), decl_block(body)] => Ok(
            Binding::from(name, ObjectDecl{params: vec!(), body}, span)))
    }

    pub fn range_decl(input: Node) -> Result<(i64, i64)> {
        match_nodes!(
        input.into_children();
            [number(lo), number(hi)] => Ok((lo, hi)),
        )
    }

    pub fn relation_decl(input: Node) -> Result<Binding<MapDecl>> {
        let span = Span::from_node(&input);
        let range = Sort::Resolved(IvySort::Bool);

        match_nodes!(
        input.into_children();
        [PROGTOK(name)] =>
            Ok(Binding::from(name, MapDecl{ domain: vec!(), range: range}, span)),
        [PROGTOK(name), lparamlist(params)] =>
            Ok(Binding::from(name, MapDecl{ domain: params, range: range}, span)),
        )
    }

    pub fn specification_decl(input: Node) -> Result<(Span, ObjectDecl)> {
        let span = Span::from_node(&input);
        match_nodes!(
        input.into_children();
            [decl_block(decls)] => Ok((span, ObjectDecl { params: vec![], body: decls }))
        )
    }

    pub fn subclass_decl(input: Node) -> Result<Binding<ClassDecl>> {
        let span = Span::from_node(&input);

        match_nodes!(
        input.into_children();
        [PROGTOK(name), PROGTOK(parent), class_slot_block(slots)] => {
            let mut fields: Vec<Binding<Sort>> = vec!();
            let mut actions: Vec<Binding<ActionDecl>> = vec!();

            for slot in slots {
                match slot {
                    ClassSlot::Field(b) => {
                        if b.decl == Sort::ToBeInferred {
                            return Err(pest_err("Field type must be declared".into(), b.span).unwrap());
                        }
                        fields.push(b);
                    },
                    ClassSlot::Action(b) => actions.push(b),
                }
            }

            Ok(Binding::from(name, ClassDecl {
                parent: Some(parent), fields, actions
            }, span))
        })
    }

    pub fn type_decl(input: Node) -> Result<Binding<Sort>> {
        let span = Span::from_node(&input);
        match_nodes!(
        input.into_children();
        [PROGTOK(sym), builtin_type(resolved)] => Ok(Binding::from(sym, resolved, span)),
        [PROGTOK(_), PROGTOK(_)] => todo!(),
        [PROGTOK(sym)] => Ok(Binding::from(sym, Sort::ToBeInferred, span)),
        [PROGTOK(sym)] => {
            Ok(Binding::from(sym, Sort::ToBeInferred, span))
        },
        [_THIS] => Ok(Binding::from("this", Sort::Resolved(IvySort::This), span)),
        )
    }

    pub fn var_decl(input: Node) -> Result<Binding<Sort>> {
        let span = input.as_pair().as_span(); // Irritating!
        match_nodes!(
        input.into_children();
        [param(Symbol { name, decl, span })] => Ok(Binding::from(name, decl, span)),
        [param(_), rval(_)] =>
            Err(Error::new_from_span(ErrorVariant::<Rule>::CustomError { message:
                    "Cannot assign within variable declaration (declare var and then assign as a distinct action)".into() }, span))
        )
    }

    pub fn decl(input: Node) -> Result<Decl> {
        match_nodes!(
        input.into_children();
        [action_decl(decl)]          => Ok(Decl::Action{decl}),
        [after_decl((span, decl))]   => Ok(Decl::AfterAction{span, decl}),
        [alias_decl(decl)]           => Ok(Decl::Alias{decl}),
        [attribute_decl((span, lhs, rhs))] => Ok(Decl::Attribute{span, lhs, rhs}),
        [axiom_decl((span, decl))]    => Ok(Decl::Axiom{span, decl}),
        [before_decl((span, decl))]    => Ok(Decl::BeforeAction{span, decl}),
        [class_decl(decl)] => Ok(Decl::Class{decl}),
        [common_decl((span, decl))] => Ok(Decl::Common{span, decl}),
        [export_decl((span, decl))]   => Ok(Decl::Export{span, decl}),
        [global_decl(decls)]         => Ok(Decl::Globals(decls)),
        [function_decl(decl)]        => Ok(Decl::Function{ decl }),
        [implement_action_decl((span, decl))] => Ok(Decl::Implement{span, decl}),
        [implementation_decl((span, decl))] => Ok(Decl::Object{ decl: Binding { name: "impl".into(), decl, span }}),
        [import_decl((span, decl))]    => Ok(Decl::Import{span, decl}),
        [invariant_decl(decl)] => Ok(Decl::Invariant { decl }),
        [instance_decl(decl)] => Ok(Decl::Instance{decl}),
        [interpret_decl((span, decl))] => Ok(Decl::Interpret{span, decl}),
        [module_decl(decl)]   => Ok(Decl::Module{decl}),
        [object_decl(decl)]   => Ok(Decl::Object{decl}),
        [process_decl(decl)]   => Ok(Decl::Object{decl}),
        [relation_decl(decl)] => Ok(Decl::Map{decl}),
        [specification_decl((span, decl))] => Ok(Decl::Object{ decl: Binding { name: "spec".into(), decl, span }}),
        [subclass_decl(decl)] => Ok(Decl::Subclass{decl}),
        [type_decl(decl)]     => Ok(Decl::Type { decl }),
        [var_decl(decl)]      => Ok(Decl::Var{ decl }),
        [stmt(stmts)..]       => Ok(Decl::Stmts(stmts.collect()))
        )
    }

    // Actions

    pub fn action(input: Node) -> Result<Action> {
        match_nodes!(
        input.into_children();
        [assert_action((span, action))]   => Ok(Action::Assert{ span, action }),
        [assign_action((span, action))]   => Ok(Action::Assign{ span, action }),
        [assign_logical_action((span, action))]   => Ok(Action::AssignLogical { span, action }),
        [assume_action((span, action))]   => Ok(Action::Assume{ span, action }),
        [call_action((span, action))]     => Ok(Action::Call { span, action }),
        [ensure_action((span, action))]   => Ok(Action::Ensure { span, action}),
        [requires_action((span, action))] => Ok(Action::Requires{ span, action }),
        )
    }

    pub fn actions(input: Node) -> Result<Vec<Action>> {
        match_nodes!(
        input.into_children();
        [action(actions)..] => Ok(actions.collect()))
    }

    pub fn assert_action(input: Node) -> Result<(Span, AssertAction)> {
        let span = Span::from_node(&input);
        match_nodes!(
        input.into_children();
        [fmla(pred)] => Ok((span, AssertAction{pred})),
        )
    }

    pub fn assign_action(input: Node) -> Result<(Span, AssignAction)> {
        let span = Span::from_node(&input);
        match_nodes!(
        input.into_children();
        [var_decl(sym), rval(rhs)] => {
            let lhs_sort = sym.decl.clone();
            Ok((span, AssignAction{
                lhs: Expr::ProgramSymbol{sym}, lhs_sort, rhs}))
        },
        [lval(lhs), rval(rhs)] => match lhs {
            Expr::App{..} | Expr::FieldAccess{..} | Expr::Index{..} | Expr::ProgramSymbol{..} | Expr::This(_) => Ok((span, AssignAction{lhs, lhs_sort: Sort::ToBeInferred, rhs})),
            _ => todo!(),
        }
        )
    }

    pub fn assume_action(input: Node) -> Result<(Span, AssumeAction)> {
        let span = Span::from_node(&input);

        match_nodes!(
        input.into_children();
        [fmla(pred)] => Ok((span, AssumeAction{pred})),
        )
    }

    pub fn call_action(input: Node) -> Result<(Span, AppExpr)> {
        let span = input.as_pair().as_span(); // Irritating!

        match_nodes!(
        input.into_children();
        [rval(call)] => match call {
            Expr::ProgramSymbol { sym } => Ok((sym.span.clone(), AppExpr {
                func: Box::new(Expr::ProgramSymbol { sym }),
                func_sort: Sort::ToBeInferred,
                args: vec!()
            })),
            Expr::App { span, expr} => Ok((span, expr)),

            Expr::BinOp { expr: BinOp { op: Verb::Equals, ..}, ..} => Err(
                Error::new_from_span(ErrorVariant::<Rule>::CustomError { message:
                    "Boolean expression with no effect (Did you mean to assign with `:=` instead)?)".into() }, span)),

            // TODO: This error message can be improved by restricting the grammar
            // to avoid arbitrary rvals being call_actions.  For details, see:
            // https://github.com/dijkstracula/irving/issues/49
            _ => Err(Error::new_from_span(ErrorVariant::<Rule>::CustomError {
                message: format!("Unexpected expression for call {:?}", call) },
                span))
        })
    }

    pub fn ensure_action(input: Node) -> Result<(Span, EnsureAction)> {
        let span = Span::from_node(&input);

        match_nodes!(
        input.into_children();
        [fmla(pred)] => Ok((span, EnsureAction{pred})),
        )
    }

    pub fn assign_logical_action(input: Node) -> Result<(Span, AssignLogicalAction)> {
        let span = Span::from_node(&input);
        match_nodes!(
        input.into_children();
        [fmla(lhs), log_term(rhs)] => {
            Ok((span, AssignLogicalAction{lhs, rhs}))
        },
        )
    }

    pub fn requires_action(input: Node) -> Result<(Span, RequiresAction)> {
        let span = Span::from_node(&input);

        match_nodes!(
        input.into_children();
        [fmla(pred)] => Ok((span, RequiresAction{pred})),
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
        [rval(tst), stmt_block(thens)] => Ok(
            If{tst, thn: thens, els: None}
        ),
        [rval(tst), stmt_block(thens), stmt_block(elses)] => Ok(
            If{tst, thn: thens, els: Some(elses)}
        ),
        [rval(tst), stmt_block(thens), if_stmt(elsif)] => Ok(
            If{tst, thn: thens, els: Some(vec!(Stmt::If(elsif)))}
        ),
        )
    }

    pub fn while_stmt(input: Node) -> Result<While> {
        match_nodes!(
        input.into_children();
        [rval(test), stmt_block(stmts)] => Ok(
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
        [langver((major, minor)), includes(idecls), decl(decls).., EOI(())] => Ok(
            Prog {
                major_version: major,
                minor_version: minor,
                includes: idecls,
                top: ObjectDecl {
                        params: vec!(),
                        body: decls.collect::<>()
                    }
                }
            )
        )
    }
}
