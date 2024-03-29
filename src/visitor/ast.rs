#![allow(unused_variables)]
#![allow(dead_code)]
#![allow(unused_imports)]

use std::error::Error;

use crate::ast::actions::*;
use crate::ast::declarations::*;
use crate::ast::expressions;
use crate::ast::expressions::*;
use crate::ast::logic;
use crate::ast::logic::*;
use crate::ast::span::Span;
use crate::ast::statements::*;
use crate::ast::toplevels::*;
use crate::typechecker::sorts::IvySort;

use super::control::ControlMut;
use super::VisitorResult;

pub trait Visitor<T, E>
where
    T: Default,
    E: Error,
{
    // Top levels
    fn begin_prog(&mut self, _ast: &mut Prog) -> VisitorResult<T, E, Prog> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_prog(&mut self, _ast: &mut Prog) -> VisitorResult<T, E, Prog> {
        Ok(ControlMut::Produce(T::default()))
    }

    // Statements

    fn action_seq(&mut self, ast: &mut Vec<Action>) -> VisitorResult<T, E, Stmt> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_if(&mut self, _ast: &mut If) -> VisitorResult<T, E, Stmt> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_if(
        &mut self,
        _ast: &mut If,
        _tst_t: T,
        _then_t: Vec<T>,
        _else_t: Option<Vec<T>>,
    ) -> VisitorResult<T, E, Stmt> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_while(&mut self, _ast: &mut While) -> VisitorResult<T, E, Stmt> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_while(
        &mut self,
        _ast: &mut While,
        _test_t: T,
        _doit_t: Vec<T>,
    ) -> VisitorResult<T, E, Stmt> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_local_vardecl(
        &mut self,
        _name: &mut Token,
        _ast: &mut Sort,
    ) -> VisitorResult<T, E, Stmt> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_local_vardecl(
        &mut self,
        _name: &mut Token,
        _ast: &mut Sort,
        _id_t: T,
        _sort_t: T,
    ) -> VisitorResult<T, E, Stmt> {
        Ok(ControlMut::Produce(T::default()))
    }

    // Actions
    fn begin_assert(&mut self, _ast: &mut AssertAction) -> VisitorResult<T, E, Action> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_assert(&mut self, _ast: &mut AssertAction) -> VisitorResult<T, E, Action> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_assign(
        &mut self,
        _span: &Span,
        _ast: &mut AssignAction,
    ) -> VisitorResult<T, E, Action> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_assign(
        &mut self,
        _span: &Span,
        _ast: &mut AssignAction,
        _lhs_t: T,
        _rhs_t: T,
    ) -> VisitorResult<T, E, Action> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_assign_logical(
        &mut self,
        _span: &Span,
        _ast: &mut AssignLogicalAction,
    ) -> VisitorResult<T, E, Action> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn finish_assign_logical(
        &mut self,
        _span: &Span,
        _ast: &mut AssignLogicalAction,
        _lhs_t: T,
        _rhs_t: T,
    ) -> VisitorResult<T, E, Action> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_assume(&mut self, _ast: &mut AssumeAction) -> VisitorResult<T, E, Action> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_assume(&mut self, _ast: &mut AssumeAction) -> VisitorResult<T, E, Action> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_call(&mut self, ast: &mut AppExpr) -> VisitorResult<T, E, Action> {
        let res: VisitorResult<T, E, Action> = self.begin_app(ast).map(|ctrl| match ctrl {
            ControlMut::Produce(t) => ControlMut::Produce(t),
            ControlMut::SkipSiblings(t) => ControlMut::SkipSiblings(t),
            ControlMut::Mutation(_, _) => todo!(), // XXX: stupid hack that will bite me later,  but not today satan!
        });
        res
    }
    fn finish_call(
        &mut self,
        span: &Span,
        ast: &mut AppExpr,
        f: T,
        args: Vec<T>,
    ) -> VisitorResult<T, E, Action> {
        let res: VisitorResult<T, E, Action> =
            self.finish_app(span, ast, f, args).map(|ctrl| match ctrl {
                ControlMut::Produce(t) => ControlMut::Produce(t),
                ControlMut::SkipSiblings(t) => ControlMut::SkipSiblings(t),
                ControlMut::Mutation(_, _) => todo!(), // XXX: stupid hack that will bite me later,  but not today satan!
            });
        res
    }

    fn begin_ensure(&mut self, _ast: &mut EnsureAction) -> VisitorResult<T, E, Action> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_ensure(
        &mut self,
        _ast: &mut EnsureAction,
        _pred_p: T,
    ) -> VisitorResult<T, E, Action> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_requires(&mut self, _ast: &mut RequiresAction) -> VisitorResult<T, E, Action> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_requires(
        &mut self,
        _ast: &mut RequiresAction,
        _pred_t: T,
    ) -> VisitorResult<T, E, Action> {
        Ok(ControlMut::Produce(T::default()))
    }

    // Declarations

    fn begin_action_decl(
        &mut self,
        _span: &Span,
        _name: &mut Token,
        _ast: &mut ActionDecl,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_action_decl(
        &mut self,
        _span: &Span,
        _name: &mut Token,
        _ast: &mut ActionDecl,
        _name_ret: T,
        _params: Vec<T>,
        ret: Option<Binding<T>>,
        _body: Option<Vec<T>>,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_after_decl(
        &mut self,
        _span: &Span,
        _ast: &mut ActionMixinDecl,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_after_decl(
        &mut self,
        _span: &Span,
        _ast: &mut ActionMixinDecl,
        _n: T,
        _p: Option<Vec<T>>,
        _r: Option<T>,
        _b: Vec<T>,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_axiom_decl(&mut self, _ast: &mut Fmla) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_axiom_decl(&mut self, _ast: &mut Fmla) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_alias_decl(&mut self, _sym: &mut Token, _e: &mut Sort) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_alias_decl(
        &mut self,
        _sym: &mut Token,
        _e: &mut Sort,
        _sym_res: T,
        _e_res: T,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_attribute_decl(
        &mut self,
        _span: &Span,
        _lhs: &mut Expr,
        _rhs: &mut Option<Expr>,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_attribute_decl(
        &mut self,
        _span: &Span,
        _lhs: &mut Expr,
        _rhs: &mut Option<Expr>,
        _lhs_t: T,
        _rhs_t: Option<T>,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_before_decl(&mut self, _ast: &mut ActionMixinDecl) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_before_decl(
        &mut self,
        _ast: &mut ActionMixinDecl,
        _n: T,
        _p: Option<Vec<T>>,
        _b: Vec<T>,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_class_decl(
        &mut self,
        _span: &Span,
        _name: &mut Token,
        _ast: &mut ClassDecl,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_class_decl(
        &mut self,
        _span: &Span,
        _name: &mut Token,
        _ast: &mut ClassDecl,
        _name_t: T,
        _parent_t: Option<T>,
        _fields_t: Vec<T>,
        _actions_t: Vec<T>,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_common_decl(&mut self, _ast: &mut Vec<Decl>) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_common_decl(&mut self, _ast: &mut Vec<Decl>) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_export_decl(&mut self, _ast: &mut ExportDecl) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_export_decl(&mut self, _ast: &mut ExportDecl) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_function_decl(
        &mut self,
        _span: &Span,
        _name: &mut Token,
        _ast: &mut FunctionDecl,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_function_decl(
        &mut self,
        _span: &Span,
        _name: &mut Token,
        _ast: &mut FunctionDecl,
        _name_t: T,
        _sort_t: Vec<T>,
        _ret_t: Option<T>,
        _body_t: Option<T>,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_global_decl(&mut self, _ast: &mut Vec<Decl>) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_global_decl(&mut self, _ast: &mut Vec<Decl>) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_implement_decl(&mut self, _ast: &mut ActionMixinDecl) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_implement_decl(
        &mut self,
        _ast: &mut ActionMixinDecl,
        _name: T,
        _params: Option<Vec<T>>,
        ret: Option<Binding<T>>,
        _body: Vec<T>,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_import_decl(&mut self, _ast: &mut ImportDecl) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_import_decl(
        &mut self,
        _span: &Span,
        _ast: &mut ImportDecl,
        _n: T,
        _p: Vec<T>,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_include_decl(&mut self, _ast: &mut Token) -> VisitorResult<T, E, Token> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_include_decl(&mut self, _ast: &mut Token) -> VisitorResult<T, E, Token> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_instance_decl(
        &mut self,
        _span: &Span,
        _name: &mut Token,
        _ast: &mut InstanceDecl,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_instance_decl(
        &mut self,
        _span: &Span,
        _name: &mut Token,
        _ast: &mut InstanceDecl,
        _n: T,
        _s: T,
        _a: Vec<T>,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_interpret_decl(
        &mut self,
        _name: &mut Token,
        _sort: &mut Sort,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn finish_interpret_decl(
        &mut self,
        _name: &mut Token,
        _sort: &mut Sort,
        _n: T,
        _s: T,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_invariant_decl(
        &mut self,
        _name: &mut Token,
        _ast: &mut Fmla,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_invariant_decl(
        &mut self,
        _name: &mut Token,
        _ast: &mut Fmla,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_map_decl(
        &mut self,
        _span: &Span,
        _name: &mut Token,
        _ast: &mut MapDecl,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_map_decl(
        &mut self,
        _span: &Span,
        _name: &mut Token,
        _ast: &mut MapDecl,
        _name_t: T,
        _domain_t: Vec<T>,
        _range_t: T,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_module_decl(
        &mut self,
        _span: &Span,
        _name: &mut Token,
        _ast: &mut ModuleDecl,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_module_decl(
        &mut self,
        _name: &mut Token,
        _ast: &mut ModuleDecl,
        _n: T,
        _p: Vec<Binding<T>>,
        _b: Vec<T>,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_object_decl(
        &mut self,
        _span: &Span,
        _name: &mut Token,
        _ast: &mut ObjectDecl,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_object_decl(
        &mut self,
        _name: &mut Token,
        _ast: &mut ObjectDecl,
        _n: T,
        _p: Vec<T>,
        _b: Vec<T>,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_typedecl(
        &mut self,
        _span: &Span,
        _name: &mut Token,
        _ast: &mut Sort,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_typedecl(
        &mut self,
        _span: &Span,
        _name: &mut Token,
        _ast: &mut Sort,
        _n: T,
        _s: T,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_vardecl(
        &mut self,
        _span: &Span,
        _name: &mut Token,
        _ast: &mut Sort,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_vardecl(
        &mut self,
        _span: &Span,
        _name: &mut Token,
        _ast: &mut Sort,
        _id_t: T,
        _sort_t: T,
    ) -> VisitorResult<T, E, Decl> {
        Ok(ControlMut::Produce(T::default()))
    }

    // Quantified formulas and first-order logic

    fn begin_exists(&mut self, _ast: &mut Exists) -> VisitorResult<T, E, Fmla> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_exists(
        &mut self,
        _ast: &mut Exists,
        _vars: Vec<T>,
        _fmla: T,
    ) -> VisitorResult<T, E, Fmla> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_forall(&mut self, _ast: &mut Forall) -> VisitorResult<T, E, Fmla> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_forall(
        &mut self,
        _ast: &mut Forall,
        _vars: Vec<T>,
        _fmla: T,
    ) -> VisitorResult<T, E, Fmla> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_logical_app(&mut self, _ast: &mut LogicApp) -> VisitorResult<T, E, Fmla> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn finish_logical_app(
        &mut self,
        _ast: &mut LogicApp,
        _func_t: T,
        _args: Vec<T>,
    ) -> VisitorResult<T, E, Fmla> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_logical_binop(&mut self, _ast: &mut LogicBinOp) -> VisitorResult<T, E, Fmla> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn finish_logical_binop(
        &mut self,
        _ast: &mut LogicBinOp,
        _lhs_ret: T,
        _op_ret: T,
        _rhs_ret: T,
    ) -> VisitorResult<T, E, Fmla> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_logical_field_access(
        &mut self,
        _span: &Span,
        _lhs: &mut Fmla,
        _rhs: &mut Symbol,
    ) -> VisitorResult<T, E, Fmla> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn finish_logical_field_access(
        &mut self,
        _lhs: &mut Fmla,
        rhs: &mut Symbol,
        _lhs_res: T,
        _rhs_res: T,
    ) -> VisitorResult<T, E, Fmla> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_logical_unary_op(
        &mut self,
        _op: &mut Verb,
        _rhs: &mut Fmla,
    ) -> VisitorResult<T, E, Fmla> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_logical_unary_op(
        &mut self,
        _op: &mut Verb,
        _rhs: &mut Fmla,
        _rhs_t: T,
    ) -> VisitorResult<T, E, Fmla> {
        Ok(ControlMut::Produce(T::default()))
    }

    // Expressions

    fn begin_app(&mut self, _ast: &mut AppExpr) -> VisitorResult<T, E, Expr> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_app(
        &mut self,
        _span: &Span,
        _ast: &mut AppExpr,
        _f: T,
        _args: Vec<T>,
    ) -> VisitorResult<T, E, Expr> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_binop(&mut self, _ast: &mut BinOp) -> VisitorResult<T, E, Expr> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_binop(
        &mut self,
        _ast: &mut BinOp,
        _lhs_ret: T,
        _op_ret: T,
        _rhs_ret: T,
    ) -> VisitorResult<T, E, Expr> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_field_access(
        &mut self,
        _span: &Span,
        _lhs: &mut Expr,
        rhs: &mut Symbol,
    ) -> VisitorResult<T, E, Expr> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_field_access(
        &mut self,
        _span: &Span,
        _lhs: &mut Expr,
        rhs: &mut Symbol,
        _lhs_res: T,
        _rhs_res: T,
    ) -> VisitorResult<T, E, Expr> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_index(&mut self, _ast: &mut IndexExpr) -> VisitorResult<T, E, Expr> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_index(&mut self, _lhs: &mut IndexExpr) -> VisitorResult<T, E, Expr> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn begin_unary_op(&mut self, _op: &mut Verb, _rhs: &mut Expr) -> VisitorResult<T, E, Expr> {
        Ok(ControlMut::Produce(T::default()))
    }
    fn finish_unary_op(
        &mut self,
        _op: &mut Verb,
        _rhs: &mut Expr,
        _rhs_t: T,
    ) -> VisitorResult<T, E, Expr> {
        Ok(ControlMut::Produce(T::default()))
    }

    // Terminals

    fn boolean(&mut self, b: &mut bool) -> VisitorResult<T, E, bool> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn identifier(&mut self, _span: &Span, _i: &mut Ident) -> VisitorResult<T, E, Ident> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn number(&mut self, _span: &Span, _n: &mut i64) -> VisitorResult<T, E, i64> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn param(&mut self, p: &mut Symbol) -> VisitorResult<T, E, Symbol> {
        self.token(&p.span, &mut p.name)?.modifying(&mut p.name);
        self.sort(&mut p.decl)?.modifying(&mut p.decl);
        Ok(ControlMut::Produce(T::default()))
    }

    fn sort(&mut self, s: &mut Sort) -> VisitorResult<T, E, Sort> {
        if let Sort::Annotated(i) = s {
            self.identifier(&Span::Todo, i)?.modifying(i);
        }
        Ok(ControlMut::Produce(T::default()))
    }

    fn logic_symbol(&mut self, sym: &mut Symbol) -> VisitorResult<T, E, Fmla> {
        // TODO: I wonder how much of this I could automate away if I had a
        // From<Symbol> for Expr?
        Ok(match self.symbol(sym)? {
            ControlMut::Produce(t) => ControlMut::Produce(t),
            ControlMut::SkipSiblings(t) => ControlMut::SkipSiblings(t),
            ControlMut::Mutation(sym, t) => ControlMut::Mutation(
                Fmla::LogicSymbol {
                    span: sym.span.clone(),
                    sym,
                },
                t,
            ),
        })
    }

    fn program_symbol(&mut self, sym: &mut Symbol) -> VisitorResult<T, E, Expr> {
        // TODO: I wonder how much of this I could automate away if I had a
        // From<Symbol> for Expr?
        Ok(match self.symbol(sym)? {
            ControlMut::Produce(t) => ControlMut::Produce(t),
            ControlMut::SkipSiblings(t) => ControlMut::SkipSiblings(t),
            ControlMut::Mutation(sym, t) => ControlMut::Mutation(Expr::ProgramSymbol { sym }, t),
        })
    }

    fn symbol(&mut self, p: &mut Symbol) -> VisitorResult<T, E, Symbol> {
        self.token(&p.span, &mut p.name)?.modifying(&mut p.name);
        self.sort(&mut p.decl)?.modifying(&mut p.decl);
        Ok(ControlMut::Produce(T::default()))
    }

    fn this(&mut self) -> VisitorResult<T, E, Expr> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn token(&mut self, _span: &Span, _s: &mut Token) -> VisitorResult<T, E, Token> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn verb(&mut self, v: &mut Verb) -> VisitorResult<T, E, Verb> {
        Ok(ControlMut::Produce(T::default()))
    }

    //
}

impl<T, E> Visitable<T, E> for Prog
where
    T: Default,
    E: Error,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T, E>) -> VisitorResult<T, E, Self> {
        visitor.begin_prog(self)?.and_then(|_| {
            let _i = self
                .includes
                .iter_mut()
                .map(|p| {
                    Ok(visitor
                        .begin_include_decl(&mut p.name)?
                        .and_then(|_| visitor.finish_include_decl(&mut p.name)))
                })
                .collect::<Result<Vec<_>, _>>()?;

            let mut this_name = String::from("top");
            let mut this_params: ParamList = vec![];
            visitor
                .begin_object_decl(&Span::Todo, &mut this_name, &mut self.top)?
                .and_then(|_| {
                    let n = visitor
                        .token(&Span::Todo, &mut this_name)?
                        .modifying(&mut this_name);
                    let p = this_params.visit(visitor)?.modifying(&mut this_params);
                    let b = self.top.body.visit(visitor)?.modifying(&mut self.top.body);
                    visitor.finish_object_decl(&mut this_name, &mut self.top, n, p, b)
                })?;
            visitor.finish_prog(self)
        })
    }
}

impl<T, E> Visitable<T, E> for Action
where
    T: Default,
    E: Error,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T, E>) -> VisitorResult<T, E, Self> {
        log::trace!(target: "visitor", "Action {:?}", self.span());
        match self {
            Action::Assert { action, .. } => visitor
                .begin_assert(action)?
                .map(|_| Ok(action.pred.visit(visitor)?.modifying(&mut action.pred)))?
                .and_then(|_| visitor.finish_assert(action)),
            Action::Assign { span, action } => visitor.begin_assign(span, action)?.and_then(|_| {
                let lhs_t = action.lhs.visit(visitor)?.modifying(&mut action.lhs);
                let rhs_t = action.rhs.visit(visitor)?.modifying(&mut action.rhs);
                visitor.finish_assign(span, action, lhs_t, rhs_t)
            }),
            Action::Assume { action, .. } => visitor
                .begin_assume(action)?
                .map(|_| Ok(action.pred.visit(visitor)?.modifying(&mut action.pred)))?
                .and_then(|_| visitor.finish_assume(action)),
            Action::Call { span, action } => visitor.begin_call(action)?.and_then(|_| {
                let func = action.func.visit(visitor)?.modifying(&mut action.func);
                let args = action.args.visit(visitor)?.modifying(&mut action.args);
                visitor.finish_call(span, action, func, args)
            }),
            Action::Ensure { action, .. } => visitor.begin_ensure(action)?.and_then(|_| {
                let p = action.pred.visit(visitor)?.modifying(&mut action.pred);
                visitor.finish_ensure(action, p)
            }),
            Action::AssignLogical { span, action } => {
                visitor.begin_assign_logical(span, action)?.and_then(|_| {
                    let lhs_t = action.lhs.visit(visitor)?.modifying(&mut action.lhs);
                    let rhs_t = action.rhs.visit(visitor)?.modifying(&mut action.rhs);
                    visitor.finish_assign_logical(span, action, lhs_t, rhs_t)
                })
            }
            Action::Requires { action, .. } => visitor.begin_requires(action)?.and_then(|_| {
                let p = action.pred.visit(visitor)?.modifying(&mut action.pred);
                visitor.finish_requires(action, p)
            }),
        }
    }
}

impl<T, E> Visitable<T, E> for Expr
where
    T: Default,
    E: Error,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T, E>) -> VisitorResult<T, E, Self> {
        let span = self.span();
        log::trace!(target: "visitor", "Expr {:?}", self);
        let t = match self {
            Expr::App { span, expr } => visitor.begin_app(expr)?.and_then(|_| {
                let func = expr.func.visit(visitor)?.modifying(&mut expr.func);
                let args = expr.args.visit(visitor)?.modifying(&mut expr.args);
                visitor.finish_app(span, expr, func, args)
            }),
            Expr::BinOp { expr, .. } => visitor.begin_binop(expr)?.and_then(|foo| {
                let l = expr.lhs.visit(visitor)?.modifying(&mut expr.lhs);
                let o = visitor.verb(&mut expr.op)?.modifying(&mut expr.op);
                let r = expr.rhs.visit(visitor)?.modifying(&mut expr.rhs);
                visitor.finish_binop(expr, l, o, r)
            }),
            Expr::Boolean { val, .. } => {
                Ok(ControlMut::Produce(visitor.boolean(val)?.modifying(val)))
            }
            Expr::FieldAccess {
                span,
                expr:
                    expressions::FieldAccess {
                        ref mut record,
                        ref mut field,
                    },
                ..
            } => visitor
                .begin_field_access(span, record, field)?
                .and_then(|_| {
                    let r = record.visit(visitor)?.modifying(record);
                    let f = visitor.symbol(field)?.modifying(field);
                    visitor.finish_field_access(span, record, field, r, f)
                }),
            Expr::Index { expr, .. } => visitor
                .begin_index(expr)?
                .and_then(|_| expr.lhs.visit(visitor))?
                .and_then(|_| expr.idx.visit(visitor))?
                .and_then(|_| visitor.finish_index(expr)),
            Expr::Number { span, val } => {
                let t = visitor.number(span, val)?.modifying(val);
                Ok(ControlMut::Produce(t))
            }
            Expr::UnaryOp {
                span: _,
                ref mut op,
                ref mut expr,
            } => visitor.begin_unary_op(op, expr)?.and_then(|_| {
                visitor.verb(op)?.modifying(op);
                let expr_t = expr.visit(visitor)?.modifying(expr);
                visitor.finish_unary_op(op, expr, expr_t)
            }),
            Expr::ProgramSymbol { sym } => visitor.program_symbol(sym),
            Expr::This(_) => visitor.this(),
        }?
        .modifying(self);
        Ok(ControlMut::Produce(t))
    }
}

impl<T, E> Visitable<T, E> for Fmla
where
    T: Default,
    E: Error,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T, E>) -> VisitorResult<T, E, Self> {
        log::trace!(target: "visitor", "Fmla {:?}", self.span());
        let t = match self {
            Fmla::Forall { fmla, .. } => visitor.begin_forall(fmla)?.and_then(|_| {
                let vars_t = fmla.vars.visit(visitor)?.modifying(&mut fmla.vars);
                let fmla_t = fmla.fmla.visit(visitor)?.modifying(&mut fmla.fmla);
                visitor.finish_forall(fmla, vars_t, fmla_t)
            }),
            Fmla::Exists { fmla, .. } => visitor.begin_exists(fmla)?.and_then(|_| {
                let vars_t = fmla.vars.visit(visitor)?.modifying(&mut fmla.vars);
                let fmla_t = fmla.fmla.visit(visitor)?.modifying(&mut fmla.fmla);
                visitor.finish_exists(fmla, vars_t, fmla_t)
            }),
            Fmla::App { app, .. } => visitor.begin_logical_app(app)?.and_then(|_| {
                let func_t = app.func.visit(visitor)?.modifying(&mut app.func);
                let args_t = app.args.visit(visitor)?.modifying(&mut app.args);
                visitor.finish_logical_app(app, func_t, args_t)
            }),
            Fmla::BinOp { binop, .. } => visitor.begin_logical_binop(binop)?.and_then(|_| {
                let l = binop.lhs.visit(visitor)?.modifying(&mut binop.lhs);
                let o = visitor.verb(&mut binop.op)?.modifying(&mut binop.op);
                let r = binop.rhs.visit(visitor)?.modifying(&mut binop.rhs);
                visitor.finish_logical_binop(binop, l, o, r)
            }),
            Fmla::Boolean { val, .. } => {
                Ok(ControlMut::Produce(visitor.boolean(val)?.modifying(val)))
            }
            Fmla::FieldAccess {
                span,
                fmla:
                    logic::FieldAccess {
                        ref mut record,
                        ref mut field,
                    },
            } => visitor
                .begin_logical_field_access(span, record, field)?
                .and_then(|_| {
                    let r = record.visit(visitor)?.modifying(record);
                    let f = visitor.symbol(field)?.modifying(field);
                    visitor.finish_logical_field_access(record, field, r, f)
                }),
            Fmla::Number { span, val } => Ok(ControlMut::Produce(
                visitor.number(span, val)?.modifying(val),
            )),
            Fmla::LogicSymbol { span, sym } => visitor.logic_symbol(sym),
            Fmla::ProgramSymbol { span, sym } => {
                Ok(ControlMut::Produce(visitor.symbol(sym)?.modifying(sym)))
            } // XXX: need to call visitor.program_symbol
            Fmla::UnaryOp { span, op, fmla } => {
                visitor.begin_logical_unary_op(op, fmla)?.and_then(|_| {
                    visitor.verb(op)?.modifying(op);
                    let fmla_t = fmla.visit(visitor)?.modifying(fmla);
                    visitor.finish_logical_unary_op(op, fmla, fmla_t)
                })
            }
        }?
        .modifying(self);
        Ok(ControlMut::Produce(t))
    }
}

impl<T, E> Visitable<T, E> for Stmt
where
    T: Default,
    E: Error,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T, E>) -> VisitorResult<T, E, Self>
    where
        Self: Sized,
    {
        //log::trace!(target: "visitor", "Stmt");
        let t = match self {
            Stmt::ActionSequence(seq) => visitor.action_seq(seq),
            Stmt::If(stmt) => visitor.begin_if(stmt)?.and_then(|_| {
                let test_t = stmt.tst.visit(visitor)?.modifying(&mut stmt.tst);
                let then_t = stmt.thn.visit(visitor)?.modifying(&mut stmt.thn);
                let else_t = stmt
                    .els
                    .as_mut()
                    .map(|stmts| Ok(stmts.visit(visitor)?.modifying(stmts)))
                    .transpose()?;
                visitor.finish_if(stmt, test_t, then_t, else_t)
            }),
            Stmt::While(stmt) => visitor.begin_while(stmt)?.and_then(|_| {
                let test = stmt.test.visit(visitor)?.modifying(&mut stmt.test);
                let doit = stmt.doit.visit(visitor)?.modifying(&mut stmt.doit);
                visitor.finish_while(stmt, test, doit)
            }),
            Stmt::VarDecl(Binding {
                ref mut name,
                ref mut decl,
                ref span,
            }) => visitor.begin_local_vardecl(name, decl)?.and_then(|_| {
                let n = visitor.token(span, name)?.modifying(name);
                let s = visitor.sort(decl)?.modifying(decl);
                visitor.finish_local_vardecl(name, decl, n, s)
            }),
        }?
        .modifying(self);
        Ok(ControlMut::Produce(t))
    }
}

impl<T, E> Visitable<T, E> for Decl
where
    T: Default,
    E: Error,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T, E>) -> VisitorResult<T, E, Self> {
        log::trace!(target: "visitor", "Decl {:?}", self.span());
        let t = match self {
            Decl::Action { decl } => helpers::walk_action_decl(visitor, decl),
            Decl::AfterAction { span, decl } => {
                visitor.begin_after_decl(span, decl)?.and_then(|_| {
                    let n = visitor
                        .identifier(span, &mut decl.name)?
                        .modifying(&mut decl.name);
                    let p = decl
                        .params
                        .as_mut()
                        .map(|ps| Ok(ps.visit(visitor)?.modifying(ps)))
                        .transpose()?;
                    let r = decl
                        .ret
                        .as_mut()
                        .map(|r| Ok(visitor.param(r)?.modifying(r)))
                        .transpose()?;
                    let b = decl.body.visit(visitor)?.modifying(&mut decl.body);
                    visitor.finish_after_decl(span, decl, n, p, r, b)
                })
            }
            Decl::Alias {
                decl:
                    Binding {
                        ref mut name,
                        ref mut decl,
                        ref span,
                    },
                ..
            } => visitor.begin_alias_decl(name, decl)?.and_then(|_| {
                let n = visitor.token(span, name)?.modifying(name);
                let s = visitor.sort(decl)?.modifying(decl);
                visitor.finish_alias_decl(name, decl, n, s)
            }),
            Decl::Attribute { span, lhs, rhs } => {
                visitor.begin_attribute_decl(span, lhs, rhs)?.and_then(|_| {
                    let l_t = lhs.visit(visitor)?.modifying(lhs);
                    let r_t = rhs
                        .as_mut()
                        .map(|rhs| Ok(rhs.visit(visitor)?.modifying(rhs)))
                        .transpose()?;
                    visitor.finish_attribute_decl(span, lhs, rhs, l_t, r_t)
                })
            }
            Decl::Axiom { decl, .. } => visitor.begin_axiom_decl(decl)?.and_then(|_| {
                let _f = decl.visit(visitor)?.modifying(decl);
                visitor.finish_axiom_decl(decl)
            }),
            Decl::BeforeAction { decl, .. } => visitor.begin_before_decl(decl)?.and_then(|_| {
                let n = visitor
                    .identifier(&Span::Todo, &mut decl.name)?
                    .modifying(&mut decl.name);
                let p = decl
                    .params
                    .as_mut()
                    .map(|ps| Ok(ps.visit(visitor)?.modifying(ps)))
                    .transpose()?;
                let b = decl.body.visit(visitor)?.modifying(&mut decl.body);
                visitor.finish_before_decl(decl, n, p, b)
            }),
            Decl::Class {
                decl:
                    Binding {
                        ref mut name,
                        decl,
                        ref span,
                    },
            } => visitor.begin_class_decl(span, name, decl)?.and_then(|_| {
                let name_t = visitor.token(span, name)?.modifying(name);
                let parent_t = decl
                    .parent
                    .as_mut()
                    .map(|p| Ok(visitor.token(span, p)?.modifying(p)))
                    .transpose()?;
                let fields_t = decl.fields.visit(visitor)?.modifying(&mut decl.fields);
                let actions_t = decl
                    .actions
                    .iter_mut()
                    .map(|a| Ok(helpers::walk_action_decl(visitor, a)?.unwrap()))
                    .collect::<Result<Vec<_>, E>>()?;
                visitor.finish_class_decl(span, name, decl, name_t, parent_t, fields_t, actions_t)
            }),
            Decl::Common { decl, .. } => visitor.begin_common_decl(decl)?.and_then(|_| {
                let _d = decl.visit(visitor)?.modifying(decl);
                visitor.finish_common_decl(decl)
            }),
            Decl::Export { span, decl } => {
                visitor.begin_export_decl(decl)?.and_then(|_| match decl {
                    ExportDecl::Action(binding) => helpers::walk_action_decl(visitor, binding),
                    ExportDecl::ForwardRef(sym) => Ok(ControlMut::Produce(
                        visitor.token(span, sym)?.modifying(sym),
                    )),
                })
            }
            Decl::Function {
                decl: Binding { name, decl, span },
                ..
            } => visitor
                .begin_function_decl(span, name, decl)?
                .and_then(|_| {
                    let n = visitor.token(span, name)?.modifying(name);
                    let p = decl.params.visit(visitor)?.modifying(&mut decl.params);
                    let r = decl
                        .ret
                        .as_mut()
                        .map(|ret| Ok(visitor.identifier(span, ret)?.modifying(ret)))
                        .transpose()?;
                    let b = decl
                        .body
                        .as_mut()
                        .map(|body| Ok(body.visit(visitor)?.modifying(body)))
                        .transpose()?;
                    visitor.finish_function_decl(span, name, decl, n, p, r, b)
                }),
            Decl::Globals(decl) => visitor.begin_global_decl(decl)?.and_then(|_| {
                let _d = decl.visit(visitor)?.modifying(decl);
                visitor.finish_global_decl(decl)
            }),
            Decl::Implement { span, decl } => visitor.begin_implement_decl(decl)?.and_then(|_| {
                let n = visitor
                    .identifier(span, &mut decl.name)?
                    .modifying(&mut decl.name);
                let params = decl
                    .params
                    .as_mut()
                    .map(|p| Ok(p.visit(visitor)?.modifying(p)))
                    .transpose()?;
                let ret = match &mut decl.ret {
                    None => None,
                    Some(sym) => Some(Binding::from(
                        sym.name.clone(),
                        visitor.param(sym)?.modifying(sym),
                        span.clone(),
                    )),
                };
                let body = decl.body.visit(visitor)?.modifying(&mut decl.body);
                visitor.finish_implement_decl(decl, n, params, ret, body)
            }),
            Decl::Import { decl, span } => visitor.begin_import_decl(decl)?.and_then(|_| {
                let n = visitor
                    .token(span, &mut decl.name)?
                    .modifying(&mut decl.name);
                let p = decl.params.visit(visitor)?.modifying(&mut decl.params);
                visitor.finish_import_decl(span, decl, n, p)
            }),
            Decl::Instance {
                decl:
                    Binding {
                        ref mut name,
                        ref mut decl,
                        ref span,
                    },
                ..
            } => visitor
                .begin_instance_decl(span, name, decl)?
                .and_then(|_| {
                    let n = visitor.token(span, name)?.modifying(name);
                    let s = visitor
                        .identifier(span, &mut decl.sort)?
                        .modifying(&mut decl.sort);
                    let a = decl
                        .args
                        .iter_mut()
                        .map(|p| Ok(visitor.token(span, &mut p.name)?.modifying(&mut p.name)))
                        .collect::<Result<Vec<_>, _>>()?;
                    visitor.finish_instance_decl(span, name, decl, n, s, a)
                }),
            Decl::Instantiate { name, prms } => todo!(),
            Decl::Interpret {
                decl:
                    InterpretDecl {
                        ref mut name,
                        ref mut sort,
                    },
                ..
            } => visitor.begin_interpret_decl(name, sort)?.and_then(|_| {
                let n = visitor.token(&Span::Todo, name)?.modifying(name);
                let s = sort.visit(visitor)?.modifying(sort);
                visitor.finish_interpret_decl(name, sort, n, s)
            }),
            Decl::Invariant { decl } => visitor
                .begin_invariant_decl(&mut decl.name, &mut decl.decl)?
                .and_then(|_| {
                    let _f = decl.decl.visit(visitor)?.modifying(&mut decl.decl);
                    visitor.finish_invariant_decl(&mut decl.name, &mut decl.decl)
                }),
            Decl::Map {
                decl:
                    Binding {
                        ref mut name,
                        ref mut decl,
                        ref mut span,
                    },
            } => visitor.begin_map_decl(span, name, decl)?.and_then(|_| {
                let n = visitor.token(span, name)?.modifying(name);
                let domain_t = decl.domain.visit(visitor)?.modifying(&mut decl.domain);
                let range_t = decl.range.visit(visitor)?.modifying(&mut decl.range);
                visitor.finish_map_decl(span, name, decl, n, domain_t, range_t)
            }),
            Decl::Module {
                decl:
                    Binding {
                        ref mut name,
                        ref mut decl,
                        ref mut span,
                    },
                ..
            } => visitor.begin_module_decl(span, name, decl)?.and_then(|_| {
                let n = visitor.token(span, name)?.modifying(name);
                let p = decl
                    .sortsyms
                    .iter_mut()
                    .map(|Binding { name, decl, span }| Ok(Binding::from(name.clone(), visitor.token(span, name)?.modifying(name), span.clone())))
                    .collect::<Result<Vec<_>, _>>()?;
                let b = decl.body.visit(visitor)?.modifying(&mut decl.body);
                visitor.finish_module_decl(name, decl, n, p, b)
            }),
            Decl::Noop => return Ok(ControlMut::Produce(T::default())),
            Decl::Object {
                decl:
                    Binding {
                        ref mut name,
                        ref mut decl,
                        ref span,
                    },
                ..
            } => visitor.begin_object_decl(span, name, decl)?.and_then(|_| {
                let n = visitor.token(span, name)?.modifying(name);
                let p = decl.params.visit(visitor)?.modifying(&mut decl.params);
                let b = decl.body.visit(visitor)?.modifying(&mut decl.body);
                visitor.finish_object_decl(name, decl, n, p, b)
            }),
            Decl::Stmts(stmts) => {
                let mut _t = stmts.visit(visitor)?.modifying(stmts);
                Ok(ControlMut::Produce(T::default()))
            }
            Decl::Subclass {
                decl:
                    Binding {
                        ref mut name,
                        ref mut decl,
                        ref span,
                    },
            } => visitor.begin_class_decl(span, name, decl)?.and_then(|_| {
                let name_t = visitor.token(span, name)?.modifying(name);
                let parent_t = decl
                    .parent
                    .as_mut()
                    .map(|p| Ok(visitor.token(span, p)?.modifying(p)))
                    .transpose()?;
                let fields_t = decl.fields.visit(visitor)?.modifying(&mut decl.fields);
                let actions_t = decl
                    .actions
                    .iter_mut()
                    .map(|a| Ok(helpers::walk_action_decl(visitor, a)?.unwrap()))
                    .collect::<Result<Vec<_>, E>>()?;
                visitor.finish_class_decl(span, name, decl, name_t, parent_t, fields_t, actions_t)
            }),
            Decl::Var {
                decl:
                    Binding {
                        ref mut name,
                        ref mut decl,
                        ref span,
                    },
                ..
            } => visitor.begin_vardecl(span, name, decl)?.and_then(|_| {
                let n = visitor.token(span, name)?.modifying(name);
                let s = visitor.sort(decl)?.modifying(decl);
                visitor.finish_vardecl(span, name, decl, n, s)
            }),
            Decl::Type {
                decl:
                    Binding {
                        ref mut name,
                        ref mut decl,
                        ref span,
                    },
                ..
            } => visitor.begin_typedecl(span, name, decl)?.and_then(|_| {
                let n = visitor.token(span, name)?.modifying(name);
                let s = visitor.sort(decl)?.modifying(decl);
                visitor.finish_typedecl(span, name, decl, n, s)
            }),
        }?
        .modifying(self);
        Ok(ControlMut::Produce(t))
    }
}

impl<T, E> Visitable<T, E> for Sort
where
    T: Default,
    E: Error,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T, E>) -> VisitorResult<T, E, Self> {
        log::trace!(target: "visitor", "Sort {:?}", self);
        visitor.sort(self)
    }
}

// Implementations for compound nodes

impl<T, E> Visitable<T, E, Vec<T>> for Vec<Expr>
where
    T: Default,
    E: Error,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T, E>) -> VisitorResult<Vec<T>, E, Self> {
        let mut res = vec![];
        for node in self {
            match node.visit(visitor)? {
                ControlMut::Produce(t) => res.push(t),
                ControlMut::SkipSiblings(t) => {
                    res.push(t);
                    break;
                }
                ControlMut::Mutation(repl, t) => {
                    *node = repl;
                    res.push(t);
                }
            };
        }
        Ok(ControlMut::Produce(res))
    }
}

impl<T, E> Visitable<T, E, Vec<T>> for Vec<Fmla>
where
    T: Default,
    E: Error,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T, E>) -> VisitorResult<Vec<T>, E, Self> {
        let mut res = vec![];
        for node in self {
            match node.visit(visitor)? {
                ControlMut::Produce(t) => res.push(t),
                ControlMut::SkipSiblings(t) => {
                    res.push(t);
                    break;
                }
                ControlMut::Mutation(repl, t) => {
                    *node = repl;
                    res.push(t);
                }
            };
        }
        Ok(ControlMut::Produce(res))
    }
}

impl<T, E> Visitable<T, E, Vec<T>> for Vec<Stmt>
where
    T: Default,
    E: Error,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T, E>) -> VisitorResult<Vec<T>, E, Self> {
        log::trace!(target: "visitor", "Vec<Stmt> (len {:?})", self.len());
        let mut res = vec![];
        for node in self {
            match node.visit(visitor)? {
                ControlMut::Produce(t) => res.push(t),
                ControlMut::SkipSiblings(t) => {
                    res.push(t);
                    break;
                }
                ControlMut::Mutation(Stmt::ActionSequence(actions), t) if actions.is_empty() => {
                    continue;
                }
                ControlMut::Mutation(repl, t) => {
                    *node = repl;
                    res.push(t);
                }
            };
        }
        Ok(ControlMut::Produce(res))
    }
}

impl<T, E> Visitable<T, E, Vec<T>> for Vec<Decl>
where
    T: Default,
    E: Error,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T, E>) -> VisitorResult<Vec<T>, E, Self> {
        let mut res = vec![];
        for node in self {
            match node {
                Decl::Noop => continue,
                _ => (),
            };
            match node.visit(visitor)? {
                ControlMut::Produce(t) => res.push(t),
                ControlMut::SkipSiblings(t) => {
                    res.push(t);
                    break;
                }

                ControlMut::Mutation(Decl::Common { decl, .. }, t)
                | ControlMut::Mutation(Decl::Globals(decl), t)
                    if decl.is_empty() =>
                {
                    continue
                }
                ControlMut::Mutation(Decl::Noop, _) => {
                    continue;
                }

                ControlMut::Mutation(repl, t) => {
                    *node = repl;
                    res.push(t);
                }
            };
        }
        Ok(ControlMut::Produce(res))
    }
}

impl<T, E> Visitable<T, E, Vec<T>> for Vec<Action>
where
    T: Default,
    E: Error,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T, E>) -> VisitorResult<Vec<T>, E, Self> {
        let mut res = vec![];
        for node in self {
            match node.visit(visitor)? {
                ControlMut::Produce(t) => res.push(t),
                ControlMut::SkipSiblings(t) => {
                    res.push(t);
                    break;
                }
                ControlMut::Mutation(repl, t) => {
                    *node = repl;
                    res.push(t);
                }
            };
        }
        Ok(ControlMut::Produce(res))
    }
}

impl<T, E> Visitable<T, E, Vec<T>> for ParamList
where
    T: Default,
    E: Error,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T, E>) -> VisitorResult<Vec<T>, E, Self> {
        let mut res = vec![];
        for node in self {
            log::trace!(target: "visitor", "Param {:?}", node);
            match visitor.param(node)? {
                ControlMut::Produce(t) => res.push(t),
                ControlMut::SkipSiblings(t) => {
                    res.push(t);
                    break;
                }
                ControlMut::Mutation(repl, t) => {
                    *node = repl;
                    res.push(t);
                }
            };
        }
        Ok(ControlMut::Produce(res))
    }
}

/// Something that can be visited by a Visitor.
pub trait Visitable<T, E, U = T>
where
    Self: Sized,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T, E>) -> VisitorResult<U, E, Self>;
}

mod helpers {
    use std::error::Error;

    use crate::ast::declarations::{self, Binding};

    use super::{Visitable, Visitor};

    // We need to walk specifically action declarations in several places in
    // Visitor, so factor that functionality out.  This probably suggests I
    // designed this part wrong, c'est la vie.
    pub fn walk_action_decl<T, E>(
        visitor: &mut dyn Visitor<T, E>,
        binding: &mut Binding<declarations::ActionDecl>,
    ) -> Result<crate::visitor::ControlMut<T, crate::ast::declarations::Decl>, E>
    where
        T: Default,
        E: Error,
    {
        let span = &binding.span;
        let name = &mut binding.name;
        let decl = &mut binding.decl;

        visitor.begin_action_decl(span, name, decl)?.and_then(|_| {
            let n = visitor.token(span, name)?.modifying(name);
            let params = decl.params.visit(visitor)?.modifying(&mut decl.params);
            let ret = match &mut decl.ret {
                None => None,
                Some(sym) => Some(Binding::from(
                    sym.name.clone(),
                    visitor.param(sym)?.modifying(sym),
                    span.clone(),
                )),
            };
            let body = decl
                .body
                .as_mut()
                .map(|b| Ok(b.visit(visitor)?.modifying(b)))
                .transpose()?;
            visitor.finish_action_decl(span, name, decl, n, params, ret, body)
        })
    }
}
