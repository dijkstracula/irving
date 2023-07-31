use std::collections::BTreeMap;

use anyhow::bail;

use super::{
    sorts::{self, ActionArgs, IvySort, Object},
    unifier::BindingResolver,
};
use crate::{
    ast::{
        actions::{self, Action},
        declarations::{self, ActionMixinDecl, Binding},
        expressions::{self, AppExpr, Expr, Sort, Symbol, Token},
        logic, statements,
    },
    passes::module_instantiation,
    typechecker::{sorts::Module, TypeError},
    visitor::{ast::Visitable, ast::Visitor, control::ControlMut, VisitorResult},
};

pub struct SortInferer {
    // All the sorts we know about, at the current and shadowing scope
    pub bindings: BindingResolver,

    // Remember the function signature (arguments; named ret) from the
    // function definition, so we are always sure how to bind those values locally.
    pub action_locals: BTreeMap<Token, Vec<Token>>,
}

impl SortInferer {
    // TODO: this should take a ref to bindings because the visitor will
    // want to hold onto it.
    pub fn new() -> Self {
        SortInferer {
            bindings: BindingResolver::new(),
            action_locals: BTreeMap::new(),
        }
    }

    // When a Before/After/Implement is mixed into an Action, we need to
    // ensure that the former, if it has parameter/return values set, are
    // in line with the latter's signature.
    fn resolve_mixin(
        &mut self,
        mixin: &mut declarations::ActionMixinDecl,
        action_sort: IvySort,
        mixin_params_sort: Option<Vec<IvySort>>,
        mixin_ret_sort: Option<IvySort>,
    ) -> anyhow::Result<(ActionMixinDecl, IvySort)> {
        let action_args = match &action_sort {
            IvySort::Action(args, _, _) => args,
            _ => unreachable!(),
        };
        let action_args = match &mixin.params {
            // a) If the mixin has no parameter signature used, we simply reuse
            // the parameter list from the action definition's sort.
            None => action_args.clone(),

            // b) If the mixin has a parameter signature, it has to match the
            // action declaration's signature in terms of arity but NOT in
            // identifier names (that is, an Ivy programmer can rebind parameter
            // names to something else, but can't add/remove them or change
            // their sorts.)
            Some(mixin_args) => {
                if action_args.len() != mixin_args.len() {
                    bail!(TypeError::LenMismatch {
                        expected: action_args.len(),
                        actual: mixin_args.len()
                    });
                } else {
                    mixin_args
                        .iter()
                        .map(|sym| sym.id.clone())
                        .collect::<Vec<_>>()
                }
            }
        };

        // To unify the mixin against its action declaration, we need to
        // extract as much as we can about the sort side of the parameter
        // list.  Unlike parameter names, we can't change the sorts in the
        // parameter list.
        let mixin_sort = match (mixin_params_sort, mixin_ret_sort) {
            (None, None) => self.bindings.new_sortvar(),
            (Some(params), None) => {
                IvySort::action_sort(action_args, params, sorts::ActionRet::Unknown)
            }
            (None, Some(ret)) => IvySort::Action(
                action_args,
                ActionArgs::Unknown,
                sorts::ActionRet::Named(Box::new(Binding::from("TODO", ret))),
            ),
            (Some(params), Some(ret)) => IvySort::Action(
                action_args,
                ActionArgs::List(params),
                sorts::ActionRet::Named(Box::new(Binding::from("TODO", ret))),
            ),
        };

        let unified = self.bindings.unify(&action_sort, &mixin_sort)?;

        // Lastly, we want to propagate what we've learned about the argument
        // list for later passes.
        let mixed_params = match &unified {
            IvySort::Action(args, ActionArgs::List(sorts), _) => args
                .iter()
                .zip(sorts.iter())
                .map(|(name, sort)| Symbol {
                    id: name.clone(),
                    sort: Sort::Resolved(sort.clone()),
                })
                .collect::<Vec<_>>(),
            _ => unreachable!(),
        };

        let mixed = ActionMixinDecl {
            name: mixin.name.clone(),
            params: Some(mixed_params),
            ret: mixin.ret.clone(), // XXX: this is wrong!
            body: mixin.body.clone(),
        };

        Ok((mixed, unified))
    }
}

impl Visitor<IvySort> for SortInferer {
    fn boolean(&mut self, _b: &mut bool) -> VisitorResult<IvySort, bool> {
        Ok(ControlMut::Produce(IvySort::Bool))
    }

    fn identifier(
        &mut self,
        i: &mut expressions::Ident,
    ) -> VisitorResult<IvySort, expressions::Ident> {
        // XXX: This is a hack for built-ins like `bool` that we shouldn't have to
        // special-case in this way.
        if i.len() == 1 {
            let s = i.get_mut(0).unwrap();
            Ok(ControlMut::Produce(s.visit(self)?.modifying(s)?))
        } else {
            // XXX: we always set include_spec to true.  Suggests we need to also
            // return an annotation indicating if this was a spec/common/etc. decl.
            let resolved = self.bindings.lookup_ident(i)?;
            Ok(ControlMut::Produce(resolved.clone()))
        }
    }

    fn number(&mut self, _n: &mut i64) -> VisitorResult<IvySort, i64> {
        Ok(ControlMut::Produce(IvySort::Number))
    }

    fn param(
        &mut self,
        p: &mut expressions::Symbol,
    ) -> VisitorResult<IvySort, expressions::Symbol> {
        println!("NBT: visiting param {:?}", p);
        match &mut p.sort {
            Sort::ToBeInferred => {
                let sortvar = self.bindings.new_sortvar();
                self.bindings.append(p.id.clone(), sortvar.clone())?;
                Ok(ControlMut::Produce(sortvar))
            }
            Sort::Annotated(id) => {
                let resolved = id.visit(self)?.modifying(id)?;
                // Note that because a parameter binds a new name, we add it
                // to the bindings here.  We do not do the same for sort!()
                self.bindings.append(p.id.clone(), resolved.clone())?;
                Ok(ControlMut::Mutation(
                    expressions::Symbol {
                        id: p.id.clone(),
                        sort: Sort::Resolved(resolved.clone()),
                    },
                    resolved,
                ))
            }
            Sort::Resolved(ivysort) => {
                self.bindings.append(p.id.clone(), ivysort.clone())?;
                Ok(ControlMut::Produce(ivysort.clone()))
            }
        }
    }

    fn sort(&mut self, s: &mut Sort) -> VisitorResult<IvySort, Sort> {
        let ctrl = match s {
            Sort::ToBeInferred => {
                let s = self.bindings.new_sortvar();
                ControlMut::Mutation(Sort::Resolved(s.clone()), s)
            }
            Sort::Annotated(ident) => {
                let resolved = self.identifier(ident)?.modifying(ident)?;
                ControlMut::Mutation(Sort::Resolved(resolved.clone()), resolved)
            }
            Sort::Resolved(ivysort) => ControlMut::Produce(ivysort.clone()),
        };
        Ok(ctrl)
    }

    fn symbol(
        &mut self,
        p: &mut expressions::Symbol,
    ) -> VisitorResult<IvySort, expressions::Symbol> {
        let sort = match &mut p.sort {
            Sort::ToBeInferred => match self.bindings.lookup_sym(&p.id) {
                None => bail!(TypeError::UnboundVariable(p.id.clone())),
                Some(s) => s.clone(),
            },
            Sort::Annotated(ident) => self.identifier(ident)?.modifying(ident)?,
            Sort::Resolved(ivysort) => ivysort.clone(),
        };
        Ok(ControlMut::Produce(sort))
    }

    fn token(
        &mut self,
        sym: &mut expressions::Token,
    ) -> VisitorResult<IvySort, expressions::Token> {
        match sym.as_str() {
            "bool" => Ok(ControlMut::Produce(IvySort::Bool)),
            "unbounded_sequence" => Ok(ControlMut::Produce(IvySort::Number)),
            "this" => Ok(ControlMut::Produce(IvySort::This)),
            // TODO: and of course other builtins.
            _ => match self.bindings.lookup_sym(sym) {
                Some(sort) => Ok(ControlMut::Produce(sort.clone())),
                None => bail!(TypeError::UnboundVariable(sym.clone())),
            },
        }
    }

    fn this(&mut self) -> VisitorResult<IvySort, Expr> {
        match self.bindings.lookup_sym("this") {
            None => bail!(TypeError::UnboundVariable("this".into())),
            Some(t) => Ok(ControlMut::Produce(t.clone())),
        }
    }

    // Actions

    fn begin_assign(&mut self, ast: &mut actions::AssignAction) -> VisitorResult<IvySort, Action> {
        self.bindings.push_scope();
        match &ast.lhs {
            Expr::App(AppExpr { args, .. }) => {
                for arg in args {
                    if let Expr::LogicSymbol(sym) = arg {
                        let s = self.bindings.new_sortvar();
                        self.bindings.append(sym.id.clone(), s)?;
                    }
                }
            }
            _ => (),
        }

        Ok(ControlMut::Produce(IvySort::Unit))
    }

    fn finish_assign(
        &mut self,
        _ast: &mut actions::AssignAction,
        lhs_sort: IvySort,
        rhs_sort: IvySort,
    ) -> VisitorResult<IvySort, Action> {
        println!("NBT: {:?} := {:?}", lhs_sort, rhs_sort);
        self.bindings.unify(&lhs_sort, &rhs_sort)?;
        self.bindings.pop_scope();
        Ok(ControlMut::Produce(IvySort::Unit))
    }

    // Statements

    fn action_seq(&mut self, ast: &mut Vec<Action>) -> VisitorResult<IvySort, statements::Stmt> {
        //XXX: kinda dumb, honestly.
        let _ = ast.visit(self)?.modifying(ast)?;
        Ok(ControlMut::Produce(IvySort::Unit))
    }

    fn begin_local_vardecl(
        &mut self,
        name: &mut Token,
        _ast: &mut Sort,
    ) -> VisitorResult<IvySort, statements::Stmt> {
        // Bind the name to _something_; we'll unify this value with its resolved sort when finishing the visit.
        let v = self.bindings.new_sortvar();
        self.bindings.append(name.clone(), v)?;
        Ok(ControlMut::Produce(IvySort::Unit))
    }
    fn finish_local_vardecl(
        &mut self,
        name: &mut Token,
        _ast: &mut Sort,
        name_sort: IvySort,
        resolved_sort: IvySort,
    ) -> VisitorResult<IvySort, statements::Stmt> {
        let resolved = self.bindings.unify(&name_sort, &resolved_sort)?;

        Ok(ControlMut::Mutation(
            statements::Stmt::VarDecl(Binding::from(
                name.clone(),
                Sort::Resolved(resolved.clone()),
            )),
            resolved,
        ))
    }

    // Expressions

    fn finish_app(
        &mut self,
        _ast: &mut expressions::AppExpr,
        fsort: IvySort,
        argsorts: Vec<IvySort>,
    ) -> VisitorResult<IvySort, Expr> {
        let retsort = self.bindings.new_sortvar();

        // XXX: This is hacky.
        let dummy_argnames = (0..argsorts.len())
            .map(|i| format!("arg{i}"))
            .collect::<Vec<_>>();
        let expected_sort =
            IvySort::action_sort(dummy_argnames, argsorts, sorts::ActionRet::Unknown);
        let unified = self.bindings.unify(&fsort, &expected_sort);
        match unified {
            Ok(IvySort::Action(_argnames, _argsorts, action_ret)) => match action_ret {
                sorts::ActionRet::Unknown => Ok(ControlMut::Produce(retsort)),
                sorts::ActionRet::Unit => Ok(ControlMut::Produce(IvySort::Unit)),
                sorts::ActionRet::Named(binding) => Ok(ControlMut::Produce(binding.decl)),
            },
            Ok(IvySort::Relation(_)) => Ok(ControlMut::Produce(IvySort::Bool)),
            Ok(unified) => bail!(TypeError::InvalidApplication(unified)),
            Err(e) => bail!(e),
        }
    }

    fn finish_binop(
        &mut self,
        ast: &mut expressions::BinOp,
        lhs_sort: IvySort,
        _op_ret: IvySort,
        rhs_sort: IvySort,
    ) -> VisitorResult<IvySort, Expr> {
        match ast.op {
            // Boolean operators
            expressions::Verb::Iff
            | expressions::Verb::Or
            | expressions::Verb::And
            | expressions::Verb::Not
            | expressions::Verb::Arrow => {
                if self
                    .bindings
                    .unify(&lhs_sort, &IvySort::Bool)
                    .and(self.bindings.unify(&IvySort::Bool, &rhs_sort))
                    .is_err()
                {
                    bail!(TypeError::UnificationError(lhs_sort, rhs_sort))
                } else {
                    Ok(ControlMut::Produce(IvySort::Bool))
                }
            }

            // Equality and comparison
            expressions::Verb::Lt
            | expressions::Verb::Le
            | expressions::Verb::Gt
            | expressions::Verb::Ge => {
                if self
                    .bindings
                    .unify(&lhs_sort, &IvySort::Number)
                    .and(self.bindings.unify(&IvySort::Number, &rhs_sort))
                    .is_err()
                {
                    bail!(TypeError::UnificationError(lhs_sort, rhs_sort))
                } else {
                    Ok(ControlMut::Produce(IvySort::Bool))
                }
            }

            expressions::Verb::Equals | expressions::Verb::Notequals => {
                if self.bindings.unify(&lhs_sort, &rhs_sort).is_err() {
                    bail!(TypeError::UnificationError(lhs_sort, rhs_sort))
                } else {
                    Ok(ControlMut::Produce(IvySort::Bool))
                }
            }

            // Numeric operators
            expressions::Verb::Plus
            | expressions::Verb::Minus
            | expressions::Verb::Times
            | expressions::Verb::Div => {
                if self
                    .bindings
                    .unify(&lhs_sort, &IvySort::Number)
                    .and(self.bindings.unify(&IvySort::Number, &rhs_sort))
                    .is_err()
                {
                    bail!(TypeError::UnificationError(lhs_sort, rhs_sort))
                } else {
                    Ok(ControlMut::Produce(IvySort::Number))
                }
            }

            // Field access
            expressions::Verb::Dot => {
                // Man, I am not sure.  This SHOULD have been typechecked when we visit the field
                // access, but I should confirm that this is actually the case.
                Ok(ControlMut::Produce(rhs_sort))
            }
        }
    }

    fn begin_field_access(
        &mut self,
        lhs: &mut Expr,
        rhs: &mut expressions::Symbol,
    ) -> VisitorResult<IvySort, Expr> {
        let lhs_sort = lhs.visit(self)?.modifying(lhs)?;

        // Note that beecause the rhs's symbol is not in our context, we can't
        // visit it without getting an unbound identifier, so simply resolve its
        // sort.
        let annotated_rhs_sort = self.sort(&mut rhs.sort)?.modifying(&mut rhs.sort)?;

        let mut is_common = false;

        // XXX: awkward amounts of cloning in here.

        // First begin by confirming that the LHS is something we can get a field out of.
        // If it is, get the field's type.
        let rhs_sort = match &lhs_sort {
            IvySort::Module(module) => {
                Ok::<Option<IvySort>, TypeError>(module.fields.get(&rhs.id).cloned())
            }
            IvySort::Object(proc) => {
                let s = proc.fields.get(&rhs.id);
                is_common = s.is_none();
                Ok(s.cloned())
            }
            IvySort::SortVar(_) => Ok(Some(self.bindings.new_sortvar())),
            sort => bail!(TypeError::NotARecord(sort.clone())),
        }?
        // Next, if the RHS expression has a known type, ensure it doesn't contradict
        // what we figured out from the field access.
        .map(|s| self.bindings.unify(&annotated_rhs_sort, &s))
        .transpose()?
        // A slightly hacky thing that should probably live elsewhere: if the
        // rhs is a non-common action, and the first argument is `this`, we need
        // to curry it.  (TODO: might be that the common aspect is not a requirement
        // but I need to understand why;  See https://github.com/dijkstracula/irving/issues/35 .)
        .map(|s| match s {
            IvySort::Action(argnames, ActionArgs::List(argsorts), ret) if !is_common => {
                let first_arg = argsorts.get(0).map(|s| self.bindings.resolve(s));
                if first_arg == Some(&IvySort::This) {
                    let remaining_argnames = argnames.into_iter().skip(1).collect::<Vec<_>>();
                    let remaining_argsorts =
                        argsorts.clone().into_iter().skip(1).collect::<Vec<_>>();

                    Ok(IvySort::action_sort(
                        remaining_argnames,
                        remaining_argsorts,
                        ret,
                    ))
                } else {
                    Ok::<_, TypeError>(IvySort::action_sort(argnames, argsorts, ret))
                }
            }
            _ => Ok(s),
        })
        .transpose()?;
        match rhs_sort {
            Some(sort) => {
                rhs.sort = Sort::Resolved(sort.clone());
                Ok(ControlMut::SkipSiblings(sort))
            }
            None => bail!(TypeError::MissingRecordField(lhs_sort, rhs.id.clone())),
        }
    }

    fn finish_unary_op(
        &mut self,
        op: &mut expressions::Verb,
        _rhs: &mut Expr,
        rhs_sort: IvySort,
    ) -> VisitorResult<IvySort, Expr> {
        match op {
            expressions::Verb::Not => Ok(ControlMut::Produce(
                self.bindings.unify(&IvySort::Bool, &rhs_sort)?,
            )),
            expressions::Verb::Minus => Ok(ControlMut::Produce(
                self.bindings.unify(&IvySort::Number, &rhs_sort)?,
            )),
            _ => unimplemented!(),
        }
    }

    // Formulas

    fn begin_forall(&mut self, _ast: &mut logic::Forall) -> VisitorResult<IvySort, logic::Fmla> {
        self.bindings.push_scope();
        Ok(ControlMut::Produce(IvySort::Unit))
    }
    fn finish_forall(
        &mut self,
        _ast: &mut logic::Forall,
        _vars: Vec<IvySort>,
        _fmla: IvySort,
    ) -> VisitorResult<IvySort, logic::Fmla> {
        self.bindings.pop_scope();
        Ok(ControlMut::Produce(IvySort::Unit))
    }

    // actions and decls

    fn begin_action_decl(
        &mut self,
        name: &mut Token,
        _ast: &mut declarations::ActionDecl,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        // Bind the name to _something_; we'll unify this value with its resolved sort when finishing the visit.
        let v = self.bindings.new_sortvar();
        self.bindings.append(name.clone(), v)?;
        self.bindings.push_scope();

        Ok(ControlMut::Produce(IvySort::Unit))
    }
    fn finish_action_decl(
        &mut self,
        name: &mut Token,
        ast: &mut declarations::ActionDecl,
        name_sort: IvySort,
        param_sorts: Vec<IvySort>,
        ret_sort: Option<Binding<IvySort>>,
        _body: Option<Vec<IvySort>>,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let mut locals = ast.params.iter().map(|p| p.id.clone()).collect::<Vec<_>>();
        if let Some(ret) = &ast.ret {
            locals.push(ret.id.clone())
        }
        self.action_locals.insert(name.clone(), locals);

        let ret_sort = match ret_sort {
            None => sorts::ActionRet::Unit,
            Some(binding) => sorts::ActionRet::Named(Box::new(binding)),
        };

        let param_names = ast
            .params
            .iter()
            .map(|sym| sym.id.clone())
            .collect::<Vec<_>>();
        let actsort = IvySort::action_sort(param_names, param_sorts, ret_sort);
        let unified = self.bindings.unify(&name_sort, &actsort)?;

        self.bindings.pop_scope();

        Ok(ControlMut::Produce(unified))
    }

    fn begin_after_decl(
        &mut self,
        ast: &mut declarations::ActionMixinDecl,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        // XXX: this feels like a hack for something, but I've forgotten for what.
        if let Some(sym) = ast.name.first() {
            self.bindings.push_scope();

            if let Some(locals) = self.action_locals.get(sym) {
                for local in locals {
                    let s = self.bindings.new_sortvar();
                    self.bindings.append(local.clone(), s)?;
                }
            }
        } else {
            todo!()
        }

        Ok(ControlMut::Produce(IvySort::Unit))
    }

    fn finish_after_decl(
        &mut self,
        ast: &mut declarations::ActionMixinDecl,
        action_sort: IvySort,
        after_params_sort: Option<Vec<IvySort>>,
        after_ret_sort: Option<IvySort>,
        _after_body_sort: Vec<IvySort>,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        self.bindings.pop_scope();
        let (new_ast, sort) =
            self.resolve_mixin(ast, action_sort, after_params_sort, after_ret_sort)?;
        Ok(ControlMut::Mutation(
            declarations::Decl::AfterAction(new_ast),
            sort,
        ))
    }

    fn begin_alias_decl(
        &mut self,
        sym: &mut Token,
        _s: &mut Sort,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let v = self.bindings.new_sortvar();
        self.bindings.append(sym.clone(), v.clone())?;
        Ok(ControlMut::Produce(v))
    }
    fn finish_alias_decl(
        &mut self,
        sym: &mut Token,
        _s: &mut Sort,
        sym_sort: IvySort,
        expr_sort: IvySort,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let unified = self.bindings.unify(&sym_sort, &expr_sort)?;
        Ok(ControlMut::Mutation(
            declarations::Decl::Alias(Binding::from(sym.clone(), Sort::Resolved(unified.clone()))),
            unified,
        ))
    }

    fn begin_before_decl(
        &mut self,
        ast: &mut declarations::ActionMixinDecl,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        if let Some(sym) = ast.name.first() {
            self.bindings.push_scope();

            if let Some(locals) = self.action_locals.get(sym) {
                for local in locals {
                    let s = self.bindings.new_sortvar();
                    self.bindings.append(local.clone(), s)?;
                }
            }
        } else {
            todo!()
        }

        Ok(ControlMut::Produce(IvySort::Unit))
    }
    fn finish_before_decl(
        &mut self,
        ast: &mut declarations::ActionMixinDecl,
        action_sort: IvySort,
        params_sort: Option<Vec<IvySort>>,
        _body_sorts: Vec<IvySort>,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        self.bindings.pop_scope();
        let (new_ast, sort) = self.resolve_mixin(ast, action_sort, params_sort, None)?;
        Ok(ControlMut::Mutation(
            declarations::Decl::BeforeAction(new_ast),
            sort,
        ))
    }

    fn finish_ensure(
        &mut self,
        _ast: &mut actions::EnsureAction,
        pred_sort: IvySort,
    ) -> VisitorResult<IvySort, Action> {
        self.bindings.unify(&IvySort::Bool, &pred_sort)?;
        Ok(ControlMut::Produce(IvySort::Unit))
    }

    fn finish_requires(
        &mut self,
        _ast: &mut actions::RequiresAction,
        pred_sort: IvySort,
    ) -> VisitorResult<IvySort, Action> {
        self.bindings.unify(&IvySort::Bool, &pred_sort)?;
        Ok(ControlMut::Produce(IvySort::Unit))
    }

    fn begin_function_decl(
        &mut self,
        name: &mut Token,
        _ast: &mut declarations::FunctionDecl,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let v = self.bindings.new_sortvar();
        self.bindings.append(name.clone(), v.clone())?;
        self.bindings.push_scope();
        Ok(ControlMut::Produce(v))
    }

    fn finish_function_decl(
        &mut self,
        _name: &mut Token,
        ast: &mut declarations::FunctionDecl,
        name_sort: IvySort,
        param_sorts: Vec<IvySort>,
        ret_sort: IvySort,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let param_names = ast
            .params
            .iter()
            .map(|sym| sym.id.clone())
            .collect::<Vec<_>>();

        let fnsort = IvySort::action_sort(
            param_names,
            param_sorts,
            sorts::ActionRet::named("TODO", ret_sort),
        );
        let unifed = self.bindings.unify(&name_sort, &fnsort)?;
        self.bindings.pop_scope();
        Ok(ControlMut::Produce(unifed))
    }

    fn begin_implement_decl(
        &mut self,
        ast: &mut declarations::ActionMixinDecl,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        if let Some(sym) = ast.name.first() {
            self.bindings.push_scope();

            if let Some(locals) = self.action_locals.get(sym) {
                for local in locals {
                    let s = self.bindings.new_sortvar();
                    self.bindings.append(local.clone(), s)?;
                }
            }
        } else {
            todo!()
        }

        Ok(ControlMut::Produce(IvySort::Unit))
    }
    fn finish_implement_decl(
        &mut self,
        ast: &mut declarations::ActionMixinDecl,
        action_sort: IvySort,
        params_sort: Option<Vec<IvySort>>,
        ret_sort: Option<Binding<IvySort>>,
        _body: Vec<IvySort>,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        self.bindings.pop_scope();
        let (new_ast, sort) =
            self.resolve_mixin(ast, action_sort, params_sort, ret_sort.map(|b| b.decl))?;
        Ok(ControlMut::Mutation(
            declarations::Decl::Implement(new_ast),
            sort,
        ))
    }

    fn begin_import_decl(
        &mut self,
        ast: &mut declarations::ImportDecl,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let v = self.bindings.new_sortvar();
        self.bindings.append(ast.name.clone(), v.clone())?;

        Ok(ControlMut::Produce(v))
    }
    fn finish_import_decl(
        &mut self,
        _ast: &mut declarations::ImportDecl,
        decl_sortvar: IvySort,
        param_sorts: Vec<IvySort>,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        // TODO
        let relsort = IvySort::Action(
            vec![],
            ActionArgs::List(param_sorts),
            sorts::ActionRet::Unit,
        );
        let unifed = self.bindings.unify(&decl_sortvar, &relsort)?;
        Ok(ControlMut::Produce(unifed))
    }

    fn finish_interpret_decl(
        &mut self,
        name: &mut Token,
        _sort: &mut Sort,
        n: IvySort,
        s: IvySort,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let unified = self.bindings.unify(&n, &s)?;
        Ok(ControlMut::Mutation(
            declarations::Decl::Interpret(declarations::InterpretDecl {
                name: name.clone(),
                sort: Sort::Resolved(unified.clone()),
            }),
            unified,
        ))
    }

    fn begin_module_decl(
        &mut self,
        name: &mut Token,
        ast: &mut declarations::ModuleDecl,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let v = self.bindings.new_sortvar();
        self.bindings.append(name.clone(), v.clone())?;

        self.bindings.push_scope();
        self.bindings
            .append("init".into(), Module::init_action_sort())?;

        // Note: we have to pull the sort arguments into scope explicitly
        // unlike action decls since the argument list AST isn't a Vec<Param>.
        for sortarg in &ast.sortsyms {
            let s = self.bindings.new_sortvar();
            self.bindings.append(sortarg.clone(), s)?;
        }

        // TODO: possibly this could be its own pass.
        //        for decl in &ast.body {
        //           if decl.name_for_binding().is_none() {
        //              bail!(TypeError::NonBindingDecl(decl.clone()));
        //         }
        //    }
        Ok(ControlMut::Produce(v))
    }
    fn finish_module_decl(
        &mut self,
        name: &mut Token,
        ast: &mut declarations::ModuleDecl,
        mod_sort: IvySort,
        param_sorts: Vec<IvySort>,
        field_sorts: Vec<IvySort>,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let args = ast
            .sortsyms
            .iter()
            .zip(param_sorts.iter())
            .map(|(name, sort)| (name.clone(), self.bindings.resolve(sort).clone()))
            .collect::<Vec<_>>();

        // A module's fields are all the declarations that bind a name.
        let mut fields = ast
            .body
            .iter()
            .zip(field_sorts.iter())
            .filter_map(|(decl, sort)| {
                decl.name_for_binding()
                    .map(|n| (n.into(), self.bindings.resolve(sort).clone()))
            })
            .collect::<BTreeMap<_, _>>();
        // Every module has an implicit "init" action.
        fields.insert("init".into(), Module::init_action_sort());

        // A module's declaration list may also include other declarations that
        // do not bind a new name - in particular, `after` and `before`, which
        // refer to (potentially non-local) identifiers defined elsewhere.  We
        // will have to actually mix these declarations in with their
        // definitions in a later pass; for now, just confirm that their
        // signatures are consistent with each other.
        let _ = ast
            .body
            .iter()
            .zip(field_sorts.iter())
            .filter_map(|(decl, curr_sort)| match decl {
                declarations::Decl::AfterAction(ActionMixinDecl { name, .. })
                | declarations::Decl::BeforeAction(ActionMixinDecl { name, .. })
                | declarations::Decl::Implement(ActionMixinDecl { name, .. }) => {
                    Some((name, curr_sort))
                }
                _ => None,
            })
            .map(|(name, curr_sort)| {
                // XXX: Can I avoid this clone, despite unify() requring a mutable
                // reference to `self.bindings`?
                let prev_sort = self.bindings.lookup_ident(name)?.clone();
                self.bindings.unify(&prev_sort, curr_sort)
            })
            .collect::<Result<Vec<_>, _>>()?;

        let module = IvySort::Module(Module {
            name: name.clone(),
            args,
            fields,
        });
        let unifed = self.bindings.unify(&mod_sort, &module)?;

        self.bindings.pop_scope();

        Ok(ControlMut::Produce(unifed))
    }

    fn begin_object_decl(
        &mut self,
        name: &mut Token,
        _ast: &mut declarations::ObjectDecl,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let v = self.bindings.new_sortvar();
        self.bindings.append(name.clone(), v.clone())?;

        // Don't create a new scope if we're at the special top-level declaration.
        // This is needed for the typechecker visiting multiple Progs and expecting
        // earlier declarations to be in scope.
        if name != "top" {
            self.bindings.push_scope();
        }

        self.bindings
            .append("init".into(), Module::init_action_sort())?;

        Ok(ControlMut::Produce(v))
    }

    fn finish_object_decl(
        &mut self,
        name: &mut Token,
        ast: &mut declarations::ObjectDecl,
        decl_sort: IvySort,
        param_sorts: Vec<IvySort>,
        body_sorts: Vec<IvySort>,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let args = ast
            .params
            .iter()
            .zip(param_sorts.iter())
            .map(|(name, sort)| (name.id.clone(), self.bindings.resolve(sort).clone()))
            .collect::<BTreeMap<_, _>>();

        let mut fields = ast
            .body
            .iter()
            .zip(body_sorts.iter())
            .filter_map(|(decl, sort)| {
                decl.name_for_binding()
                    .map(|n| (n.to_owned(), self.bindings.resolve(sort).clone()))
            })
            .collect::<BTreeMap<_, _>>();
        fields.insert("init".into(), Module::init_action_sort());

        let proc = IvySort::Object(Object { args, fields });
        let unified = self.bindings.unify(&decl_sort, &proc)?;

        // Don't create a new scope if we're at the special top-level declaration.
        // This is needed for the typechecker visiting multiple Progs and expecting
        // earlier declarations to be in scope.
        if name != "top" {
            self.bindings.push_scope();
        }
        Ok(ControlMut::Produce(unified))
    }

    fn begin_relation(
        &mut self,
        name: &mut Token,
        _ast: &mut declarations::Relation,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        // Bind the name to _something_; we'll unify this value with a function sort when finishing the visit.
        let v = self.bindings.new_sortvar();
        self.bindings.append(name.clone(), v.clone())?;
        self.bindings.push_scope();
        Ok(ControlMut::Produce(v))
    }
    fn finish_relation(
        &mut self,
        _name: &mut Token,
        _ast: &mut declarations::Relation,
        n: IvySort,
        paramsorts: Vec<IvySort>,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let relsort = IvySort::Relation(paramsorts);
        let unifed = self.bindings.unify(&n, &relsort)?;
        self.bindings.pop_scope();
        Ok(ControlMut::Produce(unifed))
    }

    fn begin_instance_decl(
        &mut self,
        name: &mut Token,
        _ast: &mut declarations::InstanceDecl,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let v = self.bindings.new_sortvar();
        self.bindings.append(name.clone(), v)?;
        Ok(ControlMut::Produce(IvySort::Unit))
    }
    fn finish_instance_decl(
        &mut self,
        name: &mut Token,
        _ast: &mut declarations::InstanceDecl,
        decl_sort: IvySort,
        module_sort: IvySort,
        mod_args_sorts: Vec<IvySort>,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        if let IvySort::Module(module) = module_sort {
            if !mod_args_sorts.is_empty() {
                // Will have to monomorphize with the module instantiation pass.
                //println!("Uh oh: {:?} {:?}", name, module);
                //for (i, x) in self.bindings.ctx.iter().enumerate() {
                //    println!("ctx[{i}]: {x:?}");
                // }

                for ((_name, s1), s2) in module.args.iter().zip(mod_args_sorts.iter()) {
                    self.bindings.unify(s1, s2)?;
                }

                let monomorphized = module_instantiation::instantiate(module, mod_args_sorts)?;
                let unified = self.bindings.unify(&decl_sort, &monomorphized)?;
                //println!("Yay? {name} {:?}", unified);

                if let IvySort::Module(Module { .. }) = unified {
                    return Ok(ControlMut::Produce(unified));
                }
                unreachable!()
            } else {
                let modsort = IvySort::Module(Module {
                    name: name.clone(),
                    args: vec![],
                    fields: module.fields,
                });
                let unifed = self.bindings.unify(&decl_sort, &modsort)?;
                Ok(ControlMut::Produce(unifed))
            }
        } else {
            bail!(TypeError::NotInstanceable(module_sort.clone()))
        }
    }

    fn begin_typedecl(
        &mut self,
        name: &mut Token,
        sort: &mut Sort,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let resolved = match sort {
            Sort::ToBeInferred => self.bindings.new_sortvar(),
            Sort::Annotated(_) => unreachable!(),
            Sort::Resolved(sort) => sort.clone(),
        };
        self.bindings.append(name.clone(), resolved.clone())?;

        let binding = Binding::from(name.clone(), Sort::Resolved(resolved.clone()));
        Ok(ControlMut::Mutation(
            declarations::Decl::Type(binding),
            resolved,
        ))
    }

    fn begin_vardecl(
        &mut self,
        name: &mut Token,
        _ast: &mut Sort,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        // Bind the name to _something_; we'll unify this value with its resolved sort when finishing the visit.
        let v = self.bindings.new_sortvar();
        self.bindings.append(name.clone(), v)?;
        Ok(ControlMut::Produce(IvySort::Unit))
    }
    fn finish_vardecl(
        &mut self,
        name: &mut Token,
        _ast: &mut Sort,
        name_sort: IvySort,
        resolved_sort: IvySort,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let resolved = self.bindings.unify(&name_sort, &resolved_sort)?;

        Ok(ControlMut::Mutation(
            declarations::Decl::Var(Binding::from(
                name.clone(),
                Sort::Resolved(resolved.clone()),
            )),
            resolved,
        ))
    }
}
