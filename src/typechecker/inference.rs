use std::collections::BTreeMap;

use super::{
    sorts::{self, ActionArgs, Class, IvySort, Object},
    unifier::BindingResolver,
    InferenceResult,
};
use crate::{
    ast::{
        actions::{self, Action},
        declarations::{self, ActionMixinDecl, Binding},
        expressions::{self, Expr, Sort, Symbol, Token},
        logic,
        span::Span,
        statements,
    },
    passes::module_instantiation,
    typechecker::{sorts::Module, TypeError},
    visitor::{ast::Visitable, ast::Visitor, control::ControlMut},
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
    ) -> Result<(ActionMixinDecl, IvySort), TypeError> {
        let action_args = match &action_sort {
            IvySort::Action(args, _, _, _) => args,
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
                    return Err(TypeError::LenMismatch {
                        expected: action_args.len(),
                        actual: mixin_args.len(),
                    });
                } else {
                    mixin_args
                        .iter()
                        .map(|sym| sym.name.clone())
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
                sorts::ActionRet::Named(Box::new(Binding::from("TODO", ret, Span::Todo))),
                sorts::ActionKind::Instance, // TODO: can we have a common mixin?
            ),
            (Some(params), Some(ret)) => IvySort::Action(
                action_args,
                ActionArgs::List(params),
                sorts::ActionRet::Named(Box::new(Binding::from("TODO", ret, Span::Todo))),
                sorts::ActionKind::Instance, // TODO: can we have a common mixin?
            ),
        };

        let unified = self
            .bindings
            .unify(&action_sort, &mixin_sort)
            .map_err(|e| e.to_typeerror(&Span::Todo))?;

        // Lastly, we want to propagate what we've learned about the argument
        // list for later passes.
        let mixed_params = match &unified {
            IvySort::Action(args, ActionArgs::List(sorts), _, _) => args
                .iter()
                .zip(sorts.iter())
                .map(|(name, sort)| Symbol::from(name, Sort::Resolved(sort.clone()), Span::Todo))
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

impl Visitor<IvySort, TypeError> for SortInferer {
    fn boolean(&mut self, _b: &mut bool) -> InferenceResult<bool> {
        Ok(ControlMut::Produce(IvySort::Bool))
    }

    fn identifier(&mut self, i: &mut expressions::Ident) -> InferenceResult<expressions::Ident> {
        // XXX: This is a hack for built-ins like `bool` that we shouldn't have to
        // special-case in this way.
        if i.len() == 1 {
            let s = i.get_mut(0).unwrap();
            Ok(ControlMut::Produce(s.visit(self)?.modifying(s)))
        } else {
            let resolved = self
                .bindings
                .lookup_ident(i)
                .map_err(|e| e.to_typeerror(&Span::Todo))?;
            Ok(ControlMut::Produce(resolved.clone()))
        }
    }

    fn number(&mut self, _span: &Span, _n: &mut i64) -> InferenceResult<i64> {
        Ok(ControlMut::Produce(IvySort::Number))
    }

    fn param(&mut self, p: &mut expressions::Symbol) -> InferenceResult<expressions::Symbol> {
        match &mut p.decl {
            Sort::ToBeInferred => {
                let sortvar = self.bindings.new_sortvar();
                self.bindings
                    .append(p.name.clone(), sortvar.clone())
                    .map_err(|e| e.to_typeerror(&Span::Todo))?;
                Ok(ControlMut::Produce(sortvar))
            }
            Sort::Annotated(id) => {
                let resolved = id.visit(self)?.modifying(id);
                // Note that because a parameter binds a new name, we add it
                // to the bindings here.  We do not do the same for sort!()
                self.bindings
                    .append(p.name.clone(), resolved.clone())
                    .map_err(|e| e.to_typeerror(&Span::Todo))?;
                Ok(ControlMut::Mutation(
                    expressions::Symbol::from(
                        p.name.clone(),
                        Sort::Resolved(resolved.clone()),
                        Span::Todo,
                    ),
                    resolved,
                ))
            }
            Sort::Resolved(ivysort) => {
                self.bindings
                    .append(p.name.clone(), ivysort.clone())
                    .map_err(|e| e.to_typeerror(&Span::Todo))?;
                Ok(ControlMut::Produce(ivysort.clone()))
            }
        }
    }

    fn sort(&mut self, s: &mut Sort) -> InferenceResult<Sort> {
        let ctrl = match s {
            Sort::ToBeInferred => {
                let s = self.bindings.new_sortvar();
                ControlMut::Mutation(Sort::Resolved(s.clone()), s)
            }
            Sort::Annotated(ident) => {
                let resolved = self.identifier(ident)?.modifying(ident);
                ControlMut::Mutation(Sort::Resolved(resolved.clone()), resolved)
            }
            Sort::Resolved(ivysort) => ControlMut::Produce(self.bindings.resolve(ivysort).clone()),
        };
        Ok(ctrl)
    }

    fn symbol(
        &mut self,
        span: &Span,
        p: &mut expressions::Symbol,
    ) -> InferenceResult<expressions::Symbol> {
        // XXX: why don't we call self.sort(p.decl)?
        let sort = match &mut p.decl {
            Sort::ToBeInferred => match self.bindings.lookup_sym(&p.name) {
                None => {
                    return Err(TypeError::Spanned {
                        span: span.clone(),
                        inner: Box::new(TypeError::UnboundVariable(p.name.clone())),
                    })
                }
                Some(s) => s.clone(),
            },
            Sort::Annotated(ident) => self.identifier(ident)?.modifying(ident),
            Sort::Resolved(ivysort) => ivysort.clone(),
        };

        let resolved = self.bindings.resolve(&sort);
        if let Sort::Resolved(_) = p.decl {
            Ok(ControlMut::Produce(sort))
        } else {
            Ok(ControlMut::Mutation(
                Binding::from(
                    p.name.clone(),
                    Sort::Resolved(resolved.clone()),
                    span.clone(),
                ),
                resolved.clone(),
            ))
        }
    }

    fn token(&mut self, sym: &mut expressions::Token) -> InferenceResult<expressions::Token> {
        match sym.as_str() {
            "bool" => Ok(ControlMut::Produce(IvySort::Bool)),
            "unbounded_sequence" => Ok(ControlMut::Produce(IvySort::Number)),
            //"this" => Ok(ControlMut::Produce(IvySort::This)),
            // TODO: and of course other builtins.
            _ => match self.bindings.lookup_sym(sym) {
                Some(sort) => Ok(ControlMut::Produce(sort.clone())),
                None => Err(TypeError::UnboundVariable(sym.clone())),
            },
        }
    }

    fn this(&mut self) -> InferenceResult<Expr> {
        match self.bindings.lookup_sym("this") {
            None => Err(TypeError::UnboundVariable("this".into())),
            Some(t) => Ok(ControlMut::Produce(t.clone())),
        }
    }

    // Actions

    fn finish_assign(
        &mut self,
        span: &Span,
        ast: &mut actions::AssignAction,
        lhs_sort: IvySort,
        rhs_sort: IvySort,
    ) -> InferenceResult<Action> {
        let unified = self
            .bindings
            .unify(&lhs_sort, &rhs_sort)
            .map_err(|e| e.to_typeerror(span))?;
        Ok(ControlMut::Mutation(
            Action::Assign {
                span: span.clone(),
                action: actions::AssignAction {
                    lhs: ast.lhs.clone(),
                    lhs_sort: Sort::Resolved(unified),
                    rhs: ast.rhs.clone(),
                },
            },
            IvySort::Unit,
        ))
    }

    // Statements

    fn action_seq(&mut self, ast: &mut Vec<Action>) -> InferenceResult<statements::Stmt> {
        //XXX: kinda dumb, honestly.
        // https://github.com/dijkstracula/irving/issues/17
        let _ = ast.visit(self)?.modifying(ast);
        Ok(ControlMut::Produce(IvySort::Unit))
    }

    fn finish_if(
        &mut self,
        _ast: &mut statements::If,
        tst_t: IvySort,
        _then_t: Vec<IvySort>,
        _else_t: Option<Vec<IvySort>>,
    ) -> InferenceResult<statements::Stmt> {
        self.bindings
            .unify(&tst_t, &IvySort::Bool)
            .map_err(|e| e.to_typeerror(&Span::Todo))?;
        Ok(ControlMut::Produce(IvySort::Unit))
    }

    fn begin_local_vardecl(
        &mut self,
        name: &mut Token,
        sort: &mut Sort,
    ) -> InferenceResult<statements::Stmt> {
        // Bind the name to _something_; we'll unify this value with its resolved sort when finishing the visit.
        let sort = self.sort(sort)?.modifying(sort);
        self.bindings
            .append(name.clone(), sort)
            .map_err(|e| e.to_typeerror(&Span::Todo))?;
        Ok(ControlMut::Produce(IvySort::Unit))
    }
    fn finish_local_vardecl(
        &mut self,
        name: &mut Token,
        _ast: &mut Sort,
        name_sort: IvySort,
        resolved_sort: IvySort,
    ) -> InferenceResult<statements::Stmt> {
        let resolved = self
            .bindings
            .unify(&name_sort, &resolved_sort)
            .map_err(|e| e.to_typeerror(&Span::Todo))?;

        Ok(ControlMut::Mutation(
            statements::Stmt::VarDecl(Binding::from(
                name.clone(),
                Sort::Resolved(resolved.clone()),
                Span::Todo,
            )),
            resolved,
        ))
    }

    // Expressions

    fn finish_app(
        &mut self,
        span: &Span,
        ast: &mut expressions::AppExpr,
        fsort: IvySort,
        argsorts: Vec<IvySort>,
    ) -> InferenceResult<Expr> {
        // XXX: This is hacky.

        let dummy_argnames = (0..argsorts.len())
            .map(|i| format!("arg{i}"))
            .collect::<Vec<_>>();
        let expected_sort =
            IvySort::action_sort(dummy_argnames, argsorts, sorts::ActionRet::Unknown);
        let unified = self
            .bindings
            .unify(&fsort, &expected_sort)
            .map_err(|e| e.to_typeerror(span))?;

        ast.func_sort = Sort::Resolved(unified.clone());

        match unified {
            IvySort::Action(_argnames, _argsorts, action_ret, _) => match action_ret {
                sorts::ActionRet::Unknown => {
                    let retsort = self.bindings.new_sortvar();
                    Ok(ControlMut::Produce(retsort))
                }
                sorts::ActionRet::Unit => Ok(ControlMut::Produce(IvySort::Unit)),
                sorts::ActionRet::Named(binding) => Ok(ControlMut::Produce(binding.decl)),
            },
            IvySort::Relation(_) => Ok(ControlMut::Produce(IvySort::Bool)),
            IvySort::Object(obj) => Ok(ControlMut::Produce(IvySort::Object(obj))),
            _ => Err(TypeError::InvalidApplication),
        }
    }

    fn finish_binop(
        &mut self,
        ast: &mut expressions::BinOp,
        lhs_sort: IvySort,
        _op_ret: IvySort,
        rhs_sort: IvySort,
    ) -> InferenceResult<Expr> {
        match ast.op {
            // Boolean operators
            expressions::Verb::Iff
            | expressions::Verb::Or
            | expressions::Verb::And
            | expressions::Verb::Not => {
                self.bindings
                    .unify(&lhs_sort, &IvySort::Bool)
                    .map_err(|e| e.to_typeerror(ast.lhs.span()))?;
                self.bindings
                    .unify(&IvySort::Bool, &rhs_sort)
                    .map_err(|e| e.to_typeerror(ast.rhs.span()))?;
                Ok(ControlMut::Produce(IvySort::Bool))
            }

            // Equality and comparison
            expressions::Verb::Lt
            | expressions::Verb::Le
            | expressions::Verb::Gt
            | expressions::Verb::Ge => {
                self.bindings
                    .unify(&lhs_sort, &IvySort::Number)
                    .map_err(|e| e.to_typeerror(ast.lhs.span()))?;
                self.bindings
                    .unify(&IvySort::Number, &rhs_sort)
                    .map_err(|e| e.to_typeerror(ast.rhs.span()))?;
                Ok(ControlMut::Produce(IvySort::Bool))
            }

            expressions::Verb::Equals | expressions::Verb::Notequals => {
                self.bindings.unify(&lhs_sort, &rhs_sort).map_err(|e| {
                    let span = Span::merge(ast.lhs.span(), ast.rhs.span());
                    e.to_typeerror(&span)
                })?;
                Ok(ControlMut::Produce(IvySort::Bool))
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
                    Err(TypeError::unification_error(&lhs_sort, &rhs_sort))
                } else {
                    Ok(ControlMut::Produce(IvySort::Number))
                }
            }

            // Field access: This should already be handled - all Dots should be
            // transformed into an Fmla::FieldAccess.
            expressions::Verb::Dot => unreachable!(),

            // Implication: The grammar should restrict Arrows to formulae.
            expressions::Verb::Arrow => unreachable!(),
        }
    }

    fn begin_field_access(
        &mut self,
        lhs: &mut Expr,
        rhs: &mut expressions::Symbol,
    ) -> InferenceResult<Expr> {
        let lhs_sort = lhs.visit(self)?.modifying(lhs);

        // Note that beecause the rhs's symbol is not in our context, we can't
        // visit it without getting an unbound identifier, so simply resolve its
        // sort.
        let annotated_rhs_sort = self.sort(&mut rhs.decl)?.modifying(&mut rhs.decl);

        let mut is_common = false;

        // XXX: awkward amounts of cloning in here.

        // First begin by confirming that the LHS is something we can get a field out of.
        // If it is, get the field's type.
        let rhs_sort = match &lhs_sort {
            IvySort::Module(module) => {
                Ok::<Option<IvySort>, TypeError>(module.fields.get(&rhs.name).cloned())
            }
            IvySort::Object(proc) => {
                let s = proc.fields.get(&rhs.name);
                is_common = s.is_none();
                Ok(s.cloned())
            }
            IvySort::SortVar(id) => Ok(Some(IvySort::SortVar(*id))),
            sort => Err(TypeError::NotARecord(sort.desc())),
        }?
        // Next, if the RHS expression has a known type, ensure it doesn't contradict
        // what we figured out from the field access.
        .map(|s| self.bindings.unify(&annotated_rhs_sort, &s))
        .transpose()
        .map_err(|e| e.to_typeerror(lhs.span()))?
        // A slightly hacky thing that should probably live elsewhere: if the
        // rhs is a non-common action, and the first argument is `this`, we need
        // to curry it.  (TODO: might be that the common aspect is not a requirement
        // but I need to understand why;  See https://github.com/dijkstracula/irving/issues/35 .)
        .map(|s| match s {
            // TODO: remove is_common and use kind instead
            IvySort::Action(argnames, ActionArgs::List(argsorts), ret, _kind) if !is_common => {
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
                rhs.decl = Sort::Resolved(sort.clone());
                Ok(ControlMut::SkipSiblings(sort))
            }
            None => Err(TypeError::MissingRecordField(rhs.name.clone())),
        }
    }

    fn finish_unary_op(
        &mut self,
        op: &mut expressions::Verb,
        rhs: &mut Expr,
        rhs_sort: IvySort,
    ) -> InferenceResult<Expr> {
        match op {
            expressions::Verb::Not => Ok(ControlMut::Produce(
                self.bindings
                    .unify(&IvySort::Bool, &rhs_sort)
                    .map_err(|e| e.to_typeerror(rhs.span()))?,
            )),
            expressions::Verb::Minus => Ok(ControlMut::Produce(
                self.bindings
                    .unify(&IvySort::Number, &rhs_sort)
                    .map_err(|e| e.to_typeerror(rhs.span()))?,
            )),
            _ => unimplemented!(),
        }
    }

    // Formulas

    fn begin_exists(&mut self, _ast: &mut logic::Exists) -> InferenceResult<logic::Fmla> {
        self.bindings.push_scope();
        Ok(ControlMut::Produce(IvySort::Bool))
    }
    fn finish_exists(
        &mut self,
        _ast: &mut logic::Exists,
        _vars: Vec<IvySort>,
        _fmla: IvySort,
    ) -> InferenceResult<logic::Fmla> {
        self.bindings.pop_scope();
        Ok(ControlMut::Produce(IvySort::Bool))
    }

    fn begin_forall(&mut self, _ast: &mut logic::Forall) -> InferenceResult<logic::Fmla> {
        self.bindings.push_scope();
        Ok(ControlMut::Produce(IvySort::Bool))
    }
    fn finish_forall(
        &mut self,
        ast: &mut logic::Forall,
        vars: Vec<IvySort>,
        _fmla: IvySort,
    ) -> InferenceResult<logic::Fmla> {
        for (Binding { name, decl, span }, sort) in ast.vars.iter_mut().zip(vars.iter()) {
            let bound_sort = self.bindings.lookup_sym(name).unwrap().clone();
            let unified = self
                .bindings
                .unify(&bound_sort, sort)
                .map_err(|e| e.to_typeerror(&span))?;
            *decl = Sort::Resolved(unified);
        }
        self.bindings.pop_scope();
        Ok(ControlMut::Produce(IvySort::Bool))
    }

    fn begin_logical_app(
        &mut self,
        ast: &mut logic::LogicApp,
    ) -> crate::visitor::VisitorResult<IvySort, TypeError, logic::Fmla> {
        self.bindings.push_scope();
        for arg in &mut ast.args {
            // A difference between a logical application and one with a program
            // term is that a wildcard logical symbol may not be in the context.
            if let logic::Fmla::LogicSymbol { sym, span } = arg {
                // XXX: I think it's fine to discard the Sort from the
                // logicsymbol here.
                let v = self.param(sym)?.modifying(sym);
                self.bindings
                    .append(sym.name.clone(), v)
                    .map_err(|e| e.to_typeerror(span))?;
            }
        }

        Ok(ControlMut::Produce(IvySort::Unit))
    }

    fn begin_assign_logical(
        &mut self,
        span: &Span,
        ast: &mut actions::AssignLogicalAction,
    ) -> crate::visitor::VisitorResult<IvySort, TypeError, Action> {
        self.bindings.push_scope();
        match &mut ast.lhs {
            logic::Fmla::App {
                app: logic::LogicApp { args, .. },
                ..
            } => {
                for arg in args {
                    if let logic::Fmla::LogicSymbol { sym, .. } = arg {
                        let s = self.param(sym).map_err(|e| e.rewrap(span))?.modifying(sym);
                        self.bindings
                            .append(sym.name.clone(), s)
                            .map_err(|e| e.to_typeerror(span))?;
                    }
                }
            }
            // TODO: is this true?
            _ => unreachable!(),
        }
        Ok(ControlMut::Produce(IvySort::Unit))
    }

    fn finish_assign_logical(
        &mut self,
        span: &Span,
        _ast: &mut actions::AssignLogicalAction,
        lhs_sort: IvySort,
        rhs_sort: IvySort,
    ) -> crate::visitor::VisitorResult<IvySort, TypeError, Action> {
        self.bindings.pop_scope();
        self.bindings
            .unify(&lhs_sort, &rhs_sort)
            .map_err(|e| e.to_typeerror(span))?;
        Ok(ControlMut::Produce(IvySort::Unit))
    }

    fn finish_logical_app(
        &mut self,
        _ast: &mut logic::LogicApp,
        func_sort: IvySort,
        arg_sorts: Vec<IvySort>,
    ) -> InferenceResult<logic::Fmla> {
        self.bindings.pop_scope();

        // XXX: This is hacky.
        let dummy_argnames = (0..arg_sorts.len())
            .map(|i| format!("logical_arg{i}"))
            .collect::<Vec<_>>();
        let expected_sort =
            IvySort::action_sort(dummy_argnames, arg_sorts, sorts::ActionRet::Unknown);
        let unified = self
            .bindings
            .unify(&func_sort, &expected_sort)
            .map_err(|e| e.to_typeerror(&Span::Todo))?;

        match unified {
            IvySort::Action(_argnames, _argsorts, action_ret, _) => match action_ret {
                sorts::ActionRet::Unknown => {
                    let retsort = self.bindings.new_sortvar();
                    Ok(ControlMut::Produce(retsort))
                }
                sorts::ActionRet::Unit => Ok(ControlMut::Produce(IvySort::Unit)),
                sorts::ActionRet::Named(binding) => Ok(ControlMut::Produce(binding.decl)),
            },
            IvySort::Relation(_) => Ok(ControlMut::Produce(IvySort::Bool)),
            _ => Err(TypeError::InvalidLogicApp),
        }
    }

    fn finish_logical_binop(
        &mut self,
        ast: &mut logic::LogicBinOp,
        lhs_sort: IvySort,
        _op_ret: IvySort,
        rhs_sort: IvySort,
    ) -> crate::visitor::VisitorResult<IvySort, TypeError, logic::Fmla> {
        match ast.op {
            // Boolean operators
            expressions::Verb::Iff
            | expressions::Verb::Or
            | expressions::Verb::And
            | expressions::Verb::Not
            | expressions::Verb::Arrow => {
                self.bindings
                    .unify(&lhs_sort, &IvySort::Bool)
                    .map_err(|e| e.to_typeerror(ast.lhs.span()))?;
                self.bindings
                    .unify(&IvySort::Bool, &rhs_sort)
                    .map_err(|e| e.to_typeerror(ast.rhs.span()))?;
                Ok(ControlMut::Produce(IvySort::Bool))
            }

            // Equality and comparison
            expressions::Verb::Lt
            | expressions::Verb::Le
            | expressions::Verb::Gt
            | expressions::Verb::Ge => {
                self.bindings
                    .unify(&lhs_sort, &IvySort::Number)
                    .map_err(|e| e.to_typeerror(ast.lhs.span()))?;
                self.bindings
                    .unify(&IvySort::Number, &rhs_sort)
                    .map_err(|e| e.to_typeerror(ast.rhs.span()))?;
                Ok(ControlMut::Produce(IvySort::Bool))
            }

            expressions::Verb::Equals | expressions::Verb::Notequals => {
                self.bindings.unify(&lhs_sort, &rhs_sort).map_err(|e| {
                    let span = Span::merge(ast.lhs.span(), ast.rhs.span());
                    e.to_typeerror(&span)
                })?;
                Ok(ControlMut::Produce(IvySort::Bool))
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
                    Err(TypeError::unification_error(&lhs_sort, &rhs_sort))
                } else {
                    Ok(ControlMut::Produce(IvySort::Number))
                }
            }

            // Field access
            expressions::Verb::Dot => {
                // This should already be handled - all Dots should be
                // transformed into an Fmla::FieldAccess.
                unreachable!()
            }
        }
    }

    fn begin_logical_field_access(
        &mut self,
        lhs: &mut logic::Fmla,
        rhs: &mut Symbol,
    ) -> InferenceResult<logic::Fmla> {
        let lhs_sort = lhs.visit(self)?.modifying(lhs);

        // Note that beecause the rhs's symbol is not in our context, we can't
        // visit it without getting an unbound identifier, so simply resolve its
        // sort.
        let annotated_rhs_sort = self.sort(&mut rhs.decl)?.modifying(&mut rhs.decl);

        let mut is_common = false;

        // XXX: awkward amounts of cloning in here.

        // First begin by confirming that the LHS is something we can get a field out of.
        // If it is, get the field's type.
        let rhs_sort = match &lhs_sort {
            IvySort::Module(module) => {
                Ok::<Option<IvySort>, TypeError>(module.fields.get(&rhs.name).cloned())
            }
            IvySort::Object(proc) => {
                let s = proc.fields.get(&rhs.name);
                is_common = s.is_none();
                Ok(s.cloned())
            }
            IvySort::SortVar(id) => Ok(Some(IvySort::SortVar(*id))),
            sort => Err(TypeError::NotARecord(sort.desc())),
        }?
        // Next, if the RHS expression has a known type, ensure it doesn't contradict
        // what we figured out from the field access.
        .map(|s| self.bindings.unify(&annotated_rhs_sort, &s))
        .transpose()
        .map_err(|e| e.to_typeerror(lhs.span()))?
        // A slightly hacky thing that should probably live elsewhere: if the
        // rhs is a non-common action, and the first argument is `this`, we need
        // to curry it.  (TODO: might be that the common aspect is not a requirement
        // but I need to understand why;  See https://github.com/dijkstracula/irving/issues/35 .)
        .map(|s| match s {
            // TODO: deprecrate is_common and use _kind instead
            IvySort::Action(argnames, ActionArgs::List(argsorts), ret, _kind) if !is_common => {
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
                rhs.decl = Sort::Resolved(sort.clone());
                Ok(ControlMut::SkipSiblings(sort))
            }
            None => Err(TypeError::MissingRecordField(rhs.name.clone())),
        }
    }

    fn finish_logical_unary_op(
        &mut self,
        op: &mut expressions::Verb,
        rhs: &mut logic::Fmla,
        rhs_sort: IvySort,
    ) -> crate::visitor::VisitorResult<IvySort, TypeError, logic::Fmla> {
        match op {
            expressions::Verb::Not => Ok(ControlMut::Produce(
                self.bindings
                    .unify(&IvySort::Bool, &rhs_sort)
                    .map_err(|e| e.to_typeerror(rhs.span()))?,
            )),
            expressions::Verb::Minus => Ok(ControlMut::Produce(
                self.bindings
                    .unify(&IvySort::Number, &rhs_sort)
                    .map_err(|e| e.to_typeerror(rhs.span()))?,
            )),
            _ => unimplemented!(),
        }
    }

    // actions and decls

    fn begin_action_decl(
        &mut self,
        span: &Span,
        name: &mut Token,
        _ast: &mut declarations::ActionDecl,
    ) -> InferenceResult<declarations::Decl> {
        // Bind the name to _something_; we'll unify this value with its resolved sort when finishing the visit.
        let v = self.bindings.new_sortvar();
        self.bindings
            .append(name.clone(), v)
            .map_err(|e| e.to_typeerror(span))?;
        self.bindings.push_scope();

        Ok(ControlMut::Produce(IvySort::Unit))
    }
    fn finish_action_decl(
        &mut self,
        span: &Span,

        name: &mut Token,
        ast: &mut declarations::ActionDecl,
        name_sort: IvySort,
        param_sorts: Vec<IvySort>,
        ret_sort: Option<Binding<IvySort>>,
        _body: Option<Vec<IvySort>>,
    ) -> InferenceResult<declarations::Decl> {
        let mut locals = ast
            .params
            .iter()
            .map(|p| p.name.clone())
            .collect::<Vec<_>>();
        if let Some(ret) = &ast.ret {
            locals.push(ret.name.clone())
        }
        self.action_locals.insert(name.clone(), locals);

        let ret_sort = match ret_sort {
            None => sorts::ActionRet::Unit,
            Some(binding) => sorts::ActionRet::Named(Box::new(binding)),
        };

        let param_names = ast
            .params
            .iter()
            .map(|sym| sym.name.clone())
            .collect::<Vec<_>>();
        let actsort = IvySort::action_sort(param_names, param_sorts, ret_sort);
        let unified = self
            .bindings
            .unify(&name_sort, &actsort)
            .map_err(|e| e.to_typeerror(span))?;

        self.bindings.pop_scope();

        Ok(ControlMut::Produce(unified))
    }

    fn begin_after_decl(
        &mut self,
        span: &Span,
        ast: &mut declarations::ActionMixinDecl,
    ) -> InferenceResult<declarations::Decl> {
        // XXX: this feels like a hack for something, but I've forgotten for what.
        if let Some(sym) = ast.name.first() {
            self.bindings.push_scope();

            if let Some(locals) = self.action_locals.get(sym) {
                for local in locals {
                    let s = self.bindings.new_sortvar();
                    self.bindings
                        .append(local.clone(), s)
                        .map_err(|e| e.to_typeerror(span))?;
                }
            }
        } else {
            todo!()
        }

        Ok(ControlMut::Produce(IvySort::Unit))
    }

    fn finish_after_decl(
        &mut self,
        span: &Span,
        ast: &mut declarations::ActionMixinDecl,
        action_sort: IvySort,
        after_params_sort: Option<Vec<IvySort>>,
        after_ret_sort: Option<IvySort>,
        _after_body_sort: Vec<IvySort>,
    ) -> InferenceResult<declarations::Decl> {
        self.bindings.pop_scope();
        let (new_ast, sort) =
            self.resolve_mixin(ast, action_sort, after_params_sort, after_ret_sort)?;
        Ok(ControlMut::Mutation(
            declarations::Decl::AfterAction {
                span: span.clone(),
                decl: new_ast,
            },
            sort,
        ))
    }

    fn begin_alias_decl(
        &mut self,
        sym: &mut Token,
        s: &mut Sort,
    ) -> InferenceResult<declarations::Decl> {
        let s = self.sort(s)?.modifying(s);
        self.bindings
            .append(sym.clone(), s.clone())
            .map_err(|e| e.to_typeerror(&Span::Todo))?;
        Ok(ControlMut::Produce(s))
    }
    fn finish_alias_decl(
        &mut self,
        sym: &mut Token,
        _s: &mut Sort,
        sym_sort: IvySort,
        expr_sort: IvySort,
    ) -> InferenceResult<declarations::Decl> {
        let unified = self
            .bindings
            .unify(&sym_sort, &expr_sort)
            .map_err(|e| e.to_typeerror(&Span::Todo))?;
        Ok(ControlMut::Mutation(
            declarations::Decl::Alias {
                decl: Binding::from(sym.clone(), Sort::Resolved(unified.clone()), Span::Todo),
            },
            unified,
        ))
    }

    fn begin_before_decl(
        &mut self,
        ast: &mut declarations::ActionMixinDecl,
    ) -> InferenceResult<declarations::Decl> {
        if let Some(sym) = ast.name.first() {
            self.bindings.push_scope();

            if let Some(locals) = self.action_locals.get(sym) {
                for local in locals {
                    let s = self.bindings.new_sortvar();
                    self.bindings
                        .append(local.clone(), s)
                        .map_err(|e| e.to_typeerror(&Span::Todo))?;
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
    ) -> InferenceResult<declarations::Decl> {
        self.bindings.pop_scope();
        let (decl, sort) = self.resolve_mixin(ast, action_sort, params_sort, None)?;

        // XXX: What's the right span here?  Should we stash a Span in ActionMixinDecl?
        Ok(ControlMut::Mutation(
            declarations::Decl::BeforeAction {
                span: Span::Optimized,
                decl,
            },
            sort,
        ))
    }

    fn begin_class_decl(
        &mut self,
        span: &Span,
        name: &mut Token,
        _ast: &mut declarations::ClassDecl,
    ) -> InferenceResult<declarations::Decl> {
        self.bindings.push_scope();

        let v = self.bindings.new_sortvar();
        self.bindings
            .append(name.clone(), v.clone())
            .map_err(|e| e.to_typeerror(span))?;
        Ok(ControlMut::Produce(v))
    }

    fn finish_class_decl(
        &mut self,
        span: &Span,
        _name: &mut Token,
        ast: &mut declarations::ClassDecl,
        name_sort: IvySort,
        parent_sort: Option<IvySort>,
        fields_sorts: Vec<IvySort>,
        actions_sorts: Vec<IvySort>,
    ) -> InferenceResult<declarations::Decl> {
        let mut fields = ast
            .fields
            .iter()
            .zip(fields_sorts.into_iter())
            .map(|(Binding { name, .. }, sort)| (name.clone(), sort))
            .collect::<BTreeMap<_, _>>();

        let mut actions = ast
            .actions
            .iter()
            .zip(actions_sorts.into_iter())
            .map(|(Binding { name, .. }, sort)| (name.clone(), sort))
            .collect::<BTreeMap<_, _>>();

        match &parent_sort {
            None => (),
            Some(IvySort::Class(parent)) => {
                // TODO: what happens on a slot redeclaration?
                fields.extend(parent.fields.clone().into_iter());
                actions.extend(parent.actions.clone().into_iter());
            }
            Some(other) => {
                return Err(TypeError::Spanned {
                    span: span.clone(),
                    inner: Box::new(TypeError::NonClassInheritance(other.clone())),
                });
            }
        }

        let cls = IvySort::Class(Class {
            parent: parent_sort.map(|parent| Box::new(parent)),
            actions: actions,
            fields: fields,
        });

        let unified = self
            .bindings
            .unify(&name_sort, &cls)
            .map_err(|e| e.to_typeerror(span))?;

        self.bindings.pop_scope();
        Ok(ControlMut::Produce(unified))
    }

    fn finish_ensure(
        &mut self,
        ast: &mut actions::EnsureAction,
        pred_sort: IvySort,
    ) -> InferenceResult<Action> {
        self.bindings
            .unify(&IvySort::Bool, &pred_sort)
            .map_err(|e| e.to_typeerror(ast.pred.span()))?;
        Ok(ControlMut::Produce(IvySort::Unit))
    }

    fn finish_requires(
        &mut self,
        ast: &mut actions::RequiresAction,
        pred_sort: IvySort,
    ) -> InferenceResult<Action> {
        self.bindings
            .unify(&IvySort::Bool, &pred_sort)
            .map_err(|e| e.to_typeerror(ast.pred.span()))?;
        Ok(ControlMut::Produce(IvySort::Unit))
    }

    fn begin_function_decl(
        &mut self,
        name: &mut Token,
        _ast: &mut declarations::FunctionDecl,
    ) -> InferenceResult<declarations::Decl> {
        let v = self.bindings.new_sortvar();
        self.bindings
            .append(name.clone(), v.clone())
            .map_err(|e| e.to_typeerror(&Span::Todo))?;
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
    ) -> InferenceResult<declarations::Decl> {
        let param_names = ast
            .params
            .iter()
            .map(|sym| sym.name.clone())
            .collect::<Vec<_>>();

        let fnsort = IvySort::action_sort(
            param_names,
            param_sorts,
            sorts::ActionRet::named("TODO", ret_sort),
        );
        let unified = self
            .bindings
            .unify(&name_sort, &fnsort)
            .map_err(|e| e.to_typeerror(&Span::Todo))?;
        self.bindings.pop_scope();
        Ok(ControlMut::Produce(unified))
    }

    fn begin_implement_decl(
        &mut self,
        ast: &mut declarations::ActionMixinDecl,
    ) -> InferenceResult<declarations::Decl> {
        if let Some(sym) = ast.name.first() {
            self.bindings.push_scope();

            if let Some(locals) = self.action_locals.get(sym) {
                for local in locals {
                    let s = self.bindings.new_sortvar();
                    self.bindings
                        .append(local.clone(), s)
                        .map_err(|e| e.to_typeerror(&Span::Todo))?;
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
    ) -> InferenceResult<declarations::Decl> {
        self.bindings.pop_scope();
        let (decl, sort) =
            self.resolve_mixin(ast, action_sort, params_sort, ret_sort.map(|b| b.decl))?;
        Ok(ControlMut::Mutation(
            declarations::Decl::Implement {
                span: Span::Optimized,
                decl,
            },
            sort,
        ))
    }

    fn begin_import_decl(
        &mut self,
        ast: &mut declarations::ImportDecl,
    ) -> InferenceResult<declarations::Decl> {
        let v = self.bindings.new_sortvar();
        self.bindings
            .append(ast.name.clone(), v.clone())
            .map_err(|e| e.to_typeerror(&Span::Todo))?;

        Ok(ControlMut::Produce(v))
    }
    fn finish_import_decl(
        &mut self,
        span: &Span,
        ast: &mut declarations::ImportDecl,
        decl_sort: IvySort,
        param_sorts: Vec<IvySort>,
    ) -> InferenceResult<declarations::Decl> {
        let param_names = ast
            .params
            .iter()
            .map(|sym| sym.name.clone())
            .collect::<Vec<_>>();
        let actsort = IvySort::action_sort(param_names, param_sorts, sorts::ActionRet::Unit);
        let unified = self
            .bindings
            .unify(&decl_sort, &actsort)
            .map_err(|e| e.to_typeerror(span))?;
        Ok(ControlMut::Produce(unified))
    }

    fn finish_interpret_decl(
        &mut self,
        name: &mut Token,
        _sort: &mut Sort,
        n: IvySort,
        s: IvySort,
    ) -> InferenceResult<declarations::Decl> {
        let unified = self
            .bindings
            .unify(&n, &s)
            .map_err(|e| e.to_typeerror(&Span::Todo))?;
        Ok(ControlMut::Mutation(
            declarations::Decl::Interpret {
                span: Span::Optimized,
                decl: declarations::InterpretDecl {
                    name: name.clone(),
                    sort: Sort::Resolved(unified.clone()),
                },
            },
            unified,
        ))
    }

    fn begin_module_decl(
        &mut self,
        name: &mut Token,
        ast: &mut declarations::ModuleDecl,
    ) -> InferenceResult<declarations::Decl> {
        let v = self.bindings.new_sortvar();
        self.bindings
            .append(name.clone(), v.clone())
            .map_err(|e| e.to_typeerror(&Span::Todo))?;

        self.bindings.push_scope();
        self.bindings
            .append("init".into(), Module::init_action_sort())
            .map_err(|e| e.to_typeerror(&Span::Todo))?;

        // Note: we have to pull the sort arguments into scope explicitly
        // unlike action decls since the argument list AST isn't a Vec<Param>.
        for sortarg in &ast.sortsyms {
            let s = self.bindings.new_sortvar();
            self.bindings
                .append(sortarg.clone(), s)
                .map_err(|e| e.to_typeerror(&Span::Todo))?;
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
    ) -> InferenceResult<declarations::Decl> {
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
                declarations::Decl::AfterAction {
                    decl: ActionMixinDecl { name, .. },
                    ..
                }
                | declarations::Decl::BeforeAction {
                    decl: ActionMixinDecl { name, .. },
                    ..
                }
                | declarations::Decl::Implement {
                    decl: ActionMixinDecl { name, .. },
                    ..
                } => Some((decl.span(), name, curr_sort)),
                _ => None,
            })
            .map(|(span, name, curr_sort)| {
                // XXX: Can I avoid this clone, despite unify() requring a mutable
                // reference to `self.bindings`?
                let prev_sort = self
                    .bindings
                    .lookup_ident(name)
                    .map_err(|e| e.to_typeerror(span))?
                    .clone();
                self.bindings
                    .unify(&prev_sort, curr_sort)
                    .map_err(|e| e.to_typeerror(span))
            })
            .collect::<Result<Vec<_>, _>>();

        let module = IvySort::Module(Module {
            name: name.clone(),
            args,
            fields,
        });
        let unified = self
            .bindings
            .unify(&mod_sort, &module)
            .map_err(|e| e.to_typeerror(&Span::Todo))?;

        self.bindings.pop_scope();

        Ok(ControlMut::Produce(unified))
    }

    fn begin_object_decl(
        &mut self,
        name: &mut Token,
        _ast: &mut declarations::ObjectDecl,
    ) -> InferenceResult<declarations::Decl> {
        let v = self.bindings.new_sortvar();
        self.bindings
            .append(name.clone(), v.clone())
            .map_err(|e| e.to_typeerror(&Span::Todo))?;

        // Don't create a new scope if we're at the special top-level declaration.
        // This is needed for the typechecker visiting multiple Progs and expecting
        // earlier declarations to be in scope.
        if name != "top" {
            self.bindings.push_scope();
        }

        self.bindings
            .append("init".into(), Module::init_action_sort())
            .map_err(|e| e.to_typeerror(&Span::Todo))?;

        Ok(ControlMut::Produce(v))
    }

    fn finish_object_decl(
        &mut self,
        name: &mut Token,
        ast: &mut declarations::ObjectDecl,
        decl_sort: IvySort,
        param_sorts: Vec<IvySort>,
        body_sorts: Vec<IvySort>,
    ) -> InferenceResult<declarations::Decl> {
        let args = ast
            .params
            .iter()
            .zip(param_sorts.iter())
            .map(|(param, sort)| {
                Binding::from(
                    param.name.clone(),
                    self.bindings.resolve(sort).clone(),
                    Span::Todo,
                )
            })
            .collect::<Vec<_>>();

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
        let unified = self
            .bindings
            .unify(&decl_sort, &proc)
            .map_err(|e| e.to_typeerror(&Span::Todo))?;

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
    ) -> InferenceResult<declarations::Decl> {
        // Bind the name to _something_; we'll unify this value with a function sort when finishing the visit.
        let v = self.bindings.new_sortvar();
        self.bindings
            .append(name.clone(), v.clone())
            .map_err(|e| e.to_typeerror(&Span::Todo))?;
        self.bindings.push_scope();
        Ok(ControlMut::Produce(v))
    }
    fn finish_relation(
        &mut self,
        _name: &mut Token,
        _ast: &mut declarations::Relation,
        n: IvySort,
        paramsorts: Vec<IvySort>,
    ) -> InferenceResult<declarations::Decl> {
        let relsort = IvySort::Relation(paramsorts);
        let unified = self
            .bindings
            .unify(&n, &relsort)
            .map_err(|e| e.to_typeerror(&Span::Todo))?;
        self.bindings.pop_scope();
        Ok(ControlMut::Produce(unified))
    }

    fn begin_include_decl(&mut self, _ast: &mut Token) -> InferenceResult<Token> {
        // XXX: We could imagine whitelisting the files that we can include here, but, eh.
        Ok(ControlMut::SkipSiblings(IvySort::Unit))
    }

    fn begin_instance_decl(
        &mut self,
        name: &mut Token,
        _ast: &mut declarations::InstanceDecl,
    ) -> InferenceResult<declarations::Decl> {
        let v = self.bindings.new_sortvar();
        self.bindings
            .append(name.clone(), v)
            .map_err(|e| e.to_typeerror(&Span::Todo))?;
        Ok(ControlMut::Produce(IvySort::Unit))
    }
    fn finish_instance_decl(
        &mut self,
        name: &mut Token,
        _ast: &mut declarations::InstanceDecl,
        decl_sort: IvySort,
        module_sort: IvySort,
        mod_args_sorts: Vec<IvySort>,
    ) -> InferenceResult<declarations::Decl> {
        if let IvySort::Module(module) = module_sort {
            if !mod_args_sorts.is_empty() {
                // Will have to monomorphize with the module instantiation pass.
                //println!("Uh oh: {:?} {:?}", name, module);
                //for (i, x) in self.bindings.ctx.iter().enumerate() {
                //    println!("ctx[{i}]: {x:?}");
                // }

                for ((_name, s1), s2) in module.args.iter().zip(mod_args_sorts.iter()) {
                    self.bindings
                        .unify(s1, s2)
                        .map_err(|e| e.to_typeerror(&Span::Todo))?;
                }

                let monomorphized = module_instantiation::instantiate(module, mod_args_sorts)?;
                let unified = self
                    .bindings
                    .unify(&decl_sort, &monomorphized)
                    .map_err(|e| e.to_typeerror(&Span::Todo))?;
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
                let unified = self
                    .bindings
                    .unify(&decl_sort, &modsort)
                    .map_err(|e| e.to_typeerror(&Span::Todo))?;
                Ok(ControlMut::Produce(unified))
            }
        } else {
            return Err(TypeError::Spanned {
                span: Span::Todo,
                inner: Box::new(TypeError::NotInstanceable(module_sort.desc())),
            });
        }
    }

    fn begin_typedecl(
        &mut self,
        span: &Span,
        name: &mut Token,
        sort: &mut Sort,
    ) -> InferenceResult<declarations::Decl> {
        let resolved = self.sort(sort)?.modifying(sort);
        self.bindings
            .append(name.clone(), resolved.clone())
            .map_err(|e| e.to_typeerror(span))?;

        let binding = Binding::from(name.clone(), Sort::Resolved(resolved.clone()), span.clone());
        Ok(ControlMut::Mutation(
            declarations::Decl::Type { decl: binding },
            resolved,
        ))
    }

    fn begin_vardecl(
        &mut self,
        span: &Span,
        name: &mut Token,
        sort: &mut Sort,
    ) -> InferenceResult<declarations::Decl> {
        let v = match sort {
            Sort::ToBeInferred => self.bindings.new_sortvar(),
            Sort::Annotated(ident) => ident.visit(self)?.modifying(ident),
            Sort::Resolved(ivysort) => ivysort.clone(),
        };

        self.bindings
            .append(name.clone(), v)
            .map_err(|e| e.to_typeerror(span))?;
        Ok(ControlMut::Produce(IvySort::Unit))
    }
    fn finish_vardecl(
        &mut self,
        span: &Span,
        name: &mut Token,
        _ast: &mut Sort,
        name_sort: IvySort,
        resolved_sort: IvySort,
    ) -> InferenceResult<declarations::Decl> {
        let resolved = self
            .bindings
            .unify(&name_sort, &resolved_sort)
            .map_err(|e| e.to_typeerror(span))?;

        if &resolved_sort != &resolved {
            Ok(ControlMut::Mutation(
                declarations::Decl::Var {
                    decl: Binding::from(
                        name.clone(),
                        Sort::Resolved(resolved.clone()),
                        span.clone(),
                    ),
                },
                resolved,
            ))
        } else {
            Ok(ControlMut::Produce(resolved_sort))
        }
    }
}
