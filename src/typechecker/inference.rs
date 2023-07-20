use std::collections::BTreeMap;

use anyhow::bail;

use super::{
    sorts::{Fargs, IvySort, Process},
    unifier::Resolver,
};
use crate::{
    ast::{
        actions::Action,
        declarations::{self, AfterDecl, BeforeDecl, Binding, ImplementDecl},
        expressions::{self, Expr, Sort, Symbol},
        statements::Stmt,
    },
    passes::module_instantiation,
    typechecker::{sorts::Module, TypeError},
    visitor::{ast::Visitable, ast::Visitor, control::ControlMut, VisitorResult},
};

pub struct TypeChecker {
    // All the sorts we know about, at the current and shadowing scope
    pub bindings: Resolver,

    // Remember the function signature (arguments; named ret) from the
    // function definition, so we are always sure how to bind those values locally.
    pub action_locals: BTreeMap<Symbol, Vec<Symbol>>,
}

impl TypeChecker {
    // TODO: this should take a ref to bindings because the visitor will
    // want to hold onto it.
    pub fn new() -> Self {
        TypeChecker {
            bindings: Resolver::new(),
            action_locals: BTreeMap::new(),
        }
    }
}

impl Visitor<IvySort> for TypeChecker {
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
            let mut s = i.get_mut(0).unwrap();
            Ok(ControlMut::Produce(s.visit(self)?.modifying(&mut s)?))
        } else {
            // XXX: we always set include_spec to true.  Suggests we need to also
            // return an annotation indicating if this was a spec/common/etc. decl.
            let resolved = self.bindings.lookup_ident(i, true)?;
            Ok(ControlMut::Produce(resolved.clone()))
        }
    }

    fn number(&mut self, _n: &mut i64) -> VisitorResult<IvySort, i64> {
        Ok(ControlMut::Produce(IvySort::Number))
    }

    fn param(
        &mut self,
        p: &mut expressions::AnnotatedSymbol,
    ) -> VisitorResult<IvySort, expressions::AnnotatedSymbol> {
        match &mut p.sort {
            Sort::ToBeInferred => Ok(ControlMut::Produce(self.bindings.new_sortvar())),
            Sort::Annotated(id) => {
                let resolved = id.visit(self)?.modifying(id)?;
                // Note that because a parameter binds a new name, we add it
                // to the bindings here.  We do not do the same for sort!()
                self.bindings.append(p.id.clone(), resolved.clone())?;
                Ok(ControlMut::Mutation(
                    expressions::AnnotatedSymbol {
                        id: p.id.clone(),
                        sort: Sort::Resolved(resolved.clone()),
                    },
                    resolved,
                ))
            }
            Sort::Resolved(ivysort) => {
                // XXX: early return if sort is already an ivyosrt?
                self.bindings.append(p.id.clone(), ivysort.clone())?;
                Ok(ControlMut::Produce(ivysort.clone()))
            }
        }
    }

    fn sort(&mut self, s: &mut Sort) -> VisitorResult<IvySort, Sort> {
        let ctrl = match s {
            Sort::ToBeInferred => ControlMut::Produce(self.bindings.new_sortvar()),
            Sort::Annotated(ident) => {
                let resolved = self.identifier(ident)?.modifying(ident)?;
                ControlMut::Mutation(Sort::Resolved(resolved.clone()), resolved)
            }
            Sort::Resolved(ivysort) => ControlMut::Produce(ivysort.clone()),
        };
        Ok(ctrl)
    }

    fn annotated_symbol(
        &mut self,
        p: &mut expressions::AnnotatedSymbol,
    ) -> VisitorResult<IvySort, expressions::AnnotatedSymbol> {
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

    fn symbol(
        &mut self,
        sym: &mut expressions::Symbol,
    ) -> VisitorResult<IvySort, expressions::Symbol> {
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

    //

    fn action_seq(&mut self, ast: &mut Vec<Action>) -> VisitorResult<IvySort, Stmt> {
        //XXX: kinda dumb, honestly.
        let _ = ast.visit(self)?.modifying(ast)?;
        Ok(ControlMut::Produce(IvySort::Unit))
    }

    //

    fn finish_app(
        &mut self,
        _ast: &mut expressions::AppExpr,
        fsort: IvySort,
        argsorts: Vec<IvySort>,
    ) -> VisitorResult<IvySort, Expr> {
        let retsort = self.bindings.new_sortvar();
        let expected_sort = IvySort::Function(Fargs::List(argsorts), Box::new(retsort));
        let unified = self.bindings.unify(&fsort, &expected_sort);
        match unified {
            Ok(IvySort::Function(_, ret)) => Ok(ControlMut::Produce(*ret.clone())),
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
        rhs: &mut expressions::AnnotatedSymbol,
    ) -> VisitorResult<IvySort, Expr> {
        let lhs_sort = lhs.visit(self)?.modifying(lhs)?;

        let mut is_common = false;

        // XXX: awkward amounts of cloning in here.
        let rhs_sort = match &lhs_sort {
            IvySort::Module(module) => {
                Ok::<Option<IvySort>, TypeError>(module.fields.get(&rhs.id).map(|s| s.clone()))
            }
            IvySort::Process(proc) => {
                let s = proc.fields.get(&rhs.id);
                is_common = s.is_none();
                Ok(s.map(|s| s.clone()))
            }
            IvySort::SortVar(_) => Ok(Some(self.bindings.new_sortvar())),
            sort => bail!(TypeError::NotARecord(sort.clone())),
        }?
        .map(|s| match s {
            // A slightly hacky thing that should probably live elsewhere:
            // if the rhs is a non-common action, and the first
            // argument is `this`, we need to curry it.
            IvySort::Function(Fargs::List(args), ret) if !is_common => {
                let first_arg = args.get(0).map(|s| self.bindings.resolve(s));
                if first_arg == Some(&IvySort::This) {
                    let remaining_args = args.clone().into_iter().skip(1).collect::<Vec<_>>();
                    Ok(IvySort::function_sort(remaining_args, *ret))
                } else {
                    Ok::<_, TypeError>(IvySort::function_sort(args, *ret))
                }
            }
            _ => Ok(s),
        })
        .transpose()?;
        match rhs_sort {
            Some(sort) => {
                rhs.sort = Sort::Resolved(sort.clone());
                Ok(ControlMut::SkipSiblings(sort.clone()))
            }
            None => bail!(TypeError::MissingRecordField(lhs_sort, rhs.id.clone())),
        }
    }

    // actions and decls

    fn begin_action_decl(
        &mut self,
        name: &mut Symbol,
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
        name: &mut Symbol,
        ast: &mut declarations::ActionDecl,
        name_sort: IvySort,
        params: Vec<IvySort>,
        ret: Option<IvySort>,
        _body: Option<Vec<IvySort>>,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let mut locals = ast.params.iter().map(|p| p.id.clone()).collect::<Vec<_>>();
        if let Some(ret) = &ast.ret {
            locals.push(ret.id.clone())
        }
        self.action_locals.insert(name.clone(), locals);

        let retsort = match ret {
            None => IvySort::Unit,
            Some(s) => s,
        };
        let actsort = IvySort::Function(Fargs::List(params), Box::new(retsort));
        let unified = self.bindings.unify(&name_sort, &actsort)?;

        self.bindings.pop_scope();

        Ok(ControlMut::Produce(unified))
    }

    fn begin_after_decl(
        &mut self,
        ast: &mut declarations::AfterDecl,
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
        ast: &mut declarations::AfterDecl,
        action_sort: IvySort,
        after_params_sort: Option<Vec<IvySort>>,
        after_ret_sort: Option<IvySort>,
        _after_body_sort: Vec<IvySort>,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        self.bindings.pop_scope();

        let mixin_sort = match (after_params_sort, after_ret_sort) {
            (None, None) => self.bindings.new_sortvar(),
            (Some(params), None) => {
                IvySort::Function(Fargs::List(params), Box::new(self.bindings.new_sortvar()))
            }
            (None, Some(ret)) => IvySort::Function(Fargs::Unknown, Box::new(ret)),
            (Some(params), Some(ret)) => IvySort::Function(Fargs::List(params), Box::new(ret)),
        };

        let unified = self.bindings.unify(&action_sort, &mixin_sort)?;

        Ok(ControlMut::Produce(unified))
    }

    fn begin_alias_decl(
        &mut self,
        sym: &mut Symbol,
        _s: &mut Sort,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let v = self.bindings.new_sortvar();
        self.bindings.append(sym.clone(), v.clone())?;
        Ok(ControlMut::Produce(v))
    }
    fn finish_alias_decl(
        &mut self,
        sym: &mut Symbol,
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
        ast: &mut declarations::BeforeDecl,
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
        _ast: &mut declarations::BeforeDecl,
        action_sort: IvySort,
        param_sort: Option<Vec<IvySort>>,
        _body_sorts: Vec<IvySort>,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        self.bindings.pop_scope();

        let mixin_sort = match param_sort {
            None => IvySort::Function(Fargs::Unknown, Box::new(self.bindings.new_sortvar())),
            Some(params) => {
                IvySort::Function(Fargs::List(params), Box::new(self.bindings.new_sortvar()))
            }
        };

        let unified = self.bindings.unify(&action_sort, &mixin_sort)?;
        Ok(ControlMut::Produce(unified))
    }

    fn begin_function_decl(
        &mut self,
        name: &mut Symbol,
        _ast: &mut declarations::FunctionDecl,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let v = self.bindings.new_sortvar();
        self.bindings.append(name.clone(), v.clone())?;
        self.bindings.push_scope();
        Ok(ControlMut::Produce(v))
    }

    fn finish_function_decl(
        &mut self,
        _name: &mut Symbol,
        _ast: &mut declarations::FunctionDecl,
        name_sort: IvySort,
        param_sorts: Vec<IvySort>,
        ret_sort: IvySort,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let fnsort = IvySort::function_sort(param_sorts, ret_sort);
        let unifed = self.bindings.unify(&name_sort, &fnsort)?;
        self.bindings.pop_scope();
        Ok(ControlMut::Produce(unifed))
    }

    fn begin_implement_decl(
        &mut self,
        ast: &mut declarations::ImplementDecl,
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
        _ast: &mut declarations::ImplementDecl,
        name: IvySort,
        params: Option<Vec<IvySort>>,
        ret: Option<IvySort>,
        _body: Option<Vec<IvySort>>,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let psort = match params {
            None => Fargs::Unknown,
            Some(s) => Fargs::List(s),
        };
        let retsort = match ret {
            None => self.bindings.new_sortvar(),
            Some(s) => s,
        };
        let actsort = IvySort::Function(psort, Box::new(retsort));
        let _unifed = self.bindings.unify(&name, &actsort)?;

        self.bindings.pop_scope();
        Ok(ControlMut::Produce(IvySort::Unit))
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
        let relsort = IvySort::Function(Fargs::List(param_sorts), Box::new(IvySort::Unit));
        let unifed = self.bindings.unify(&decl_sortvar, &relsort)?;
        Ok(ControlMut::Produce(unifed))
    }

    fn begin_module_decl(
        &mut self,
        name: &mut Symbol,
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
        name: &mut Symbol,
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
                declarations::Decl::AfterAction(AfterDecl { name, .. })
                | declarations::Decl::BeforeAction(BeforeDecl { name, .. })
                | declarations::Decl::Implement(ImplementDecl { name, .. }) => {
                    Some((name, curr_sort))
                }
                _ => None,
            })
            .map(|(name, curr_sort)| {
                // XXX: Can I avoid this clone, despite unify() requring a mutable
                // reference to `self.bindings`?
                let prev_sort = self.bindings.lookup_ident(name, true)?.clone();
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
        name: &mut Symbol,
        ast: &mut declarations::ObjectDecl,
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
        _name: &mut Symbol,
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

        let proc = IvySort::Process(Process { args, fields });
        let unified = self.bindings.unify(&decl_sort, &proc)?;
        self.bindings.pop_scope();
        Ok(ControlMut::Produce(unified))
    }

    fn begin_relation(
        &mut self,
        name: &mut Symbol,
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
        _name: &mut Symbol,
        _ast: &mut declarations::Relation,
        n: IvySort,
        paramsorts: Vec<IvySort>,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        // A relation is a bool-producing function for our purposes.
        // TODO: contemplate defaultdict-style "default functions" like the C++
        // extraction code uses.

        let relsort = IvySort::Function(Fargs::List(paramsorts), Box::new(IvySort::Bool));
        let unifed = self.bindings.unify(&n, &relsort)?;
        self.bindings.pop_scope();
        Ok(ControlMut::Produce(unifed))
    }

    fn begin_instance_decl(
        &mut self,
        name: &mut Symbol,
        _ast: &mut declarations::InstanceDecl,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let v = self.bindings.new_sortvar();
        self.bindings.append(name.clone(), v)?;
        Ok(ControlMut::Produce(IvySort::Unit))
    }
    fn finish_instance_decl(
        &mut self,
        name: &mut Symbol,
        _ast: &mut declarations::InstanceDecl,
        decl_sort: IvySort,
        module_sort: IvySort,
        mod_args_sorts: Vec<IvySort>,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        if let IvySort::Module(module) = module_sort {
            if mod_args_sorts.len() > 0 {
                // Will have to monomorphize with the module instantiation pass.
                //println!("Uh oh: {:?} {:?}", name, decl_sort);
                //for (i, x) in self.bindings.ctx.iter().enumerate() {
                //    println!("ctx[{i}]: {x:?}");
                // }

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
        name: &mut Symbol,
        sort: &mut Sort,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let binding = match sort {
            Sort::ToBeInferred => self.bindings.new_sortvar(),
            Sort::Annotated(_) => unreachable!(),
            Sort::Resolved(sort) => sort.clone(),
        };
        self.bindings.append(name.clone(), binding.clone())?;
        Ok(ControlMut::SkipSiblings(binding))
    }

    fn begin_vardecl(
        &mut self,
        name: &mut Symbol,
        _ast: &mut Sort,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        // Bind the name to _something_; we'll unify this value with its resolved sort when finishing the visit.
        let v = self.bindings.new_sortvar();
        self.bindings.append(name.clone(), v)?;
        Ok(ControlMut::Produce(IvySort::Unit))
    }
    fn finish_vardecl(
        &mut self,
        name: &mut Symbol,
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
