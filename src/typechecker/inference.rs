use std::collections::HashMap;

use anyhow::bail;

use super::{
    sorts::{Fargs, IvySort, Process},
    unifier::Resolver,
};
use crate::{
    ast::{
        actions::Action,
        declarations::{self, AfterDecl, BeforeDecl, ImplementDecl},
        expressions::{self, Expr, Symbol, TypeName},
        statements::Stmt,
    },
    typechecker::{sorts::Module, TypeError},
    visitor::{
        control::ControlMut,
        visitor::{Visitable, Visitor},
        VisitorResult,
    },
};

pub struct TypeChecker {
    // All the sorts we know about, at the current and shadowing scope
    pub bindings: Resolver,

    // Remember the function signature (arguments; named ret) from the
    // function definition, so we are always sure how to bind those values locally.
    pub action_locals: HashMap<Symbol, Vec<Symbol>>,
}

impl TypeChecker {
    // TODO: this should take a ref to bindings because the visitor will
    // want to hold onto it.
    pub fn new() -> Self {
        TypeChecker {
            bindings: Resolver::new(),
            action_locals: HashMap::new(),
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

    fn param(&mut self, p: &mut expressions::Param) -> VisitorResult<IvySort, expressions::Param> {
        let sort = match &mut p.sort {
            Some(idents) => {
                // TODO: I don't know how to do instance resolution yet, argh
                assert!(idents.len() == 1);
                let sym = idents.get_mut(0).unwrap();
                sym.visit(self)?.modifying(sym)?
            }
            None => self.bindings.new_sortvar(),
        };
        self.bindings.append(p.id.clone(), sort.clone())?;
        Ok(ControlMut::Produce(sort))
    }

    fn symbol(
        &mut self,
        sym: &mut expressions::Symbol,
    ) -> VisitorResult<IvySort, expressions::Symbol> {
        match sym.as_str() {
            "bool" => Ok(ControlMut::Produce(IvySort::Bool)),
            "unbounded_sequence" => Ok(ControlMut::Produce(IvySort::Number)),
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

            _ => unimplemented!(),
        }
    }

    fn begin_field_access(
        &mut self,
        lhs: &mut Expr,
        rhs: &mut expressions::Symbol,
    ) -> VisitorResult<IvySort, Expr> {
        let lhs_sort = lhs.visit(self)?.modifying(lhs)?;

        let rhs_sort = match &lhs_sort {
            IvySort::Module(module) => Ok(module.fields.get(rhs).map(|s| s.clone())),
            IvySort::Process(proc) => {
                let mut s = proc.impl_fields.get(rhs).or(proc.spec_fields.get(rhs));
                let is_common = s.is_none();
                s = s
                    .or(proc.common_impl_fields.get(rhs))
                    .or(proc.common_spec_fields.get(rhs));

                // XXX: Lots of cloning in here that should be improved somehow.
                let s = s.map(|s| s.clone());
                s.map(|s| match s {
                    IvySort::Function(Fargs::List(args), ret) if !is_common => {
                        // A slightly hacky thing that should probably live elsewhere:
                        // if the rhs is a non-common action, and the first
                        // argument is `this`, we need to curry it.
                        let first_arg = args.get(0).map(|s| self.bindings.resolve(s));
                        if first_arg == Some(&lhs_sort) {
                            let remaining_args =
                                args.clone().into_iter().skip(1).collect::<Vec<_>>();
                            Ok(IvySort::function_sort(remaining_args, *ret))
                        } else {
                            Ok::<_, TypeError>(IvySort::function_sort(args, *ret))
                        }
                    }
                    _ => Ok(s),
                })
                .transpose()
            }
            sort => bail!(TypeError::NotARecord(sort.clone())),
        }?
        .map(|s| self.bindings.resolve(&s).clone());
        println!("NBT: {:?}", rhs_sort);
        match rhs_sort {
            Some(sort) => Ok(ControlMut::SkipSiblings(sort.clone())),
            None => bail!(TypeError::UnboundVariable(rhs.clone())),
        }
    }

    // actions

    fn begin_action_decl(
        &mut self,
        ast: &mut declarations::ActionDecl,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        // Bind the name to _something_; we'll unify this value with its resolved sort when finishing the visit.
        let v = self.bindings.new_sortvar();
        self.bindings.append(ast.name.clone(), v)?;
        self.bindings.push_scope();

        Ok(ControlMut::Produce(IvySort::Unit))
    }
    fn finish_action_decl(
        &mut self,
        ast: &mut declarations::ActionDecl,
        name: IvySort,
        params: Vec<IvySort>,
        ret: Option<IvySort>,
        _body: Option<Vec<IvySort>>,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let mut locals = ast.params.iter().map(|p| p.id.clone()).collect::<Vec<_>>();
        if let Some(ret) = &ast.ret {
            locals.push(ret.id.clone())
        }
        self.action_locals.insert(ast.name.clone(), locals);

        let retsort = match ret {
            None => IvySort::Unit,
            Some(s) => s,
        };
        let actsort = IvySort::Function(Fargs::List(params), Box::new(retsort));
        let unified = self.bindings.unify(&name, &actsort)?;

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
        _ast: &mut declarations::AfterDecl,
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
        _e: &mut Expr,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let v = self.bindings.new_sortvar();
        self.bindings.append(sym.clone(), v.clone())?;
        Ok(ControlMut::Produce(v))
    }
    fn finish_alias_decl(
        &mut self,
        _sym: &mut Symbol,
        _e: &mut Expr,
        sym_sort: IvySort,
        expr_sort: IvySort,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let unifed = self.bindings.unify(&sym_sort, &expr_sort)?;
        Ok(ControlMut::Produce(unifed))
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

    // decls

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
        ast: &mut declarations::ModuleDecl,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let v = self.bindings.new_sortvar();
        self.bindings.append(ast.name.clone(), v.clone())?;

        self.bindings.push_scope();
        self.bindings.append("this".into(), v.clone())?;
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
            .collect::<HashMap<_, _>>();
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

        let module = IvySort::Module(Module { args, fields });
        let unifed = self.bindings.unify(&mod_sort, &module)?;

        self.bindings.pop_scope();

        Ok(ControlMut::Produce(unifed))
    }

    fn begin_isolate_decl(
        &mut self,
        ast: &mut declarations::IsolateDecl,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        bail!(TypeError::UnnormalizedIsolate(ast.clone()))
    }

    fn begin_normalized_isolate_decl(
        &mut self,
        ast: &mut declarations::NormalizedIsolateDecl,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let v = self.bindings.new_sortvar();
        self.bindings.append(ast.name.clone(), v.clone())?;

        self.bindings.push_scope();
        self.bindings.append("this".into(), v.clone())?;
        self.bindings
            .append("init".into(), Module::init_action_sort())?;

        Ok(ControlMut::Produce(v))
    }
    fn finish_normalized_isolate_decl(
        &mut self,
        ast: &mut declarations::NormalizedIsolateDecl,
        decl_sort: IvySort,
        param_sorts: Vec<IvySort>,
        impl_sorts: Vec<IvySort>,
        spec_sorts: Vec<IvySort>,
        common_impl_sorts: Vec<IvySort>,
        common_spec_sorts: Vec<IvySort>,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let args = ast
            .params
            .iter()
            .zip(param_sorts.iter())
            .map(|(name, sort)| (name.id.clone(), self.bindings.resolve(sort).clone()))
            .collect::<HashMap<_, _>>();

        let mut impl_fields = ast
            .impl_decls
            .iter()
            .zip(impl_sorts.iter())
            .filter_map(|(decl, sort)| {
                decl.name_for_binding()
                    .map(|n| (n.into(), self.bindings.resolve(sort).clone()))
            })
            .collect::<HashMap<_, _>>();
        // Every module has an implicit "init" action.
        impl_fields.insert("init".into(), Module::init_action_sort());

        let spec_fields = ast
            .spec_decls
            .iter()
            .zip(spec_sorts.iter())
            .filter_map(|(decl, sort)| {
                decl.name_for_binding()
                    .map(|n| (n.into(), self.bindings.resolve(sort).clone()))
            })
            .collect::<HashMap<_, _>>();

        let common_impl_fields = ast
            .common_impl_decls
            .iter()
            .zip(common_impl_sorts.iter())
            .filter_map(|(decl, sort)| {
                decl.name_for_binding()
                    .map(|n| (n.into(), self.bindings.resolve(sort).clone()))
            })
            .collect::<HashMap<_, _>>();

        let common_spec_fields = ast
            .common_spec_decls
            .iter()
            .zip(common_spec_sorts.iter())
            .filter_map(|(decl, sort)| {
                decl.name_for_binding()
                    .map(|n| (n.into(), self.bindings.resolve(sort).clone()))
            })
            .collect::<HashMap<_, _>>();

        let proc = IvySort::Process(Process {
            args,
            impl_fields,
            spec_fields,
            common_impl_fields,
            common_spec_fields,
        });

        // XXX: Can I avoid this clone?
        let this = self.bindings.lookup_sym("this").unwrap().clone();
        let _ = self.bindings.unify(&this, &proc)?;
        let unified = self.bindings.unify(&decl_sort, &proc)?;
        self.bindings.pop_scope();

        Ok(ControlMut::Produce(unified))
    }

    fn begin_relation(
        &mut self,
        ast: &mut declarations::Relation,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        // Bind the name to _something_; we'll unify this value with a function sort when finishing the visit.
        let v = self.bindings.new_sortvar();
        self.bindings.append(ast.name.clone(), v.clone())?;
        Ok(ControlMut::Produce(v))
    }
    fn finish_relation(
        &mut self,
        _ast: &mut declarations::Relation,
        n: IvySort,
        paramsorts: Vec<IvySort>,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        // A relation is a bool-producing function for our purposes.
        // TODO: contemplate defaultdict-style "default functions" like the C++
        // extraction code uses.

        let relsort = IvySort::Function(Fargs::List(paramsorts), Box::new(IvySort::Bool));
        let unifed = self.bindings.unify(&n, &relsort)?;
        Ok(ControlMut::Produce(unifed))
    }

    fn begin_instance_decl(
        &mut self,
        ast: &mut declarations::InstanceDecl,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let v = self.bindings.new_sortvar();
        self.bindings.append(ast.name.clone(), v)?;
        Ok(ControlMut::Produce(IvySort::Unit))
    }
    fn finish_instance_decl(
        &mut self,
        _ast: &mut declarations::InstanceDecl,
        decl_sort: IvySort,
        module_sort: IvySort,
        mod_args_sorts: Vec<IvySort>,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        if mod_args_sorts.len() > 0 {
            // Will have to monomorphize with the module instantiation pass.
            todo!()
        }
        if let IvySort::Module(Module { args: _, fields }) = module_sort {
            let modsort = IvySort::Module(Module {
                args: vec![],
                fields: fields,
            });
            let unifed = self.bindings.unify(&decl_sort, &modsort)?;
            Ok(ControlMut::Produce(unifed))
        } else {
            bail!(TypeError::NotInstanceable(module_sort.clone()))
        }
    }

    fn begin_typedecl(
        &mut self,
        ast: &mut expressions::Type,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let sort = match &ast.ident {
            TypeName::Name(n) => {
                self.bindings.append(n.clone(), ast.sort.clone())?;
                ast.sort.clone()
            }
            TypeName::This => self.bindings.lookup_sym("this").unwrap().clone(),
        };
        Ok(ControlMut::SkipSiblings(sort))
    }

    fn begin_vardecl(
        &mut self,
        ast: &mut expressions::Term,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        // Bind the name to _something_; we'll unify this value with its resolved sort when finishing the visit.
        let v = self.bindings.new_sortvar();
        self.bindings.append(ast.id.clone(), v)?;
        Ok(ControlMut::Produce(IvySort::Unit))
    }
    fn finish_vardecl(
        &mut self,
        _ast: &mut expressions::Term,
        id_sort: IvySort,
        resolved_sort: Option<IvySort>,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        if let Some(s2) = resolved_sort {
            self.bindings.unify(&id_sort, &s2)?;
            Ok(ControlMut::Produce(s2))
        } else {
            Ok(ControlMut::Produce(id_sort.clone()))
        }
    }
}
