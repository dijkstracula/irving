use std::collections::HashMap;

use anyhow::bail;

use super::{
    sorts::{Fargs, IvySort, Process},
    unifier::Resolver,
};
use crate::{
    ast::{
        actions::Action,
        declarations,
        expressions::{self, Expr, Symbol},
        statements::Stmt,
    },
    typechecker::{sorts, TypeError},
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
        // TODO: we'll have to walk each element in the identifier, and ensure
        // that all but the final value is a Module.
        if i.len() == 1 {
            let mut s = i.get_mut(0).unwrap();
            Ok(ControlMut::Produce(s.visit(self)?.modifying(&mut s)?))
        } else {
            // ???
            todo!()
        }
    }

    fn number(&mut self, _n: &mut i64) -> VisitorResult<IvySort, i64> {
        Ok(ControlMut::Produce(IvySort::Number))
    }

    fn param(&mut self, p: &mut expressions::Param) -> VisitorResult<IvySort, expressions::Param> {
        match &mut p.sort {
            Some(idents) => {
                // TODO: I don't know how to do instance resolution yet, argh
                assert!(idents.len() == 1);
                let sym = idents.get_mut(0).unwrap();
                sym.visit(self)?
                    .modifying(sym)
                    .map(|s| ControlMut::Produce(s))
            }
            None => Ok(ControlMut::Produce(self.bindings.new_sortvar())),
        }
    }

    fn symbol(
        &mut self,
        sym: &mut expressions::Symbol,
    ) -> VisitorResult<IvySort, expressions::Symbol> {
        println!("NBT: symbol: {sym:?}");
        match sym.as_str() {
            "bool" => Ok(ControlMut::Produce(IvySort::Bool)),
            // TODO: and of course other builtins.
            _ => {
                let sort = self.bindings.lookup(sym);

                match sort {
                    // XXX: ST: wtf???
                    Some(sort) => Ok(ControlMut::Produce(sort.clone())),
                    None => bail!(TypeError::UnboundVariable(sym.clone())),
                }
            }
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
        if let Ok(IvySort::Function(_, ret)) = self.bindings.unify(&fsort, &expected_sort) {
            Ok(ControlMut::Produce(*ret.clone()))
        } else {
            bail!(TypeError::InvalidApplication(fsort))
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
        let recordsort = match lhs.visit(self)?.modifying(lhs)? {
            IvySort::Process(proc) => proc,
            sort => bail!(TypeError::NotARecord(sort)),
        };

        match recordsort
            .impl_fields
            .get(rhs)
            .or(recordsort.spec_fields.get(rhs))
            .or(recordsort.common_spec_fields.get(rhs))
        {
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

        for param in &ast.params {
            let s = self.bindings.new_sortvar();
            self.bindings.append(param.id.clone(), s)?;
        }
        if let Some(param) = &ast.ret {
            let s = self.bindings.new_sortvar();
            self.bindings.append(param.id.clone(), s)?;
        }

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
            None => self.bindings.new_sortvar(),
            Some(s) => s,
        };
        let actsort = IvySort::Function(Fargs::List(params), Box::new(retsort));
        let _unifed = self.bindings.unify(&name, &actsort)?;

        self.bindings.pop_scope();

        Ok(ControlMut::Produce(IvySort::Unit))
    }

    fn begin_after_decl(
        &mut self,
        ast: &mut declarations::AfterDecl,
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

    fn finish_after_decl(
        &mut self,
        _ast: &mut declarations::AfterDecl,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        self.bindings.pop_scope();
        Ok(ControlMut::Produce(IvySort::Unit))
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
    ) -> VisitorResult<IvySort, declarations::Decl> {
        self.bindings.pop_scope();
        Ok(ControlMut::Produce(IvySort::Unit))
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

    fn begin_module_decl(
        &mut self,
        ast: &mut declarations::ModuleDecl,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        bail!(TypeError::UnnormalizedModule(ast.clone()))
    }

    fn begin_normalized_module_decl(
        &mut self,
        ast: &mut declarations::NormalizedModuleDecl,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let v = self.bindings.new_sortvar();
        self.bindings.append(ast.name.clone(), v)?;

        Ok(ControlMut::Produce(IvySort::Unit))
    }
    fn finish_normalized_module_decl(
        &mut self,
        ast: &mut declarations::NormalizedModuleDecl,
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
            .map(|(name, sort)| (name.id.clone(), self.bindings.resolve(sort)))
            .collect::<Vec<_>>();

        let impl_fields = ast
            .impl_decls
            .iter()
            .zip(impl_sorts.iter())
            .filter_map(|(decl, sort)| {
                decl.name_for_binding()
                    .map(|n| (n.clone(), self.bindings.resolve(sort)))
            })
            .collect::<HashMap<_, _>>();

        let spec_fields = ast
            .spec_decls
            .iter()
            .zip(spec_sorts.iter())
            .filter_map(|(decl, sort)| {
                decl.name_for_binding()
                    .map(|n| (n.clone(), self.bindings.resolve(sort)))
            })
            .collect::<HashMap<_, _>>();

        let common_impl_fields = ast
            .common_impl_decls
            .iter()
            .zip(common_impl_sorts.iter())
            .filter_map(|(decl, sort)| {
                decl.name_for_binding()
                    .map(|n| (n.clone(), self.bindings.resolve(sort)))
            })
            .collect::<HashMap<_, _>>();

        let common_spec_fields = ast
            .common_spec_decls
            .iter()
            .zip(common_spec_sorts.iter())
            .filter_map(|(decl, sort)| {
                decl.name_for_binding()
                    .map(|n| (n.clone(), self.bindings.resolve(sort)))
            })
            .collect::<HashMap<_, _>>();

        let proc = IvySort::Process(Process {
            args,
            impl_fields,
            spec_fields,
            common_impl_fields,
            common_spec_fields,
        });

        let unifed = self.bindings.unify(&decl_sort, &proc)?;
        Ok(ControlMut::Produce(unifed))
    }

    fn begin_relation(
        &mut self,
        ast: &mut declarations::Relation,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        // Bind the name to _something_; we'll unify this value with a function sort when finishing the visit.
        let v = self.bindings.new_sortvar();
        self.bindings.append(ast.name.clone(), v)?;
        Ok(ControlMut::Produce(IvySort::Unit))
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

    fn begin_typedecl(
        &mut self,
        ast: &mut expressions::Type,
    ) -> VisitorResult<IvySort, declarations::Decl> {
        let sortname = match &ast.ident {
            expressions::TypeName::Name(n) => n,
            expressions::TypeName::This => todo!(),
        };
        self.bindings.append(sortname.clone(), ast.sort.clone())?;
        Ok(ControlMut::SkipSiblings(ast.sort.clone()))
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
