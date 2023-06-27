use anyhow::bail;

use super::{sorts::IvySort, unifier::Resolver};
use crate::{
    ast::{
        declarations,
        expressions::{self, Expr},
    },
    typechecker::TypeError,
    visitor::{
        control::ControlMut,
        visitor::{Visitable, Visitor},
        VisitorResult,
    },
};

pub struct TypeChecker {
    pub bindings: Resolver,
}

impl TypeChecker {
    // TODO: this should take a ref to bindings because the visitor will
    // want to hold onto it.
    pub fn new() -> Self {
        TypeChecker {
            bindings: Resolver::new(),
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

    fn finish_app(
        &mut self,
        _ast: &mut expressions::AppExpr,
        fsort: IvySort,
        argsorts: Vec<IvySort>,
    ) -> VisitorResult<IvySort, Expr> {
        let retsort = self.bindings.new_sortvar();

        let expected_sort = IvySort::Function(argsorts, Box::new(retsort));
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
            .or(recordsort.commonspec_fields.get(rhs))
        {
            Some(sort) => Ok(ControlMut::SkipSiblings(sort.clone())),
            None => bail!(TypeError::UnboundVariable(rhs.clone())),
        }
    }

    // decls

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
        let relsort = IvySort::Function(paramsorts, Box::new(IvySort::Bool));
        let _unifed = self.bindings.unify(&n, &relsort)?;
        Ok(ControlMut::Produce(IvySort::Unit))
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
        Ok(ControlMut::SkipSiblings(IvySort::Unit))
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
        println!("NBT: {id_sort:?}, {resolved_sort:?}");
        if let Some(s2) = resolved_sort {
            self.bindings.unify(&id_sort, &s2)?;
        }
        Ok(ControlMut::Produce(IvySort::Unit))
    }
}
