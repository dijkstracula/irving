use std::collections::BTreeMap;

use crate::{
    ast::expressions::{Expr, Symbol},
    typechecker::sorts::{Fargs, IvySort, Module, Process},
};

use super::{ControlMut, VisitorResult};

pub trait Visitor<T>
where
    T: Default,
{
    // IvySorts

    fn uninterpreted(&mut self) -> VisitorResult<T, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn this(&mut self) -> VisitorResult<T, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn unit(&mut self) -> VisitorResult<T, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn top(&mut self) -> VisitorResult<T, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn bool(&mut self) -> VisitorResult<T, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn number(&mut self) -> VisitorResult<T, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn bitvec(&mut self, _width: &mut u8) -> VisitorResult<T, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn vector(&mut self, _elem_sort: &mut IvySort, _elem_sort_t: T) -> VisitorResult<T, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn range(&mut self, _lo: &mut Expr, _hi: &mut Expr) -> VisitorResult<T, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn enumeration(&mut self, _discriminants: &mut Vec<Symbol>) -> VisitorResult<T, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn function(
        &mut self,
        _args: &mut Fargs,
        _ret: &mut IvySort,
        _args_t: Option<Vec<T>>,
        _ret_t: T,
    ) -> VisitorResult<T, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn relation(&mut self, _args: &mut Vec<IvySort>, _args_t: Vec<T>) -> VisitorResult<T, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn subclass(&mut self, _cname: &mut Symbol) -> VisitorResult<T, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn module(
        &mut self,
        _mod: &mut Module,
        _args_t: Vec<T>,
        _fields_t: BTreeMap<String, T>,
    ) -> VisitorResult<T, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn process(
        &mut self,
        _proc: &mut Process,
        _args_t: BTreeMap<Symbol, T>,
        _impl_fields_t: BTreeMap<Symbol, T>,
        _spec_fields_t: BTreeMap<Symbol, T>,
        _common_impl_fields_t: BTreeMap<Symbol, T>,
        _common_spec_fields_t: BTreeMap<Symbol, T>,
    ) -> VisitorResult<T, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }
}

impl<T> Visitable<T> for IvySort
where
    T: Default,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<T, IvySort> {
        let t = match self {
            IvySort::Uninterpreted => visitor.uninterpreted(),
            IvySort::This => visitor.this(),
            IvySort::Unit => visitor.unit(),
            IvySort::Top => visitor.top(),
            IvySort::Bool => visitor.bool(),
            IvySort::Number => visitor.number(),
            IvySort::BitVec(width) => visitor.bitvec(width),
            IvySort::Vector(t) => {
                let t_ret = t.visit(visitor)?.modifying(t)?;
                visitor.vector(t, t_ret)
            }
            IvySort::Range(lo, hi) => visitor.range(lo.as_mut(), hi.as_mut()),
            IvySort::Enum(discs) => visitor.enumeration(discs),
            IvySort::Function(ref mut fargs, ref mut ret) => {
                let farg_t = match fargs {
                    Fargs::Unknown => None,
                    Fargs::List(sorts) => Some(
                        sorts
                            .iter_mut()
                            .map(|s| s.visit(visitor)?.modifying(s))
                            .collect::<Result<Vec<_>, _>>()?,
                    ),
                };
                let ret_t = ret.visit(visitor)?.modifying(ret)?;
                visitor.function(fargs, ret, farg_t, ret_t)
            }
            IvySort::Relation(args) => {
                let args_t = args
                    .iter_mut()
                    .map(|s| s.visit(visitor)?.modifying(s))
                    .collect::<Result<Vec<_>, _>>()?;
                visitor.relation(args, args_t)
            }
            IvySort::Subclass(cls) => visitor.subclass(cls),
            IvySort::Module(module) => {
                let args_t = module
                    .args
                    .iter_mut()
                    .map(|(_, s)| s.visit(visitor)?.modifying(s))
                    .collect::<Result<Vec<_>, _>>()?;
                let fields_t = module
                    .fields
                    .iter_mut()
                    .map(|(k, v)| {
                        let v_t = v.visit(visitor)?.modifying(v).map(|t| (k.clone(), t));
                        v_t
                    })
                    .collect::<Result<BTreeMap<_, _>, _>>()?;
                visitor.module(module, args_t, fields_t)
            }
            IvySort::Process(_) => todo!(),
            IvySort::SortVar(_) => todo!(),
        }?
        .modifying(self)?;
        todo!()
    }
}

/// Something that can be visited by a Visitor.
pub trait Visitable<T, U = T>
where
    Self: Sized,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T>) -> VisitorResult<U, IvySort>;
}
