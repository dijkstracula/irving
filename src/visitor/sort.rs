use std::{collections::BTreeMap, error::Error};

use crate::{
    ast::expressions::{self, Token},
    typechecker::sorts::{ActionArgs, ActionRet, IvySort, Module, Object},
};

use super::{ControlMut, VisitorResult};

pub trait Visitor<T, E>
where
    T: Default,
    E: Error,
{
    // IvySorts

    fn uninterpreted(&mut self) -> VisitorResult<T, E, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn this(&mut self) -> VisitorResult<T, E, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn unit(&mut self) -> VisitorResult<T, E, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn top(&mut self) -> VisitorResult<T, E, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn bool(&mut self) -> VisitorResult<T, E, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn number(&mut self) -> VisitorResult<T, E, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn bitvec(&mut self, _width: &mut u8) -> VisitorResult<T, E, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn vector(
        &mut self,
        _elem_sort: &mut IvySort,
        _elem_sort_t: T,
    ) -> VisitorResult<T, E, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn range(&mut self, _lo: i64, _hi: i64) -> VisitorResult<T, E, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn enumeration(&mut self, _discriminants: &mut Vec<Token>) -> VisitorResult<T, E, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn action(
        &mut self,
        _arg_syms: Vec<expressions::Token>,
        _args: &mut ActionArgs,
        _ret: &mut ActionRet,
        _args_t: Option<Vec<T>>,
        _ret_t: T,
    ) -> VisitorResult<T, E, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn relation(
        &mut self,
        _args: &mut Vec<IvySort>,
        _args_t: Vec<T>,
    ) -> VisitorResult<T, E, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn subclass(&mut self, _cname: &mut Token) -> VisitorResult<T, E, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn module(
        &mut self,
        _mod: &mut Module,
        _args_t: Vec<(String, T)>,
        _fields_t: BTreeMap<String, T>,
    ) -> VisitorResult<T, E, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn object(
        &mut self,
        _proc: &mut Object,
        _args_t: BTreeMap<Token, T>,
        _impl_fields_t: BTreeMap<Token, T>,
        _spec_fields_t: BTreeMap<Token, T>,
        _common_impl_fields_t: BTreeMap<Token, T>,
        _common_spec_fields_t: BTreeMap<Token, T>,
    ) -> VisitorResult<T, E, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }

    fn sortvar(&mut self, _id: &mut usize) -> VisitorResult<T, E, IvySort> {
        Ok(ControlMut::Produce(T::default()))
    }
}

impl<T, E> Visitable<T, E> for IvySort
where
    T: Default,
    E: Error,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T, E>) -> VisitorResult<T, E, IvySort> {
        let t = match self {
            IvySort::Uninterpreted => visitor.uninterpreted(),
            IvySort::This => visitor.this(),
            IvySort::Unit => visitor.unit(),
            IvySort::Top => visitor.top(),
            IvySort::Bool => visitor.bool(),
            IvySort::Number => visitor.number(),
            IvySort::BitVec(width) => visitor.bitvec(width),
            IvySort::Vector(t) => {
                let t_ret = t.visit(visitor)?.modifying(t);
                visitor.vector(t, t_ret)
            }
            IvySort::Range(lo, hi) => visitor.range(*lo, *hi),
            IvySort::Enum(discs) => visitor.enumeration(discs),
            IvySort::Action(fargnames, ref mut fargsorts, ref mut ret) => {
                let farg_t = match fargsorts {
                    ActionArgs::Unknown => None,
                    ActionArgs::List(sorts) => Some(
                        sorts
                            .iter_mut()
                            .map(|s| Ok(s.visit(visitor)?.modifying(s)))
                            .collect::<Result<Vec<_>, _>>()?,
                    ),
                };
                let ret_t = match ret {
                    crate::typechecker::sorts::ActionRet::Unknown => todo!(),
                    crate::typechecker::sorts::ActionRet::Unit => {
                        let mut s = IvySort::Unit;
                        s.visit(visitor)?.modifying(&mut s)
                    }
                    crate::typechecker::sorts::ActionRet::Named(binding) => {
                        binding.decl.visit(visitor)?.modifying(&mut binding.decl)
                    }
                };

                /*ret.visit(visitor)?.modifying(ret)?;*/
                visitor.action(fargnames.clone(), fargsorts, ret, farg_t, ret_t)
            }
            IvySort::Relation(args) => {
                let args_t = args
                    .iter_mut()
                    .map(|s| Ok(s.visit(visitor)?.modifying(s)))
                    .collect::<Result<Vec<_>, _>>()?;
                visitor.relation(args, args_t)
            }
            IvySort::Subclass(cls) => visitor.subclass(cls),
            IvySort::Module(module) => {
                let args_t = module
                    .args
                    .iter_mut()
                    .map(|(name, s)| Ok((name.clone(), s.visit(visitor)?.modifying(s))))
                    .collect::<Result<Vec<_>, _>>()?;
                let fields_t = module
                    .fields
                    .iter_mut()
                    .map(|(k, v)| Ok((k.clone(), v.visit(visitor)?.modifying(v))))
                    .collect::<Result<BTreeMap<_, _>, _>>()?;
                visitor.module(module, args_t, fields_t)
            }
            IvySort::Object(_) => todo!(),
            IvySort::SortVar(id) => visitor.sortvar(id),
        }?
        .modifying(self);
        Ok(ControlMut::Produce(t))
    }
}

/// Something that can be visited by a Visitor.
pub trait Visitable<T, E, U = T>
where
    Self: Sized,
    E: Error,
{
    fn visit(&mut self, visitor: &mut dyn Visitor<T, E>) -> VisitorResult<U, E, IvySort>;
}
