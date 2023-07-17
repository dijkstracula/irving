#![allow(dead_code)]

use std::collections::BTreeMap;

use crate::{
    ast::expressions::{Expr, Symbol},
    visitor::{sort::Visitor, ControlMut},
};

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct Process {
    pub args: BTreeMap<Symbol, IvySort>,
    pub fields: BTreeMap<Symbol, IvySort>,
    pub actions: BTreeMap<Symbol, IvySort>,
}

// TODO: this module is non-monomorphized (e.g. module type parameters are
// still in the argument list).  We're good with this??
#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct Module {
    pub name: Symbol,
    pub args: Vec<(Symbol, IvySort)>, // Each of these will be SortVars
    pub fields: BTreeMap<String, IvySort>,
}

impl Module {
    pub fn init_action_sort() -> IvySort {
        IvySort::function_sort(vec![], IvySort::Unit)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub enum Fargs {
    Unknown, /* Still to be unified. */
    List(Vec<IvySort>),
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub enum IvySort {
    Uninterpreted,
    This,
    Unit,
    Top,
    Bool,
    Number,
    BitVec(u8),
    Vector(Box<IvySort>),
    Range(Box<Expr>, Box<Expr>),
    Enum(Vec<Symbol>),
    Function(Fargs, Box<IvySort>),
    Relation(Vec<IvySort>),
    Subclass(Symbol),
    Module(Module),
    Process(Process),

    // A SortVar contains the index of its referrent into the typing context.
    SortVar(usize),
}

impl IvySort {
    pub fn function_sort(args: Vec<IvySort>, ret: IvySort) -> IvySort {
        IvySort::Function(Fargs::List(args), Box::new(ret))
    }

    pub fn range_sort(lo: Expr, hi: Expr) -> IvySort {
        IvySort::Range(Box::new(lo), Box::new(hi))
    }

    pub fn is_sortvar(&self) -> bool {
        match self {
            IvySort::SortVar(_) => true,
            _ => false,
        }
    }
}

impl Default for IvySort {
    fn default() -> Self {
        IvySort::SortVar(31337)
    }
}

pub struct SortSubstituter {
    mapping: BTreeMap<IvySort, IvySort>,
}

impl SortSubstituter {
    pub fn new(mapping: BTreeMap<IvySort, IvySort>) -> Self {
        Self { mapping }
    }

    fn subst(&self, sort: IvySort) -> crate::visitor::VisitorResult<IvySort, IvySort> {
        match self.mapping.get(&sort) {
            Some(replacement) => Ok(ControlMut::Produce(replacement.clone())),
            None => Ok(ControlMut::Produce(sort)),
        }
    }
}

impl Visitor<IvySort> for SortSubstituter {
    fn uninterpreted(&mut self) -> crate::visitor::VisitorResult<IvySort, IvySort> {
        self.subst(IvySort::Uninterpreted)
    }

    fn this(&mut self) -> crate::visitor::VisitorResult<IvySort, IvySort> {
        self.subst(IvySort::This)
    }

    fn unit(&mut self) -> crate::visitor::VisitorResult<IvySort, IvySort> {
        self.subst(IvySort::Unit)
    }

    fn top(&mut self) -> crate::visitor::VisitorResult<IvySort, IvySort> {
        self.subst(IvySort::Top)
    }

    fn bool(&mut self) -> crate::visitor::VisitorResult<IvySort, IvySort> {
        self.subst(IvySort::Bool)
    }

    fn number(&mut self) -> crate::visitor::VisitorResult<IvySort, IvySort> {
        self.subst(IvySort::Number)
    }

    fn bitvec(&mut self, width: &mut u8) -> crate::visitor::VisitorResult<IvySort, IvySort> {
        self.subst(IvySort::BitVec(*width))
    }

    fn vector(
        &mut self,
        _original_elem: &mut IvySort,
        substituted_elem: IvySort,
    ) -> crate::visitor::VisitorResult<IvySort, IvySort> {
        self.subst(IvySort::Vector(Box::new(substituted_elem)))
    }

    fn range(
        &mut self,
        lo: &mut Expr,
        hi: &mut Expr,
    ) -> crate::visitor::VisitorResult<IvySort, IvySort> {
        self.subst(IvySort::Range(Box::new(lo.clone()), Box::new(hi.clone())))
    }

    fn enumeration(
        &mut self,
        discriminants: &mut Vec<Symbol>,
    ) -> crate::visitor::VisitorResult<IvySort, IvySort> {
        self.subst(IvySort::Enum(discriminants.clone()))
    }

    fn function(
        &mut self,
        _args: &mut Fargs,
        _ret: &mut IvySort,
        args_t: Option<Vec<IvySort>>,
        ret_t: IvySort,
    ) -> crate::visitor::VisitorResult<IvySort, IvySort> {
        let args = match args_t {
            None => Fargs::Unknown,
            Some(args) => Fargs::List(args),
        };
        self.subst(IvySort::Function(args, Box::new(ret_t)))
    }

    fn relation(
        &mut self,
        _args: &mut Vec<IvySort>,
        substituted: Vec<IvySort>,
    ) -> crate::visitor::VisitorResult<IvySort, IvySort> {
        self.subst(IvySort::Relation(substituted))
    }

    fn subclass(&mut self, cname: &mut Symbol) -> crate::visitor::VisitorResult<IvySort, IvySort> {
        self.subst(IvySort::Subclass(cname.clone()))
    }

    fn module(
        &mut self,
        m: &mut Module,
        args_t: Vec<(String, IvySort)>,
        fields_t: BTreeMap<String, IvySort>,
    ) -> crate::visitor::VisitorResult<IvySort, IvySort> {
        self.subst(IvySort::Module(Module {
            name: m.name.clone(),
            args: args_t,
            fields: fields_t,
        }))
    }

    fn process(
        &mut self,
        _proc: &mut Process,
        _args_t: BTreeMap<Symbol, IvySort>,
        _impl_fields_t: BTreeMap<Symbol, IvySort>,
        _spec_fields_t: BTreeMap<Symbol, IvySort>,
        _common_impl_fields_t: BTreeMap<Symbol, IvySort>,
        _common_spec_fields_t: BTreeMap<Symbol, IvySort>,
    ) -> crate::visitor::VisitorResult<IvySort, IvySort> {
        todo!()
    }

    fn sortvar(&mut self, id: &mut usize) -> crate::visitor::VisitorResult<IvySort, IvySort> {
        self.subst(IvySort::SortVar(*id))
    }
}
