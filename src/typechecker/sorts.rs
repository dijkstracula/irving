#![allow(dead_code)]

use std::{collections::BTreeMap, fmt::Display};

use crate::{
    ast::{
        declarations::Binding,
        expressions::{Expr, Token},
    },
    visitor::{sort::Visitor, ControlMut},
};

use super::{InferenceResult, TypeError};

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct Object {
    pub args: BTreeMap<Token, IvySort>,
    pub fields: BTreeMap<Token, IvySort>,
}

// TODO: this module is non-monomorphized (e.g. module type parameters are
// still in the argument list).  We're good with this??
#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct Module {
    pub name: Token,
    pub args: Vec<(Token, IvySort)>, // Each of these will be SortVars
    pub fields: BTreeMap<String, IvySort>,
}

impl Module {
    pub fn init_action_sort() -> IvySort {
        IvySort::action_sort(vec![], vec![], ActionRet::Unit)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub enum ActionArgs {
    Unknown, /* Still to be unified. */
    List(Vec<IvySort>),
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub enum ActionRet {
    Unknown,                      /* Still to be unified. */
    Unit,                         /* An implicit Void-producing action */
    Named(Box<Binding<IvySort>>), /* An explicit (possibly Void-producing) return type */
}

impl ActionRet {
    pub fn named<S>(name: S, sort: IvySort) -> Self
    where
        S: Into<String>,
    {
        ActionRet::Named(Box::new(Binding::from(name.into(), sort)))
    }
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
    Enum(Vec<Token>),
    Action(Vec<Token>, ActionArgs, ActionRet),
    Relation(Vec<IvySort>),
    Subclass(Token),
    Module(Module),
    Object(Object),

    // A SortVar contains the index of its referrent into the typing context.
    SortVar(usize),
}

impl Display for IvySort {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IvySort::BitVec(width) => write!(f, "BitVec({width})"),
            IvySort::Vector(elm) => write!(f, "vector({elm})"),
            IvySort::Range(min, max) => write!(f, "{{{:?}..{:?}}}", min, max),
            IvySort::Enum(discs) => write!(f, "{{ ... {} discriminants ... }}", discs.len()),
            IvySort::Action(_, args, ret) => {
                write!(f, "(")?;
                match args {
                    ActionArgs::Unknown => write!(f, "?")?,
                    ActionArgs::List(args) => {
                        for (i, a) in args.iter().enumerate() {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "{}", a)?;
                        }
                    }
                }
                write!(f, ")")?;

                match ret {
                    ActionRet::Unknown => write!(f, " -> ?"),
                    ActionRet::Unit => write!(f, " -> unit"),
                    ActionRet::Named(binding) => write!(f, "-> {}", binding.decl),
                }
            }
            IvySort::Relation(args) => {
                write!(f, "relation({:#?})", args)
            }
            IvySort::Module(module) => write!(f, "{}", module.name),
            _ => write!(f, "{}", self.desc()),
        }
    }
}

impl IvySort {
    pub fn action_sort(arg_names: Vec<Token>, arg_sorts: Vec<IvySort>, ret: ActionRet) -> IvySort {
        // TODO: Do we want this?  Seems like a panic is strictly worse than constructing
        // the sort and then letting unification fail shortly thereafter.
        //if arg_names.len() != arg_sorts.len() {
        //    panic!("mismatch between arg_names and arg_sorts");
        //}

        IvySort::Action(arg_names, ActionArgs::List(arg_sorts), ret)
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

    pub fn desc(&self) -> &'static str {
        match self {
            IvySort::Uninterpreted => "uninterpreted",
            IvySort::This => "this",
            IvySort::Unit => "unit",
            IvySort::Top => "top",
            IvySort::Bool => "boolean",
            IvySort::Number => "unbounded_sequence",
            IvySort::BitVec(_) => "bitvec",
            IvySort::Vector(_) => "vector",
            IvySort::Range(_, _) => "range",
            IvySort::Enum(_) => "enum",
            IvySort::Action(_, _, _) => "action",
            IvySort::Relation(_) => "relation",
            IvySort::Subclass(_) => "object",
            IvySort::Module(_) => "module",
            IvySort::Object(_) => "object",
            IvySort::SortVar(_) => "sortvar",
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

    fn subst(&self, sort: IvySort) -> InferenceResult<IvySort> {
        match self.mapping.get(&sort) {
            Some(replacement) => Ok(ControlMut::Produce(replacement.clone())),
            None => Ok(ControlMut::Produce(sort)),
        }
    }
}

impl Visitor<IvySort, TypeError> for SortSubstituter {
    fn uninterpreted(&mut self) -> InferenceResult<IvySort> {
        self.subst(IvySort::Uninterpreted)
    }

    fn this(&mut self) -> InferenceResult<IvySort> {
        self.subst(IvySort::This)
    }

    fn unit(&mut self) -> InferenceResult<IvySort> {
        self.subst(IvySort::Unit)
    }

    fn top(&mut self) -> InferenceResult<IvySort> {
        self.subst(IvySort::Top)
    }

    fn bool(&mut self) -> InferenceResult<IvySort> {
        self.subst(IvySort::Bool)
    }

    fn number(&mut self) -> InferenceResult<IvySort> {
        self.subst(IvySort::Number)
    }

    fn bitvec(&mut self, width: &mut u8) -> InferenceResult<IvySort> {
        self.subst(IvySort::BitVec(*width))
    }

    fn action(
        &mut self,
        arg_syms: Vec<Token>,
        _args: &mut ActionArgs,
        ret: &mut ActionRet,
        args_t: Option<Vec<IvySort>>,
        ret_t: IvySort,
    ) -> InferenceResult<IvySort> {
        let args = match args_t {
            None => ActionArgs::Unknown,
            Some(args) => ActionArgs::List(args),
        };

        let ret = match ret_t {
            IvySort::Unit => ActionRet::Unit,
            s => match ret {
                ActionRet::Unknown | ActionRet::Unit => unreachable!(),
                ActionRet::Named(b) => ActionRet::named(b.name.clone(), s),
            },
        };

        self.subst(IvySort::Action(arg_syms, args, ret))
    }

    fn vector(
        &mut self,
        _original_elem: &mut IvySort,
        substituted_elem: IvySort,
    ) -> InferenceResult<IvySort> {
        self.subst(IvySort::Vector(Box::new(substituted_elem)))
    }

    fn range(&mut self, lo: &mut Expr, hi: &mut Expr) -> InferenceResult<IvySort> {
        self.subst(IvySort::Range(Box::new(lo.clone()), Box::new(hi.clone())))
    }

    fn enumeration(&mut self, discriminants: &mut Vec<Token>) -> InferenceResult<IvySort> {
        self.subst(IvySort::Enum(discriminants.clone()))
    }

    fn relation(
        &mut self,
        _args: &mut Vec<IvySort>,
        substituted: Vec<IvySort>,
    ) -> InferenceResult<IvySort> {
        self.subst(IvySort::Relation(substituted))
    }

    fn subclass(&mut self, cname: &mut Token) -> InferenceResult<IvySort> {
        self.subst(IvySort::Subclass(cname.clone()))
    }

    fn module(
        &mut self,
        m: &mut Module,
        args_t: Vec<(String, IvySort)>,
        fields_t: BTreeMap<String, IvySort>,
    ) -> InferenceResult<IvySort> {
        self.subst(IvySort::Module(Module {
            name: m.name.clone(),
            args: args_t,
            fields: fields_t,
        }))
    }

    fn object(
        &mut self,
        _proc: &mut Object,
        _args_t: BTreeMap<Token, IvySort>,
        _impl_fields_t: BTreeMap<Token, IvySort>,
        _spec_fields_t: BTreeMap<Token, IvySort>,
        _common_impl_fields_t: BTreeMap<Token, IvySort>,
        _common_spec_fields_t: BTreeMap<Token, IvySort>,
    ) -> InferenceResult<IvySort> {
        todo!()
    }

    fn sortvar(&mut self, id: &mut usize) -> InferenceResult<IvySort> {
        self.subst(IvySort::SortVar(*id))
    }
}
