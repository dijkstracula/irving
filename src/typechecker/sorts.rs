#![allow(dead_code)]

use std::{collections::BTreeMap, fmt::Display};

use crate::{
    ast::{declarations::Binding, expressions::Token, span::Span},
    visitor::{sort::Visitor, ControlMut},
};

use super::{InferenceResult, TypeError};

// Classes

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct Class {
    pub name: String,
    pub parent: Option<Box<IvySort>>,
    pub actions: BTreeMap<Token, IvySort>,
    pub fields: BTreeMap<Token, IvySort>,
}

// Objects, parameterized and singletons

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct Object {
    pub args: Vec<Binding<IvySort>>,
    pub fields: BTreeMap<Token, IvySort>,
}

// Modules

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
        ActionRet::Named(Box::new(Binding::from(
            name.into(),
            sort,
            Span::IgnoredForTesting,
        )))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub enum ActionKind {
    /// A "callback" into the environment.
    Imported,

    /// An exposed action that the environment may choose to call.
    /// Implicitly an instance action.
    Exported,

    /// The default state of an action: tied to a particular instance
    /// of a parameterized action.
    Instance,

    /// A "static" action of sorts.
    Common,

    /// For unification purposes
    Unknown,
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
    Range(i64, i64),
    Enum(Vec<Token>),
    Action(Vec<Token>, ActionArgs, ActionRet, ActionKind),
    Relation(Vec<IvySort>),
    Class(Class),
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
            IvySort::Action(_, args, ret, _) => {
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
        IvySort::Action(
            arg_names,
            ActionArgs::List(arg_sorts),
            ret,
            ActionKind::Instance,
        )
    }

    pub fn exported_action_sort(
        arg_names: Vec<Token>,
        arg_sorts: Vec<IvySort>,
        ret: ActionRet,
    ) -> IvySort {
        IvySort::Action(
            arg_names,
            ActionArgs::List(arg_sorts),
            ret,
            ActionKind::Exported,
        )
    }

    pub fn imported_action_sort(
        arg_names: Vec<Token>,
        arg_sorts: Vec<IvySort>,
        ret: ActionRet,
    ) -> IvySort {
        IvySort::Action(
            arg_names,
            ActionArgs::List(arg_sorts),
            ret,
            ActionKind::Imported,
        )
    }

    pub fn common_action_sort(
        arg_names: Vec<Token>,
        arg_sorts: Vec<IvySort>,
        ret: ActionRet,
    ) -> IvySort {
        IvySort::Action(
            arg_names,
            ActionArgs::List(arg_sorts),
            ret,
            ActionKind::Common,
        )
    }

    pub fn is_sortvar(&self) -> bool {
        match self {
            IvySort::SortVar(_) => true,
            _ => false,
        }
    }

    pub fn desc(&self) -> String {
        match self {
            IvySort::Uninterpreted => "uninterpreted".into(),
            IvySort::This => "this".into(),
            IvySort::Unit => "unit".into(),
            IvySort::Top => "top".into(),
            IvySort::Bool => "boolean".into(),
            IvySort::Number => "unbounded_sequence".into(),
            IvySort::BitVec(_) => "bitvec".into(),
            IvySort::Vector(_) => "vector".into(),
            IvySort::Range(_, _) => "range".into(),
            IvySort::Enum(_) => "enum".into(),
            IvySort::Action(_, _, _, _) => "action".into(),
            IvySort::Relation(_) => "relation".into(),
            IvySort::Class(cls) => cls.name.clone(),
            IvySort::Module(_) => "module".into(),
            IvySort::Object(_) => "object".into(),
            IvySort::SortVar(_) => "sortvar".into(),
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
        kind: &mut ActionKind,
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

        self.subst(IvySort::Action(arg_syms, args, ret, kind.clone()))
    }

    fn vector(
        &mut self,
        _original_elem: &mut IvySort,
        substituted_elem: IvySort,
    ) -> InferenceResult<IvySort> {
        self.subst(IvySort::Vector(Box::new(substituted_elem)))
    }

    fn range(&mut self, lo: i64, hi: i64) -> InferenceResult<IvySort> {
        self.subst(IvySort::Range(lo, hi))
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

    fn class(
        &mut self,
        name: String,
        parent: Option<Box<IvySort>>,
        actions: BTreeMap<Token, IvySort>,
        fields: BTreeMap<Token, IvySort>,
    ) -> InferenceResult<IvySort> {
        self.subst(IvySort::Class(Class {
            name,
            parent,
            actions,
            fields,
        }))
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
