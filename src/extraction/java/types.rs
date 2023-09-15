use crate::{
    ast::expressions::{self},
    typechecker::sorts::{IvySort, Module},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JavaType {
    Boolean,
    Char,
    Long,
    Range(i64, i64),
    ArrayList(Box<JavaType>),
    Object(String, Vec<JavaType>),
    Void,
}

impl JavaType {
    pub fn object(typ: String) -> Self {
        Self::Object(typ, vec![])
    }

    pub fn as_jref(&self) -> String {
        match self {
            JavaType::Boolean => "Boolean".into(),
            JavaType::Char => "Character".into(),
            JavaType::Long => "Long".into(),
            JavaType::Range(_, _) => "Long".into(),
            JavaType::ArrayList(t) => format!("ArrayList<{}>", t.as_jref()),
            JavaType::Object(clazz, ts) => {
                if ts.is_empty() {
                    clazz.clone()
                } else {
                    format!(
                        "{}<{}>",
                        clazz,
                        ts.iter().map(|t| t.as_jref()).collect::<Vec<_>>().join(",")
                    )
                }
            }
            JavaType::Void => "Void".into(),
        }
    }

    pub fn as_jval(&self) -> String {
        match self {
            JavaType::Boolean => "boolean".into(),
            JavaType::Char => "char".into(),
            JavaType::Long => "long".into(),
            JavaType::Range(_, _) => "long".into(),
            _ => self.as_jref(),
        }
    }

    // TODO: these should just call into the stdlib on the Melina side.
    // Can we assume that `random` is in scope?
    // https://github.com/dijkstracula/irving/issues/61
    pub fn generator(&self) -> String {
        match self {
            JavaType::Boolean => "random::nextBoolean()".into(),
            JavaType::Char => "() -> new Character(random::nextInt(0, 256))".into(),
            JavaType::Long => "random::nextLong()".into(),
            JavaType::Range(_, _) => todo!(),
            JavaType::ArrayList(_) => todo!(),
            JavaType::Object(_, _) => todo!(),
            JavaType::Void => todo!(),
        }
    }
}

impl From<IvySort> for JavaType {
    fn from(value: IvySort) -> Self {
        match value {
            IvySort::Uninterpreted => Self::Long,
            IvySort::Unit => Self::Void,
            IvySort::This => todo!(),
            IvySort::Top => Self::object("Object".into()),
            IvySort::Bool => Self::Boolean,
            IvySort::Number => Self::Long,
            IvySort::BitVec(width) => {
                if width < 64 {
                    Self::Long
                } else if width < 8 {
                    Self::Char
                } else {
                    todo!()
                }
            }
            IvySort::Vector(elem_type) => {
                Self::ArrayList(Box::new(Into::<JavaType>::into(*elem_type)))
            }
            IvySort::Range(lo, hi) => Self::Range(lo, hi),
            IvySort::Enum(_) => todo!(),
            IvySort::Action(_, _, _, _) => todo!(),
            IvySort::Relation(_) => todo!(),
            IvySort::Subclass(_) => todo!(),
            IvySort::Module(Module { name, args, .. }) => {
                let args: Vec<JavaType> =
                    args.into_iter().map(|(_, sort)| sort.into()).collect::<_>();
                Self::Object(name, args)
            }
            IvySort::Object(_) => todo!(),
            IvySort::SortVar(_) => todo!(),
        }
    }
}

impl From<&IvySort> for JavaType {
    fn from(value: &IvySort) -> Self {
        match value {
            IvySort::Uninterpreted => Self::Long,
            IvySort::Unit => Self::Void,
            IvySort::This => todo!(),
            IvySort::Top => Self::object("Object".into()),
            IvySort::Bool => Self::Boolean,
            IvySort::Number => Self::Long,
            IvySort::BitVec(width) => {
                if *width < 64 {
                    Self::Long
                } else if *width < 8 {
                    Self::Char
                } else {
                    todo!()
                }
            }
            IvySort::Vector(elem_type) => {
                let jelem: JavaType = elem_type.as_ref().into();
                Self::ArrayList(Box::new(jelem))
            }
            IvySort::Range(lo, hi) => Self::Range(*lo, *hi),
            IvySort::Enum(_) => todo!(),
            IvySort::Action(_, _, _, _) => todo!(),
            IvySort::Relation(_) => todo!(),
            IvySort::Subclass(_) => todo!(),
            IvySort::Module(Module { name, args, .. }) => {
                let args: Vec<JavaType> = args.iter().map(|(_, sort)| sort.into()).collect::<_>();
                Self::Object(name.clone(), args)
            }
            IvySort::Object(_) => todo!(),
            IvySort::SortVar(_) => todo!(),
        }
    }
}

impl From<&expressions::Sort> for JavaType {
    fn from(s: &expressions::Sort) -> Self {
        match s {
            expressions::Sort::ToBeInferred => panic!("Uninferred sort"),
            expressions::Sort::Annotated(ident) => panic!("Unresolved sort {ident:?}"),
            expressions::Sort::Resolved(ivysort) => {
                ivysort.clone().into() // XXX: poor choices lead to this clone.a
            }
        }
    }
}
