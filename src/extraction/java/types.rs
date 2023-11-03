use crate::{
    ast::expressions::{self, Token},
    typechecker::sorts::{IvySort, Module},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JavaType {
    Boolean,
    Char,
    Long,
    BoundedSequence(i64, i64),
    Enum(Vec<Token>),
    ArrayList(Box<JavaType>),
    Map(Vec<JavaType>, Box<JavaType>),
    Object(String, Vec<JavaType>),
    Void,
}

impl JavaType {
    pub fn object(typ: String) -> Self {
        Self::Object(typ, vec![])
    }

    pub fn melina_generator(&self) -> String {
        match self {
            JavaType::Boolean => todo!(),
            JavaType::Char => todo!(),
            JavaType::Long => "ctx.randomLong()".into(),
            JavaType::BoundedSequence(min, max) => format!("ctx.randomBounded({}, {})", min, max),
            JavaType::Enum(discs) => format!("ctx.randomBounded(0, {}", discs.len()),
            JavaType::ArrayList(_) => todo!(),
            JavaType::Map(_, _) => todo!(),
            JavaType::Object(name, _) => name.clone(),
            JavaType::Void => todo!(),
        }
    }

    pub fn as_jref(&self) -> String {
        match self {
            JavaType::Boolean => "Boolean".into(),
            JavaType::Char => "Character".into(),
            JavaType::Long => "Long".into(),
            JavaType::BoundedSequence(_, _) => "Long".into(),
            JavaType::Enum(_) => "Integer".into(),
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
            JavaType::Map(keys, val) => {
                let keys = keys
                    .iter()
                    .map(|t| t.as_jref())
                    .collect::<Vec<_>>()
                    .join(",");
                let val = val.as_jref();
                format!("HashMap<{},{}>", keys, val)
            }
            JavaType::Void => "Void".into(),
        }
    }

    pub fn as_jval(&self) -> String {
        match self {
            JavaType::Boolean => "boolean".into(),
            JavaType::Char => "char".into(),
            JavaType::Long => "long".into(),
            JavaType::BoundedSequence(_, _) => "long".into(),
            _ => self.as_jref(),
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
            IvySort::BoundedSequence(lo, hi) => Self::BoundedSequence(lo, hi),
            IvySort::Enum(discs) => Self::Enum(discs),
            IvySort::Action(_, _, _, _) => todo!(),
            IvySort::Map(keys, val) => {
                let keys: Vec<JavaType> = keys.into_iter().map(|sort| sort.into()).collect::<_>();
                let val: JavaType = val.as_ref().into();
                Self::Map(keys, Box::new(val))
            }
            IvySort::Class(cls) => Self::Object(cls.name, vec![]),
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
            IvySort::BoundedSequence(lo, hi) => Self::BoundedSequence(*lo, *hi),
            IvySort::Enum(discs) => Self::Enum(discs.clone()),
            IvySort::Action(_, _, _, _) => todo!(),
            IvySort::Map(keys, val) => {
                let keys: Vec<JavaType> = keys.into_iter().map(|sort| sort.into()).collect::<_>();
                let val: JavaType = val.as_ref().into();
                Self::Map(keys, Box::new(val))
            }
            IvySort::Class(_) => todo!(),
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
