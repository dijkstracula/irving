use crate::{
    ast::expressions::Expr,
    typechecker::sorts::{IvySort, Module},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JavaType {
    Boolean,
    Char,
    Long,
    BoundedLong(i64, i64),
    ArrayList(Box<JavaType>),
    Object(String, Vec<JavaType>),
    Void,
}

impl JavaType {
    pub fn object(typ: String) -> Self {
        Self::Object(typ, vec![])
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
            IvySort::Range(lo, hi) => match (*lo, *hi) {
                (Expr::Number(lo), Expr::Number(hi)) => Self::BoundedLong(lo, hi),
                _ => todo!(),
            },
            IvySort::Enum(_) => todo!(),
            IvySort::Function(_, _) => todo!(),
            IvySort::Relation(_) => todo!(),
            IvySort::Subclass(_) => todo!(),
            IvySort::Module(Module { name, args, .. }) => {
                let args: Vec<JavaType> =
                    args.into_iter().map(|(_, sort)| sort.into()).collect::<_>();
                Self::Object(name, args)
            }
            IvySort::Process(_) => todo!(),
            IvySort::SortVar(_) => todo!(),
        }
    }
}
