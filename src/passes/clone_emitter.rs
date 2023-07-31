use crate::{
    ast::{
        actions::{self, Action, AssignAction},
        expressions::{self, Expr},
    },
    visitor::{ast::Visitor, ControlMut, VisitorResult},
};

/// In order to preserve call-by-value semantics when extracting to languages
/// with call-by-reference semantics, we have to provide a mechanism by which
/// the extraction backend will ensure a distinct unaliased version of some
/// identifier.
///
/// Per the discussion here: https://github.com/dijkstracula/irving/issues/44
/// A clone needs to happen when a preexisting value, already bound to a name,
/// is rebound to a new name.  The cases to consider, therefore, are:
///
/// 1) Assignments when the right-hand side expression is bound to an existing
/// identifier;
/// 2) Action calls when a given free variable is already bound to an existing
/// identifier;
/// 3) Return values when the value is bound to an existing identifier.  (This
/// is a special case of 1), when the existing identifier is the distinguished
/// return parameter, I think, so we don't have to handle it separately.)
///
/// TODO: I'd like to not actually call this "clone" -- having to clone the
/// backing data is an implementation detail for a particular extraction
/// backend.  It's really about ensuring that something isn't an alias of
/// something else; is there a sensible name that somehow reflects that?
pub struct CloneEmitter;

impl CloneEmitter {
    pub fn new() -> Self {
        Self
    }

    fn clone_if_ident(e: &Expr) -> Option<Expr> {
        match e {
            Expr::App(_)
            | Expr::FieldAccess(_)
            | Expr::Index(_)
            | Expr::LogicSymbol(_)
            | Expr::ProgramSymbol(_)
            | Expr::This => Some(Expr::Clone(Box::new(e.clone()))),
            _ => None,
        }
    }
}
impl Visitor<()> for CloneEmitter {
    fn finish_assign(
        &mut self,
        ast: &mut actions::AssignAction,
        _lhs_t: (),
        _rhs_t: (),
    ) -> VisitorResult<(), Action> {
        if let Some(cloned) = CloneEmitter::clone_if_ident(&ast.rhs) {
            Ok(ControlMut::Mutation(
                Action::Assign(AssignAction {
                    lhs: ast.lhs.clone(),
                    rhs: cloned,
                }),
                (),
            ))
        } else {
            Ok(ControlMut::Produce(()))
        }
    }

    fn finish_app(
        &mut self,
        ast: &mut expressions::AppExpr,
        _f: (),
        _args: Vec<()>,
    ) -> VisitorResult<(), Expr> {
        for arg in &mut ast.args {
            if let Some(cloned) = CloneEmitter::clone_if_ident(&arg) {
                *arg = cloned;
            }
        }
        Ok(ControlMut::Produce(()))
    }
}
