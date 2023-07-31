#[cfg(test)]
mod tests {
    use crate::ast::actions::{Action, AssignAction};
    use crate::ast::expressions::*;
    use crate::passes::clone_emitter::CloneEmitter;
    use crate::tests::helpers;
    use crate::visitor::ast::Visitable;

    #[test]
    fn clone_emit_assign_numeric_literal() {
        let mut action = helpers::action_from_src("a := 42");
        let expected = Action::Assign(AssignAction {
            lhs: Expr::inferred_progsym("a"),
            rhs: Expr::Number(42),
        });
        assert_eq!(action, expected);

        let mut ce = CloneEmitter::new();
        action
            .visit(&mut ce)
            .unwrap()
            .modifying(&mut action)
            .unwrap();
        assert_eq!(action, expected);
    }

    #[test]
    fn clone_emit_assign_ident() {
        let mut action = helpers::action_from_src("b := a");
        assert_eq!(
            action,
            Action::Assign(AssignAction {
                lhs: Expr::inferred_progsym("b"),
                rhs: Expr::inferred_progsym("a"),
            })
        );

        let mut ce = CloneEmitter::new();
        action
            .visit(&mut ce)
            .unwrap()
            .modifying(&mut action)
            .unwrap();
        assert_eq!(
            action,
            Action::Assign(AssignAction {
                lhs: Expr::inferred_progsym("b"),
                rhs: Expr::Clone(Box::new(Expr::inferred_progsym("a"))),
            })
        );
    }

    #[test]
    fn clone_emit_assign_field_access() {
        let mut action = helpers::action_from_src("b := a.x");
        assert_eq!(
            action,
            Action::Assign(AssignAction {
                lhs: Expr::inferred_progsym("b"),
                rhs: Expr::FieldAccess(FieldAccess {
                    record: Box::new(Expr::inferred_progsym("a")),
                    field: Symbol {
                        id: "x".into(),
                        sort: Sort::ToBeInferred
                    }
                })
            })
        );

        let mut ce = CloneEmitter::new();
        action
            .visit(&mut ce)
            .unwrap()
            .modifying(&mut action)
            .unwrap();
        assert_eq!(
            action,
            Action::Assign(AssignAction {
                lhs: Expr::inferred_progsym("b"),
                rhs: Expr::Clone(Box::new(Expr::FieldAccess(FieldAccess {
                    record: Box::new(Expr::inferred_progsym("a")),
                    field: Symbol {
                        id: "x".into(),
                        sort: Sort::ToBeInferred
                    }
                })))
            })
        );
    }

    #[test]
    fn test_fn_app() {
        let mut action = helpers::rval_from_src("f(a, 123)");
        let mut ce = CloneEmitter::new();
        action
            .visit(&mut ce)
            .unwrap()
            .modifying(&mut action)
            .unwrap();

        assert_eq!(
            action,
            Expr::App(AppExpr {
                func: Box::new(Expr::inferred_progsym("f")),
                args: vec!(
                    // Clone wrapper
                    Expr::Clone(Box::new(Expr::inferred_progsym("a"))),
                    // No clone wrapper
                    Expr::Number(123)
                )
            })
        );
    }
}
