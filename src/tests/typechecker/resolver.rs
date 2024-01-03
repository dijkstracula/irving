#[cfg(test)]
mod tests {
    use crate::{
        ast::{declarations::Binding, span::Span},
        typechecker::{
            sorts::{
                ActionArgs, ActionKind,
                ActionRet::{self, Named},
                IvySort, Module,
            },
            unifier::{BindingResolver, ResolverError},
        },
    };

    fn resolver_with_bindings() -> BindingResolver {
        let mut r = BindingResolver::new();

        // type pid: 0..3
        r.append("pid".into(), IvySort::BoundedSequence(0, 3))
            .unwrap();

        r.append(
            "net".into(),
            IvySort::Module(Module {
                name: "net".into(),
                args: [].into(),
                fields: [("socket".into(), IvySort::Number)].into(),
            }),
        )
        .unwrap();

        r
    }

    #[test]
    fn test_nonrecord_lookup() {
        let r = resolver_with_bindings();

        assert_eq!(r.lookup_sym("nonsense"), None);
        assert_eq!(
            r.lookup_ident(&vec!("nonsense".to_owned())),
            Err(ResolverError::UnboundVariable("nonsense".into()))
        );

        let pid_sort = IvySort::BoundedSequence(0, 3);
        assert_eq!(r.lookup_sym("pid"), Some(&pid_sort));
        assert_eq!(r.lookup_ident(&vec!("pid".to_owned())), Ok(&pid_sort));
        assert_eq!(r.lookup_ident(&vec!("pid".to_owned())), Ok(&pid_sort));

        assert_eq!(
            r.lookup_ident(&vec!("pid".to_owned(), "uhoh".to_owned())),
            Err(ResolverError::NotARecord(pid_sort))
        );
    }

    #[test]
    fn test_record_lookup() {
        let r = resolver_with_bindings();

        assert_eq!(
            r.lookup_ident(&vec!("net".to_owned())),
            Ok(&IvySort::Module(Module {
                name: "net".into(),
                args: [].into(),
                fields: [("socket".into(), IvySort::Number)].into()
            }))
        );

        assert_eq!(
            r.lookup_ident(&vec!("net".to_owned(), "socket".to_owned())),
            Ok(&IvySort::Number)
        );
    }

    #[test]
    fn test_path_lookup() {
        let mut r = BindingResolver::new();
        r.push_named_scope("a");
        r.push_named_scope("b");
        r.pop_named_scope();
        r.push_named_scope("x");
        r.push_anonymous_scope();
        r.push_named_scope("y");
        r.push_anonymous_scope();
        r.pop_anonymous_scope();
        r.push_anonymous_scope();

        assert_eq!(
            r.named_scope_path(),
            vec!("a".to_string(), "x".to_string(), "y".to_string())
        );
    }

    #[test]
    fn fresh_sortvars() {
        let mut r = BindingResolver::new();

        // Primitive case: we can create distinct sortvars.
        let si = r.new_sortvar();
        let si2 = r.fresh_sortvars(&si);
        assert_ne!(si, si2);

        // Compound case: we can recurse into complicated sorts
        let si = IvySort::Module(Module {
            name: "array".into(),
            args: vec![
                Binding::from("domain", si, Span::IgnoredForTesting), 
                Binding::from("range", si2, Span::IgnoredForTesting)],
            fields: [
                ("this".into(), IvySort::This),
                ("t".into(), IvySort::This),
                ("init".into(), Module::init_action_sort()),
            ]
            .into(),
        });
        let si2 = r.fresh_sortvars(&si);
        assert_ne!(si, si2);
    }

    #[test]
    fn sortvar_unification() {
        let mut r = BindingResolver::new();
        let t = r.new_sortvar();
        assert!(r.unify(&t, &IvySort::Number).is_ok());
        assert!(r.unify(&t, &IvySort::Bool).is_err());
    }

    #[test]
    fn generic_unification() {
        let mut r = BindingResolver::new();
        let t = r.new_generic("t");
        assert!(r.unify(&t, &IvySort::Number).is_ok());
        assert!(r.unify(&t, &IvySort::Bool).is_err());
    }

    #[test]
    fn fresh_sortvar_consistent_mapping() {
        let mut r = BindingResolver::new();
        let t = r.new_sortvar();
        assert_eq!(t, IvySort::SortVar(0));

        // Note that we have a consistent use of SortVar(0) here.
        let si = IvySort::Module(Module {
            name: "box".into(),
            args: vec![Binding::from("range", t.clone(), Span::IgnoredForTesting)],
            fields: [
                ("this".into(), IvySort::This),
                ("t".into(), IvySort::This),
                ("init".into(), Module::init_action_sort()),
                (
                    "get".into(),
                    IvySort::Action(
                        vec![],
                        ActionArgs::List(vec![]),
                        Named(Box::new(Binding::from(
                            "ret",
                            t.clone(),
                            Span::IgnoredForTesting,
                        ))),
                        ActionKind::Exported,
                    ),
                ),
                (
                    "put".into(),
                    IvySort::Action(
                        vec!["val".to_owned()],
                        ActionArgs::List(vec![t.clone()]),
                        ActionRet::Unit,
                        ActionKind::Exported,
                    ),
                ),
            ]
            .into(),
        });
        let si2 = r.fresh_sortvars(&si);
        let t = IvySort::SortVar(1);
        assert_eq!(
            si2,
            IvySort::Module(Module {
                name: "box".into(),
                args: vec![Binding::from("range", t.clone(), Span::IgnoredForTesting)],
                fields: [
                    ("this".into(), IvySort::This),
                    ("t".into(), IvySort::This),
                    ("init".into(), Module::init_action_sort()),
                    (
                        "get".into(),
                        IvySort::Action(
                            vec![],
                            ActionArgs::List(vec!()),
                            Named(Box::new(Binding::from(
                                "ret",
                                t.clone(),
                                Span::IgnoredForTesting
                            ))),
                            ActionKind::Exported
                        )
                    ),
                    (
                        "put".into(),
                        IvySort::Action(
                            vec!("val".to_owned()),
                            ActionArgs::List(vec!(t.clone())),
                            ActionRet::Unit,
                            ActionKind::Exported
                        )
                    )
                ]
                .into(),
            })
        );
    }
}
