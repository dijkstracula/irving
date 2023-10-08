#[cfg(test)]
mod tests {
    use crate::typechecker::{
        sorts::{IvySort, Module},
        unifier::{BindingResolver, ResolverError},
    };

    fn resolver_with_bindings() -> BindingResolver {
        let mut r = BindingResolver::new();

        // type pid: 0..3
        r.append("pid".into(), IvySort::Range(0, 3)).unwrap();

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

        let pid_sort = IvySort::Range(0, 3);
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

        assert_eq!(r.named_scope_path(), vec!("a".to_string(), "x".to_string(), "y".to_string()));
    }
}
