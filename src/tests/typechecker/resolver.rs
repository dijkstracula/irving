#[cfg(test)]
mod tests {
    use crate::{
        ast::{expressions::Expr, span::Span},
        typechecker::{
            sorts::{IvySort, Module},
            unifier::{BindingResolver, ResolverError},
        },
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
}
