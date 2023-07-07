#[cfg(test)]
mod tests {
    use crate::{
        ast::expressions::Expr,
        typechecker::{
            sorts::{IvySort, Module},
            unifier::Resolver,
            TypeError,
        },
    };

    fn resolver_with_bindings() -> Resolver {
        let mut r = Resolver::new();

        // type pid: 0..3
        r.append(
            "pid".into(),
            IvySort::Range(Box::new(Expr::Number(0)), Box::new(Expr::Number(3))),
        )
        .unwrap();

        r.append(
            "net".into(),
            IvySort::Module(Module {
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
            r.lookup_ident(&vec!("nonsense".to_owned()), true),
            Err(TypeError::UnboundVariable("nonsense".into()))
        );

        let pid_sort = IvySort::Range(Box::new(Expr::Number(0)), Box::new(Expr::Number(3)));
        assert_eq!(r.lookup_sym("pid"), Some(&pid_sort));
        assert_eq!(r.lookup_ident(&vec!("pid".to_owned()), true), Ok(&pid_sort));
        assert_eq!(
            r.lookup_ident(&vec!("pid".to_owned()), false),
            Ok(&pid_sort)
        );

        assert_eq!(
            r.lookup_ident(&vec!("pid".to_owned(), "uhoh".to_owned()), true),
            Err(TypeError::NotARecord(pid_sort.clone()))
        );
    }

    #[test]
    fn test_record_lookup() {
        let r = resolver_with_bindings();

        assert_eq!(
            r.lookup_ident(&vec!("net".to_owned()), true),
            Ok(&IvySort::Module(Module {
                args: [].into(),
                fields: [("socket".into(), IvySort::Number)].into()
            }))
        );

        assert_eq!(
            r.lookup_ident(&vec!("net".to_owned(), "socket".to_owned()), true),
            Ok(&IvySort::Number)
        );
    }
}
