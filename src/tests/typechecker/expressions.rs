#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use crate::{
        ast::{
            declarations::Binding,
            expressions::{self, Sort},
            span::Span,
        },
        tests::helpers,
        typechecker::{
            inference::SortInferer,
            sorts::{self, ActionArgs, IvySort, Module, Object},
            unifier::ResolverError,
            TypeError,
        },
        visitor::ast::Visitable,
    };

    #[test]
    fn test_unbound_this() {
        let prog = "this";
        let mut ast = helpers::rval_from_src(prog);
        let mut tc = SortInferer::new();

        // `this` has to be explicitly bound from the enclosing context.
        let res = ast.visit(&mut tc).expect_err("visit");
        assert_eq!(res, TypeError::UnboundVariable("this".into()));
    }

    #[test]
    fn test_lt_binop() {
        let prog = "1 < 2";
        let mut ast = helpers::rval_from_src(prog);

        let mut si = SortInferer::new();
        let res = ast.visit(&mut si).unwrap().modifying(&mut ast);
        assert_eq!(res, IvySort::Bool);
    }

    #[test]
    fn test_eq_binop() {
        let prog = "foo = 2";
        let mut ast = helpers::rval_from_src(prog);

        let mut si = SortInferer::new();
        si.bindings.append("foo".into(), IvySort::Number).unwrap();

        let res = ast.visit(&mut si).unwrap().modifying(&mut ast);
        assert_eq!(res, IvySort::Bool);
    }

    #[test]
    fn test_numeric_binop() {
        let prog = "1 + 2";
        let mut ast = helpers::rval_from_src(prog);

        let mut tc = SortInferer::new();
        let res = ast.visit(&mut tc).unwrap().modifying(&mut ast);
        assert_eq!(res, IvySort::Number);
    }

    #[test]
    fn test_invalid_numeric_binop() {
        let prog = "1 + true";
        let mut ast = helpers::rval_from_src(prog);

        let mut tc = SortInferer::new();
        let res = ast.visit(&mut tc);
        assert_eq!(
            res.unwrap_err(),
            TypeError::unification_error(&IvySort::Number, &IvySort::Bool)
        )
    }

    #[test]
    fn test_unary_op() {
        let prog = "~true";
        let mut ast = helpers::rval_from_src(prog);

        let mut tc = SortInferer::new();
        let res = ast.visit(&mut tc).unwrap().modifying(&mut ast);
        assert_eq!(res, IvySort::Bool);
    }

    #[test]
    fn test_ident() {
        let prog = "foo";

        let mut tc = SortInferer::new();

        // Unbound identifiers should not be resolvable.
        let mut identop = helpers::rval_from_src(prog);
        let res = identop.visit(&mut tc);
        assert_eq!(
            res.unwrap_err(),
            TypeError::Spanned {
                span: Span::IgnoredForTesting,
                inner: Box::new(TypeError::UnboundVariable("foo".into()))
            }
        );

        // Bindings at the top level should be resolvable.
        let mut identop = helpers::rval_from_src(prog);
        tc.bindings.append("foo".into(), IvySort::Number).unwrap();
        assert_eq!(
            identop.visit(&mut tc).unwrap().modifying(&mut identop),
            IvySort::Number
        );

        // Now try pushing a scope and shadow the same variable
        let mut identop = helpers::rval_from_src(prog);
        tc.bindings.push_anonymous_scope();
        tc.bindings.append("foo".into(), IvySort::Bool).unwrap();
        assert_eq!(
            identop.visit(&mut tc).unwrap().modifying(&mut identop),
            IvySort::Bool
        );

        // Redundantly shadowing a variable in the current scope
        // is for the moment fine if it's the same type...
        tc.bindings.append("foo".into(), IvySort::Bool).unwrap();

        assert_eq!(
            tc.bindings.append("foo".into(), IvySort::Number),
            Err(ResolverError::ReboundVariable {
                sym: "foo".into(),
                prev: IvySort::Bool,
                new: IvySort::Number,
            })
        )
    }

    #[test]
    fn test_call_resolved() {
        let prog = "f()";
        let mut callop = helpers::rval_from_src(prog);
        assert!(matches!(
            callop,
            expressions::Expr::App {
                expr: expressions::AppExpr {
                    func_sort: Sort::ToBeInferred,
                    ..
                },
                ..
            }
        ));

        let mut tc = SortInferer::new();
        tc.bindings
            .append(
                "f".into(),
                IvySort::action_sort(
                    vec![],
                    vec![],
                    sorts::ActionRet::named("ret", IvySort::Number),
                ),
            )
            .unwrap();
        let res = callop.visit(&mut tc).expect("visit").modifying(&mut callop);

        assert!(matches!(
            callop,
            expressions::Expr::App {
                expr: expressions::AppExpr {
                    func_sort: Sort::Resolved(_),
                    ..
                },
                ..
            }
        ));
        assert_eq!(res, IvySort::Number);
    }

    #[test]
    fn test_call_unresolved() {
        let prog = "f()";
        let mut callop = helpers::rval_from_src(prog);

        let mut tc = SortInferer::new();
        let var = tc.bindings.new_sortvar();
        tc.bindings.append("f".into(), var).unwrap();
        let res = callop.visit(&mut tc).expect("visit").modifying(&mut callop);
        assert_eq!(res, IvySort::SortVar(1));
    }

    #[test]
    fn test_call_invalid() {
        let prog = "f()";
        let mut callop = helpers::rval_from_src(prog);

        let mut tc = SortInferer::new();
        tc.bindings.append("f".into(), IvySort::Number).unwrap();

        let res = callop.visit(&mut tc).unwrap_err();
        assert_eq!(
            res,
            TypeError::Spanned {
                span: Span::IgnoredForTesting,
                inner: Box::new(TypeError::unification_error(
                    &IvySort::Number,
                    &IvySort::Action(
                        vec!(),
                        ActionArgs::List(vec!()),
                        sorts::ActionRet::Unknown,
                        sorts::ActionKind::Unknown
                    )
                ))
            }
        )
    }

    #[test]
    fn test_call_parameterized_obj() {
        let fragment = "host(0)";
        let mut callop = helpers::rval_from_src(fragment);

        let host_sort = IvySort::Object(Object {
            args: [Binding::from(
                "self",
                IvySort::BoundedSequence(0, 3),
                Span::IgnoredForTesting,
            )]
            .into(),
            fields: [("init".to_owned(), Module::init_action_sort())].into(),
        });
        let mut si = SortInferer::new();
        si.bindings.append("host".into(), host_sort).unwrap();

        let indexed_sort = callop.visit(&mut si).unwrap().modifying(&mut callop);

        assert_eq!(
            indexed_sort,
            IvySort::Object(Object {
                args: [].into(), // We've curried out the argument!
                fields: [("init".to_owned(), Module::init_action_sort())].into(),
            })
        );
    }

    #[test]
    fn test_call_parameterized_obj_invalid_parameter() {
        let fragment = "host(true)";
        let mut callop = helpers::rval_from_src(fragment);

        let host_sort = IvySort::Object(Object {
            args: [Binding::from(
                "self",
                IvySort::BoundedSequence(0, 3),
                Span::IgnoredForTesting,
            )]
            .into(),
            fields: [("init".to_owned(), Module::init_action_sort())].into(),
        });
        let mut si = SortInferer::new();
        si.bindings.append("host".into(), host_sort).unwrap();

        let err = callop.visit(&mut si).unwrap_err();
        assert_eq!(
            err,
            TypeError::Spanned {
                span: Span::IgnoredForTesting,
                inner: Box::new(TypeError::UnificationError(
                    "boolean".into(),
                    "{0..3}".into()
                ))
            }
        );
    }

    #[test]
    fn test_field_access() {
        let prog = "a.b";

        let mut tc = SortInferer::new();

        // Accessing 'b' should be fine when 'a' is bound to a Process.
        let procsort = Object {
            args: vec![],
            fields: BTreeMap::from([("b".into(), IvySort::Bool)]),
        };
        tc.bindings
            .append("a".into(), IvySort::Object(procsort))
            .unwrap();

        let mut getop = helpers::rval_from_src(prog);
        let res = getop.visit(&mut tc).expect("visit").modifying(&mut getop);
        assert_eq!(res, IvySort::Bool);

        // But it doesn't make sense to access 'b' if 'a' is a scalar type.
        tc.bindings.push_anonymous_scope();
        tc.bindings.append("a".into(), IvySort::Number).unwrap();

        let mut getop = helpers::rval_from_src(prog);
        let res = getop.visit(&mut tc).unwrap_err();
        assert_eq!(res, TypeError::NotARecord(IvySort::Number.desc()));
    }

    #[test]
    fn test_field_access_call() {
        let prog = "a.b(a.b(0))";

        let mut tc = SortInferer::new();

        let procsort = Object {
            args: vec![],
            fields: BTreeMap::from([(
                "b".into(),
                IvySort::action_sort(
                    vec!["n".into()],
                    vec![IvySort::BoundedSequence(0, 3)],
                    sorts::ActionRet::Named(Box::new(Binding::from(
                        "ret",
                        IvySort::Number,
                        Span::IgnoredForTesting,
                    ))),
                ),
            )]),
        };
        tc.bindings
            .append("a".into(), IvySort::Object(procsort))
            .unwrap();

        let mut callop = helpers::rval_from_src(prog);
        callop.visit(&mut tc).expect("visit").modifying(&mut callop);

        assert!(matches!(
            callop,
            expressions::Expr::App {
                expr: expressions::AppExpr {
                    func_sort: Sort::Resolved(_),
                    ..
                },
                ..
            }
        ));
    }
}
