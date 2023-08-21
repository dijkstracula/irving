#[cfg(test)]
mod tests {
    use crate::{
        ast::{
            declarations::{ActionDecl, Binding, Decl},
            expressions::{Expr, Sort},
            span::Span,
            statements::Stmt,
        },
        parser::ivy::{IvyParser, Rule},
        typechecker::{
            inference::SortInferer,
            sorts::{self, IvySort},
            unifier::ResolverError,
        },
        visitor::ast::Visitable,
    };
    use pest_consume::Parser;

    fn isolate_from_src(prog: &str) -> Decl {
        let res = IvyParser::parse_with_userdata(Rule::process_decl, prog, prog.into())
            .expect("Parsing failed")
            .single()
            .unwrap();
        let (span, decl) = IvyParser::process_decl(res).expect("AST generation failed");
        Decl::Object { span, decl }
    }

    fn decl_from_src(src: &str) -> Decl {
        let parsed = IvyParser::parse_with_userdata(Rule::decl, src, src.into())
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(parsed).expect("AST generation failed")
    }

    fn expr_from_src(src: &str) -> Expr {
        let parsed = IvyParser::parse_with_userdata(Rule::rval, src, src.into())
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::rval(parsed).expect("AST generation failed")
    }

    #[test]
    fn noop_action_decl() {
        let mut decl = decl_from_src("action a() {}");
        let sort = IvySort::action_sort(vec![], vec![], sorts::ActionRet::Unit);

        let mut tc = SortInferer::new();
        let res = decl.visit(&mut tc).unwrap().modifying(&mut decl);
        assert_eq!(res, sort);

        assert_eq!(tc.bindings.lookup_sym("a"), Some(&sort));
    }

    #[test]
    fn noop_action_decl_with_local() {
        let mut decl = decl_from_src("action a() { var b = 42 } ");

        let mut tc = SortInferer::new();
        let res = decl.visit(&mut tc).unwrap().modifying(&mut decl);
        assert_eq!(
            res,
            IvySort::action_sort(vec!(), vec!(), sorts::ActionRet::Unit)
        );

        // Make sure that `b` does not escape the local context.
        assert_eq!(tc.bindings.lookup_sym("b"), None);
    }

    #[test]
    fn ident_action_decl() {
        let mut decl = decl_from_src("action id(x: bool) returns (b: bool) = { b := x }");

        let mut tc = SortInferer::new();
        let res = decl.visit(&mut tc).unwrap().modifying(&mut decl);
        assert_eq!(
            res,
            IvySort::action_sort(
                vec!("x".into()),
                vec!(IvySort::Bool),
                sorts::ActionRet::named("b", IvySort::Bool)
            )
        );

        assert_eq!(
            tc.bindings.lookup_sym("id"),
            Some(&IvySort::action_sort(
                vec!("x".into()),
                vec!(IvySort::Bool),
                sorts::ActionRet::named("b", IvySort::Bool)
            ))
        );

        // Make sure that `x` and `b` do not escape the local context.
        assert_eq!(tc.bindings.lookup_sym("b"), None);
        assert_eq!(tc.bindings.lookup_sym("x"), None);
    }

    #[test]
    fn action_forward_ref() {
        let mut action_decl = decl_from_src("action id(a: bool) returns (b: bool)");
        let mut imp_decl = decl_from_src("implement id { b := a }");

        let mut tc = SortInferer::new();

        action_decl
            .visit(&mut tc)
            .unwrap()
            .modifying(&mut action_decl);
        imp_decl.visit(&mut tc).unwrap().modifying(&mut imp_decl);

        assert_eq!(
            tc.bindings.lookup_sym("id"),
            Some(&IvySort::action_sort(
                vec!("a".into()),
                vec!(IvySort::Bool),
                sorts::ActionRet::named("b", IvySort::Bool)
            ))
        );
    }

    #[test]
    fn unknown_ident_in_action_impl() {
        let mut action_decl = decl_from_src("action id(a: bool) returns (b: bool)");
        let mut imp_decl = decl_from_src("implement id { foo := a }");

        let mut tc = SortInferer::new();

        action_decl
            .visit(&mut tc)
            .unwrap()
            .modifying(&mut action_decl);

        assert_eq!(
            imp_decl.visit(&mut tc).unwrap_err(),
            ResolverError::UnboundVariable("foo".into()).to_typeerror(&Span::IgnoredForTesting)
        )
    }

    #[test]
    fn action_before_after() {
        let mut action_decl = decl_from_src("action id(a: bool) returns (b: bool) { b := a }");
        let mut before_decl = decl_from_src("before id { require a = false | a = true }");
        let mut after_decl = decl_from_src("after id { ensure a = b }");

        let mut tc = SortInferer::new();

        action_decl
            .visit(&mut tc)
            .unwrap()
            .modifying(&mut action_decl);
        before_decl
            .visit(&mut tc)
            .unwrap()
            .modifying(&mut before_decl);
        after_decl
            .visit(&mut tc)
            .unwrap()
            .modifying(&mut after_decl);

        assert_eq!(
            tc.bindings.lookup_sym("id"),
            Some(&IvySort::action_sort(
                vec!("a".into()),
                vec!(IvySort::Bool),
                sorts::ActionRet::named("b", IvySort::Bool)
            ))
        );

        // Make sure that `a` and `b` do not escape the local context.
        assert_eq!(tc.bindings.lookup_sym("b"), None);
        assert_eq!(tc.bindings.lookup_sym("a"), None);
    }

    #[test]
    fn inconsistent_mixin() {
        let mut action_decl = decl_from_src("action const_true returns (b: bool) { b := true }");
        let mut before_decl = decl_from_src("after const_true(uhoh: bool) { }");

        let mut tc = SortInferer::new();

        action_decl
            .visit(&mut tc)
            .unwrap()
            .modifying(&mut action_decl);

        before_decl.visit(&mut tc).unwrap_err();
    }

    #[test]
    fn action_call_nullary_action() {
        let mut prog = isolate_from_src(
            "process m = {
            type this
            alias t = this

            # doit's first argument is not `this`, so field access
            # should not modify the action signature.
            action doit returns (y: bool)
        }",
        );

        let mut tc = SortInferer::new();
        let _ = prog.visit(&mut tc).unwrap().modifying(&mut prog);

        // The type of the action should be nullary to a bool.
        let action_sort = tc
            .bindings
            .lookup_ident(&vec!["m".into(), "doit".into()])
            .unwrap()
            .clone();
        assert_eq!(
            action_sort,
            IvySort::action_sort(vec![], vec![], sorts::ActionRet::named("y", IvySort::Bool))
        );

        // Applying the action should produce a bool.
        let mut action_app = expr_from_src("m.doit()");
        let res = action_app
            .visit(&mut tc)
            .unwrap()
            .modifying(&mut action_app);
        assert_eq!(res, IvySort::Bool);
    }

    #[test]
    fn action_call_unary() {
        let mut prog = isolate_from_src(
            "process m = {
            type this
            alias t = this
            action doit(x: bool) returns (y: bool)
        }",
        );

        let mut tc = SortInferer::new();
        let _ = prog.visit(&mut tc).unwrap().modifying(&mut prog);

        let action_sort = tc
            .bindings
            .lookup_ident(&vec!["m".into(), "doit".into()])
            .unwrap()
            .clone();
        assert_eq!(
            action_sort,
            IvySort::action_sort(
                vec!("x".into()),
                vec![IvySort::Bool],
                sorts::ActionRet::named("y", IvySort::Bool)
            )
        );

        let mut action_app = expr_from_src("m.doit()");
        let err = action_app.visit(&mut tc).unwrap_err();
        assert_eq!(
            err,
            ResolverError::LenMismatch(1, 0).to_typeerror(&Span::IgnoredForTesting)
        );

        let mut action_app = expr_from_src("m.doit(42)");
        let err = action_app.visit(&mut tc).unwrap_err();
        assert_eq!(
            err,
            ResolverError::UnificationError(IvySort::Bool, IvySort::Number)
                .to_typeerror(&Span::IgnoredForTesting)
        );

        let mut action_app = expr_from_src("m.doit(true)");
        let res = action_app
            .visit(&mut tc)
            .unwrap()
            .modifying(&mut action_app);
        assert_eq!(res, IvySort::Bool);
    }

    #[test]
    fn action_call_curry_this() {
        let mut prog = isolate_from_src(
            "process m = {
            type this
            alias t = this

            # Note that the first argument being `this` means that it is
            # morally a `() -> bool`, since the LHS of a field access to
            # make the call is implicitly going to be the first argument.

            action doit(x: t) returns (y: bool)
        }",
        );

        let mut tc = SortInferer::new();
        let _ = prog.visit(&mut tc).unwrap().modifying(&mut prog);

        let action_sort = tc
            .bindings
            .lookup_ident(&vec!["m".into(), "doit".into()])
            .unwrap()
            .clone();
        assert_eq!(
            action_sort,
            IvySort::action_sort(
                vec!["x".into()],
                vec![IvySort::This],
                sorts::ActionRet::named("y", IvySort::Bool)
            )
        );

        let mut action_app = expr_from_src("m.doit()");
        let res = action_app
            .visit(&mut tc)
            .unwrap()
            .modifying(&mut action_app);
        assert_eq!(res, IvySort::Bool);

        let mut action_app = expr_from_src("m.doit(m)");
        let err = action_app.visit(&mut tc).unwrap_err();
        assert_eq!(
            err,
            ResolverError::LenMismatch(0, 1).to_typeerror(&Span::Todo)
        );
    }

    #[test]
    fn implicit_nullary_action_call() {
        let mut prog = isolate_from_src(
            "process m = {
            type this
            alias t = this

            # A nullary action.  I should be able to invoke this action
            # either as `m.doit()` or `m.doit`!
            action doit returns (y: bool)
        }",
        );

        let mut tc = SortInferer::new();
        let _ = prog.visit(&mut tc).unwrap().modifying(&mut prog);

        let action_sort = tc
            .bindings
            .lookup_ident(&vec!["m".into(), "doit".into()])
            .unwrap()
            .clone();
        assert_eq!(
            action_sort,
            IvySort::action_sort(vec![], vec![], sorts::ActionRet::named("y", IvySort::Bool))
        );

        let mut action_app = expr_from_src("m.doit()");
        let res = action_app
            .visit(&mut tc)
            .unwrap()
            .modifying(&mut action_app);
        assert_eq!(res, IvySort::Bool);

        /*
        TODO: https://github.com/dijkstracula/irving/issues/36
        let mut action_app = expr_from_src("m.doit");
        let res = action_app
            .visit(&mut tc)
            .unwrap()
            .modifying(&mut action_app);
        assert_eq!(res, IvySort::Bool);
        */
    }

    #[test]
    fn local_vardecl() {
        let stmts = match decl_from_src(
            "action doit = {
                var foo: bool;
            }
        }",
        ) {
            Decl::Action {
                span: _,
                decl:
                    Binding {
                        decl:
                            ActionDecl {
                                body: Some(stmts), ..
                            },
                        ..
                    },
            } => stmts,
            decl => panic!("Got back a {:?} rater than a Decl::Action", decl),
        };
        assert_eq!(
            stmts.get(0),
            Some(&Stmt::VarDecl(Binding {
                name: "foo".into(),
                decl: Sort::Annotated(["bool".into()].into())
            }))
        );
    }

    #[test]
    fn local_vardecl_and_init() {
        let mut prog = decl_from_src(
            "action doit = {
                var foo: bool;
                foo := true;
            }
        }",
        );

        let mut tc = SortInferer::new();
        let _ = prog.visit(&mut tc).unwrap().modifying(&mut prog);

        let mut prog = decl_from_src(
            "action doit = {
                var foo: bool;
                foo := 42; # uh oh!
            }
        }",
        );
        let _ = prog.visit(&mut tc).unwrap_err();
    }

    #[test]
    fn action_read_relation() {
        let mut iso = isolate_from_src(
            "process foo = {
            type node
            relation failed(X: node)

            action connect(x: node, y: node) = {
                require ~failed(y);
            }
        } ",
        );

        let mut tc = SortInferer::new();
        let _ = iso.visit(&mut tc).unwrap().modifying(&mut iso);
    }

    #[test]
    fn action_read_relation_logical() {
        let mut iso = isolate_from_src(
            "process foo = {
            type node
            relation failed(X: node)

            after init {
                require ~failed(N);
            }
        } ",
        );

        let mut tc = SortInferer::new();
        let _ = iso.visit(&mut tc).unwrap().modifying(&mut iso);
    }
}
