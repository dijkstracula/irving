#[cfg(test)]
mod tests {
    use crate::{
        ast::{declarations::Decl, expressions::Expr},
        parser::ivy::{IvyParser, Rule},
        passes::isolate_normalizer::IsolateNormalizer,
        typechecker::{
            inference::TypeChecker,
            sorts::{Fargs, IvySort},
            TypeError,
        },
        visitor::visitor::Visitable,
    };
    use pest_consume::Parser;

    fn isolate_from_src(prog: &str) -> Decl {
        let res = IvyParser::parse(Rule::isolate_decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut proc = Decl::Isolate(IvyParser::isolate_decl(res).expect("AST generation failed"));

        // Since we'll be assuming that the normalizer has run prior to
        // typechecking, enforce it here.
        let mut norm = IsolateNormalizer::new();
        proc.visit(&mut norm).unwrap().modifying(&mut proc).unwrap();
        proc
    }

    fn decl_from_src(src: &str) -> Decl {
        let parsed = IvyParser::parse(Rule::decl, &src)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(parsed).expect("AST generation failed")
    }

    fn expr_from_src(src: &str) -> Expr {
        let parsed = IvyParser::parse(Rule::expr, &src)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::expr(parsed).expect("AST generation failed")
    }

    #[test]
    fn test_noop_action_decl() {
        let mut decl = decl_from_src("action a() {}");
        let sort = IvySort::Function(Fargs::List(vec![]), Box::new(IvySort::Unit));

        let mut tc = TypeChecker::new();
        let res = decl.visit(&mut tc).unwrap().modifying(&mut decl).unwrap();
        assert_eq!(res, sort);

        assert_eq!(tc.bindings.lookup_sym(&"a".to_owned()), Some(&sort));
    }

    #[test]
    fn test_noop_action_decl_with_local() {
        let mut decl = decl_from_src("action a() { var b = 42 } ");

        let mut tc = TypeChecker::new();
        let res = decl.visit(&mut tc).unwrap().modifying(&mut decl).unwrap();
        assert_eq!(
            res,
            IvySort::Function(Fargs::List(vec!()), Box::new(IvySort::Unit))
        );

        // Make sure that `b` does not escape the local context.
        assert_eq!(tc.bindings.lookup_sym(&"b".to_owned()), None);
    }

    #[test]
    fn test_ident_action_decl() {
        let mut decl = decl_from_src("action id(x: bool) returns (b: bool) = { b := x }");

        let mut tc = TypeChecker::new();
        let res = decl.visit(&mut tc).unwrap().modifying(&mut decl).unwrap();
        assert_eq!(
            res,
            IvySort::Function(Fargs::List(vec!(IvySort::Bool)), Box::new(IvySort::Bool))
        );

        assert_eq!(
            tc.bindings.lookup_sym(&"id".to_owned()),
            Some(&IvySort::Function(
                Fargs::List(vec!(IvySort::Bool)),
                Box::new(IvySort::Bool)
            ))
        );

        // Make sure that `x` and `b` do not escape the local context.
        assert_eq!(tc.bindings.lookup_sym(&"b".to_owned()), None);
        assert_eq!(tc.bindings.lookup_sym(&"x".to_owned()), None);
    }

    #[test]
    fn test_action_forward_ref() {
        let mut action_decl = decl_from_src("action id(a: bool) returns (b: bool)");
        let mut imp_decl = decl_from_src("implement id { b := a }");

        let mut tc = TypeChecker::new();

        action_decl
            .visit(&mut tc)
            .unwrap()
            .modifying(&mut action_decl)
            .unwrap();
        imp_decl
            .visit(&mut tc)
            .unwrap()
            .modifying(&mut imp_decl)
            .unwrap();

        assert_eq!(
            tc.bindings.lookup_sym(&"id".to_owned()),
            Some(&IvySort::Function(
                Fargs::List(vec!(IvySort::Bool)),
                Box::new(IvySort::Bool)
            ))
        );
    }

    #[test]
    fn test_unknown_ident_in_action_impl() {
        let mut action_decl = decl_from_src("action id(a: bool) returns (b: bool)");
        let mut imp_decl = decl_from_src("implement id { foo := a }");

        let mut tc = TypeChecker::new();

        action_decl
            .visit(&mut tc)
            .unwrap()
            .modifying(&mut action_decl)
            .unwrap();

        assert_eq!(
            imp_decl
                .visit(&mut tc)
                .unwrap_err()
                .downcast::<TypeError>()
                .unwrap(),
            TypeError::UnboundVariable("foo".into())
        )
    }

    #[test]
    fn test_action_before_after() {
        let mut action_decl = decl_from_src("action id(a: bool) returns (b: bool) { b := a }");
        let mut before_decl = decl_from_src("before id { require a = false | a = true }");
        let mut after_decl = decl_from_src("after id { ensure a = b }");

        let mut tc = TypeChecker::new();

        action_decl
            .visit(&mut tc)
            .unwrap()
            .modifying(&mut action_decl)
            .unwrap();
        before_decl
            .visit(&mut tc)
            .unwrap()
            .modifying(&mut before_decl)
            .unwrap();
        after_decl
            .visit(&mut tc)
            .unwrap()
            .modifying(&mut after_decl)
            .unwrap();

        assert_eq!(
            tc.bindings.lookup_sym(&"id".to_owned()),
            Some(&IvySort::Function(
                Fargs::List(vec!(IvySort::Bool)),
                Box::new(IvySort::Bool)
            ))
        );

        // Make sure that `a` and `b` do not escape the local context.
        assert_eq!(tc.bindings.lookup_sym(&"b".to_owned()), None);
        assert_eq!(tc.bindings.lookup_sym(&"a".to_owned()), None);
    }

    #[test]
    fn test_inconsistent_mixin() {
        let mut action_decl = decl_from_src("action const_true returns (b: bool) { b := true }");
        let mut before_decl = decl_from_src("after const_true(uhoh: bool) { }");

        let mut tc = TypeChecker::new();

        action_decl
            .visit(&mut tc)
            .unwrap()
            .modifying(&mut action_decl)
            .unwrap();

        before_decl.visit(&mut tc).unwrap_err();
    }

    #[test]
    fn test_action_call_nullary_action() {
        let mut prog = isolate_from_src(
            "process m = {
            type this
            alias t = this

            # doit's first argument is not `this`, so field access
            # should not modify the action signature.
            action doit returns (y: bool)
        }",
        );

        let mut tc = TypeChecker::new();
        let _ = prog.visit(&mut tc).unwrap().modifying(&mut prog).unwrap();

        // The type of the action should be nullary to a bool.
        let action_sort = tc
            .bindings
            .lookup_ident(&vec!["m".into(), "doit".into()], true)
            .unwrap()
            .clone();
        assert_eq!(action_sort, IvySort::function_sort(vec![], IvySort::Bool));

        // Applying the action should produce a bool.
        let mut action_app = expr_from_src("m.doit()");
        let res = action_app
            .visit(&mut tc)
            .unwrap()
            .modifying(&mut action_app)
            .unwrap();
        assert_eq!(res, IvySort::Bool);
    }

    #[test]
    fn test_action_call_unary() {
        let mut prog = isolate_from_src(
            "process m = {
            type this
            alias t = this
            action doit(x: bool) returns (y: bool)
        }",
        );

        let mut tc = TypeChecker::new();
        let _ = prog.visit(&mut tc).unwrap().modifying(&mut prog).unwrap();

        let action_sort = tc
            .bindings
            .lookup_ident(&vec!["m".into(), "doit".into()], true)
            .unwrap()
            .clone();
        assert_eq!(
            action_sort,
            IvySort::function_sort(vec![IvySort::Bool], IvySort::Bool)
        );

        let mut action_app = expr_from_src("m.doit()");
        let err = action_app.visit(&mut tc).unwrap_err();
        assert_eq!(
            err.downcast::<TypeError>().unwrap(),
            TypeError::LenMismatch([IvySort::Bool].into(), vec!())
        );

        let mut action_app = expr_from_src("m.doit(42)");
        let err = action_app.visit(&mut tc).unwrap_err();
        assert_eq!(
            err.downcast::<TypeError>().unwrap(),
            TypeError::UnificationError(IvySort::Bool, IvySort::Number)
        );

        let mut action_app = expr_from_src("m.doit(true)");
        let res = action_app
            .visit(&mut tc)
            .unwrap()
            .modifying(&mut action_app)
            .unwrap();
        assert_eq!(res, IvySort::Bool);
    }

    #[test]
    fn test_action_call_curry_this() {
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

        let mut tc = TypeChecker::new();
        let isolate_sort = prog.visit(&mut tc).unwrap().modifying(&mut prog).unwrap();

        let action_sort = tc
            .bindings
            .lookup_ident(&vec!["m".into(), "doit".into()], true)
            .unwrap()
            .clone();
        assert_eq!(
            action_sort,
            IvySort::function_sort(vec![IvySort::SortVar(0)], IvySort::Bool)
        );

        let mut action_app = expr_from_src("m.doit()");
        let res = action_app
            .visit(&mut tc)
            .unwrap()
            .modifying(&mut action_app)
            .unwrap();
        assert_eq!(res, IvySort::Bool);

        let mut action_app = expr_from_src("m.doit(m)");
        let err = action_app.visit(&mut tc).unwrap_err();
        assert_eq!(
            err.downcast::<TypeError>().unwrap(),
            TypeError::LenMismatch(vec!(), [isolate_sort].into())
        );
    }
}
