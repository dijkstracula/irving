#[cfg(test)]
mod tests {
    use crate::{
        parser::ivy::{IvyParser, Rule},
        typechecker::{
            inference::TypeChecker,
            sorts::{Fargs, IvySort},
            TypeError,
        },
        visitor::visitor::Visitable,
    };
    use pest_consume::Parser;

    #[test]
    fn test_noop_action_decl() {
        let prog = "action a() {}";
        let parsed = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut decl = IvyParser::decl(parsed).expect("AST generation failed");
        let sort = IvySort::Function(Fargs::List(vec![]), Box::new(IvySort::Unit));

        let mut tc = TypeChecker::new();
        let res = decl.visit(&mut tc).unwrap().modifying(&mut decl).unwrap();
        assert_eq!(res, sort);

        assert_eq!(tc.bindings.lookup_sym(&"a".to_owned()), Some(&sort));
    }

    #[test]
    fn test_noop_action_decl_with_local() {
        let prog = "action a() { var b = 42 } ";
        let parsed = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut decl = IvyParser::decl(parsed).expect("AST generation failed");

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
        let prog = "action id(x: bool) returns (b: bool) = { b := x }";
        let parsed = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut decl = IvyParser::decl(parsed).expect("AST generation failed");

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
        let prog = "action id(a: bool) returns (b: bool)";
        let parsed = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut action_decl = IvyParser::decl(parsed).expect("AST generation failed");

        let prog = "implement id { b := a }";
        let parsed = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut imp_decl = IvyParser::decl(parsed).expect("AST generation failed");

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
        let prog = "action id(a: bool) returns (b: bool)";
        let parsed = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut action_decl = IvyParser::decl(parsed).expect("AST generation failed");

        let prog = "implement id { foo := a }";
        let parsed = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut imp_decl = IvyParser::decl(parsed).expect("AST generation failed");

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
        let prog = "action id(a: bool) returns (b: bool) { b := a }";
        let parsed = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut action_decl = IvyParser::decl(parsed).expect("AST generation failed");

        let prog = "before id { require a = false | a = true }";
        let parsed = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut before_decl = IvyParser::decl(parsed).expect("AST generation failed");
        let prog = "after id { ensure a = b }";
        let parsed = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut after_decl = IvyParser::decl(parsed).expect("AST generation failed");

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
        let prog = "action const_true returns (b: bool) { b := true }";
        let parsed = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut action_decl = IvyParser::decl(parsed).expect("AST generation failed");

        let prog = "after const_true(uhoh: bool) { }";
        let parsed = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut before_decl = IvyParser::decl(parsed).expect("AST generation failed");

        let mut tc = TypeChecker::new();

        action_decl
            .visit(&mut tc)
            .unwrap()
            .modifying(&mut action_decl)
            .unwrap();

        before_decl.visit(&mut tc).unwrap_err();
    }
}
