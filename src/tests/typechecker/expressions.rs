#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{
        ast::expressions::Expr,
        parser::ivy::{IvyParser, Rule},
        typechecker::{
            inference::TypeChecker,
            sorts::{IvySort, Process},
            TypeError,
        },
        visitor::visitor::Visitable,
    };
    use pest_consume::Parser;

    #[test]
    fn test_bool_binop() {
        let prog = "1 < 2";
        let parsed = IvyParser::parse(Rule::expr, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut binop = IvyParser::expr(parsed).expect("AST generation failed");

        let mut tc = TypeChecker::new();
        let res = binop.visit(&mut tc).unwrap().modifying(&mut binop).unwrap();
        assert_eq!(res, IvySort::Bool);
    }

    #[test]
    fn test_numeric_binop() {
        let prog = "1 + 2";
        let parsed = IvyParser::parse(Rule::expr, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut binop = IvyParser::expr(parsed).expect("AST generation failed");

        let mut tc = TypeChecker::new();
        let res = binop.visit(&mut tc).unwrap().modifying(&mut binop).unwrap();
        assert_eq!(res, IvySort::Number);
    }

    #[test]
    fn test_invalid_numeric_binop() {
        let prog = "1 + true";
        let parsed = IvyParser::parse(Rule::expr, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut binop = IvyParser::expr(parsed).expect("AST generation failed");

        let mut tc = TypeChecker::new();
        let res = binop.visit(&mut tc);
        assert_eq!(
            res.unwrap_err().downcast::<TypeError>().unwrap(),
            TypeError::UnificationError(IvySort::Number, IvySort::Bool)
        )
    }

    #[test]
    fn test_ident() {
        let prog = "foo";
        let parsed = IvyParser::parse(Rule::expr, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut identop = IvyParser::expr(parsed).expect("AST generation failed");

        // Unbound identifiers should not be resolvable.

        let mut tc = TypeChecker::new();
        let res = identop.visit(&mut tc);
        assert_eq!(
            res.unwrap_err().downcast::<TypeError>().unwrap(),
            TypeError::UnboundVariable("foo".into())
        );

        // Bindings at the top level should be resolvable.
        tc.bindings.append("foo".into(), IvySort::Number).unwrap();
        assert_eq!(
            identop
                .visit(&mut tc)
                .unwrap()
                .modifying(&mut identop)
                .unwrap(),
            IvySort::Number
        );

        // Now try pushing a scope and shadow the same variable
        tc.bindings.push_scope();
        tc.bindings.append("foo".into(), IvySort::Bool).unwrap();
        assert_eq!(
            identop
                .visit(&mut tc)
                .unwrap()
                .modifying(&mut identop)
                .unwrap(),
            IvySort::Bool
        );

        // Redundantly shadowing a variable in the current scope
        // is for the moment fine if it's the same type...
        tc.bindings.append("foo".into(), IvySort::Bool).unwrap();

        assert_eq!(
            tc.bindings.append("foo".into(), IvySort::Number),
            Err(TypeError::SortMismatch {
                expected: IvySort::Bool,
                actual: IvySort::Number
            })
        )
    }

    #[test]
    fn test_call_resolved() {
        let prog = "f()";
        let parsed = IvyParser::parse(Rule::expr, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut callop = IvyParser::expr(parsed).expect("AST generation failed");

        let mut tc = TypeChecker::new();
        tc.bindings
            .append(
                "f".into(),
                IvySort::Function(vec![], Box::new(IvySort::Number)),
            )
            .unwrap();
        let res = callop
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut callop)
            .expect("mutation");
        assert_eq!(res, IvySort::Number);
    }

    #[test]
    fn test_call_unresolved() {
        let prog = "f()";
        let parsed = IvyParser::parse(Rule::expr, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut callop = IvyParser::expr(parsed).expect("AST generation failed");

        let mut tc = TypeChecker::new();
        let var = tc.bindings.new_sortvar();
        tc.bindings.append("f".into(), var).unwrap();
        let res = callop
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut callop)
            .unwrap();
        assert_eq!(res, IvySort::SortVar(1));
    }

    #[test]
    fn test_call_invalid() {
        let prog = "f()";
        let parsed = IvyParser::parse(Rule::expr, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut callop = IvyParser::expr(parsed).expect("AST generation failed");

        let mut tc = TypeChecker::new();
        tc.bindings.append("f".into(), IvySort::Number).unwrap();

        let res = callop
            .visit(&mut tc)
            .unwrap_err()
            .downcast::<TypeError>()
            .unwrap();
        assert_eq!(res, TypeError::InvalidApplication(IvySort::Number))
    }

    #[test]
    fn test_field_access() {
        let prog = "a.b";
        let parsed = IvyParser::parse(Rule::expr, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut getop = IvyParser::expr(parsed).expect("AST generation failed");

        // Accessing 'b' should be fine when 'a' is bound to a Process.
        let procsort = Process {
            args: vec![],
            impl_fields: HashMap::from([("b".into(), IvySort::Bool)]),
            spec_fields: HashMap::from([]),
            commonspec_fields: HashMap::from([]),
        };

        let mut tc = TypeChecker::new();
        tc.bindings
            .append("a".into(), IvySort::Process(procsort))
            .unwrap();

        let res = getop
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut getop)
            .unwrap();
        assert_eq!(res, IvySort::Bool);

        // But it doesn't make sense to access 'b' if 'a' is a scalar type.
        tc.bindings.push_scope();
        tc.bindings.append("a".into(), IvySort::Number).unwrap();

        let res = getop
            .visit(&mut tc)
            .unwrap_err()
            .downcast::<TypeError>()
            .unwrap();
        assert_eq!(res, TypeError::NotARecord(IvySort::Number));
    }
}
