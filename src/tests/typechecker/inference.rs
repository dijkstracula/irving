#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{
        parser::ivy::{IvyParser, Rule},
        typechecker::{
            inference::TypeChecker,
            sorts::{IvySort, Process},
            Error,
        },
        visitor::{control::Control, visitor::Visitor},
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

        let mut cg = TypeChecker::new();
        let res = cg.visit_expr(&mut binop);
        assert_eq!(res, Ok(Control::Continue(IvySort::Bool)));
    }

    #[test]
    fn test_numeric_binop() {
        let prog = "1 + 2";
        let parsed = IvyParser::parse(Rule::expr, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut binop = IvyParser::expr(parsed).expect("AST generation failed");

        let mut cg = TypeChecker::new();
        let res = cg.visit_expr(&mut binop);
        assert_eq!(res, Ok(Control::Continue(IvySort::Number)));
    }

    #[test]
    fn test_invalid_numeric_binop() {
        let prog = "1 + true";
        let parsed = IvyParser::parse(Rule::expr, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut binop = IvyParser::expr(parsed).expect("AST generation failed");

        let mut cg = TypeChecker::new();
        let res = cg.visit_expr(&mut binop);
        assert_eq!(
            res,
            Err(Error::UnificationError(IvySort::Number, IvySort::Bool))
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

        let mut cg = TypeChecker::new();

        // Unbound identifiers should not be resolvable.
        assert_eq!(
            cg.visit_expr(&mut identop),
            Err(Error::UnboundVariable("foo".into()))
        );

        // Bindings at the top level should be resolvable.
        cg.bindings.append("foo".into(), IvySort::Number).unwrap();
        assert_eq!(
            cg.visit_expr(&mut identop),
            Ok(Control::Continue(IvySort::Number))
        );

        // Now try pushing a scope and shadow the same variable
        cg.bindings.push_scope();
        cg.bindings.append("foo".into(), IvySort::Bool).unwrap();
        assert_eq!(
            cg.visit_expr(&mut identop),
            Ok(Control::Continue(IvySort::Bool))
        );

        // Redundantly shadowing a variable in the current scope
        // is for the moment fine if it's the same type...
        cg.bindings.append("foo".into(), IvySort::Bool).unwrap();

        assert_eq!(
            cg.bindings.append("foo".into(), IvySort::Number),
            Err(Error::SortMismatch {
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

        let mut cg = TypeChecker::new();
        cg.bindings
            .append(
                "f".into(),
                IvySort::Function(vec![], Box::new(IvySort::Number)),
            )
            .unwrap();
        let res = cg.visit_expr(&mut callop);
        assert_eq!(res, Ok(Control::Continue(IvySort::Number)));
    }

    #[test]
    fn test_call_unresolved() {
        let prog = "f()";
        let parsed = IvyParser::parse(Rule::expr, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut callop = IvyParser::expr(parsed).expect("AST generation failed");

        let mut cg = TypeChecker::new();
        let var = cg.new_sortvar();
        cg.bindings.append("f".into(), var).unwrap();
        let res = cg.visit_expr(&mut callop);
        assert_eq!(res, Ok(Control::Continue(IvySort::SortVar(1))));
    }

    #[test]
    fn test_call_invalid() {
        let prog = "f()";
        let parsed = IvyParser::parse(Rule::expr, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut callop = IvyParser::expr(parsed).expect("AST generation failed");

        let mut cg = TypeChecker::new();
        cg.bindings.append("f".into(), IvySort::Number).unwrap();
        let res = cg.visit_expr(&mut callop);
        assert_eq!(res, Err(Error::InvalidApplication(IvySort::Number)))
    }

    #[test]
    fn test_field_access() {
        let prog = "a.b";
        let parsed = IvyParser::parse(Rule::expr, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut getop = IvyParser::expr(parsed).expect("AST generation failed");

        let procsort = Process {
            args: vec![],
            impl_fields: HashMap::from([("b".into(), IvySort::Bool)]),
            spec_fields: HashMap::from([]),
            commonspec_fields: HashMap::from([]),
        };

        let mut cg = TypeChecker::new();
        cg.bindings
            .append("a".into(), IvySort::Process(procsort))
            .unwrap();

        let res = cg.visit_expr(&mut getop);
        assert_eq!(res, Ok(Control::Continue(IvySort::Bool)));
    }
}
