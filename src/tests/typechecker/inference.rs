#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{
        ast::expressions::Expr,
        parser::ivy::{IvyParser, Rule},
        typechecker::{
            inference::TypeChecker,
            sorts::{IvySort, Module},
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

        // Accessing 'b' should be fine when 'a' is bound to a Process.
        let procsort = Module {
            args: vec![],
            impl_fields: HashMap::from([("b".into(), IvySort::Bool)]),
            spec_fields: HashMap::from([]),
            commonspec_fields: HashMap::from([]),
        };

        let mut cg = TypeChecker::new();
        cg.bindings
            .append("a".into(), IvySort::Module(procsort))
            .unwrap();

        let res = cg.visit_expr(&mut getop);
        assert_eq!(res, Ok(Control::Continue(IvySort::Bool)));

        // But it doesn't make sense to access 'b' if 'a' is a scalar type.
        cg.bindings.push_scope();
        cg.bindings.append("a".into(), IvySort::Number).unwrap();

        let res = cg.visit_expr(&mut getop);
        assert_eq!(res, Err(Error::NotARecord(IvySort::Number)));
    }

    // decls

    #[test]
    fn test_relation_decl_unannotated() {
        let prog = "relation is_up(X, Y)";

        let parsed = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut decl_ast = IvyParser::decl(parsed).expect("AST generation failed");

        let mut tc = TypeChecker::new();
        let _res = tc.visit_decl(&mut decl_ast).unwrap();

        assert_eq!(
            tc.bindings.lookup(&"is_up".to_owned()),
            Some(&IvySort::Function(
                [IvySort::SortVar(0), IvySort::SortVar(1)].into(),
                Box::new(IvySort::Bool)
            ))
        )
    }

    #[test]
    fn test_relation_decl_annotated() {
        let prog = "relation is_up(X: bool, Y: bool)";

        let parsed = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut decl_ast = IvyParser::decl(parsed).expect("AST generation failed");

        let mut tc = TypeChecker::new();
        let _res = tc.visit_decl(&mut decl_ast).unwrap();

        assert_eq!(
            tc.bindings.lookup(&"is_up".to_owned()),
            Some(&IvySort::Function(
                [IvySort::Bool, IvySort::Bool].into(),
                Box::new(IvySort::Bool)
            ))
        )
    }

    #[test]
    fn test_typedecl_uninterp() {
        let prog = "type node";

        let parsed = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut decl_ast = IvyParser::decl(parsed).expect("AST generation failed");

        let mut tc = TypeChecker::new();
        let _res = tc.visit_decl(&mut decl_ast).unwrap();

        assert_eq!(
            tc.bindings.lookup(&"node".to_owned()),
            Some(&IvySort::Uninterpreted)
        )
    }

    #[test]
    fn test_typedecl_enum() {
        let prog = "type status = {on, off}";

        let parsed = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut decl_ast = IvyParser::decl(parsed).expect("AST generation failed");

        let mut tc = TypeChecker::new();
        let _res = tc.visit_decl(&mut decl_ast).unwrap();

        assert_eq!(
            tc.bindings.lookup(&"status".to_owned()),
            Some(&IvySort::Enum(["on".into(), "off".into()].into()))
        )
    }

    #[test]
    fn test_typedecl_range() {
        let prog = "type numbers = {0..100}";

        let parsed = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut decl_ast = IvyParser::decl(parsed).expect("AST generation failed");

        let mut tc = TypeChecker::new();
        let _res = tc.visit_decl(&mut decl_ast).unwrap();

        assert_eq!(
            tc.bindings.lookup(&"numbers".to_owned()),
            Some(&IvySort::Range(
                Box::new(Expr::Number(0)),
                Box::new(Expr::Number(100))
            ))
        )
    }
}
