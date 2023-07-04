#[cfg(test)]
mod tests {
    use crate::{
        ast::expressions::Expr,
        parser::ivy::{IvyParser, Rule},
        typechecker::{
            inference::TypeChecker,
            sorts::{Fargs, IvySort},
        },
        visitor::visitor::Visitable,
    };
    use pest_consume::Parser;
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
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast)
            .unwrap();
        assert_eq!(
            res,
            IvySort::Function(
                Fargs::List(vec!(IvySort::SortVar(1), IvySort::SortVar(2))),
                Box::new(IvySort::Bool)
            )
        );

        assert_eq!(
            tc.bindings.lookup_sym(&"is_up".to_owned()),
            Some(&IvySort::Function(
                Fargs::List(vec!(IvySort::SortVar(1), IvySort::SortVar(2))),
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
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast)
            .unwrap();
        assert_eq!(
            res,
            IvySort::Function(
                Fargs::List(vec!(IvySort::Bool, IvySort::Bool)),
                Box::new(IvySort::Bool)
            )
        );

        assert_eq!(
            tc.bindings.lookup_sym(&"is_up".to_owned()),
            Some(&IvySort::Function(
                Fargs::List(vec!(IvySort::Bool, IvySort::Bool)),
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
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast)
            .unwrap();
        assert_eq!(res, IvySort::Uninterpreted);

        assert_eq!(
            tc.bindings.lookup_sym(&"node".to_owned()),
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
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast)
            .unwrap();
        assert_eq!(res, IvySort::Enum(["on".into(), "off".into()].into()));

        assert_eq!(
            tc.bindings.lookup_sym(&"status".to_owned()),
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
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast)
            .unwrap();
        assert_eq!(
            res,
            IvySort::Range(Box::new(Expr::Number(0)), Box::new(Expr::Number(100)))
        );

        assert_eq!(
            tc.bindings.lookup_sym(&"numbers".to_owned()),
            Some(&IvySort::Range(
                Box::new(Expr::Number(0)),
                Box::new(Expr::Number(100))
            ))
        )
    }

    #[test]
    fn test_vardecl_inferred() {
        let prog = "var i";

        let parsed = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut decl_ast = IvyParser::decl(parsed).expect("AST generation failed");

        let mut tc = TypeChecker::new();
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast)
            .unwrap();
        assert_eq!(res, IvySort::SortVar(0));

        assert_eq!(
            tc.bindings.lookup_sym(&"i".to_owned()),
            Some(&IvySort::SortVar(0))
        )
    }

    #[test]
    fn test_vardecl_annotated() {
        let prog = "var b: bool";

        let parsed = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut decl_ast = IvyParser::decl(parsed).expect("AST generation failed");

        let mut tc = TypeChecker::new();
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast)
            .unwrap();
        assert_eq!(res, IvySort::Bool);
        assert_eq!(
            tc.bindings.lookup_sym(&"b".to_owned()),
            Some(&IvySort::Bool)
        )
    }
}
