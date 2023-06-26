#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{
        parser::ivy::{IvyParser, Rule},
        typechecker::{
            inference::TypeChecker,
            sorts::{IvySort, Process},
            TypeError,
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
        assert_eq!(res, IvySort::Unit);

        assert_eq!(
            tc.bindings.lookup(&"is_up".to_owned()),
            Some(IvySort::Function(
                [IvySort::SortVar(1), IvySort::SortVar(2)].into(),
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
        assert_eq!(res, IvySort::Unit);

        assert_eq!(
            tc.bindings.lookup(&"is_up".to_owned()),
            Some(IvySort::Function(
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
        let res = decl_ast
            .visit(&mut tc)
            .expect("visit")
            .modifying(&mut decl_ast)
            .unwrap();
        assert_eq!(res, IvySort::Unit);

        assert_eq!(
            tc.bindings.lookup(&"node".to_owned()),
            Some(IvySort::Uninterpreted)
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
        assert_eq!(res, IvySort::Unit);

        assert_eq!(
            tc.bindings.lookup(&"status".to_owned()),
            Some(IvySort::Enum(["on".into(), "off".into()].into()))
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
        assert_eq!(res, IvySort::Unit);

        assert_eq!(
            tc.bindings.lookup(&"numbers".to_owned()),
            Some(IvySort::Range(
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
        assert_eq!(res, IvySort::Unit);

        assert_eq!(
            tc.bindings.lookup(&"i".to_owned()),
            Some(IvySort::SortVar(0))
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
        assert_eq!(res, IvySort::Unit);

        assert_eq!(tc.bindings.lookup(&"b".to_owned()), Some(IvySort::Bool))
    }
}
