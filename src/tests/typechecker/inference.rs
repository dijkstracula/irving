#[cfg(test)]
mod tests {
    use crate::{
        parser::ivy::{IvyParser, Rule},
        typechecker::{inference::TypeChecker, sorts::IvySort, Error},
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
}
