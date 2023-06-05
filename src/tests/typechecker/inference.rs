#[cfg(test)]
mod tests {
    use crate::{
        parser::ivy::{IvyParser, Rule},
        typechecker::{inference::ConstraintGenerator, sorts::IvySort},
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

        let mut cg = ConstraintGenerator::new();
        let (expr_type, _) = match cg.visit_expr(&mut binop).unwrap() {
            Control::Continue((expr_type, constraints)) => (expr_type, constraints),
            _ => unreachable!(),
        };
        assert_eq!(expr_type, IvySort::Bool);
    }

    #[test]
    fn test_numeric_binop() {
        let prog = "1 + 2";
        let parsed = IvyParser::parse(Rule::expr, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut binop = IvyParser::expr(parsed).expect("AST generation failed");

        let mut cg = ConstraintGenerator::new();
        let (expr_type, _) = match cg.visit_expr(&mut binop).unwrap() {
            Control::Continue((expr_type, constraints)) => (expr_type, constraints),
            _ => unreachable!(),
        };
        assert_eq!(expr_type, IvySort::Number);
    }
}
