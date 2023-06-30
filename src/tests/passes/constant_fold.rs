#[cfg(test)]
mod tests {
    use crate::ast::expressions::*;
    use crate::parser::ivy::{IvyParser, Rule};
    use crate::passes::constant_fold::ConstantFold;
    use crate::visitor::visitor::Visitable;
    use pest_consume::Parser;

    #[test]
    fn fold_constants() {
        let expr = "(19 * 2) + 4";
        let res = IvyParser::parse(Rule::expr, &expr)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut ast = IvyParser::expr(res).expect("AST generation failed");

        let mut cf = ConstantFold;
        ast.visit(&mut cf).unwrap();

        assert_eq!(ast, Expr::Number(42));
    }

    #[test]
    fn fold_zero_add() {
        let expr = "foo + 0";
        let res = IvyParser::parse(Rule::expr, &expr)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut ast = IvyParser::expr(res).expect("AST generation failed");

        let mut cf = ConstantFold;
        ast.visit(&mut cf).unwrap();

        assert_eq!(ast, Expr::Symbol("foo".into()));
    }
}
