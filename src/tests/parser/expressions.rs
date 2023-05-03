#[cfg(test)]
mod tests {
    use pest_consume::Parser;
    use crate::ast;
    use crate::parser::{IvyParser, Result, Rule};

    // Expressions

    fn parse_expr(fragment: &str) -> Result<ast::Expr> {
        let res = IvyParser::parse(Rule::expr, fragment)
            .expect("Parsing failed").single().unwrap();
        IvyParser::expr(res)
    }

    #[test]
    fn parse_symbol_expr() {
        assert!(parse_expr("a").is_ok());
    }

    #[test]
    fn parse_symbol_with_underscore() {
        assert!(parse_expr("hello_world").is_ok());
    }

    #[test]
    fn parse_dot_expr() {
        assert!(parse_expr("a.b").is_ok());
    }

    #[test]
    fn parse_unary_fnapp() {
        let _ast = parse_expr("foo(a)").expect("Parsing failed");
        println!("{:?}", _ast);
    }

    #[test]
    fn parse_multi_fnapp() {
        parse_expr("foo(a, b)").expect("Parsing failed");
    }

    #[test]
    fn parse_logical_implication() {
        assert!(parse_expr("x = y & y = z -> x = z").is_ok());
    }

    #[test]
    fn parse_universal_quant() {
        let _ast = parse_expr("forall x . x").expect("Parse");
    }

    #[test]
    fn parse_universal_quant2() {
        assert!(parse_expr("forall x,y,z . x = y & y = z -> x = z").is_ok());
    }

    #[test]
    fn parse_existential_quant() {
        assert!(parse_expr("exists S. S < x").is_ok());
    }

    #[test]
    fn parse_nested_quants() {
        parse_expr("end(X) = end(Y) & forall I. 0 <= I & I < end(X) -> value(X,I) = value(Y,I)").expect("Parsing failed");
    }
}