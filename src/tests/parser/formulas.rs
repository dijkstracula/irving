#[cfg(test)]
mod tests {
    use pest_consume::Parser;
    use crate::ast;
    use crate::parser::{IvyParser, Result, Rule};

    fn parse_expr(fragment: &str) -> Result<ast::Expr> {
        let res = IvyParser::parse(Rule::expr, fragment)
            .expect("Parsing failed").single().unwrap();
        IvyParser::expr(res)
    }

    #[test]
    fn parse_logical_implication() {
        println!("{:?}", parse_expr("x = y & y = z -> x = z").unwrap());
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
    fn parse_universal_quant3() {
        let _ast = parse_expr("forall X:node,Y,Z. X=Y & Y=Z -> X=Y").expect("parse");
        println!("{:?}", _ast);
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