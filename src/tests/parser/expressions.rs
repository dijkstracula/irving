#[cfg(test)]
mod tests {
    use crate::ast::expressions::*;
    use crate::parser::ivy::{IvyParser, Result, Rule};
    use pest_consume::Parser;

    // Expressions

    fn parse_expr(fragment: &str) -> Result<Expr> {
        let res = IvyParser::parse(Rule::expr, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::expr(res)
    }

    #[test]
    fn parse_symbol_expr() {
        let _ast = parse_expr("a").unwrap();
        assert_eq!(_ast, Expr::Symbol("a".into()));
    }

    #[test]
    fn parse_negation_expr() {
        let _ast = parse_expr("~a").unwrap();
    }

    #[test]
    fn parse_number_expr() {
        let _ast = parse_expr("42").unwrap();
    }

    #[test]
    fn parse_negative_number_expr() {
        let _ast = parse_expr("-42").unwrap();
    }

    #[test]
    fn parse_sub() {
        let _ast = parse_expr("42 - 1").unwrap();
        assert_eq!(
            _ast,
            Expr::BinOp(BinOp {
                lhs: Box::new(Expr::Number(42)),
                op: Verb::Minus,
                rhs: Box::new(Expr::Number(1)),
            })
        );
    }

    #[test]
    fn parse_complex_expr() {
        let fragment = "sock.send(host(1-self).sock.id, val)";
        let _ast = parse_expr(fragment).unwrap();
        println!("{:?}", _ast);
    }

    #[test]
    fn parse_pred() {
        let _ast = parse_expr("i > 0").unwrap();
        assert_eq!(
            _ast,
            Expr::BinOp(BinOp {
                lhs: Box::new(Expr::Symbol("i".into())),
                op: Verb::Gt,
                rhs: Box::new(Expr::Number(0)),
            })
        );
    }

    #[test]
    fn parse_pred_2() {
        let _ast = parse_expr("b = true | b = false").unwrap();
        assert_eq!(
            _ast,
            Expr::BinOp(BinOp {
                lhs: Box::new(Expr::BinOp(BinOp {
                    lhs: Box::new(Expr::Symbol("b".into())),
                    op: Verb::Equals,
                    rhs: Box::new(Expr::Boolean(true))
                })),
                op: Verb::Or,
                rhs: Box::new(Expr::BinOp(BinOp {
                    lhs: Box::new(Expr::Symbol("b".into())),
                    op: Verb::Equals,
                    rhs: Box::new(Expr::Boolean(false))
                }))
            })
        );
    }

    #[test]
    fn parse_symbol_with_underscore() {
        assert!(parse_expr("hello_world").is_ok());
    }

    #[test]
    fn parse_dot_expr() {
        let _ast = parse_expr("a.b").unwrap();
        assert_eq!(
            _ast,
            Expr::FieldAccess(FieldAccess {
                record: Box::new(Expr::Symbol("a".into())),
                field: "b".into()
            })
        );
    }

    #[test]
    fn parse_unary_fnapp() {
        let _ast = parse_expr("foo(a)").expect("Parsing failed");
    }

    #[test]
    fn parse_fnapp_and_index() {
        let _ast = parse_expr("foo(a).b.c").unwrap();
    }

    #[test]
    fn parse_nested_fnapp() {
        let _ast = parse_expr("sock.send(host(1-self).sock.id, val)").unwrap();
    }

    #[test]
    fn parse_unary_fnapp_and_negation() {
        let _ast = parse_expr("~foo(a)").expect("Parsing failed");
    }

    #[test]
    fn parse_multi_fnapp() {
        let _ast = parse_expr("foo(a, b)").unwrap();
    }

    #[test]
    fn parse_fnapp_in_conj() {
        let _ast = parse_expr("foo(a,b) & b").unwrap();
    }
}
