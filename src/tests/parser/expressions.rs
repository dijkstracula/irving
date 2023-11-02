#[cfg(test)]
mod tests {
    use std::path::PathBuf;
    use std::rc::Rc;

    use crate::ast::expressions::{self, *};
    use crate::ast::logic::{Fmla, LogicBinOp};
    use crate::ast::span::Span;
    use crate::parser::ivy::{IvyParser, ParserState, Result, Rule};
    use crate::tests::helpers;
    use pest_consume::Parser;

    // Expressions

    fn parse_rval<S>(fragment: S) -> Result<Expr>
    where
        S: Into<String>,
    {
        let fragment = fragment.into();
        let user_data = Rc::new(ParserState::new(
            PathBuf::from(file!()),
            fragment.to_string(),
        ));
        let res = IvyParser::parse_with_userdata(Rule::rval, &fragment, user_data)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::rval(res)
    }

    fn parse_fmla<S>(fragment: S) -> Result<Fmla>
    where
        S: Into<String>,
    {
        let fragment = fragment.into();
        let user_data = Rc::new(ParserState::new(
            PathBuf::from(file!()),
            fragment.to_string(),
        ));
        let res = IvyParser::parse_with_userdata(Rule::fmla, &fragment, user_data)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::fmla(res)
    }

    #[test]
    fn parse_progsym() {
        let ast = parse_rval("a").unwrap();
        assert_eq!(ast, helpers::inferred_progsym("a"));
    }

    #[test]
    fn parse_logicvar() {
        let fragment = "X";
        IvyParser::parse_with_userdata(Rule::rval, fragment, fragment)
            .expect_err("A logic var is not an expression");
        parse_fmla(fragment).unwrap();
    }

    #[test]
    fn parse_negation_expr() {
        let _ast = parse_rval("~a").unwrap();
    }

    #[test]
    fn parse_number_expr() {
        let ast = parse_rval("42").unwrap();
        assert!(matches!(ast, expressions::Expr::Number { val: 42, .. }));
    }

    #[test]
    fn parse_negative_number_expr() {
        let ast = parse_rval("-42").unwrap();
        assert!(matches!(ast, expressions::Expr::UnaryOp { .. }));
    }

    #[test]
    fn parse_le() {
        let ast = parse_rval("0 <= s").unwrap();
        assert!(matches!(
            ast,
            expressions::Expr::BinOp {
                expr: expressions::BinOp { op: Verb::Le, .. },
                ..
            }
        ))
    }

    #[test]
    fn parse_sub() {
        let span = Span::IgnoredForTesting;

        let ast = parse_rval("42 - 1").unwrap();
        assert_eq!(
            ast,
            Expr::BinOp {
                span: Span::IgnoredForTesting,
                expr: BinOp {
                    lhs: Box::new(Expr::Number {
                        span: span.clone(),
                        val: 42
                    }),
                    op: Verb::Minus,
                    rhs: Box::new(Expr::Number { span: span, val: 1 })
                }
            }
        );
    }

    #[test]
    fn parse_complex_expr() {
        let fragment = "sock.send(host(1-self).sock.id, val)";
        let ast = parse_rval(fragment).unwrap();
        assert!(matches!(ast, expressions::Expr::App { .. }));
    }

    #[test]
    fn parse_pred() {
        let ast = parse_rval("i > 0").unwrap();
        assert_eq!(
            ast,
            Expr::BinOp {
                span: Span::IgnoredForTesting,
                expr: BinOp {
                    lhs: Box::new(helpers::inferred_progsym("i")),
                    op: Verb::Gt,
                    rhs: Box::new(Expr::Number {
                        span: Span::IgnoredForTesting,
                        val: 0
                    })
                }
            }
        );
    }

    #[test]
    fn parse_pred_2() {
        let ast = parse_rval("b = true | b = false").unwrap();
        assert_eq!(
            ast,
            Expr::BinOp {
                span: Span::IgnoredForTesting,
                expr: BinOp {
                    lhs: Box::new(Expr::BinOp {
                        span: Span::IgnoredForTesting,
                        expr: BinOp {
                            lhs: Box::new(helpers::inferred_progsym("b")),
                            op: Verb::Equals,
                            rhs: Box::new(Expr::Boolean {
                                span: Span::IgnoredForTesting,
                                val: true
                            })
                        }
                    }),
                    op: Verb::Or,
                    rhs: Box::new(Expr::BinOp {
                        span: Span::IgnoredForTesting,
                        expr: BinOp {
                            lhs: Box::new(helpers::inferred_progsym("b")),
                            op: Verb::Equals,
                            rhs: Box::new(Expr::Boolean {
                                span: Span::IgnoredForTesting,
                                val: false
                            })
                        }
                    }),
                }
            }
        );
    }

    #[test]
    fn parse_conjunction_fmla() {
        let ast = parse_fmla("X & Y").unwrap();

        let expected = Fmla::BinOp {
            span: Span::IgnoredForTesting,
            binop: LogicBinOp {
                lhs: helpers::inferred_logicsym("X").into(),
                op: Verb::And,
                rhs: helpers::inferred_logicsym("Y").into(),
            },
        };
        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_symbol_with_underscore() {
        assert!(parse_rval("hello_world").is_ok());
    }

    #[test]
    fn parse_dot_expr() {
        let ast = parse_rval("a.b").unwrap();
        assert_eq!(
            ast,
            Expr::FieldAccess {
                span: Span::IgnoredForTesting,
                expr: FieldAccess {
                    record: Box::new(helpers::inferred_progsym("a")),
                    field: Symbol::from("b", Sort::ToBeInferred, Span::IgnoredForTesting),
                }
            }
        );
    }

    #[test]
    fn parse_unary_fnapp() {
        let _ast = parse_rval("foo(a)").expect("Parsing failed");
    }

    #[test]
    fn parse_fnapp_and_index() {
        let _ast = parse_rval("foo(a).b.c").unwrap();
    }

    #[test]
    fn parse_nested_fnapp() {
        let _ast = parse_rval("sock.send(host(1-self).sock.id, val)").unwrap();
    }

    #[test]
    fn parse_unary_fnapp_and_negation() {
        let _ast = parse_rval("~foo(a)").expect("Parsing failed");
    }

    #[test]
    fn parse_multi_fnapp() {
        let _ast = parse_rval("foo(a, b)").unwrap();
    }

    #[test]
    fn parse_fnapp_in_conj() {
        let _ast = parse_rval("foo(a,b) & b").unwrap();
    }
}
