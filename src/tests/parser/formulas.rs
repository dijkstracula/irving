#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::ast::expressions;
    use crate::ast::expressions::*;
    use crate::ast::logic::*;
    use crate::parser::ivy::{IvyParser, Result, Rule};
    use pest_consume::Parser;

    fn parse_fmla<S>(fragment: S) -> Result<Fmla>
    where
        S: Into<Rc<str>>,
    {
        let rc = fragment.into();
        let res = IvyParser::parse_with_userdata(Rule::fmla, &rc, rc.clone())
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::fmla(res)
    }

    #[test]
    fn parse_term() {
        let ast = parse_fmla("X").unwrap();
        let expected = Fmla::Pred(Expr::inferred_logicsym("X"));
        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_negation() {
        let ast = parse_fmla("~X").unwrap();
        let expected = Fmla::Pred(Expr::UnaryOp {
            op: Verb::Not,
            expr: Box::new(Expr::inferred_logicsym("X")),
        });
        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_parens_negation() {
        let ast = parse_fmla("~(X)").expect("Parsing failed");
        let expected = Fmla::Pred(Expr::UnaryOp {
            op: Verb::Not,
            expr: Box::new(Expr::inferred_logicsym("X")),
        });
        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_term_with_annotation() {
        parse_fmla("X:int").expect("Parsing failed");
    }

    #[test]
    fn parse_pred() {
        parse_fmla("end(X)").expect("Parsing failed");
    }

    #[test]
    fn parse_conjunction() {
        let ast = parse_fmla("X = Y & Y = Z").unwrap();
        let lhs = expressions::BinOp {
            lhs: Expr::inferred_logicsym("X").into(),
            op: Verb::Equals,
            rhs: Expr::inferred_logicsym("Y").into(),
        };
        let rhs = expressions::BinOp {
            lhs: Expr::inferred_logicsym("Y").into(),
            op: Verb::Equals,
            rhs: Expr::inferred_logicsym("Z").into(),
        };
        let expected = Fmla::Pred(Expr::BinOp(expressions::BinOp {
            lhs: Expr::BinOp(lhs).into(),
            op: Verb::And,
            rhs: Expr::BinOp(rhs).into(),
        }));
        assert_eq!(ast, expected);
    }

    #[test]
    fn parse_logical_implication() {
        let _ast = parse_fmla("X = Y & Y = Z -> X = Z").unwrap();
    }

    #[test]
    fn parse_universal_quant() {
        let _ast = parse_fmla("forall X . X").expect("Parse");
        println!("{:?}", _ast);
    }

    #[test]
    fn parse_universal_quant2() {
        parse_fmla("forall X,Y,Z . X = Y & Y = Z -> X = Z").unwrap();
    }

    #[test]
    fn parse_universal_quant3() {
        let ast = parse_fmla("forall X:node,Y,Z. X=Y & Y=Z -> X=Z").expect("parse");

        let x_equals_y = expressions::BinOp {
            lhs: Expr::inferred_logicsym("X").into(),
            op: Verb::Equals,
            rhs: Expr::inferred_logicsym("Y").into(),
        };
        let y_equals_z = expressions::BinOp {
            lhs: Expr::inferred_logicsym("Y").into(),
            op: Verb::Equals,
            rhs: Expr::inferred_logicsym("Z").into(),
        };

        let antecedent = Expr::BinOp(expressions::BinOp {
            lhs: Expr::BinOp(x_equals_y).into(),
            op: Verb::And,
            rhs: Expr::BinOp(y_equals_z).into(),
        });
        let consequence = Expr::BinOp(expressions::BinOp {
            lhs: Expr::inferred_logicsym("X").into(),
            op: Verb::Equals,
            rhs: Expr::inferred_logicsym("Z").into(),
        });

        let implication = Fmla::Pred(Expr::BinOp(expressions::BinOp {
            lhs: Box::new(antecedent),
            op: Verb::Arrow,
            rhs: Box::new(consequence),
        }));

        assert_eq!(
            ast,
            Fmla::Forall(Forall {
                vars: vec![
                    Symbol::from("X", Sort::Annotated(vec!["node".into()])),
                    Symbol::from("Y", Sort::ToBeInferred),
                    Symbol::from("Z", Sort::ToBeInferred),
                ],
                fmla: Box::new(implication)
            })
        );
    }

    #[test]
    fn parse_existential_quant() {
        assert!(parse_fmla("exists S. S < x").is_ok());
    }

    #[test]
    fn parse_nested_quants() {
        let _ast = parse_fmla(
            "end(X) = end(Y) & (forall I. 0 <= I & I < end(X) -> value(X,I) = value(Y,I))",
        )
        .unwrap();
    }

    #[test]
    fn parse_nested_quants2() {
        parse_fmla("end(X) = end(Y) & forall I. 0 <= I & I < end(X) -> value(X,I) = value(Y,I)")
            .expect("Parsing failed");
    }
}
