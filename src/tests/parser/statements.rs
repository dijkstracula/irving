#[cfg(test)]
mod tests {
    use crate::{
        ast::{actions::*, expressions::*, logic::*, span::Span, statements::Stmt},
        parser::ivy::{IvyParser, Rule},
        tests::helpers,
    };
    use pest_consume::Parser;

    // Statements

    #[test]
    fn parse_assign() {
        let fragment = "a := b";
        helpers::stmt_from_src(fragment);
    }

    #[test]
    fn parse_assign_to_relation() {
        let fragment = "r(X, y) := false";
        helpers::stmt_from_src(fragment);
    }

    #[test]
    fn parse_assign_to_relation_2() {
        let fragment = "r(0) := (1 = 1)";
        helpers::stmt_from_src(fragment);
    }

    #[test]
    fn parse_assign_with_rhs_logicvar() {
        let fragment = "link(x,Y) := Y = y";
        helpers::stmt_from_src(fragment);
    }

    #[test]
    fn parse_assign_annot() {
        let fragment = "var a:int := b";
        helpers::stmt_from_src(fragment);
    }

    #[test]
    fn parse_assign_to_field() {
        let fragment = "foo.bar := false";
        let ast = helpers::stmt_from_src(fragment);
        assert_eq!(
            ast,
            Stmt::ActionSequence(
                [Action::Assign {
                    span: Span::IgnoredForTesting,
                    action: AssignAction {
                        lhs: Expr::FieldAccess {
                            span: Span::IgnoredForTesting,
                            expr: FieldAccess {
                                record: Box::new(helpers::inferred_progsym("foo")),
                                field: Symbol::from("bar", Sort::ToBeInferred),
                            }
                        },
                        rhs: Expr::Boolean {
                            span: Span::IgnoredForTesting,
                            val: false
                        }
                    }
                }]
                .into()
            )
        );
    }

    #[test]
    fn parse_assign_to_field_of_this() {
        let fragment = "this.bit := false";
        helpers::stmt_from_src(fragment);
    }

    #[test]
    fn parse_assert_action() {
        let fragment = "assert x < y";
        helpers::stmt_from_src(fragment);
    }

    #[test]
    fn parse_assume_action() {
        let fragment = "assume x < y";
        helpers::stmt_from_src(fragment);
    }

    #[test]
    fn parse_ensure_action() {
        let fragment = "ensure ~failed(y)";
        helpers::stmt_from_src(fragment);
    }

    #[test]
    fn parse_ensure_action_2() {
        let fragment = "ensure msg_count = 0 -> host(0).contents.eq(host(1).contents)";
        helpers::stmt_from_src(fragment);
    }

    #[test]
    fn parse_requires_action() {
        let fragment = "require ~failed(y)";
        let res = IvyParser::parse_with_userdata(Rule::requires_action, fragment, fragment.into())
            .expect("Parsing failed")
            .single()
            .unwrap();

        let (_, stmt) = IvyParser::requires_action(res).unwrap();
        assert_eq!(
            stmt,
            RequiresAction {
                pred: Fmla::Pred(Expr::UnaryOp {
                    span: Span::IgnoredForTesting,
                    op: Verb::Not,
                    expr: Box::new(Expr::App {
                        span: Span::IgnoredForTesting,
                        expr: AppExpr {
                            func: Box::new(helpers::inferred_progsym("failed".to_owned())),
                            args: [helpers::inferred_progsym("y")].into()
                        }
                    })
                })
            }
        );
    }

    #[test]
    fn parse_requires_action_2() {
        let fragment = "require x >= 0";
        let res = IvyParser::parse_with_userdata(Rule::requires_action, fragment, fragment.into())
            .expect("Parsing failed")
            .single()
            .unwrap();
        let (_, stmt) = IvyParser::requires_action(res).unwrap();
        assert_eq!(
            stmt,
            RequiresAction {
                pred: Fmla::Pred(Expr::BinOp {
                    span: Span::IgnoredForTesting,
                    expr: BinOp {
                        lhs: Box::new(helpers::inferred_progsym("x")),
                        op: Verb::Ge,
                        rhs: Box::new(Expr::Number {
                            span: Span::IgnoredForTesting,
                            val: 0
                        })
                    }
                })
            }
        );
    }

    #[test]
    fn parse_if_stmt() {
        let fragment = "if 1 < 2 { a := 42; }";
        helpers::stmt_from_src(fragment);
    }

    #[test]
    fn parse_stmt_block() {
        let fragment = "requires a = 0; a := 42; ensure a = 42";
        helpers::stmt_from_src(fragment);
    }

    #[test]
    fn parse_if_stmt_with_else() {
        let fragment = "if 1 < 2 { a := 42; } else { a := 43; }";
        helpers::stmt_from_src(fragment);
    }

    #[test]
    fn parse_while() {
        let fragment = "while 1 < 2 { a := a + 1; }";
        helpers::stmt_from_src(fragment);
    }

    #[test]
    fn parse_multi_stmt_while() {
        let fragment = "while i > 0 {
            sum := sum + f(i);
            i := i - 1
        }";
        helpers::stmt_from_src(fragment);
    }
}
