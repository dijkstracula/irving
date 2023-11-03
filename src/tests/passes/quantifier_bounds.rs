#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use pest_consume::Parser;

    use crate::{
        ast::{
            declarations::Binding,
            expressions::{Sort, Verb},
            logic::{self, Fmla},
            span::Span,
        },
        error::IrvingError,
        parser::ivy::{IvyParser, ParserState, Rule},
        passes::quantifier_bounds::QuantBounds,
        tests::helpers::{self, logical_number},
        typechecker::{inference::SortInferer, sorts::IvySort},
        visitor::ast::Visitable,
    };

    fn typecheck_fmla(fragment: &str) -> Result<Fmla, IrvingError> {
        let user_data = Rc::new(ParserState::new(file!(), fragment));
        let res = IvyParser::parse_with_userdata(Rule::fmla, fragment, user_data)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut ast = IvyParser::fmla(res)?;

        let mut si = SortInferer::new();
        si.bindings
            .append("a_number".into(), IvySort::Number)
            .unwrap();
        ast.visit(&mut si)?.modifying(&mut ast);
        Ok(ast)
    }

    #[test]
    fn succ_literal() {
        let one = helpers::logical_number(1);
        let two = QuantBounds::succ(&one);

        assert_eq!(two, helpers::logical_number(2));
    }

    #[test]
    fn succ_binop_of_literals() {
        let two = Fmla::BinOp {
            span: Span::IgnoredForTesting,
            binop: logic::LogicBinOp {
                lhs: Box::new(helpers::logical_number(1)),
                op: Verb::Plus,
                rhs: Box::new(helpers::logical_number(1)),
            },
        };
        let three = QuantBounds::succ(&two);

        assert_eq!(three, helpers::logical_number(3));
    }

    #[test]
    fn succ_compound() {
        let foo_plus_one = Fmla::BinOp {
            span: Span::IgnoredForTesting,
            binop: logic::LogicBinOp {
                lhs: Box::new(Fmla::ProgramSymbol {
                    span: Span::IgnoredForTesting,
                    sym: Binding::from("foo", Sort::ToBeInferred, Span::IgnoredForTesting),
                }),
                op: Verb::Plus,
                rhs: Box::new(helpers::logical_number(1)),
            },
        };
        let foo_plus_two = Fmla::BinOp {
            span: Span::IgnoredForTesting,
            binop: logic::LogicBinOp {
                lhs: Box::new(Fmla::ProgramSymbol {
                    span: Span::IgnoredForTesting,
                    sym: Binding::from("foo", Sort::ToBeInferred, Span::IgnoredForTesting),
                }),
                op: Verb::Plus,
                rhs: Box::new(helpers::logical_number(2)),
            },
        };

        let foo_plus_one_plus_one = QuantBounds::succ(&foo_plus_one);
        assert_eq!(foo_plus_one_plus_one, foo_plus_two);
    }

    #[test]
    fn op_on_logicsym() {
        assert_eq!(
            QuantBounds::op_on_logicsym(&logic::LogicBinOp {
                lhs: Box::new(helpers::logical_number(0)),
                op: Verb::Equals,
                rhs: Box::new(helpers::logical_number(1))
            }),
            None
        );
    }

    #[test]
    fn bounds_from_binop_literals() {
        let var = Binding::from("N".to_owned(), Sort::ToBeInferred, Span::IgnoredForTesting);

        assert_eq!(
            QuantBounds::bounds_from_ast(
                &var,
                &helpers::logical_number(1),
                Verb::Lt,
                &helpers::logical_number(5)
            ),
            Some((
                Some(helpers::logical_number(1)),
                Some(helpers::logical_number(5))
            ))
        );
        assert_eq!(
            QuantBounds::bounds_from_ast(
                &var,
                &helpers::logical_number(1),
                Verb::Le,
                &helpers::logical_number(7)
            ),
            Some((
                Some(helpers::logical_number(1)),
                Some(helpers::logical_number(8))
            ))
        );
    }

    #[test]
    fn bounds_from_binop_logicvar() {
        let _var = Binding::from("N".to_owned(), Sort::ToBeInferred, Span::IgnoredForTesting);

        /*
        // N < 5 means we have to check [0, 5)
        assert_eq!(
            QuantBounds::bounds_from_ast(
                &var,
                &helpers::inferred_logicsym("N"),
                Verb::Lt,
                &helpers::number(5)
            ),
            Some((None, Some(helpers::number(5))))
        );

        // N <= 5 means we have to check [0, 5]
        assert_eq!(
            QuantBounds::bounds_from_ast(
                &var,
                &helpers::inferred_logicsym("N"),
                Verb::Le,
                &helpers::number(5)
            ),
            Some((None, Some(helpers::number(6))))
        );

        // N <= 5 means we have to check [6, ..]
        assert_eq!(
            QuantBounds::bounds_from_ast(
                &var,
                &helpers::inferred_logicsym("N"),
                Verb::Gt,
                &helpers::number(5)
            ),
            Some((Some(helpers::number(6)), None))
        );
        */
    }

    #[test]
    fn bounds_for_sort() {
        assert_eq!(
            //While this is a finite sort, we can't enumerate boolean values as
            //sequences (though maybe we should; iterate through [0, 1] and cast
            //to the appropriate bool?)
            QuantBounds::bounds_for_sort(&IvySort::Bool),
            (None, None)
        );
        assert_eq!(
            QuantBounds::bounds_for_sort(&IvySort::Number),
            (Some(logical_number(0)), None)
        );
        assert_eq!(
            // Irritating: Ivy ranges are _inclusive_ so the interval is closed!
            // This sort has four inhabitants: 0, 1, 2, and 3.
            QuantBounds::bounds_for_sort(&IvySort::BoundedSequence(0, 3)),
            // Meanwhile, the rest of the world uses half-open intervals.
            (Some(logical_number(0)), Some(logical_number(4)))
        );
    }

    #[test]
    fn bounds_from_forall_nats_positive() {
        let mut fmla = typecheck_fmla("forall N. N >= 1").expect("parse and typecheck");

        let mut qb = QuantBounds::new_forall();
        fmla.visit(&mut qb).unwrap().modifying(&mut fmla);

        let _n = helpers::inferred_logicsym("N");
        let one = helpers::logical_number(1);
        println!("NBT: {:?}", qb.bounds.get("N"));

        // The counterproof, if found, will be lying on [0, 1)
        assert!(qb.bounds.get("N").unwrap().contains(&(None, Some(one))));
        //assert!(qb.bounds.get("N").unwrap().contains(&(BinOp { lhs: Box::new, n_plus_one)));
    }

    #[test]
    fn bounds_from_forall_nats_not_negative() {
        let mut fmla = typecheck_fmla("forall N. ~(N < 1)").expect("parse and typecheck");

        let mut qb = QuantBounds::new_forall();
        fmla.visit(&mut qb).unwrap().modifying(&mut fmla);

        let _n = helpers::inferred_logicsym("N");
        let one = helpers::logical_number(1);

        // The counterproof, if found, will be lying on [0, 1)
        assert!(qb.bounds.get("N").unwrap().contains(&(None, Some(one))));
    }

    #[test]
    fn bounds_from_forall_nats_disjunction() {
        let mut fmla = typecheck_fmla("forall N. N > 0 | N = 0").expect("parse and typecheck");

        let mut qb = QuantBounds::new_forall();
        fmla.visit(&mut qb).unwrap().modifying(&mut fmla);

        let zero = helpers::logical_number(0);
        let one = helpers::logical_number(1);

        assert!(qb.bounds.get("N").unwrap().contains(&(Some(zero), None)));
        assert!(qb.bounds.get("N").unwrap().contains(&(None, Some(one))));
    }

    #[test]
    fn bounds_from_forall_nats_conjunction() {
        let mut fmla = typecheck_fmla("forall N. N < 10 & N = 0").expect("parse and typecheck");

        let mut qb = QuantBounds::new_forall();
        fmla.visit(&mut qb).unwrap().modifying(&mut fmla);

        let zero = helpers::logical_number(0);
        assert!(qb.bounds.get("N").unwrap().contains(&(Some(zero), None)));
    }

    #[test]
    fn bounds_from_forall_nats_monotonic_lt() {
        let mut fmla = typecheck_fmla("forall N. N < 10 -> N < 5").expect("parse and typecheck");

        let mut qb = QuantBounds::new_forall();
        fmla.visit(&mut qb).unwrap().modifying(&mut fmla);

        let five = helpers::logical_number(5);
        let ten = helpers::logical_number(10);

        assert!(qb.bounds.get("N").unwrap().contains(&(None, Some(ten))));
        assert!(qb.bounds.get("N").unwrap().contains(&(Some(five), None)));
        //assert!(qb.bounds.get("N").unwrap().contains(&(n.clone(), ten)));
    }

    #[test]
    fn bounds_from_forall_nats_false_monotonic_gt() {
        let mut fmla = typecheck_fmla("forall N. N > 5 -> N > 10").expect("parse and typecheck");

        let mut qb = QuantBounds::new_forall();
        fmla.visit(&mut qb).unwrap().modifying(&mut fmla);

        let six = helpers::logical_number(6);
        let eleven = helpers::logical_number(11);

        assert!(qb.bounds.get("N").unwrap().contains(&(None, Some(eleven))));
        assert!(qb.bounds.get("N").unwrap().contains(&(Some(six), None)));
        //assert!(qb.bounds.get("N").unwrap().contains(&(n.clone(), ten)));
    }
}
