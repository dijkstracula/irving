#[cfg(test)]
mod tests {
    use pest_consume::Parser;

    use crate::{
        ast::{
            expressions::{self, BinOp, Verb},
            logic::Fmla,
        },
        error::IrvingError,
        parser::ivy::{IvyParser, Rule},
        passes::quantifier_bounds::QuantBounds,
        tests::helpers,
        typechecker::{inference::SortInferer, sorts::IvySort},
        visitor::ast::Visitable,
    };

    fn typecheck_fmla(fragment: &str) -> Result<Fmla, IrvingError> {
        let res = IvyParser::parse_with_userdata(Rule::fmla, fragment, fragment.into())
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
        let one = helpers::number(1);
        let two = QuantBounds::succ(&one);

        assert_eq!(two, helpers::number(2));
    }

    #[test]
    fn succ_binop_of_literals() {
        let two = helpers::plus(helpers::number(1), helpers::number(1));
        let three = QuantBounds::succ(&two);

        assert_eq!(three, helpers::number(3));
    }

    #[test]
    fn succ_compound() {
        let foo_plus_one = helpers::plus(helpers::inferred_progsym("foo"), helpers::number(1));
        let foo_plus_two = QuantBounds::succ(&foo_plus_one);

        assert_eq!(
            foo_plus_two,
            helpers::plus(helpers::inferred_progsym("foo"), helpers::number(2))
        );
    }

    #[test]
    fn op_on_logicsym() {
        assert_eq!(
            QuantBounds::op_on_logicsym(&BinOp {
                lhs: Box::new(helpers::number(0)),
                op: Verb::Equals,
                rhs: Box::new(helpers::number(1))
            }),
            None
        );

        assert_eq!(
            QuantBounds::op_on_logicsym(&BinOp {
                lhs: Box::new(helpers::inferred_logicsym("N")),
                op: Verb::Equals,
                rhs: Box::new(helpers::number(1))
            }),
            Some(&expressions::Symbol {
                name: "N".into(),
                decl: expressions::Sort::ToBeInferred
            })
        );
    }

    #[test]
    fn bounds_from_binop() {
        assert_eq!(
            QuantBounds::bounds_from_ast(&helpers::number(1), Verb::Lt, &helpers::number(5)),
            Some((helpers::number(1), helpers::number(5)))
        );
        assert_eq!(
            QuantBounds::bounds_from_ast(&helpers::number(1), Verb::Le, &helpers::number(7)),
            Some((helpers::number(1), helpers::number(8)))
        );
        assert_eq!(
            QuantBounds::bounds_from_ast(&helpers::number(5), Verb::Gt, &helpers::number(3)),
            Some((helpers::number(3), helpers::number(5)))
        );
        assert_eq!(
            QuantBounds::bounds_from_ast(&helpers::number(5), Verb::Ge, &helpers::number(2)),
            Some((helpers::number(2), helpers::number(6)))
        );
    }

    #[test]
    fn bounds_from_forall_nats_positive() {
        let mut fmla = typecheck_fmla("forall N. N >= 1").expect("parse and typecheck");

        let mut qb = QuantBounds::new_forall();
        fmla.visit(&mut qb).unwrap().modifying(&mut fmla);

        let n = helpers::inferred_logicsym("N");
        let one = helpers::number(1);
        assert!(qb.bounds.get("N").unwrap().contains(&(
            BinOp {
                lhs: Box::new(n),
                op: Verb::Ge,
                rhs: Box::new(one)
            },
            true
        )));
        //assert!(qb.bounds.get("N").unwrap().contains(&(BinOp { lhs: Box::new, n_plus_one)));
    }

    #[test]
    fn bounds_from_forall_nats_not_negative() {
        let mut fmla = typecheck_fmla("forall N. ~(N < 1)").expect("parse and typecheck");

        let mut qb = QuantBounds::new_forall();
        fmla.visit(&mut qb).unwrap().modifying(&mut fmla);

        let n = helpers::inferred_logicsym("N");
        let one = helpers::number(1);

        assert!(qb.bounds.get("N").unwrap().contains(&(
            BinOp {
                lhs: Box::new(n),
                op: Verb::Lt,
                rhs: Box::new(one)
            },
            false
        )));
    }

    #[test]
    fn bounds_from_forall_nats_disjunction() {
        let mut fmla = typecheck_fmla("forall N. N > 0 | N = 0").expect("parse and typecheck");

        let mut qb = QuantBounds::new_forall();
        fmla.visit(&mut qb).unwrap().modifying(&mut fmla);

        // TODO: why doesn't ivy_to_cpp generate the range given below???
        let n = helpers::inferred_logicsym("N");
        let zero = helpers::number(0);
        println!("NBT: {:?}", qb.bounds.get("N"));

        assert!(qb.bounds.get("N").unwrap().contains(&(
            BinOp {
                lhs: Box::new(n),
                op: Verb::Gt,
                rhs: Box::new(zero)
            },
            true
        )));
        //assert!(qb.bounds.get("N").unwrap().contains(&(zero, n)));
    }

    #[test]
    fn bounds_from_forall_nats_conjunction() {
        let mut fmla = typecheck_fmla("forall N. N < 10 & N = 0").expect("parse and typecheck");

        let mut qb = QuantBounds::new_forall();
        fmla.visit(&mut qb).unwrap().modifying(&mut fmla);

        assert!(qb.bounds.get("N").unwrap().len() == 0);
    }

    #[test]
    fn bounds_from_forall_nats_monotonic_lt() {
        let mut fmla = typecheck_fmla("forall N. N < 10 -> N < 5").expect("parse and typecheck");

        let mut qb = QuantBounds::new_forall();
        fmla.visit(&mut qb).unwrap().modifying(&mut fmla);

        let n = helpers::inferred_logicsym("N");
        let five = helpers::number(5);
        let ten = helpers::number(10);

        assert!(qb.bounds.get("N").unwrap().contains(&(
            BinOp {
                lhs: Box::new(n.clone()),
                op: Verb::Lt,
                rhs: Box::new(ten)
            },
            false
        )));
        assert!(qb.bounds.get("N").unwrap().contains(&(
            BinOp {
                lhs: Box::new(n),
                op: Verb::Lt,
                rhs: Box::new(five)
            },
            true
        )));
        //assert!(qb.bounds.get("N").unwrap().contains(&(n.clone(), ten)));
    }
}
