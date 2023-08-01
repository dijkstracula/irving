#[cfg(test)]
mod tests {
    use crate::ast::logic::*;
    use crate::parser::ivy::{IvyParser, Rule};
    use crate::typechecker::inference::SortInferer;
    use crate::visitor::ast::Visitable;
    use pest_consume::Parser;

    fn parse_fmla(fragment: &str) -> anyhow::Result<Fmla> {
        let res = IvyParser::parse_with_userdata(Rule::fmla, fragment, fragment.into())
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut ast = IvyParser::fmla(res)?;

        let mut tc = SortInferer::new();
        ast.visit(&mut tc)?.modifying(&mut ast)?;
        Ok(ast)
    }

    #[test]
    fn universal_quant() {
        let _ = parse_fmla("forall X . X").expect("Parse and typecheck");
        let _ = parse_fmla("forall X . X = X || X ~= X").expect("Parse and typecheck");
    }

    #[test]
    fn universal_quant_annotated() {
        let _ = parse_fmla("forall N: unbounded_sequence . 0 <= N").expect("Parse and typecheck");
        let _ = parse_fmla("forall B: bool . 0 <= B").expect_err("0 is incomparable with a Bool");
    }

    #[test]
    fn nested_quants() {
        let _ = parse_fmla("forall N . exists N2 . N < N2").expect("Parse and typecheck");
        let _ = parse_fmla("exists N . forall N2 . N <= N2").expect("Parse and typecheck");
    }
}
