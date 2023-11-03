#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::ast::logic::*;
    use crate::error::IrvingError;
    use crate::parser::ivy::{IvyParser, ParserState, Rule};
    use crate::typechecker::inference::SortInferer;
    use crate::typechecker::sorts::IvySort;
    use crate::visitor::ast::Visitable;
    use pest_consume::Parser;

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
    fn universal_quant() {
        let _ = typecheck_fmla("forall X . X").expect("Parse and typecheck");
        let _ = typecheck_fmla("forall X . X = X || X ~= X").expect("Parse and typecheck");
    }

    #[test]
    fn universal_quant_annotated() {
        let _ =
            typecheck_fmla("forall N: nat . 0 <= N").expect("Parse and typecheck");
        let _ =
            typecheck_fmla("forall B: bool . 0 <= B").expect_err("0 is incomparable with a Bool");
    }

    #[test]
    fn nested_quants() {
        let _ = typecheck_fmla("forall N . exists N2 . N < N2").expect("Parse and typecheck");
        let _ = typecheck_fmla("exists N . forall N2 . N <= N2").expect("Parse and typecheck");
    }

    #[test]
    fn progterm_in_fmls() {
        typecheck_fmla("forall N. N > 0 | N = 0").expect("parse and typecheck");
        typecheck_fmla("forall N. 0 = 0").expect("parse and typecheck");
        typecheck_fmla("forall N. a_number >= N").expect("parse and typecheck");
    }
}
