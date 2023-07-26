#[cfg(test)]
mod tests {
    use crate::ast::logic::*;
    use crate::parser::ivy::{IvyParser, Rule};
    use crate::typechecker::inference::TypeChecker;
    use crate::visitor::ast::Visitable;
    use pest_consume::Parser;

    fn parse_fmla(fragment: &str) -> anyhow::Result<Fmla> {
        let res = IvyParser::parse(Rule::fmla, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut ast = IvyParser::fmla(res)?;

        let mut tc = TypeChecker::new();
        ast.visit(&mut tc)?.modifying(&mut ast)?;
        Ok(ast)
    }

    #[test]
    fn parse_universal_quant() {
        let _ast = parse_fmla("forall X . X").expect("Parse and typecheck");
        println!("{:?}", _ast);
    }
}
