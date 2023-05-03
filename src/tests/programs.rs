#[cfg(test)]
mod tests {
    use pest_consume::Parser;
    use crate::parser::{IvyParser, Rule};

    #[test]
    fn test_state_and_actions() {
        let prog = include_str!("programs/001_state_and_actions.ivy");
        let res = IvyParser::parse(Rule::prog, &prog)
            .expect("Parsing failed")
            .single().unwrap();
        let _ast = IvyParser::prog(res)
            .expect("AST generation failed");
    }
}