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

    #[test]
    fn test_safety_and_invariants() {
        let prog = include_str!("programs/002_safety_and_invariants.ivy");
        let res = IvyParser::parse(Rule::prog, &prog)
            .expect("Parsing failed")
            .single().unwrap();
        let _ast = IvyParser::prog(res)
            .expect("AST generation failed");
    }

    #[test]
    fn test_modules() {
        let prog = include_str!("programs/003_modules.ivy");
        let res = IvyParser::parse(Rule::prog, &prog)
            .expect("Parsing failed")
            .single().unwrap();
        let _ast = IvyParser::prog(res)
            .expect("AST generation failed");
    }

    #[test]
    fn test_loops() {
        let prog = include_str!("programs/004_loops.ivy");
        let res = IvyParser::parse(Rule::prog, &prog)
            .expect("Parsing failed")
            .single().unwrap();
        let _ast = IvyParser::prog(res)
            .expect("AST generation failed");
    }

    #[test]
    fn test_append() {
        let prog = include_str!("programs/100_append.ivy");
        let res = IvyParser::parse(Rule::prog, &prog)
            .expect("Parsing failed")
            .single().unwrap();
        let _ast = IvyParser::prog(res)
            .expect("AST generation failed");
    }
}