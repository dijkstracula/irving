#[cfg(test)]
mod parser {
    use crate::parser::ivy::{IvyParser, Rule};
    use pest_consume::Parser;

    #[test]
    fn test_state_and_actions() {
        let prog = include_str!("programs/001_state_and_actions.ivy");
        let res = IvyParser::parse(Rule::prog, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let _ast = IvyParser::prog(res).expect("AST generation failed");
    }

    #[test]
    fn test_safety_and_invariants() {
        let prog = include_str!("programs/002_safety_and_invariants.ivy");
        let res = IvyParser::parse(Rule::prog, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let _ast = IvyParser::prog(res).expect("AST generation failed");
    }

    #[test]
    fn test_modules() {
        let prog = include_str!("programs/003_modules.ivy");
        let res = IvyParser::parse(Rule::prog, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let _ast = IvyParser::prog(res).expect("AST generation failed");
    }

    #[test]
    fn test_loops() {
        let prog = include_str!("programs/004_loops.ivy");
        let res = IvyParser::parse(Rule::prog, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let _ast = IvyParser::prog(res).expect("AST generation failed");
    }

    #[test]
    fn test_isolates() {
        let prog = include_str!("programs/005_isolate_and_mixins.ivy");
        let res = IvyParser::parse(Rule::prog, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let _ast = IvyParser::prog(res).expect("AST generation failed");
    }

    #[test]
    fn test_append() {
        let prog = include_str!("programs/100_append.ivy");
        let res = IvyParser::parse(Rule::prog, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let _ast = IvyParser::prog(res).expect("AST generation failed");
    }

    #[test]
    fn test_append2() {
        let prog = include_str!("programs/101_append2.ivy");
        let res = IvyParser::parse(Rule::prog, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let _ast = IvyParser::prog(res).expect("AST generation failed");
    }
}

#[cfg(test)]
mod typechecker {
    use crate::{
        parser::ivy::{IvyParser, Rule},
        typechecker::inference::TypeChecker,
    };
    use pest_consume::Parser;

    #[test]
    fn test_state_and_actions() {
        let prog = include_str!("programs/005_isolate_and_mixins.ivy");
        let res = IvyParser::parse(Rule::prog, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut prog = IvyParser::prog(res).expect("AST generation failed");

        let mut tc = TypeChecker::new();
        let _res = tc.visit(&mut prog);
    }
}
