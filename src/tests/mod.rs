mod extraction;
mod parser;
mod passes;
mod programs;
mod typechecker;

#[cfg(test)]
mod helpers {
    use crate::{
        ast::toplevels::Prog,
        parser::ivy::{IvyParser, Rule},
        passes::isolate_normalizer::IsolateNormalizer,
        typechecker::inference::TypeChecker,
        visitor::visitor::Visitable,
    };
    use pest_consume::Parser;

    pub fn prog_from_filename(path: &str) -> Prog {
        let prog = std::fs::read_to_string(path).unwrap();
        let res = IvyParser::parse(Rule::prog, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::prog(res).expect("AST generation failed")
    }

    #[allow(dead_code)]
    pub fn typechecked_from_filename(path: &str) -> Prog {
        let text = std::fs::read_to_string(path).unwrap();
        let res = IvyParser::parse(Rule::prog, &text)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut prog = IvyParser::prog(res).expect("AST generation failed");

        let mut nm = IsolateNormalizer::new();
        let mut tc = TypeChecker::new();
        prog.visit(&mut nm)
            .expect("ast normalizing")
            .modifying(&mut prog)
            .unwrap();

        prog.visit(&mut tc)
            .expect("typechecking")
            .modifying(&mut prog)
            .unwrap();

        prog
    }
}
