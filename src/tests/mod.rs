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
}
