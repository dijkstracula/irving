mod extraction;
mod parser;
mod passes;
mod programs;
mod typechecker;

#[cfg(test)]
mod helpers {
    use crate::{
        ast::{declarations::Decl, toplevels::Prog},
        extraction::ivy::Extractor,
        parser::ivy::{IvyParser, Rule},
        passes::{global_lowerer::GlobalLowerer, isolate_normalizer::IsolateNormalizer},
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

        let mut gl = GlobalLowerer::new();
        let mut nm = IsolateNormalizer::new();
        let mut tc = TypeChecker::new();

        prog.visit(&mut gl)
            .expect("lowering globals")
            .modifying(&mut prog)
            .unwrap();

        prog.visit(&mut nm)
            .expect("ast normalizing")
            .modifying(&mut prog)
            .unwrap();

        let mut pprint = Extractor::new();
        prog.visit(&mut pprint)
            .expect("prettyprint")
            .modifying(&mut prog)
            .unwrap();
        println!("{}", pprint.pp.out);

        prog.visit(&mut tc)
            .expect("typechecking")
            .modifying(&mut prog)
            .unwrap();

        prog
    }

    pub fn decl_from_src(prog: &str) -> Decl {
        let res = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(res).expect("AST generation failed")
    }
}
