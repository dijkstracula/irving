#[cfg(test)]
mod tests {
    use crate::{
        ast::declarations::Decl,
        parser::ivy::{IvyParser, Rule},
    };
    use pest_consume::Parser;

    fn module_from_src(prog: &str) -> Decl {
        let res = IvyParser::parse(Rule::module_decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        Decl::Module(IvyParser::module_decl(res).expect("AST generation failed"))
    }
}
