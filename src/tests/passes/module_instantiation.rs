#[cfg(test)]
mod tests {
    use crate::ast::declarations::Decl;
    use crate::parser::ivy::{IvyParser, Rule};
    use crate::passes::module_instantiation::ModuleInstantiation;
    use crate::visitor::visitor::Visitable;
    use pest_consume::Parser;

    fn module_from_src(prog: &str) -> Decl {
        let res = IvyParser::parse(Rule::module_decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        Decl::Module(IvyParser::module_decl(res).expect("AST generation failed"))
    }

    #[test]
    fn test_module_instantiation() {
        let mut iso = module_from_src(
            "module array(domain, range) = { 
            type this

            action get(a:this,x:domain) returns (y:range)
            action set(a:this,x:domain,y:range) returns (a:this)
        }",
        );
        let expected = module_from_src(
            "module array = { 
            type this

            action get(a:this,x:int) returns (y:bool)
            action set(a:this,x:int,y:bool) returns (a:this)
        }",
        );

        let mut mi = ModuleInstantiation::new(
            [
                ("domain".into(), ["int".into()].into()),
                ("range".into(), ["bool".into()].into()),
            ]
            .into(),
        );

        iso.visit(&mut mi).unwrap().modifying(&mut iso).unwrap();
        assert_eq!(iso, expected);
    }
}
