#[cfg(test)]
mod tests {
    use crate::ast::declarations::Decl;
    use crate::parser::ivy::{IvyParser, Rule};
    use crate::passes::module_instantiation::{ModuleInstantiation, ModuleInstantiationError};
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

    #[test]
    fn test_module_name_collision() {
        let vecimpl = "module vec(t) = {
            type this

            # Note that the LHS of this alias is bound to a module parameter
            alias t = this
        }";
        let parsed = IvyParser::parse(Rule::module_decl, &vecimpl)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut vecdecl =
            Decl::Module(IvyParser::module_decl(parsed).expect("AST generation failed"));

        let mut mi = ModuleInstantiation::new([("t".into(), ["int".into()].into())].into());
        let err = vecdecl.visit(&mut mi).unwrap_err();
        assert_eq!(
            err.downcast::<ModuleInstantiationError>().unwrap(),
            ModuleInstantiationError::ModuleArgumentRebinding("t".into())
        );
    }
}
