#[cfg(test)]
mod tests {
    use crate::ast::declarations::Decl;
    use crate::parser::ivy::{IvyParser, Rule};
    use crate::passes::module_instantiation;
    use crate::typechecker::inference::TypeChecker;
    use crate::typechecker::sorts::{IvySort, Module};
    use crate::visitor::ast::Visitable;
    use pest_consume::Parser;

    fn sort_from_module_src(prog: &str) -> Module {
        let res = IvyParser::parse(Rule::module_decl, &prog)
            .expect("Parsing")
            .single()
            .unwrap();
        let mut module = Decl::Module(IvyParser::module_decl(res).expect("AST generation"));

        let mut tc = TypeChecker::new();
        match module
            .visit(&mut tc)
            .expect("Typechecking")
            .modifying(&mut module)
            .expect("Ast mutatinon")
        {
            IvySort::Module(m) => m,
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_module_instantiation() {
        let module = sort_from_module_src(
            "module array(domain, range) = { 
            type this

            action get(a:this,x:domain) returns (y:range)
            action set(a:this,x:domain,y:range) returns (a:this)
        }",
        );

        let expected = sort_from_module_src(
            "module array = { 
            type this

            action get(a:this,x:unbounded_sequence) returns (y:bool)
            action set(a:this,x:unbounded_sequence,y:bool) returns (a:this)
        }",
        );

        let instantiated =
            module_instantiation::instantiate(module, vec![IvySort::Number, IvySort::Bool])
                .expect("instantiation");
        assert_eq!(instantiated, IvySort::Module(expected));
    }

    /*
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
    */
}
