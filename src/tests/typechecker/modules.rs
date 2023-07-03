#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{
        ast::declarations::Decl,
        parser::ivy::{IvyParser, Rule},
        typechecker::{
            inference::TypeChecker,
            sorts::{IvySort, Module},
        },
        visitor::visitor::Visitable,
    };
    use pest_consume::Parser;

    fn module_from_src(prog: &str) -> Decl {
        let res = IvyParser::parse(Rule::module_decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        Decl::Module(IvyParser::module_decl(res).expect("AST generation failed"))
    }

    #[test]
    fn test_empty_module() {
        let mut iso = module_from_src("module m = { }");

        let sort = IvySort::Module(Module {
            args: vec![],
            fields: HashMap::new(),
        });

        let mut tc = TypeChecker::new();
        let res = iso.visit(&mut tc).unwrap().modifying(&mut iso).unwrap();
        assert_eq!(res, sort);
        assert_eq!(tc.bindings.lookup(&"m".to_owned()), Some(sort));
    }

    #[test]
    fn test_mod_bind_and_alias_this() {
        let mut iso = module_from_src(
            "module array(domain, range) = { 
            type this
            alias t = this
        }",
        );

        let sort = IvySort::Module(Module {
            args: vec![
                ("domain".into(), IvySort::SortVar(1)),
                ("range".into(), IvySort::SortVar(2)),
            ],
            fields: [
                ("this".into(), IvySort::SortVar(0)),
                ("t".into(), IvySort::SortVar(0)),
            ]
            .into(),
        });

        let mut tc = TypeChecker::new();
        let res = iso.visit(&mut tc).unwrap().modifying(&mut iso).unwrap();
        assert_eq!(res, sort);
        assert_eq!(tc.bindings.lookup(&"array".to_owned()), Some(sort));

        // `this` should not escape its scope.
        assert_eq!(tc.bindings.lookup(&"this".to_owned()), None);
    }

    #[test]
    fn test_mod_actions() {
        let mut iso = module_from_src(
            "module array(domain, range) = { 
            type this
        }",
        );

        let sort = IvySort::Module(Module {
            args: vec![
                ("domain".into(), IvySort::SortVar(1)),
                ("range".into(), IvySort::SortVar(2)),
            ],
            fields: [("this".into(), IvySort::SortVar(0))].into(),
        });

        let mut tc = TypeChecker::new();
        let res = iso.visit(&mut tc).unwrap().modifying(&mut iso).unwrap();
        assert_eq!(res, sort);
        assert_eq!(tc.bindings.lookup(&"array".to_owned()), Some(sort));

        // `this` should not escape its scope.
        assert_eq!(tc.bindings.lookup(&"this".to_owned()), None);
    }
}
