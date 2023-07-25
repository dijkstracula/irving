#[cfg(test)]
mod tests {
    use crate::{
        tests::helpers,
        typechecker::{
            inference::TypeChecker,
            sorts::{self, IvySort, Module},
        },
        visitor::ast::Visitable,
    };

    #[test]
    fn test_empty_module() {
        let mut iso = helpers::module_from_src("module m = { }");

        let sort = IvySort::Module(Module {
            name: "m".into(),
            args: vec![],
            fields: [("init".into(), Module::init_action_sort())].into(),
        });

        let mut tc = TypeChecker::new();
        let res = iso.visit(&mut tc).unwrap().modifying(&mut iso).unwrap();
        assert_eq!(res, sort);
        assert_eq!(tc.bindings.lookup_sym("m"), Some(&sort));
    }

    #[test]
    fn test_after_init() {
        let mut iso = helpers::module_from_src(
            "module m = { 
            after init { }
        }",
        );

        let sort = IvySort::Module(Module {
            name: "m".into(),
            args: vec![],
            fields: [("init".into(), Module::init_action_sort())].into(),
        });

        let mut tc = TypeChecker::new();
        let res = iso.visit(&mut tc).unwrap().modifying(&mut iso).unwrap();
        assert_eq!(res, sort);
        assert_eq!(tc.bindings.lookup_sym("m"), Some(&sort));
    }

    #[test]
    fn test_mod_bind_and_alias_this() {
        let mut iso = helpers::module_from_src(
            "module array(domain, range) = { 
            type this
            alias t = this
        }",
        );

        let sort = IvySort::Module(Module {
            name: "array".into(),
            args: vec![
                ("domain".into(), IvySort::SortVar(1)),
                ("range".into(), IvySort::SortVar(2)),
            ],
            fields: [
                ("this".into(), IvySort::This),
                ("t".into(), IvySort::This),
                ("init".into(), Module::init_action_sort()),
            ]
            .into(),
        });

        let mut tc = TypeChecker::new();
        let res = iso.visit(&mut tc).unwrap().modifying(&mut iso).unwrap();
        assert_eq!(res, sort);
        assert_eq!(tc.bindings.lookup_sym("array"), Some(&sort));

        // `this` should not escape its scope.
        assert_eq!(tc.bindings.lookup_sym("this"), None);
    }

    #[test]
    fn test_mod_actions() {
        let mut iso = helpers::module_from_src(
            "module array(domain, range) = { 
            type this

            action get(a:this,x:domain) returns (y:range)
            action set(a:this,x:domain,y:range) returns (a:this)
        }",
        );

        let sort = IvySort::Module(Module {
            name: "array".into(),
            args: vec![
                ("domain".into(), IvySort::SortVar(1)),
                ("range".into(), IvySort::SortVar(2)),
            ],
            fields: [
                ("this".into(), IvySort::This),
                (
                    "get".into(),
                    IvySort::action_sort(
                        vec!["a".into(), "x".into()],
                        vec![IvySort::This, IvySort::SortVar(1)],
                        sorts::ActionRet::named("y".into(), IvySort::SortVar(2)),
                    ),
                ),
                (
                    "set".into(),
                    IvySort::action_sort(
                        vec!["a".into(), "x".into(), "y".into()],
                        vec![IvySort::This, IvySort::SortVar(1), IvySort::SortVar(2)],
                        sorts::ActionRet::named("a".into(), IvySort::This),
                    ),
                ),
                ("init".into(), Module::init_action_sort()),
            ]
            .into(),
        });

        let mut tc = TypeChecker::new();
        let res = iso.visit(&mut tc).unwrap().modifying(&mut iso).unwrap();

        assert_eq!(res, sort);
        assert_eq!(tc.bindings.lookup_sym("array"), Some(&sort));

        // `this` should not escape its scope.
        assert_eq!(tc.bindings.lookup_sym("this"), None);
    }

    #[test]
    fn multimod_instantiation() {
        // Our dependent module.
        let mut tcp = helpers::tcp_moduledecl();
        let mut tc = TypeChecker::new();
        tcp.visit(&mut tc).unwrap().modifying(&mut tcp).unwrap();

        let mut proc = helpers::process_from_decl(
            "process host = { 
                instance net: tcp.net(unbounded_sequence)
            }",
        );

        proc.visit(&mut tc).unwrap().modifying(&mut proc).unwrap();
        println!(
            "{:?}",
            tc.bindings.lookup_ident(&vec!["host".into(), "net".into()])
        );
        println!("{:?}", tc.bindings.resolve(&IvySort::SortVar(2)));
    }
}
