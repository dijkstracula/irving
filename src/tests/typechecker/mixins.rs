#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use crate::{
        ast::declarations::Decl,
        parser::ivy::{IvyParser, Rule},
        typechecker::{
            inference::SortInferer,
            sorts::{self, IvySort, Module, Object},
            TypeError,
        },
        visitor::ast::Visitable,
    };
    use pest_consume::Parser;

    fn process_from_src(prog: &str) -> Decl {
        let res = IvyParser::parse_with_userdata(Rule::process_decl, prog, prog.into())
            .expect("Parsing failed")
            .single()
            .unwrap();
        Decl::Object(IvyParser::process_decl(res).expect("AST generation failed"))
    }

    #[test]
    fn test_action_and_impl() {
        let mut proc = process_from_src(
            "process p = { 
            action foo(x: bool) returns (y: bool)
            implement foo { y := x }
        }",
        );

        let sort = IvySort::Object(Object {
            args: BTreeMap::from([]),
            fields: [
                ("init".to_owned(), Module::init_action_sort()),
                (
                    "foo".to_owned(),
                    IvySort::Action(
                        ["x".into()].into(),
                        sorts::ActionArgs::List([IvySort::Bool].into()),
                        sorts::ActionRet::named("y", IvySort::Bool),
                    ),
                ),
            ]
            .into(),
        });

        let mut tc = SortInferer::new();
        let res = proc.visit(&mut tc).unwrap().modifying(&mut proc).unwrap();
        assert_eq!(res, sort);

        assert_eq!(tc.bindings.lookup_sym("p"), Some(&sort));
    }

    #[test]
    fn test_action_and_impl_identical_sigs() {
        let mut proc = process_from_src(
            "process p = { 
            action foo(x: bool) returns (y: bool)
            implement foo(x: bool) returns (y: bool) { y := x }
        }",
        );

        let sort = IvySort::Object(Object {
            args: BTreeMap::from([]),
            fields: [
                ("init".to_owned(), Module::init_action_sort()),
                (
                    "foo".to_owned(),
                    IvySort::Action(
                        ["x".into()].into(),
                        sorts::ActionArgs::List([IvySort::Bool].into()),
                        sorts::ActionRet::named("y", IvySort::Bool),
                    ),
                ),
            ]
            .into(),
        });

        let mut tc = SortInferer::new();
        let res = proc.visit(&mut tc).unwrap().modifying(&mut proc).unwrap();
        assert_eq!(res, sort);
        assert_eq!(tc.bindings.lookup_sym("p"), Some(&sort));
    }

    #[test]
    fn test_action_and_impl_rebound_sigs() {
        let mut proc = process_from_src(
            "process p = { 
            action foo(x: bool) returns (y: bool)
            implement foo(z: bool) returns (q: bool) { y := x }
        }",
        );

        let sort = IvySort::Object(Object {
            args: BTreeMap::from([]),
            fields: [
                ("init".to_owned(), Module::init_action_sort()),
                (
                    "foo".to_owned(),
                    IvySort::Action(
                        ["x".into()].into(),
                        sorts::ActionArgs::List([IvySort::Bool].into()),
                        sorts::ActionRet::named("y", IvySort::Bool),
                    ),
                ),
            ]
            .into(),
        });

        let mut tc = SortInferer::new();
        let res = proc.visit(&mut tc).unwrap().modifying(&mut proc).unwrap();
        assert_eq!(res, sort);
        assert_eq!(tc.bindings.lookup_sym("p"), Some(&sort));
    }

    #[test]
    fn test_action_and_impl_different_sorts() {
        let mut proc = process_from_src(
            "process p = { 
            action foo(x: bool) returns (y: bool)
            implement foo(x: unbounded_sequence) returns (q: bool) { y := true }
        }",
        );

        let mut tc = SortInferer::new();
        let res = proc.visit(&mut tc).expect_err("Should not typecheck");
        assert_eq!(
            res.downcast::<TypeError>().unwrap(),
            TypeError::UnificationError(IvySort::Bool, IvySort::Number)
        );
    }
}
