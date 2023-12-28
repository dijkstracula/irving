#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::{
        extraction::java::extraction::Extractor,
        tests::helpers,
        typechecker::{
            inference::SortInferer,
            sorts::{self, IvySort},
        },
        visitor::ast::Visitable,
    };

    fn normalize_output(input: &str) -> String {
        let input = input.replace("\n", "");
        input.split_whitespace().join(" ").into()
    }

    #[test]
    fn extract_action_forward_ref() {
        let fragment = "action foo(i: nat, b: bool)";
        let mut ast = helpers::decl_from_src(fragment);

        let mut tc = SortInferer::new();
        ast.visit(&mut tc).expect("typechecking failed");

        let mut e = Extractor::<String>::new();
        ast.visit(&mut e).expect("extraction failed");
        assert_eq!(
            "protected Action2<Long, Boolean, Void> foo = new Action2<>()",
            e.pp.out
        );
    }

    #[test]
    fn extract_action_and_decl() {
        let fragment = "action foo(i: nat, b: bool) = { } ";
        let mut ast = helpers::decl_from_src(fragment);

        let mut tc = SortInferer::new();
        ast.visit(&mut tc).expect("typechecking failed");

        let mut e = Extractor::<String>::new();
        ast.visit(&mut e).expect("extraction failed");
        assert_eq!(
            "protected Action2<Long, Boolean, Void> foo = new Action2<>((Long i, Boolean b) -> {return null;})",
            e.pp.out
                .replace('\n', "")
                .replace("    ", "")
        );
    }

    #[test]
    fn extract_after_decl() {
        let fragment = "after foo(i: nat, b: bool) { count := 0 } ";
        let mut ast = helpers::decl_from_src(fragment);

        let mut tc = SortInferer::new();
        tc.bindings
            .append(
                "foo".into(),
                IvySort::action_sort(
                    vec!["i".into(), "b".into()],
                    vec![IvySort::Number, IvySort::Bool],
                    sorts::ActionRet::Unit,
                ),
            )
            .unwrap();

        tc.bindings.append("count".into(), IvySort::Number).unwrap();

        ast.visit(&mut tc).expect("typechecking failed");

        let mut e = Extractor::<String>::new();
        ast.visit(&mut e).expect("extraction failed");
        assert_eq!(
            "foo.onAfter((Long i, Boolean b) -> {
    count = 0;
})",
            e.pp.out
        );
    }

    #[test]
    fn extract_after_init() {
        // After init is inlined right in the constructor, rather than
        // emitting a call to an Action object.
        let fragment = "after init { count := 0 } ";
        let mut ast = helpers::decl_from_src(fragment);

        let mut tc = SortInferer::new();
        tc.bindings
            .append("init".into(), sorts::Module::init_action_sort())
            .unwrap();

        tc.bindings.append("count".into(), IvySort::Number).unwrap();

        ast.visit(&mut tc).expect("typechecking failed");

        let mut e = Extractor::<String>::new();
        ast.visit(&mut e).expect("extraction failed");
        assert_eq!("count = 0", e.pp.out);
    }

    #[test]
    fn extract_assign_nat_lit() {
        let fragment = "count := 0";
        let mut ast = helpers::action_from_decl(fragment);

        let mut si = SortInferer::new();
        si.bindings.append("count".into(), IvySort::Number).unwrap();
        ast.visit(&mut si)
            .expect("typechecking failed")
            .modifying(&mut ast);

        let mut e = Extractor::<String>::new();
        ast.visit(&mut e).expect("extraction failed");
        assert_eq!("count = 0", e.pp.out);
    }

    #[test]
    fn extract_assign_nat_nonlit() {
        let fragment = "count := (1 + 1)";
        let mut ast = helpers::action_from_decl(fragment);

        let mut si = SortInferer::new();
        si.bindings.append("count".into(), IvySort::Number).unwrap();
        ast.visit(&mut si)
            .expect("typechecking failed")
            .modifying(&mut ast);

        let mut e = Extractor::<String>::new();
        ast.visit(&mut e).expect("extraction failed");
        assert_eq!("count = (1 + 1) < 0 ? 0 : (1 + 1)", e.pp.out);
    }

    #[test]
    fn extract_assign_range_lit() {
        let fragment = "count := 0";
        let mut ast = helpers::action_from_decl(fragment);

        let mut si = SortInferer::new();
        si.bindings
            .append("count".into(), IvySort::BoundedSequence(0, 3))
            .unwrap();
        ast.visit(&mut si)
            .expect("typechecking failed")
            .modifying(&mut ast);

        let mut e = Extractor::<String>::new();
        ast.visit(&mut e).expect("extraction failed");
        assert_eq!("count = 0", e.pp.out);
    }

    #[test]
    fn extract_assign_range_saturated_lit() {
        let fragment = "count := 42";
        let mut ast = helpers::action_from_decl(fragment);

        let mut si = SortInferer::new();
        si.bindings
            .append("count".into(), IvySort::BoundedSequence(0, 3))
            .unwrap();
        ast.visit(&mut si)
            .expect("typechecking failed")
            .modifying(&mut ast);

        let mut e = Extractor::<String>::new();
        ast.visit(&mut e).expect("extraction failed");
        assert_eq!("count = 3", e.pp.out);
    }

    #[test]
    fn extract_assign_range_binop() {
        let fragment = "count := (1 + 1)";
        let mut ast = helpers::action_from_decl(fragment);

        let mut si = SortInferer::new();
        si.bindings
            .append("count".into(), IvySort::BoundedSequence(0, 3))
            .unwrap();
        ast.visit(&mut si)
            .expect("typechecking failed")
            .modifying(&mut ast);

        let mut e = Extractor::<String>::new();
        ast.visit(&mut e).expect("extraction failed");
        assert_eq!(
            "count = ((1 + 1) >= 3 ? 3 : ((1 + 1) < 0 ? 0 : (1 + 1)))",
            e.pp.out
        );
    }

    #[test]
    fn extract_call() {
        let fragment = "inc(1)";
        let mut ast = helpers::rval_from_src(fragment);

        let mut tc = SortInferer::new();
        tc.bindings
            .append(
                "inc".into(),
                IvySort::action_sort(
                    vec!["i".into()],
                    vec![IvySort::Number],
                    sorts::ActionRet::named("ret", IvySort::Number),
                ),
            )
            .unwrap();
        ast.visit(&mut tc).expect("typechecking failed");

        let mut e = Extractor::<String>::new();
        ast.visit(&mut e).expect("extraction failed");
        assert_eq!(fragment, e.pp.out);
    }

    #[test]
    fn extract_call_compound_arg() {
        let fragment = "inc(1 + 1)";
        let mut ast = helpers::rval_from_src(fragment);

        let mut tc = SortInferer::new();
        tc.bindings
            .append(
                "inc".into(),
                IvySort::action_sort(
                    vec!["i".into()],
                    vec![IvySort::Number],
                    sorts::ActionRet::named("ret", IvySort::Number),
                ),
            )
            .unwrap();
        ast.visit(&mut tc).expect("typechecking failed");

        let mut e = Extractor::<String>::new();
        ast.visit(&mut e).expect("extraction failed");
        assert_eq!("inc((1 + 1) < 0 ? 0 : (1 + 1))", e.pp.out);
    }

    #[test]
    fn extract_field_access() {
        let fragment = "a.b";

        let mut ast = helpers::rval_from_src(fragment);

        let mut e = Extractor::<String>::new();
        ast.visit(&mut e).expect("extraction failed");
        assert_eq!(fragment, e.pp.out);
    }

    #[test]
    fn extract_module_simple() {
        let mut ast = helpers::module_from_src(
            "module nat_pair = { 
                var x: nat
                var y: nat
        }");
        let mut tc = SortInferer::new();
        ast.visit(&mut tc).expect("typechecking failed");

        let mut e = Extractor::<String>::new();
        ast.visit(&mut e).expect("extraction failed");

        assert!(e.pp.out.contains("class IvyMod_nat_pair ")); // module name mangling
        assert!(e.pp.out.contains("long x;"));
        assert!(e.pp.out.contains("long y;"));
    }

    #[test]
    fn extract_module_parameterized() {
        let mut ast = helpers::module_from_src(
            "module pair(t) = { 
                var x: t
                var y: t
        }");
        let mut tc = SortInferer::new();
        ast.visit(&mut tc).expect("typechecking failed");

        let mut e = Extractor::<String>::new();
        ast.visit(&mut e).expect("extraction failed");

        assert!(e.pp.out.contains("class IvyMod_pair ")); // module name mangling
        assert!(e.pp.out.contains("t x;"));
        assert!(e.pp.out.contains("t y;"));
    }

    // Logic

    #[test]
    fn extract_forall() {
        let mut e = Extractor::<String>::new();

        let fragment = "forall X: nat . X > 5 -> X > 10";
        let mut ast = helpers::fmla_from_src(fragment);
        let mut tc = SortInferer::new();
        ast.visit(&mut tc).expect("typechecking failed");
        ast.visit(&mut e).expect("traversal failed");

        assert_eq!(
            normalize_output(&e.pp.out),
            "IntStream.range(6, 11).allMatch(X -> { return !(X > 5) || X > 10;})"
        );
    }

    #[test]
    fn extract_nested_forall() {
        let mut e = Extractor::<String>::new();

        let fragment = "forall P1:pid . forall P2:pid . P1 <= P2";

        let mut ast = helpers::fmla_from_src(fragment);
        let mut si = SortInferer::new();
        si.bindings
            .append("pid".into(), IvySort::BoundedSequence(0, 2))
            .unwrap();

        ast.visit(&mut si).expect("typechecking failed");
        ast.visit(&mut e).expect("traversal failed");
        assert_eq!(
            normalize_output(&e.pp.out),
            normalize_output(
                "IntStream.range(0, 3).allMatch(P1 -> {
                return IntStream.range(0, 3).allMatch(P2 -> {
                    return P1 <= P2;
                });})"
            )
        );
    }

    // Terminals

    #[test]
    fn extract_bool() {
        let mut e = Extractor::<String>::new();
        let fragment = "true";
        let mut ast = helpers::rval_from_src(fragment);
        ast.visit(&mut e).expect("traversal failed");
        assert_eq!(fragment, e.pp.out);

        let mut e = Extractor::<String>::new();
        let fragment = "false";
        let mut ast = helpers::rval_from_src(fragment);
        ast.visit(&mut e).expect("traversal failed");
        assert_eq!(fragment, e.pp.out);
    }

    #[test]
    fn extract_arith() {
        let mut e = Extractor::<String>::new();

        let fragment = "43 - 1";
        let mut ast = helpers::rval_from_src(fragment);
        ast.visit(&mut e).expect("traversal failed");
        assert_eq!(fragment, e.pp.out);
    }

    #[test]
    fn extract_vardecl() {
        let fragment = "var i: nat";
        let mut ast = helpers::decl_from_src(fragment);

        let mut tc = SortInferer::new();
        ast.visit(&mut tc).expect("typechecking failed");

        let mut e = Extractor::<String>::new();
        ast.visit(&mut e).expect("extraction failed");
        assert_eq!("long i", e.pp.out);
    }
}
