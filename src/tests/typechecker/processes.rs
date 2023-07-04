#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{
        ast::{declarations::Decl, expressions::Expr},
        parser::ivy::{IvyParser, Rule},
        passes::isolate_normalizer::IsolateNormalizer,
        typechecker::{
            inference::TypeChecker,
            sorts::{IvySort, Module, Process},
        },
        visitor::visitor::Visitable,
    };
    use pest_consume::Parser;

    fn isolate_from_src(prog: &str) -> Decl {
        let res = IvyParser::parse(Rule::isolate_decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut proc = Decl::Isolate(IvyParser::isolate_decl(res).expect("AST generation failed"));

        // Since we'll be assuming that the normalizer has run prior to
        // typechecking, enforce it here.
        let mut norm = IsolateNormalizer::new();
        proc.visit(&mut norm).unwrap().modifying(&mut proc).unwrap();
        proc
    }

    fn typechecker_with_bindings() -> TypeChecker {
        let mut tc = TypeChecker::new();

        // type pid: 0..3
        tc.bindings
            .append(
                "pid".into(),
                IvySort::Range(Box::new(Expr::Number(0)), Box::new(Expr::Number(3))),
            )
            .unwrap();
        // interpret byte -> bv[8]
        tc.bindings
            .append("byte".into(), IvySort::BitVec(8))
            .unwrap();

        tc.bindings
            .append(
                "sock".into(),
                IvySort::Module(Module {
                    args: [].into(),
                    fields: [].into(),
                }),
            )
            .unwrap();
        tc.bindings
            .append(
                "file".into(),
                IvySort::Module(Module {
                    args: [].into(),
                    fields: [].into(),
                }),
            )
            .unwrap();
        tc
    }

    #[test]
    fn test_empty_process() {
        let mut iso = isolate_from_src("process p = { }");

        let sort = IvySort::Process(Process {
            args: vec![],
            impl_fields: HashMap::new(),
            spec_fields: HashMap::new(),
            common_impl_fields: HashMap::new(),
            common_spec_fields: HashMap::new(),
        });

        let mut tc = TypeChecker::new();
        let res = iso.visit(&mut tc).unwrap().modifying(&mut iso).unwrap();
        assert_eq!(res, sort);

        assert_eq!(tc.bindings.lookup_sym(&"p".to_owned()), Some(&sort));
    }

    #[test]
    fn test_proc_with_params() {
        let mut iso = isolate_from_src("process host(self:pid) = {}");
        let sort = IvySort::Process(Process {
            args: vec![(
                "self".into(),
                IvySort::Range(Box::new(Expr::Number(0)), Box::new(Expr::Number(3))),
            )],
            impl_fields: HashMap::new(),
            spec_fields: HashMap::new(),
            common_impl_fields: HashMap::new(),
            common_spec_fields: HashMap::new(),
        });

        let mut tc = typechecker_with_bindings();
        let res = iso.visit(&mut tc).unwrap().modifying(&mut iso).unwrap();
        assert_eq!(res, sort);

        assert_eq!(tc.bindings.lookup_sym(&"host".to_owned()), Some(&sort));
    }

    #[test]
    fn test_proc_with_implicit_impl() {
        let mut iso = isolate_from_src(
            "process host(self:pid) = {
            var is_up: bool
        }",
        );
        let sort = IvySort::Process(Process {
            args: vec![(
                "self".into(),
                IvySort::Range(Box::new(Expr::Number(0)), Box::new(Expr::Number(3))),
            )],
            impl_fields: [("is_up".into(), IvySort::Bool)].into(),
            spec_fields: HashMap::new(),
            common_impl_fields: HashMap::new(),
            common_spec_fields: HashMap::new(),
        });

        let mut tc = typechecker_with_bindings();
        let res = iso.visit(&mut tc).unwrap().modifying(&mut iso).unwrap();
        assert_eq!(res, sort);

        assert_eq!(tc.bindings.lookup_sym(&"host".to_owned()), Some(&sort),);
    }

    #[test]
    fn test_append1_host() {
        let mut iso = isolate_from_src(
            "process host(self:pid) = {
                export action append(val: byte)
                import action show(content: file)
                instance sock: net.socket
                var contents: file

                after init {
                    contents := file.empty;
                }

                implement append {
                    contents := contents.append(val);
                    sock.send(host(1-self).sock.id, val);
                    show(contents);
                }

                implement sock.recv(src: tcp.endpoint, val:byte) {
                    contents := contents.append(val);
                    show(contents);
                }
        }",
        );

        let mut tc = typechecker_with_bindings();
        let _res = iso.visit(&mut tc).unwrap().modifying(&mut iso).unwrap();
    }
}
