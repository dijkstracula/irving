#[cfg(test)]
mod tests {
    use std::{collections::BTreeMap, vec};

    use crate::{
        ast::{declarations::Decl, expressions::Expr},
        parser::ivy::{IvyParser, Rule},
        typechecker::{
            inference::TypeChecker,
            sorts::{IvySort, Module, Process},
        },
        visitor::ast::Visitable,
    };
    use pest_consume::Parser;

    fn isolate_from_src(prog: &str) -> Decl {
        let res = IvyParser::parse(Rule::isolate_decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        Decl::Isolate(IvyParser::isolate_decl(res).expect("AST generation failed"))
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
                "net".into(),
                IvySort::Module(Module {
                    name: "net".into(),
                    args: [].into(),
                    fields: [(
                        "socket".into(),
                        IvySort::Module(Module {
                            name: "socket".into(),
                            args: vec![],
                            fields: [
                                ("id".into(), tc.bindings.lookup_sym("pid").unwrap().clone()),
                                (
                                    "send".into(),
                                    IvySort::function_sort(
                                        vec![tc.bindings.lookup_sym("pid").unwrap().clone()],
                                        IvySort::Unit,
                                    ),
                                ),
                                (
                                    "recv".into(),
                                    IvySort::function_sort(
                                        vec![IvySort::Bool, IvySort::BitVec(8)],
                                        IvySort::Unit,
                                    ),
                                ),
                            ]
                            .into(),
                        }),
                    )]
                    .into(),
                }),
            )
            .unwrap();

        // TODO: this would be a good test for the module instantiator in its own right.
        let vecimpl = "module vec(elems) = {
            type this
            alias t = this

            action empty returns (a: t)

            action append(a:t,e:elems) returns (b:t)
        }";
        let parsed = IvyParser::parse(Rule::module_decl, &vecimpl)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let vecdecl = Decl::Module(IvyParser::module_decl(parsed).expect("AST generation failed"));

        let mut filedecl = vecdecl.clone();
        /*
        let mut mr = ModuleInstantiation::new([("elems".into(), vec!["byte".into()])].into());
        filedecl
            .visit(&mut mr)
            .unwrap()
            .modifying(&mut filedecl)
            .unwrap();
        */

        let filesort = filedecl
            .visit(&mut tc)
            .unwrap()
            .modifying(&mut filedecl)
            .unwrap();

        tc.bindings.append("file".into(), filesort).unwrap();
        tc
    }

    #[test]
    fn test_empty_process() {
        let mut iso = isolate_from_src("process p = { }");

        let sort = IvySort::Process(Process {
            args: BTreeMap::from([]),
            fields: BTreeMap::from([]),
            actions: [("init".to_owned(), Module::init_action_sort())].into(),
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
            args: [(
                "self".into(),
                IvySort::Range(Box::new(Expr::Number(0)), Box::new(Expr::Number(3))),
            )]
            .into(),
            actions: [("init".to_owned(), Module::init_action_sort())].into(),
            fields: BTreeMap::new(),
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
            args: [(
                "self".into(),
                IvySort::Range(Box::new(Expr::Number(0)), Box::new(Expr::Number(3))),
            )]
            .into(),
            actions: [
                ("is_up".into(), IvySort::Bool),
                ("init".to_owned(), Module::init_action_sort()),
            ]
            .into(),
            fields: BTreeMap::new(),
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

                implement sock.recv(src: bool, val:byte) {
                    contents := contents.append(val);
                    show(contents);
                }
        }",
        );

        let mut tc = typechecker_with_bindings();
        let _res = iso.visit(&mut tc).unwrap().modifying(&mut iso).unwrap();
    }
}
