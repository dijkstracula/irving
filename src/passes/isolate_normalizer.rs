#![allow(dead_code)]

/* Raises declarations from implementation/specification/common blocks
 * within an IsolateDecl to the top level.
 */

use anyhow::bail;
use thiserror::Error;

use crate::ast::declarations::*;
use crate::ast::expressions::{AnnotatedSymbol, Symbol};

use crate::visitor::visitor::Visitable;
use crate::visitor::*;

pub struct NormalizerState {
    /// All common actions will need to have the parameter list prepended to them, since
    /// they do not close over their enclosing module's arguments.
    params: Vec<AnnotatedSymbol>,

    impls: Vec<Decl>,
    common_impls: Vec<Decl>,

    specs: Vec<Decl>,
    common_specs: Vec<Decl>,

    /// Are we in a subtree of an implementation block?
    in_impl: bool,

    /// Are we in a subtree of a specification block?
    in_spec: bool,

    /// Are we in a subtree of a common block?
    in_common: bool,
}

impl NormalizerState {
    pub fn new(params: Vec<AnnotatedSymbol>) -> Self {
        Self {
            params,

            impls: vec![],
            common_impls: vec![],
            specs: vec![],
            common_specs: vec![],

            in_impl: false,
            in_spec: false,
            in_common: false,
        }
    }
}

pub struct IsolateNormalizer {
    /// One for each isolate that we're currently within.
    states: Vec<NormalizerState>,
}

impl IsolateNormalizer {
    pub fn new() -> Self {
        IsolateNormalizer { states: vec![] }
    }

    pub fn push_state(&mut self, params: Vec<AnnotatedSymbol>) {
        self.states.push(NormalizerState::new(params))
    }

    pub fn pop_state(&mut self) {
        self.states.pop().unwrap();
    }

    pub fn state(&mut self) -> &NormalizerState {
        self.states.last().unwrap()
    }

    pub fn state_mut(&mut self) -> &mut NormalizerState {
        self.states.last_mut().unwrap()
    }
}

impl Visitor<()> for IsolateNormalizer {
    fn begin_isolate_decl(
        &mut self,
        _name: &mut Symbol,
        ast: &mut IsolateDecl,
    ) -> VisitorResult<(), Decl> {
        self.push_state(ast.params.clone());

        ast.body.visit(self)?.modifying(&mut ast.body)?;
        ast.body = std::mem::take(&mut ast.body)
            .into_iter()
            .filter_map(|decl| match decl {
                Decl::Common(_)
                | Decl::Globals(_)
                | Decl::Implementation(_)
                | Decl::Specification(_) => Some(decl),
                _ => {
                    self.state_mut().impls.push(decl);
                    None
                }
            })
            .collect::<Vec<_>>();
        Ok(ControlMut::Produce(()))
    }
    fn finish_isolate_decl(
        &mut self,
        name: &mut Symbol,
        ast: &mut IsolateDecl,
        _n: (),
        _p: Vec<()>,
        _b: Vec<()>,
    ) -> VisitorResult<(), Decl> {
        self.state_mut().impls.retain(|decl| match decl {
            Decl::Noop => false,
            _ => true,
        });

        let normalized = NormalizedIsolateDecl {
            params: ast.params.clone(),
            impl_decls: std::mem::take(&mut self.state_mut().impls),
            spec_decls: std::mem::take(&mut self.state_mut().specs),
            common_spec_decls: std::mem::take(&mut self.state_mut().common_specs),
            common_impl_decls: std::mem::take(&mut self.state_mut().common_impls),
        };

        self.pop_state();

        Ok(ControlMut::Mutation(
            Decl::NormalizedIsolate(Binding {
                name: name.to_owned(),
                decl: normalized,
            }),
            (),
        ))
    }

    fn begin_implementation_decl(&mut self, _ast: &mut Vec<Decl>) -> VisitorResult<(), Decl> {
        if self.state().in_spec {
            bail!(NormalizerError::BadNesting {
                inner: "implementation",
                outer: "specification"
            });
        }
        if self.state().in_impl {
            bail!(NormalizerError::BadNesting {
                inner: "implementation",
                outer: "implementation"
            });
        }

        self.state_mut().in_impl = true;
        Ok(ControlMut::Produce(()))
    }
    fn finish_implementation_decl(&mut self, ast: &mut Vec<Decl>) -> VisitorResult<(), Decl> {
        ast.retain(|decl| match decl {
            Decl::Noop => false,
            _ => true,
        });

        if self.state().in_common {
            self.state_mut().common_impls.append(ast);
        } else {
            self.state_mut().impls.append(ast);
        }

        self.state_mut().in_impl = false;
        Ok(ControlMut::Mutation(Decl::Noop, ()))
    }

    fn begin_specification(&mut self, _ast: &mut Vec<Decl>) -> VisitorResult<(), Decl> {
        if self.state().in_spec {
            bail!(NormalizerError::BadNesting {
                inner: "specification",
                outer: "specification"
            });
        }
        if self.state_mut().in_impl {
            bail!(NormalizerError::BadNesting {
                inner: "specification",
                outer: "implementation"
            });
        }

        self.state_mut().in_spec = true;
        Ok(ControlMut::Produce(()))
    }
    fn finish_specification(&mut self, ast: &mut Vec<Decl>) -> VisitorResult<(), Decl> {
        ast.retain(|decl| match decl {
            Decl::Noop => false,
            _ => true,
        });

        if self.state().in_common {
            self.state_mut().common_specs.append(ast);
        } else {
            self.state_mut().specs.append(ast);
        }

        self.state_mut().in_spec = false;
        Ok(ControlMut::Mutation(Decl::Noop, ()))
    }

    fn begin_common_decl(&mut self, _ast: &mut Vec<Decl>) -> VisitorResult<(), Decl> {
        if self.state().in_common {
            bail!(NormalizerError::BadNesting {
                inner: "common",
                outer: "common"
            });
        }

        self.state_mut().in_common = true;
        Ok(ControlMut::Produce(()))
    }
    fn finish_common_decl(&mut self, ast: &mut Vec<Decl>) -> VisitorResult<(), Decl> {
        ast.retain(|decl| match decl {
            Decl::Noop => false,
            _ => true,
        });

        if self.state().in_spec {
            self.state_mut().common_specs.append(ast);
        } else {
            self.state_mut().common_impls.append(ast);
        }

        self.state_mut().in_common = false;
        Ok(ControlMut::Mutation(Decl::Noop, ()))
    }

    //

    /*
    // TODO: if we're still emitting common blocks, we don't actually want to do this...
    fn finish_action_decl(
            &mut self,
            ast: &mut ActionDecl,
            _name: (),
            _params: Vec<()>,
            ret: Option<()>,
            _body: Option<Vec<()>>,
        ) -> VisitorResult<(), Decl> {
        if !self.in_common {
            return Ok(ControlMut::Produce(()))
        }

        if let Some(mod_params) = &self.curr_module_params {
            let params = mod_params.iter().chain(ast.params.iter()).map(|s| s.clone()).collect::<Vec<_>>();

            Ok(ControlMut::Mutation(Decl::Action(ActionDecl {
                name: ast.name.clone(),
                params,
                ret: ast.ret.clone(),
                body: ast.body.clone(),
            }), ()))
        } else {
            Ok(ControlMut::Produce(()))
        }
    }
    */
}

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum NormalizerError {
    #[error("{inner:?} declaration cannot be nested in {outer}")]
    BadNesting {
        inner: &'static str,
        outer: &'static str,
    },
}
