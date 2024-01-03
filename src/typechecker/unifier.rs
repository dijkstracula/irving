use std::{
    collections::{BTreeMap, HashMap},
    vec,
};

use crate::{
    ast::{
        declarations::{self, Binding},
        expressions::*,
        span::Span,
    },
    typechecker::sorts::{ActionArgs, Module, Object},
};

use super::{
    sorts::{ActionKind, ActionRet, Class, IvySort},
    TypeError,
};

pub struct Scope {
    pub decl_name: Option<Token>,
    bindings: HashMap<Token, IvySort>,
}

impl Scope {
    fn new_unnamed() -> Self {
        Self {
            decl_name: None,
            bindings: HashMap::<_, _>::new(),
        }
    }

    fn new_decled<S>(decl: S) -> Self
    where
        S: Into<String>,
    {
        Self {
            decl_name: Some(decl.into()),
            bindings: HashMap::<_, _>::new(),
        }
    }

    fn contains_key(&self, name: &str) -> bool {
        self.bindings.contains_key(name)
    }

    fn get(&self, name: &str) -> Option<&IvySort> {
        self.bindings.get(name)
    }

    fn insert<S>(&mut self, name: S, sort: IvySort)
    where
        S: Into<String>,
    {
        self.bindings.insert(name.into(), sort);
    }
}

pub struct BindingResolver {
    // A scope is a mapping of names to sorts, with additionally
    // an optional declaration
    pub scopes: Vec<Scope>,
    pub ctx: Vec<IvySort>,
}

impl BindingResolver {
    pub fn new() -> Self {
        let mut s = Self {
            scopes: vec![],
            ctx: vec![],
        };
        s.push_anonymous_scope();
        s
    }

    // Name bindings

    // Named scopes are for lexical scopes that have an associated declaration
    // with them (such as function declarations, objects/classes, etc...)

    pub fn push_named_scope<S>(&mut self, name: S)
    where
        S: Into<String>,
    {
        self.scopes.push(Scope::new_decled(name))
    }

    pub fn pop_named_scope(&mut self) {
        match self.scopes.pop() {
            None => {
                panic!("popping an empty sort scope");
            }
            Some(scope) if scope.decl_name.is_none() => {
                panic!("Expecting to pop a named scope but got an anonymous scope");
            }
            Some(_) => (),
        }
    }

    // Anonymous scopes are for lexical scopes where we don't have to derive a named path.

    pub fn push_anonymous_scope(&mut self) {
        self.scopes.push(Scope::new_unnamed())
    }

    pub fn pop_anonymous_scope(&mut self) {
        match self.scopes.pop() {
            None => {
                panic!("popping an empty sort scope");
            }
            Some(scope) if scope.decl_name.is_some() => {
                panic!(
                    "Expecting to pop an anonymous scope but got named scope {}",
                    scope.decl_name.unwrap()
                );
            }
            Some(_) => (),
        }
    }

    pub fn named_scope_path(&self) -> Ident {
        self.scopes
            .iter()
            .filter_map(|scope| scope.decl_name.clone())
            .collect::<Ident>()
    }

    //

    /// Creates a new Sort with fresh SortVars.
    fn fresh_sortvars_iter(
        &mut self,
        svar_map: &mut BTreeMap<usize, usize>,
        sort: &IvySort,
    ) -> IvySort {
        match sort {
            IvySort::Vector(sort) => {
                IvySort::Vector(Box::new(self.fresh_sortvars_iter(svar_map, sort)))
            }
            IvySort::Action(argnames, argsorts, ret, kind) => {
                let argnames = argnames.clone();
                let argsorts = match argsorts {
                    ActionArgs::Unknown => ActionArgs::Unknown,
                    ActionArgs::List(sorts) => ActionArgs::List(
                        sorts
                            .iter()
                            .map(|s| self.fresh_sortvars_iter(svar_map, s))
                            .collect(),
                    ),
                };
                let ret = match ret {
                    ActionRet::Unknown => ActionRet::Unknown,
                    ActionRet::Unit => ActionRet::Unit,
                    ActionRet::Named(binding) => {
                        ActionRet::Named(Box::new(declarations::Binding::from(
                            &binding.name,
                            self.fresh_sortvars_iter(svar_map, &binding.decl),
                            binding.span.clone(),
                        )))
                    }
                };
                let kind = kind.clone();
                IvySort::Action(argnames, argsorts, ret, kind)
            }
            IvySort::Map(domain, range) => {
                let domain = domain
                    .iter()
                    .map(|s| self.fresh_sortvars_iter(svar_map, s))
                    .collect();
                let range = Box::new(self.fresh_sortvars_iter(svar_map, range.as_ref()));
                IvySort::Map(domain, range)
            }
            IvySort::Class(clz) => {
                let name = clz.name.clone();
                let parent = clz
                    .parent
                    .as_ref()
                    .map(|s| Box::new(self.fresh_sortvars_iter(svar_map, s.as_ref())));
                let actions = clz
                    .actions
                    .iter()
                    .map(|(k, v)| (k.clone(), self.fresh_sortvars_iter(svar_map, v)))
                    .collect();
                let fields = clz
                    .fields
                    .iter()
                    .map(|(k, v)| (k.clone(), self.fresh_sortvars_iter(svar_map, v)))
                    .collect();
                IvySort::Class(Class {
                    name,
                    parent,
                    actions,
                    fields,
                })
            }
            IvySort::Module(module) => {
                let name = module.name.clone();
                let args = module
                    .args
                    .iter()
                    .map(|Binding { name, decl, span }| Binding::from(name, self.fresh_sortvars_iter(svar_map, decl), span.clone()))
                    .collect();
                let fields = module
                    .fields
                    .iter()
                    .map(|(k, v)| (k.clone(), self.fresh_sortvars_iter(svar_map, v)))
                    .collect();
                IvySort::Module(Module { name, args, fields })
            }
            IvySort::Object(obj) => {
                let args = obj
                    .args
                    .iter()
                    .map(|b| {
                        Binding::from(
                            &b.name,
                            self.fresh_sortvars_iter(svar_map, &b.decl),
                            b.span.clone(),
                        )
                    })
                    .collect();
                let fields = obj
                    .fields
                    .iter()
                    .map(|(k, v)| (k.clone(), self.fresh_sortvars_iter(svar_map, v)))
                    .collect();
                IvySort::Object(Object { args, fields })
            }
            // XXX: https://github.com/dijkstracula/irving/issues/69
            IvySort::Generic(id, name) => match svar_map.get(id) {
                None => {
                    let new = self.new_generic(name);
                    let new_id = match &new {
                        IvySort::Generic(id, _) => *id,
                        _ => unreachable!(),
                    };

                    svar_map.insert(*id, new_id);
                    new
                }
                Some(id) => IvySort::Generic(*id, name.clone()),
            },
            IvySort::SortVar(id) => match svar_map.get(id) {
                None => {
                    let new = self.new_sortvar();
                    let new_id = match &new {
                        IvySort::SortVar(id) => *id,
                        _ => unreachable!(),
                    };

                    svar_map.insert(*id, new_id);
                    new
                }
                Some(id) => IvySort::SortVar(*id),
            },
            s => s.clone(),
        }
    }

    pub fn fresh_sortvars(&mut self, sort: &IvySort) -> IvySort {
        let mut svar_map = BTreeMap::new();
        self.fresh_sortvars_iter(&mut svar_map, sort)
    }

    //

    pub fn lookup_sym(&self, sym: &str) -> Option<&IvySort> {
        self.scopes
            .iter()
            .rfind(|scope| scope.contains_key(sym))
            .and_then(|scope| scope.get(sym))
            .map(|s| self.resolve(s))
    }

    pub fn lookup_ident(&self, id: &Ident) -> Result<&IvySort, ResolverError> {
        let mut idents = id.iter();

        let mut curr_sym = idents.next().unwrap();
        let mut curr_sort = self.lookup_sym(curr_sym);

        for field in idents {
            match curr_sort {
                Some(IvySort::Module(Module { fields, .. })) => {
                    curr_sort = fields.get(field);
                }
                Some(IvySort::Object(Object { args, fields })) => {
                    let fld = args.iter().find_map(|binding| {
                        if &binding.name == field {
                            Some(&binding.decl)
                        } else {
                            None
                        }
                    });
                    curr_sort = fld.or(fields.get(field))
                }
                Some(IvySort::Class(Class {
                    actions, fields, ..
                })) => {
                    curr_sort = actions
                        .iter()
                        .find_map(|b| if b.0 == field { Some(b.1) } else { None })
                        .or_else(|| {
                            fields
                                .iter()
                                .find_map(|b| if b.0 == field { Some(b.1) } else { None })
                        });
                }
                Some(sort) => {
                    return Err(ResolverError::NotARecord(sort.clone()));
                }
                None => return Err(ResolverError::UnboundVariable(curr_sym.clone())),
            }
            curr_sym = field;
        }

        match curr_sort {
            None => Err(ResolverError::UnboundVariable(curr_sym.clone())),
            Some(s) => Ok(s),
        }
    }

    pub fn append(&mut self, sym: Token, sort: IvySort) -> Result<(), ResolverError> {
        if self.scopes.last().is_none() {
            panic!("Appending into an empty scope");
        }

        // Only check the current scope, shadowing should be fine, right?
        if let Some(existing) = self.scopes.last().unwrap().get(&sym) {
            if existing.is_sortvar() || sort.is_sortvar() {
                let existing = existing.clone();
                let unified = self.unify(&existing, &sort)?;

                self.scopes.last_mut().unwrap().insert(sym, unified);
                return Ok(());
            } else if existing != &sort {
                return Err(ResolverError::ReboundVariable {
                    sym,
                    prev: existing.clone(),
                    new: sort,
                });
            }
        }

        log::trace!(target:"binding", "Inserting {}:{:?} into context (depth {})", sym, sort, self.scopes.len());

        self.scopes.last_mut().unwrap().insert(sym, sort);

        Ok(())
    }

    // Unification

    pub fn resolve<'a>(&'a self, sort: &'a IvySort) -> &'a IvySort {
        match sort {
            IvySort::Generic(original_id, _) | IvySort::SortVar(original_id) => {
                let mut curr_id = *original_id;
                let mut next_sort = &self.ctx[curr_id];

                loop {
                    match next_sort {
                        IvySort::Generic(new_id, _) | 
                        IvySort::SortVar(new_id) => {
                            //eprintln!("resolve: {:?}", self.ctx);
                            if new_id == &curr_id {
                                //TODO: occurs check?
                                break;
                            }
                            curr_id = *new_id;
                            next_sort = &self.ctx[curr_id];
                        }
                        _ => break
                    }
                }
                next_sort
            } 
            sort => sort
        }
    }

    fn unify_vec(
        &mut self,
        lhs: &Vec<IvySort>,
        rhs: &Vec<IvySort>,
    ) -> Result<Vec<IvySort>, ResolverError> {
        log::debug!(target: "sort-inference", "unify({lhs:?},{rhs:?})");
        if lhs.len() != rhs.len() {
            // XXX: which is expected and which is actual?
            Err(ResolverError::LenMismatch(lhs.len(), rhs.len()))
        } else {
            let mut args = vec![];
            for (a1, a2) in lhs.iter().zip(rhs.iter()) {
                args.push(self.unify(a1, a2)?);
            }
            Ok(args)
        }
    }

    fn unify_action_args(
        &mut self,
        lhs: &ActionArgs,
        rhs: &ActionArgs,
    ) -> Result<ActionArgs, ResolverError> {
        log::debug!(target: "sort-inference", "unify({lhs:?},{rhs:?})");
        match (lhs, rhs) {
            (ActionArgs::Unknown, ActionArgs::Unknown) => Ok(ActionArgs::Unknown),
            (ActionArgs::Unknown, ActionArgs::List(rhs)) => Ok(ActionArgs::List(rhs.clone())),
            (ActionArgs::List(lhs), ActionArgs::Unknown) => Ok(ActionArgs::List(lhs.clone())),
            (ActionArgs::List(lhs), ActionArgs::List(rhs)) => {
                Ok(ActionArgs::List(self.unify_vec(lhs, rhs)?))
            }
        }
    }

    fn unify_action_rets(
        &mut self,
        lhs: &ActionRet,
        rhs: &ActionRet,
    ) -> Result<ActionRet, ResolverError> {
        log::debug!(target: "sort-inference", "unify({lhs:?},{rhs:?})");
        match (lhs, rhs) {
            (ActionRet::Unknown, ActionRet::Unknown) => Ok(ActionRet::Unknown),

            (ActionRet::Unknown, ActionRet::Unit)
            | (ActionRet::Unit, ActionRet::Unknown)
            | (ActionRet::Unit, ActionRet::Unit) => Ok(ActionRet::Unit),

            (ActionRet::Unknown, n @ ActionRet::Named(_))
            | (n @ ActionRet::Named(_), ActionRet::Unknown) => Ok(n.clone()),

            (ActionRet::Unit, ActionRet::Named(binding))
            | (ActionRet::Named(binding), ActionRet::Unit) => self
                .unify(&binding.decl, &IvySort::Unit)
                .map(|s| ActionRet::named(binding.name.clone(), s)),
            (ActionRet::Named(lhs), ActionRet::Named(rhs)) => {
                // TODO: we can't support alpha-renaming in unification because we assume argument names
                // are bound to consistent names.
                //if lhs.name != rhs.name {
                //    return Err(TypeError::FargMismatch { expected: lhs.name.clone(), actual: rhs.name.clone() });
                // }
                self.unify(&lhs.decl, &rhs.decl)
                    .map(|s| ActionRet::named(lhs.name.clone(), s))
            }
        }
    }

    fn unify_action_kinds(lhs: &ActionKind, rhs: &ActionKind) -> Result<ActionKind, ResolverError> {
        match (lhs, rhs) {
            (ActionKind::Unknown, rhs) => Ok(rhs.clone()),
            (lhs, ActionKind::Unknown) => Ok(lhs.clone()),
            (lhs, rhs) if lhs == rhs => Ok(lhs.clone()),
            (lhs, rhs) => Err(ResolverError::ActionKindMismatch(lhs.clone(), rhs.clone())),
        }
    }

    pub fn unify(&mut self, lhs: &IvySort, rhs: &IvySort) -> Result<IvySort, ResolverError> {
        let lhs = self.resolve(lhs).clone();
        let rhs = self.resolve(rhs).clone();
        log::debug!(target: "sort-inference", "unify({lhs:?},{rhs:?})");
        match (&lhs, &rhs) {
            // SortVars and Generics unify with anything; two sortvars, to break
            // symmetry, resolve to the lower index into the typing context.
            (IvySort::SortVar(i), IvySort::SortVar(j)) | 
            (IvySort::SortVar(i), IvySort::Generic(j, _)) |
            (IvySort::Generic(i, _), IvySort::SortVar(j)) |
            (IvySort::Generic(i, _), IvySort::Generic(j, _)) => {
                if i < j {
                    self.ctx[*j] = lhs.clone();
                    Ok(lhs)
                } else if i > j {
                    self.ctx[*i] = rhs.clone();
                    Ok(rhs)
                } else {
                    Ok(lhs)
                }
            }
            (IvySort::SortVar(i), _) | (IvySort::Generic(i, _), _) => {
                self.ctx[*i] = rhs.clone();
                Ok(rhs)
            }
            (_, IvySort::SortVar(j)) | (_, IvySort::Generic(j, _)) => {
                self.ctx[*j] = lhs.clone();
                Ok(lhs)
            }
            (
                IvySort::Action(lhsargnames, lhsargsorts, lhsret, lhskind),
                IvySort::Action(rhsargnames, rhsargsorts, rhsret, rhskind),
            ) => {
                if lhsargnames.len() != rhsargnames.len() {
                    return Err(ResolverError::LenMismatch(
                        lhsargnames.len(),
                        rhsargnames.len(),
                    ));
                }
                let args = self.unify_action_args(lhsargsorts, rhsargsorts)?;
                let ret = self.unify_action_rets(lhsret, rhsret)?;
                let kind = BindingResolver::unify_action_kinds(lhskind, rhskind)?;

                // XXX: choosing the LHS' argument list arbitrarily!  Maybe we
                // can't treat the arguments to unify() as symmetric...
                Ok(IvySort::Action(lhsargnames.clone(), args, ret, kind))
            }

            // These subtyping relationships are meant to hack around the lack
            // of `interpret t -> nat` in unbounded_sequence.
            (IvySort::Number, m @ IvySort::Module(Module { name, .. }))
            | (m @ IvySort::Module(Module { name, .. }), IvySort::Number) => {
                log::warn!("Unsound hack: unify module {name} with number");
                Ok(m.clone())
            }

            // These subtyping relationships are meant to capture part of the
            // aspect that in Ivy, numerals' types are entirely inferred from
            // context.
            (IvySort::Number, IvySort::BoundedSequence(lo, hi))
            | (IvySort::BoundedSequence(lo, hi), IvySort::Number) => {
                // This subtyping relationship is fine, because Ivy's range
                // datatype saturates arithmetic operations.
                Ok(IvySort::BoundedSequence(lo.clone(), hi.clone()))
            }
            (IvySort::Number, IvySort::Uninterpreted)
            | (IvySort::Uninterpreted, IvySort::Number) => Ok(IvySort::Uninterpreted),
            // TODO: I need to experiment with weird typings like
            // `var x: bool := 42`: if the types are truly inferred from
            // context, then `42` should be treated of course as a bool.  Is
            // this actually what happens???

            // This subtyping relationship is for indexing into a parameterized
            // object by its parameters.
            (
                IvySort::Action(_, ActionArgs::List(fargs), _, _),
                IvySort::Object(Object {
                    args,
                    fields: obj_fields,
                }),
            )
            | (
                IvySort::Object(Object {
                    args,
                    fields: obj_fields,
                }),
                IvySort::Action(_, ActionArgs::List(fargs), _, _),
            ) => {
                // XXX: I'm not sure about this anymore.
                //let unified = match fret {
                //    ActionRet::Unknown => p.clone(),
                //    ActionRet::Unit => IvySort::Unit,
                //    ActionRet::Named(binding) => self.unify(&binding.decl, p)?,
                //};

                // XXX: args is unordered so we can't unify them in the multiple parameter case.
                // see https://github.com/dijkstracula/irving/issues/25 .
                if fargs.len() != args.len() || fargs.len() > 1 {
                    return Err(ResolverError::LenMismatch(fargs.len(), args.len()));
                }

                self.unify(fargs.get(0).unwrap(), &args.iter().next().unwrap().decl)?;

                let p = IvySort::Object(Object {
                    // If we get this far, we can treat the object as having had its
                    // `self` arguments applied, so "curry those out".
                    // XXX: this is wrong if there's more than one parameter!
                    args: vec![],
                    fields: obj_fields.clone(),
                });

                Ok(p)
            }

            // This subtyping relation says that "calling" a map with well-typed
            // arguments of the correct arity produces a the map's range.
            // TODO: should we constrain the ActionKind parameter?
            // TODO: what if the action return type is known?  Can that ever happen?
            (
                IvySort::Action(_, ActionArgs::List(aargsorts), ActionRet::Unknown, _),
                IvySort::Map(mapdom, maprange),
            )
            | (
                IvySort::Map(mapdom, maprange),
                IvySort::Action(_, ActionArgs::List(aargsorts), ActionRet::Unknown, _),
            ) => {
                let unified_domain = self.unify_vec(mapdom, aargsorts)?;
                Ok(IvySort::Map(unified_domain, maprange.clone()))
            }

            // This subtyping relations says that a nullary action is interchangable
            // with applying it.  (My least favourite programming language "feature"!!!!)
            (IvySort::Action(args, _, ActionRet::Named(is), _), sort)
            | (sort, IvySort::Action(args, _, ActionRet::Named(is), _))
                if args.len() == 0 =>
            {
                let unified = self.unify(&is.as_ref().decl, sort)?;
                Ok(unified)
            }

            // This subtyping relation says that two classes that share a common ancestor
            // should type to that ancestor.
            (
                c1 @ IvySort::Class(Class {
                    name: c1_name,
                    parent: p1,
                    ..
                }),
                c2 @ IvySort::Class(Class {
                    name: c2_name,
                    parent: p2,
                    ..
                }),
            ) => {
                // TODO: the thing I really need to do is recurse into all the fields' and actions'
                // sorts, probably in resolve()?
                if c1_name == c2_name {
                    // XXX: This is not strictly sound; we need to do better
                    // than just key on the class name.
                    return Ok(c1.clone());
                }
                if p1.is_some() || p2.is_some() {
                    // XXX: obviously extend this to find the common ancestor.
                    if c1 == c2 {
                        return Ok(c1.clone());
                    }
                    match (p1, p2) {
                        (None, Some(x)) if x.as_ref() == c1 => return Ok(c1.clone()),
                        (Some(x), None) if x.as_ref() == c2 => return Ok(c2.clone()),
                        _ => todo!(),
                    }
                }
                Err(ResolverError::UnificationError(c1.clone(), c2.clone()))
            }

            // This subtyping relationship says that `this` shoudl only
            // unify with processes or modules.  (Or classes?)
            (IvySort::This, s @ IvySort::Object(_))
            | (IvySort::This, s @ IvySort::Module(_))
            | (IvySort::This, s @ IvySort::Class(_))
            | (s @ IvySort::Class(_), IvySort::This)
            | (s @ IvySort::Module(_), IvySort::This)
            | (s @ IvySort::Object(_), IvySort::This) => Ok(s.clone()),

            (t1, t2) => {
                if t1 == t2 {
                    Ok(lhs)
                } else {
                    Err(ResolverError::UnificationError(t1.clone(), t2.clone()))
                }
            }
        }
    }

    pub fn new_sortvar(&mut self) -> IvySort {
        let s = IvySort::SortVar(self.ctx.len());
        self.ctx.push(s.clone());
        s
    }

    pub fn new_generic<S>(&mut self, name: S) -> IvySort where S: Into<String> {
        let s = IvySort::Generic(self.ctx.len(), name.into());
        self.ctx.push(s.clone());
        s
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolverError {
    ActionKindMismatch(ActionKind, ActionKind),
    LenMismatch(usize, usize),
    //MissingField(String),
    NotARecord(IvySort),
    ReboundVariable {
        sym: Token,
        prev: IvySort,
        new: IvySort,
    },
    UnificationError(IvySort, IvySort),
    UnboundVariable(String),
}

impl ResolverError {
    pub fn to_typeerror(self, span: &Span) -> TypeError {
        println!("NBT: {:?}", span);
        TypeError::Spanned {
            span: span.clone(),
            inner: Box::new(match self {
                ResolverError::ActionKindMismatch(_, _) => TypeError::ActionKindMismatch,
                ResolverError::LenMismatch(lhs, rhs) => TypeError::LenMismatch {
                    // XXX: This is sensitive to the order of arguments to unify, which is suboptimal.
                    expected: lhs,
                    actual: rhs,
                },
                //ResolverError::MissingField(field) => TypeError::MissingRecordField(field),
                ResolverError::NotARecord(sort) => TypeError::NotARecord(sort.desc()),
                ResolverError::ReboundVariable { sym, prev, new } => TypeError::ReboundVariable {
                    sym,
                    prev: prev.desc().to_owned(),
                    new: new.desc().to_owned(),
                },
                ResolverError::UnificationError(lhs, rhs) => {
                    TypeError::unification_error(&lhs, &rhs)
                }
                ResolverError::UnboundVariable(var) => TypeError::UnboundVariable(var),
            }),
        }
    }
}
