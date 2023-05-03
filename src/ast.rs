#![allow(dead_code)]

/// Corresponds to a file/line pairing, and possibly additionally docstrings to
/// be reconstructed in the extracted code.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Annotation {
    docstring: Vec<String>,
    file: String,
    line: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ident {
    /// Used for all symbols occurring in the source, including operators like
    /// `+` and `&`.
    Sub {
        val: String,
        subscripts: Vec<Ident>
    },

    /// Numerical identifiers are used as temporaries internally.    
    Num {
        val: u64
    },

    /// Represents members of a namespace.
    Dot {
        ns: String,
        subscripts: Vec<Ident>
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Verb {
    Iff, Or, And, Lt, Leq, Gt, Gtq, Equals, Notequals,
    Plus, Minus, Times, Div,
    Empty, True, False, 
    Colon, Comma, Dot
}

pub type Symbol = String;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AppExpr {
    pub func: Box<Expr>,
    pub args: Vec<Expr>
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub enum Formula {
    Forall {
        vars: Vec<Symbol>,
        expr: Box<Expr>
    },
    Exists {
        vars: Vec<Symbol>,
        expr: Box<Expr>
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub enum Expr {
    App(AppExpr),

    BinOp {
        lhs: Box<Expr>,
        op: Verb,
        rhs: Box<Expr>
    },

    Formula(Formula),

    Number(i64),

    Pi {
        vars: Vec<Expr>,
        body: Box<Expr>,
    },

    Subscript {
        val: Box<Expr>,
        subscripts: Vec<Expr>
    },

    Symbol(Symbol),

    /// Used internally only as placeholders
    Variable {
        idx: u64,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ActionKind {
    Internal,
    External,
    Imported,
    Exported
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub name: Symbol,
    pub typ: Symbol
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeclSig {
    pub name: Symbol,
    pub params: Vec<Param>,
}

pub type DeclRet = Option<Param>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ActionDecl {
    pub name: String,
    pub kind: ActionKind,
    pub params: Vec<Param>,
    pub ret: Option<Param>,
    pub body: Option<Vec<Decl>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDecl {
    pub name: Symbol,
    pub params: Vec<Param>,
    pub ret: Symbol
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub name: Symbol,
    pub params: Vec<Param>,
    pub body: Vec<Decl>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Relation {
    pub name: Symbol,
    pub params: Vec<Param>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub sort: Symbol,
    pub supr: Option<Symbol>,
    /* spec: TypeSpec */
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Var {
    pub name: Symbol,
    pub typ: Option<Symbol>,
    //is_destructor: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub enum Decl {

    Action(ActionDecl),

    Axiom(Expr),

    Function(FunctionDecl),

    Header {
        file: String,
    },

    Group {
        decls: Vec<Decl>,
    },

    Include {
        file: Expr,
    },

    Init {
        body: Stmt,
    },

    Instance {
        objname: Expr,
        modname: Expr,
        prms: Vec<Expr>,
    },

    Instantiate {
        name: Expr,
        prms: Vec<Expr>,
    },

    Interpretation {
        itype: Expr,
        ctype: Expr,
    },

    Module(Module),

    Object {
        name: Expr,
        body: Box<Decl>,
    },

    Relation(Relation),

    Stmt(Stmt),

    Type(Type),

    Var(Var),

}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub struct If {
    pub tst: Expr,
    pub thn: Vec<Stmt>,
    pub els: Option<Vec<Stmt>>
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub struct While {
    pub test: Expr,
    pub doit: Vec<Stmt>
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub enum Stmt {
    CompoundActions(Vec<Action>),
    If(If),
    While(While)
}

// Actions

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub struct AssignAction {
    pub lhs: Expr,
    pub rhs: Expr
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Action {
    Assign(AssignAction),
}


// Top levels


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Prog {
    pub major_version: u8,
    pub minor_version: u8,

    pub decls: Vec<Decl>
}