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
#[allow(clippy::large_enum_variant)]
pub enum Expr {
    App {
        func: Box<Expr>,
        args: Vec<Expr>,
    },

    BinOp {
        lhs: Box<Expr>,
        op: Verb,
        rhs: Box<Expr>
    },

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
pub struct Action {
    pub name: String,
    pub kind: ActionKind,
    pub params: Vec<Param>,
    pub ret: Option<Param>,
    pub body: Option<Vec<Decl>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub name: Symbol,
    pub params: Vec<Param>,
    pub body: Vec<Decl>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub enum Decl {

    Action(Action),

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

    Stmt(Stmt),

    Type {
        sort: Expr,
        supr: Option<Expr>,
        /* spec: TypeSpec */
    },

    Var {
        typing: Expr,
        is_destructor: bool,
        def: Option<Expr>,
    }

}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub struct Assign {
    pub lhs: Expr,
    pub rhs: Expr
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
    Assign(Assign),
    If(If),
    While(While)
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Prog {
    pub major_version: u8,
    pub minor_version: u8,

    pub decls: Vec<Decl>
}