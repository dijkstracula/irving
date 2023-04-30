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

    Number {
        val: i64
    },

    Pi {
        vars: Vec<Expr>,
        body: Box<Expr>,
    },

    Subscript {
        val: Box<Expr>,
        subscripts: Vec<Expr>
    },

    Symbol {
        name: String,
    },

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
#[allow(clippy::large_enum_variant)]
pub enum Stmt {

}

#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(clippy::large_enum_variant)]
pub enum Decl {
    Action {
        name: String,
        kind: ActionKind,
        inputs: Vec<Expr>,
        outputs: Vec<Expr>,
        body: Option<Stmt>,
    },

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

    Module {
        name: Expr,
        prms: Vec<Expr>,
        body: Box<Decl>,
    },

    Object {
        name: Expr,
        body: Box<Decl>,
    },

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
pub struct Prog {
    pub major_version: u8,
    pub minor_version: u8,

    pub decls: Vec<Decl>
}