use std::fmt;

use crate::{impl_display_for_enum, impl_obvious_conversion};
use crate::token::Token;
use crate::ty::Type;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Attribute(String);

impl fmt::Display for Attribute {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#[{}]", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Expr {
    attrs: Vec<Attribute>,
    kind: ExprKind,
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for attr in self.attrs.iter() {
            writeln!(f, "{attr}")?;
        }
        self.kind.fmt(f)
    }
}

impl Expr {
    pub fn new(kind: impl Into<ExprKind>) -> Self {
        Self {
            attrs: Vec::new(),
            kind: kind.into(),
        }
    }

    pub fn add_attr(&mut self, attr: Attribute) {
        self.attrs.push(attr);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Const(Expr);

impl fmt::Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprKind {
    Lit(Lit),
    Call(Call),
    MacCall(MacCall),
}

impl_display_for_enum!(ExprKind;
    Lit,
    Call,
    MacCall,
);
impl_obvious_conversion!(ExprKind;
    Lit,
    Call,
    MacCall,
);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LitKind {
    Bool,
    Byte,
    Char,
    Integer,
    Float,
    Str,
    ByteStr,
    CStr,
    Err,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Lit {
    kind: LitKind,
    symbol: String,
}

impl fmt::Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.symbol.fmt(f)
    }
}

impl<S: Into<String>> From<S> for Lit {
    fn from(symbol: S) -> Self {
        Self {
            kind: LitKind::Str,
            symbol: format!("\"{}\"", symbol.into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Call {
    func: Box<Expr>,
    args: Vec<Expr>,
}

impl fmt::Display for Call {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.func)?;
        let mut iter = self.args.iter();
        if let Some(arg) = iter.next() {
            write!(f, "{arg}")?;
            for arg in iter {
                write!(f, ", {arg}")?;
            }
        }
        write!(f, ")")
    }
}

impl Call {
    pub fn new(func: impl Into<Expr>, args: Vec<Expr>) -> Self {
        Self {
            func: Box::new(func.into()),
            args,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
    segments: Vec<PathSegment>,
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut iter = self.segments.iter();
        if let Some(segment) = iter.next() {
            write!(f, "{segment}")?;
            for segment in iter {
                write!(f, "::{segment}")?;
            }
        }
        Ok(())
    }
}

impl Path {
    pub fn single(ident: impl Into<PathSegment>) -> Self {
        Self {
            segments: vec![ident.into()],
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PathSegment {
    ident: String,
    args: Option<Vec<GenericArg>>,
}

impl fmt::Display for PathSegment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.ident)?;
        if let Some(args) = &self.args {
            write!(f, "<")?;
            let mut iter = args.iter();
            if let Some(arg) = iter.next() {
                write!(f, "{arg}")?;
                for arg in iter {
                    write!(f, ", {arg}")?;
                }
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

impl<S: Into<String>> From<S> for PathSegment {
    fn from(ident: S) -> Self {
        Self {
            ident: ident.into(),
            args: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GenericArg {
    Lifetime(String),
    Type(Type),
    Const(Const),
}

impl fmt::Display for GenericArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Lifetime(lifetime) => write!(f, "'{lifetime}"),
            Self::Type(ty) => write!(f, "{ty}"),
            Self::Const(constant) => write!(f, "{constant}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MacDelimiter {
    Parenthesis,
    Bracket,
    Brace,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DelimArgs {
    delim: MacDelimiter,
    tokens: Vec<Token>,
}

impl fmt::Display for DelimArgs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.delim {
            MacDelimiter::Parenthesis => {
                write!(f, "(")?;
            },
            MacDelimiter::Bracket => {
                write!(f, "[")?;
            },
            MacDelimiter::Brace => {
                write!(f, "{{")?;
            },
        }
        let mut iter = self.tokens.iter();
        if let Some(token) = iter.next() {
            write!(f, "{token}")?;
            for token in iter {
                write!(f, ", {token}")?;
            }
        }
        match self.delim {
            MacDelimiter::Parenthesis => {
                write!(f, ")")
            },
            MacDelimiter::Bracket => {
                write!(f, "]")
            },
            MacDelimiter::Brace => {
                write!(f, "}}")
            },
        }
    }
}

impl From<Vec<Token>> for DelimArgs {
    fn from(tokens: Vec<Token>) -> Self {
        Self {
            delim: MacDelimiter::Parenthesis,
            tokens,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MacCall {
    pub path: Path,
    pub args: DelimArgs,
}

impl fmt::Display for MacCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}!{}", self.path, self.args)?;
        Ok(())
    }
}

impl MacCall {
    pub fn new(path: Path, args: impl Into<DelimArgs>) -> Self {
        Self {
            path,
            args: args.into(),
        }
    }
}
