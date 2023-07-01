use std::fmt;

use crate::expr::Lit;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Eq,
    Plus,
    Comma,
    Semi,
    Lit(Lit),
    Ident(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Eq => write!(f, "=="),
            Self::Plus => write!(f, "+"),
            Self::Comma => write!(f, ","),
            Self::Semi => write!(f, ";"),
            Self::Lit(lit) => write!(f, "{lit}"),
            Self::Ident(ident) => write!(f, "{ident}"),
        }
    }
}

impl Token {
    pub fn lit(lit: impl Into<Lit>) -> Self {
        Self::Lit(lit.into())
    }

    pub fn ident(ident: impl Into<String>) -> Self {
        Self::Ident(ident.into())
    }
}
