use std::fmt;
use std::ops::{Deref, DerefMut};

use crate::expr::Lit;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum KeywordToken {
    As,
    Async,
    Await,
    Box,
    Break,
    Const,
    Continue,
    Crate,
    Dyn,
    Else,
    Enum,
    Extern,
    False,
    Fn,
    For,
    If,
    Impl,
    In,
    Let,
    Loop,
    Match,
    Mod,
    Move,
    Mut,
    Pub,
    Ref,
    Return,
    Self_,
    Static,
    Struct,
    Super,
    Trait,
    True,
    Try,
    Type,
    Unsafe,
    Use,
    Where,
    While,
    Yield,
}

impl fmt::Display for KeywordToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::As => write!(f, "as"),
            Self::Async => write!(f, "async"),
            Self::Await => write!(f, "await"),
            Self::Box => write!(f, "box"),
            Self::Break => write!(f, "break"),
            Self::Const => write!(f, "const"),
            Self::Continue => write!(f, "continue"),
            Self::Crate => write!(f, "crate"),
            Self::Dyn => write!(f, "dyn"),
            Self::Else => write!(f, "else"),
            Self::Enum => write!(f, "enum"),
            Self::Extern => write!(f, "extern"),
            Self::False => write!(f, "false"),
            Self::Fn => write!(f, "fn"),
            Self::For => write!(f, "for"),
            Self::If => write!(f, "if"),
            Self::Impl => write!(f, "impl"),
            Self::In => write!(f, "in"),
            Self::Let => write!(f, "let"),
            Self::Loop => write!(f, "loop"),
            Self::Match => write!(f, "match"),
            Self::Mod => write!(f, "mod"),
            Self::Move => write!(f, "move"),
            Self::Mut => write!(f, "mut"),
            Self::Pub => write!(f, "pub"),
            Self::Ref => write!(f, "ref"),
            Self::Return => write!(f, "return"),
            Self::Self_ => write!(f, "self"),
            Self::Static => write!(f, "static"),
            Self::Struct => write!(f, "struct"),
            Self::Super => write!(f, "super"),
            Self::Trait => write!(f, "trait"),
            Self::True => write!(f, "true"),
            Self::Try => write!(f, "try"),
            Self::Type => write!(f, "type"),
            Self::Unsafe => write!(f, "unsafe"),
            Self::Use => write!(f, "use"),
            Self::Where => write!(f, "where"),
            Self::While => write!(f, "while"),
            Self::Yield => write!(f, "yield"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOpToken {
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Caret,
    And,
    Or,
    Shl,
    Shr,
}

impl fmt::Display for BinOpToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),
            Self::Percent => write!(f, "%"),
            Self::Caret => write!(f, "^"),
            Self::And => write!(f, "&"),
            Self::Or => write!(f, "|"),
            Self::Shl => write!(f, "<<"),
            Self::Shr => write!(f, ">>"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Delimiter {
    /// ()
    Parenthesis,
    /// {}
    Brace,
    /// []
    Bracket,
    Invisible,
}

impl Delimiter {
    pub fn open(&self) -> &'static str {
        match self {
            Self::Parenthesis => "(",
            Self::Brace => "{",
            Self::Bracket => "[",
            Self::Invisible => "",
        }
    }

    pub fn close(&self) -> &'static str {
        match self {
            Self::Parenthesis => ")",
            Self::Brace => "}",
            Self::Bracket => "]",
            Self::Invisible => "",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Eq,
    Lt,
    Le,
    EqEq,
    Ne,
    Ge,
    Gt,
    AndAnd,
    OrOr,
    Not,
    Tilde,
    BinOp(BinOpToken),
    BinOpEq(BinOpToken),
    /* Structural symbols */
    At,
    Dot,
    DotDot,
    DotDotDot,
    DotDotEq,
    Comma,
    Semi,
    Colon,
    ModSep,
    LArrow,
    RArrow,
    FatArrow,
    Pound,
    Dollar,
    Question,
    SingleQuote,
    OpenDelim(Delimiter),
    CloseDelim(Delimiter),
    Lit(Lit),
    Ident(String),
    Lifetime(String),
    Keyword(KeywordToken),
    Eof,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Eq => write!(f, "="),
            Self::Lt => write!(f, "<"),
            Self::Le => write!(f, "<="),
            Self::EqEq => write!(f, "=="),
            Self::Ne => write!(f, "!="),
            Self::Ge => write!(f, ">="),
            Self::Gt => write!(f, ">"),
            Self::AndAnd => write!(f, "&&"),
            Self::OrOr => write!(f, "||"),
            Self::Not => write!(f, "!"),
            Self::Tilde => write!(f, "~"),
            Self::BinOp(op) => write!(f, "{op}"),
            Self::BinOpEq(op) => write!(f, "{op}="),
            Self::At => write!(f, "@"),
            Self::Dot => write!(f, "."),
            Self::DotDot => write!(f, ".."),
            Self::DotDotDot => write!(f, "..."),
            Self::DotDotEq => write!(f, "..="),
            Self::Comma => write!(f, ","),
            Self::Semi => write!(f, ";"),
            Self::Colon => write!(f, ":"),
            Self::ModSep => write!(f, "::"),
            Self::LArrow => write!(f, "<-"),
            Self::RArrow => write!(f, "->"),
            Self::FatArrow => write!(f, "=>"),
            Self::Pound => write!(f, "#"),
            Self::Dollar => write!(f, "$"),
            Self::Question => write!(f, "?"),
            Self::SingleQuote => write!(f, "'"),
            Self::OpenDelim(delim) => write!(f, "{}", delim.open()),
            Self::CloseDelim(delim) => write!(f, "{}", delim.close()),
            Self::Lit(lit) => write!(f, "{lit}"),
            Self::Ident(ident) => write!(f, "{ident}"),
            Self::Lifetime(lifetime) => write!(f, "'{lifetime}"),
            Self::Keyword(keyword) => write!(f, "{keyword}"),
            Self::Eof => write!(f, ""),
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

    /// `Token::lifetime("a")` => `'a`
    pub fn lifetime(lifetime: impl Into<String>) -> Self {
        Self::Lifetime(lifetime.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct TokenStream(Vec<Token>);

impl fmt::Display for TokenStream {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, token) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            write!(f, "{}", token)?;
        }
        Ok(())
    }
}

impl From<Vec<Token>> for TokenStream {
    fn from(tokens: Vec<Token>) -> Self {
        Self(tokens)
    }
}

impl Deref for TokenStream {
    type Target = Vec<Token>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for TokenStream {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl IntoIterator for TokenStream {
    type Item = Token;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl TokenStream {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn and(mut self, other: Self) -> Self {
        self.extend(other);
        self
    }

    pub fn aggregate(tss: impl IntoIterator<Item = TokenStream>) -> Self {
        let mut tokens = vec![];
        for ts in tss {
            tokens.extend(ts);
        }
        Self(tokens)
    }
}
