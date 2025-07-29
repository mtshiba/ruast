use std::fmt;
use std::ops::{Deref, DerefMut};

use crate::expr::Lit;

/// A string that is confirmed at the time of construction to be a valid Rust identifier.
#[cfg(feature = "checked-ident")]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier(String);

#[cfg(feature = "checked-ident")]
impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[cfg(feature = "checked-ident")]
impl Deref for Identifier {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[cfg(feature = "checked-ident")]
impl Identifier {
    pub fn new(ident: impl Into<String>) -> Result<Self, String> {
        let ident = check_ident(ident)?;
        Ok(Self(ident))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

#[cfg(feature = "checked-ident")]
pub fn check_ident(maybe_ident: impl Into<String>) -> Result<String, String> {
    use unicode_ident;

    let ident = maybe_ident.into();
    let mut chars = ident.chars();
    let Some(first) = chars.next() else {
        return Err(ident);
    };
    if !unicode_ident::is_xid_start(first) {
        return Err(ident);
    }

    if chars.all(unicode_ident::is_xid_continue) && KeywordToken::try_from(&ident[..]).is_err() {
        Ok(ident)
    } else {
        Err(ident)
    }
}

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

impl TryFrom<&str> for KeywordToken {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "as" => Ok(Self::As),
            "async" => Ok(Self::Async),
            "await" => Ok(Self::Await),
            "box" => Ok(Self::Box),
            "break" => Ok(Self::Break),
            "const" => Ok(Self::Const),
            "continue" => Ok(Self::Continue),
            "crate" => Ok(Self::Crate),
            "dyn" => Ok(Self::Dyn),
            "else" => Ok(Self::Else),
            "enum" => Ok(Self::Enum),
            "extern" => Ok(Self::Extern),
            "false" => Ok(Self::False),
            "fn" => Ok(Self::Fn),
            "for" => Ok(Self::For),
            "if" => Ok(Self::If),
            "impl" => Ok(Self::Impl),
            "in" => Ok(Self::In),
            "let" => Ok(Self::Let),
            "loop" => Ok(Self::Loop),
            "match" => Ok(Self::Match),
            "mod" => Ok(Self::Mod),
            "move" => Ok(Self::Move),
            "mut" => Ok(Self::Mut),
            "pub" => Ok(Self::Pub),
            "ref" => Ok(Self::Ref),
            "return" => Ok(Self::Return),
            "self" | "Self" => Ok(Self::Self_),
            "static" => Ok(Self::Static),
            "struct" => Ok(Self::Struct),
            "super" => Ok(Self::Super),
            "trait" => Ok(Self::Trait),
            "true" => Ok(Self::True),
            "try" => Ok(Self::Try),
            "type" => Ok(Self::Type),
            "unsafe" => Ok(Self::Unsafe),
            "use" => Ok(Self::Use),
            "where" => Ok(Self::Where),
            "while" => Ok(Self::While),
            "yield" => Ok(Self::Yield),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOpToken {
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `%`
    Percent,
    /// `^`
    Caret,
    /// `&&`
    LazyAnd,
    /// `||`
    LazyOr,
    /// `&`
    BitAnd,
    /// `|`
    BitOr,
    /// `^`
    BitXor,
    /// `<<`
    Shl,
    /// `>>`
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
            Self::LazyAnd => write!(f, "&&"),
            Self::LazyOr => write!(f, "||"),
            Self::BitAnd => write!(f, "&"),
            Self::BitOr => write!(f, "|"),
            Self::BitXor => write!(f, "^"),
            Self::Shl => write!(f, "<<"),
            Self::Shr => write!(f, ">>"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

#[cfg(feature = "tokenize")]
impl From<Delimiter> for proc_macro2::Delimiter {
    fn from(delim: Delimiter) -> Self {
        match delim {
            Delimiter::Parenthesis => Self::Parenthesis,
            Delimiter::Brace => Self::Brace,
            Delimiter::Bracket => Self::Bracket,
            Delimiter::Invisible => Self::None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    /// `=`
    Eq,
    /// `<`
    Lt,
    /// `<=`
    Le,
    /// `==`
    EqEq,
    /// `!=`
    Ne,
    /// `>=`
    Ge,
    /// `>`
    Gt,
    /// `&`
    And,
    /// `|`
    Or,
    /// `!`
    Not,
    /// `~`
    Tilde,
    BinOp(BinOpToken),
    BinOpEq(BinOpToken),
    /* Structural symbols */
    /// `@`
    At,
    /// `.`
    Dot,
    /// `..`
    DotDot,
    /// `...`
    DotDotDot,
    /// `..=`
    DotDotEq,
    /// `,`
    Comma,
    /// `;`
    Semi,
    /// `:`
    Colon,
    /// `::`
    ModSep,
    /// `<-`
    LArrow,
    /// `->`
    RArrow,
    /// `=>`
    FatArrow,
    /// `#`
    Pound,
    /// `$`
    Dollar,
    /// `?`
    Question,
    /// `'`
    SingleQuote,
    OpenDelim(Delimiter),
    CloseDelim(Delimiter),
    Lit(Lit),
    Ident(String),
    Lifetime(String),
    Keyword(KeywordToken),
    /// Note that this variant outputs the stored string as it is (without displaying a leading `///`).
    DocComment(String),
    /// When print this variant as an element of a `TokenStream`, it is displayed combined with the following tokens (no spacing).
    Joint(Box<Token>),
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
            Self::And => write!(f, "&"),
            Self::Or => write!(f, "|"),
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
            Self::DocComment(comment) => write!(f, "{comment}"),
            Self::Joint(token) => write!(f, "{token}"),
            Self::Eof => write!(f, ""),
        }
    }
}

impl Token {
    pub fn lit(lit: impl Into<Lit>) -> Self {
        Self::Lit(lit.into())
    }

    pub fn verbatim(lit: impl Into<String>) -> Self {
        Self::DocComment(lit.into())
    }

    pub fn ident(ident: impl Into<String>) -> Self {
        Self::Ident(ident.into())
    }

    #[cfg(feature = "checked-ident")]
    pub fn checked_ident(ident: impl Into<String>) -> Result<Self, String> {
        let ident = check_ident(ident)?;
        Ok(Self::Ident(ident))
    }

    /// `Token::lifetime("a")` => `'a`
    pub fn lifetime(lifetime: impl Into<String>) -> Self {
        Self::Lifetime(lifetime.into())
    }

    pub const fn is_keyword(&self) -> bool {
        matches!(self, Self::Keyword(_))
    }

    pub const fn is_ident(&self) -> bool {
        matches!(self, Self::Ident(_))
    }

    pub const fn is_lit(&self) -> bool {
        matches!(self, Self::Lit(_))
    }

    pub const fn is_joint(&self) -> bool {
        matches!(self, Self::Joint(_))
    }

    pub fn into_joint(self) -> Self {
        Self::Joint(Box::new(self))
    }
}

/// This structure is not related to `proc_macro2::TokenStream`.
/// However, it can be converted to `proc_marco2::TokenStream` by enabling the `quote` feature.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct TokenStream(Vec<Token>);

impl fmt::Display for TokenStream {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut previous_was_joint = false;
        for (i, token) in self.0.iter().enumerate() {
            if i > 0 && !previous_was_joint {
                write!(f, " ")?;
            }
            write!(f, "{token}")?;
            previous_was_joint = token.is_joint();
        }
        Ok(())
    }
}

impl From<Vec<Token>> for TokenStream {
    fn from(tokens: Vec<Token>) -> Self {
        Self(tokens)
    }
}

impl From<Token> for TokenStream {
    fn from(token: Token) -> Self {
        Self(vec![token])
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
