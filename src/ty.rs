use std::fmt;

use crate::expr::{Path, Const, PathSegment};
use crate::stmt::Param;
use crate::token::{TokenStream, Token, Delimiter, KeywordToken, BinOpToken};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MutTy {
    pub mutable: bool,
    pub ty: Box<Type>,
}

impl fmt::Display for MutTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.mutable {
            write!(f, "mut ")?;
        }
        write!(f, "{ty}", ty = self.ty)
    }
}

impl From<MutTy> for TokenStream {
    fn from(value: MutTy) -> Self {
        let mut ts = TokenStream::new();
        if value.mutable {
            ts.push(Token::Keyword(KeywordToken::Mut));
        }
        ts.extend(TokenStream::from(*value.ty));
        ts
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ref {
    pub lifetime: Option<String>,
    pub ty: MutTy,
}

impl fmt::Display for Ref {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "&")?;
        if let Some(lifetime) = &self.lifetime {
            write!(f, "'{lifetime} ")?;
        }
        write!(f, "{ty}", ty = self.ty)
    }
}

impl From<Ref> for TokenStream {
    fn from(value: Ref) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::BinOp(BinOpToken::And));
        if let Some(lifetime) = value.lifetime {
            ts.push(Token::Lifetime(lifetime));
        }
        ts.extend(TokenStream::from(value.ty));
        ts
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BareFn {
    pub generic_params: Vec<GenericParam>,
    pub inputs: Vec<Param>,
    pub output: Box<Type>,
}

impl fmt::Display for BareFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "fn(")?;
        for (i, param) in self.inputs.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{param}")?;
        }
        write!(f, ") -> {}", self.output)
    }
}

impl From<BareFn> for TokenStream {
    fn from(value: BareFn) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Fn));
        ts.push(Token::OpenDelim(Delimiter::Parenthesis));
        for (i, param) in value.inputs.iter().enumerate() {
            if i > 0 {
                ts.push(Token::Comma);
            }
            ts.extend(TokenStream::from(param.clone()));
        }
        ts.push(Token::CloseDelim(Delimiter::Parenthesis));
        ts.push(Token::RArrow);
        ts.extend(TokenStream::from(*value.output));
        ts
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GenericParam {
    pub ident: String,
    pub bounds: Vec<GenericBound>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PolyTraitRef {
    pub bound_generic_params: Vec<GenericParam>,
    pub trait_ref: Path,
}

impl fmt::Display for PolyTraitRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.bound_generic_params.is_empty() {
            // TODO:
        }
        write!(f, "{trait_ref}", trait_ref = self.trait_ref)
    }
}

impl From<PolyTraitRef> for TokenStream {
    fn from(value: PolyTraitRef) -> Self {
        TokenStream::from(value.trait_ref)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GenericBound {
    Trait(PolyTraitRef),
    Outlives(String),
}

impl fmt::Display for GenericBound {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Trait(trait_ref) => write!(f, "{trait_ref}"),
            Self::Outlives(lifetime) => write!(f, "'{lifetime}"),
        }
    }
}

impl From<GenericBound> for TokenStream {
    fn from(value: GenericBound) -> Self {
        match value {
            GenericBound::Trait(trait_ref) => TokenStream::from(trait_ref),
            GenericBound::Outlives(lifetime) => TokenStream::from(vec![Token::Lifetime(lifetime)]),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitObject {
    pub is_dyn: bool,
    pub bounds: Vec<GenericBound>,
}

impl fmt::Display for TraitObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_dyn {
            write!(f, "dyn ")?;
        }
        write!(f, "{bounds}", bounds = self.bounds.iter().map(|b| format!("{b}")).collect::<Vec<_>>().join(" + "))
    }
}

impl From<TraitObject> for TokenStream {
    fn from(value: TraitObject) -> Self {
        let mut ts = TokenStream::new();
        if value.is_dyn {
            ts.push(Token::Keyword(KeywordToken::Dyn));
        }
        for (i, bound) in value.bounds.into_iter().enumerate() {
            if i > 0 {
                ts.push(Token::BinOp(BinOpToken::Plus));
            }
            ts.extend(TokenStream::from(bound.clone()));
        }
        ts
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImplTrait {
    pub bounds: Vec<GenericBound>,
}

impl fmt::Display for ImplTrait {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "impl {bounds}", bounds = self.bounds.iter().map(|b| format!("{b}")).collect::<Vec<_>>().join(" + "))
    }
}

impl From<ImplTrait> for TokenStream {
    fn from(value: ImplTrait) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Impl));
        for (i, bound) in value.bounds.into_iter().enumerate() {
            if i > 0 {
                ts.push(Token::BinOp(BinOpToken::Plus));
            }
            ts.extend(TokenStream::from(bound));
        }
        ts
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Slice(Box<Type>),
    Array(Box<Type>, Box<Const>),
    Ref(Ref),
    BareFn(BareFn),
    Never,
    Path(Path),
    TraitObject(TraitObject),
    ImplTrait(ImplTrait),
    Infer,
    ImplicitSelf,
    Err,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Slice(ty) => write!(f, "[{ty}]"),
            Self::Array(ty, len) => write!(f, "[{ty}; {len}]"),
            Self::Ref(r) => write!(f, "{r}"),
            Self::BareFn(bare_fn) => write!(f, "{bare_fn}"),
            Self::Never => write!(f, "!"),
            Self::Path(path) => write!(f, "{path}"),
            Self::TraitObject(trait_object) => write!(f, "{trait_object}"),
            Self::ImplTrait(impl_trait) => write!(f, "{impl_trait}"),
            Self::Infer => write!(f, "_"),
            Self::ImplicitSelf => write!(f, ""),
            Self::Err => write!(f, "<Err>"),
        }
    }
}

impl<P: Into<PathSegment>> From<P> for Type {
    fn from(p: P) -> Self {
        Self::Path(Path::single(p))
    }
}

impl From<Type> for TokenStream {
    fn from(value: Type) -> Self {
        match value {
            Type::Slice(ty) => {
                let mut ts = TokenStream::new();
                ts.push(Token::OpenDelim(Delimiter::Bracket));
                ts.extend(TokenStream::from(*ty));
                ts.push(Token::CloseDelim(Delimiter::Bracket));
                ts
            },
            Type::Array(ty, len) => {
                let mut ts = TokenStream::new();
                ts.push(Token::OpenDelim(Delimiter::Bracket));
                ts.extend(TokenStream::from(*ty));
                ts.push(Token::Semi);
                ts.extend(TokenStream::from(*len));
                ts.push(Token::CloseDelim(Delimiter::Bracket));
                ts
            },
            Type::Ref(ref_) => TokenStream::from(ref_),
            Type::BareFn(bare_fn) => TokenStream::from(bare_fn),
            Type::Never => TokenStream::from(vec![Token::Not]),
            Type::Path(path) => TokenStream::from(path),
            Type::TraitObject(trait_object) => TokenStream::from(trait_object),
            Type::ImplTrait(impl_trait) => TokenStream::from(impl_trait),
            Type::Infer => TokenStream::from(vec![Token::ident("_")]),
            Type::ImplicitSelf => TokenStream::new(),
            Type::Err => TokenStream::from(vec![Token::ident("<Err>")]),
        }
    }
}
