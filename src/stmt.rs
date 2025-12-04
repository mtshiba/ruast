use core::fmt::Write;
use std::fmt;
use std::hash::Hash;
use std::ops::{Deref, Index, IndexMut};

use crate::expr::{
    Async, Attribute, Call, ConstBlock, DelimArgs, Expr, MacCall, MethodCall, Path, Range,
    TryBlock, UnsafeBlock,
};
use crate::token::{BinOpToken, Delimiter, KeywordToken, Token, TokenStream};
use crate::ty::Type;
use crate::{
    impl_display_for_enum, impl_hasitem_methods, impl_obvious_conversion, ForLoop, GenericParam,
    HasPrecedence, Lit, Mutability, OperatorPrecedence,
};

#[cfg(feature = "fuzzing")]
use crate::token::String;

#[cfg(feature = "tokenize")]
crate::impl_to_tokens!(
    Local,
    LocalKind,
    PatField,
    IdentPat,
    StructPat,
    TupleStructPat,
    RefPat,
    Pat,
    Param,
    FnDecl,
    Fn,
    LoadedMod,
    Mod,
    Block,
    FieldDef,
    Fields,
    Variant,
    EnumDef,
    StructDef,
    UnionDef,
    TraitDef,
    Impl,
    MacroDef,
    ExternBlock,
    ExternCrate,
    Item,
    ItemKind,
    Use,
    StaticItem,
    ConstItem,
    TyAlias,
    AssocItemKind,
    AssocItem,
    Empty,
    Semi,
    Stmt,
);

pub trait Ident {
    fn ident(&self) -> &str;
}
pub trait AddVisibility<K> {
    fn inherited(item: impl Into<K>) -> Self;
    fn public(item: impl Into<K>) -> Self;
}
pub trait MaybeIdent {
    fn ident(&self) -> Option<&str>;
}

impl<I: Ident> MaybeIdent for I {
    fn ident(&self) -> Option<&str> {
        Some(self.ident())
    }
}

pub trait EmptyItem {
    type Input;
    fn empty(ident: impl Into<Self::Input>) -> Self;
}

/// This index should not be kept after the item is removed.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct ItemIndex(pub(crate) usize);

pub trait HasItem<Itm: MaybeIdent = Item> {
    fn items(&self) -> &[Itm];
    fn items_mut(&mut self) -> &mut Vec<Itm>;
    fn with_item(mut self, item: impl Into<Itm>) -> Self
    where
        Self: Sized,
    {
        self.add_item(item);
        self
    }
    fn add_item(&mut self, item: impl Into<Itm>) -> ItemIndex {
        self.items_mut().push(item.into());
        ItemIndex(self.items().len() - 1)
    }
    fn add_pub_item<K>(&mut self, item: impl Into<K>) -> ItemIndex
    where
        Itm: AddVisibility<K>,
    {
        let item = Itm::public(item);
        self.items_mut().push(item);
        ItemIndex(self.items().len() - 1)
    }
    fn with_pub_item<K>(mut self, item: impl Into<K>) -> Self
    where
        Self: Sized,
        Itm: AddVisibility<K>,
    {
        self.add_pub_item(item);
        self
    }
    fn try_remove_item(&mut self, index: usize) -> Option<Itm> {
        self.items_mut().get(index)?;
        Some(self.items_mut().remove(index))
    }
    fn remove_item(&mut self, index: ItemIndex) -> Itm {
        self.items_mut().remove(index.0)
    }
    fn try_remove_item_by_id(&mut self, ident: &str) -> Option<Itm> {
        let index = self
            .items()
            .iter()
            .position(|item| item.ident() == Some(ident))?;
        Some(self.try_remove_item(index).unwrap())
    }
    fn get_item(&self, index: usize) -> Option<&Itm> {
        self.items().get(index)
    }
    fn get_item_mut(&mut self, index: usize) -> Option<&mut Itm> {
        self.items_mut().get_mut(index)
    }
    fn get_item_by_id(&self, ident: &str) -> Option<&Itm> {
        self.items().iter().find(|item| item.ident() == Some(ident))
    }
}

pub trait IntoItem {
    /// Creates an item with the given visibility.
    fn into_item(self, visibility: Visibility) -> Item;
    /// Creates an item with inherited (mostly private) visibility.
    fn into_inherited_item(self) -> Item
    where
        Self: Sized,
    {
        self.into_item(Visibility::Inherited)
    }
    /// Creates an item with public visibility.
    fn into_public_item(self) -> Item
    where
        Self: Sized,
    {
        self.into_item(Visibility::Public)
    }
    /// Creates an item with the given visibility scope.
    fn into_scoped_item(self, scope: VisibilityScope) -> Item
    where
        Self: Sized,
    {
        self.into_item(Visibility::Scoped(scope))
    }
}

macro_rules! impl_into_item {
    ($ty: ident) => {
        impl IntoItem for $ty {
            fn into_item(self, vis: Visibility) -> Item {
                Item::new(vis, ItemKind::$ty(self))
            }
        }
    };
}

pub trait Semicolon {
    fn semi(self) -> Semi;
}

impl<E: Into<Expr>> Semicolon for E {
    fn semi(self) -> Semi {
        Semi::new(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RefWithInnerAttrs<'i, 'a, I> {
    pub inner: Vec<&'a Attribute>,
    pub item: &'i I,
}

impl<I> Deref for RefWithInnerAttrs<'_, '_, I> {
    type Target = I;

    fn deref(&self) -> &Self::Target {
        self.item
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WithInnerAttrs<I> {
    pub inner: Vec<Attribute>,
    pub item: I,
}

impl<I> Deref for WithInnerAttrs<I> {
    type Target = I;

    fn deref(&self) -> &Self::Target {
        &self.item
    }
}

/// `'let' pat (:ty)? (= expr)?;`
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Local {
    pub pat: Pat,
    pub ty: Option<Type>,
    pub kind: LocalKind,
}

impl fmt::Display for Local {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "let {pat}", pat = self.pat)?;
        if let Some(ty) = &self.ty {
            write!(f, ": {ty}")?;
        }
        write!(f, "{kind};", kind = self.kind)
    }
}

impl From<Local> for TokenStream {
    fn from(value: Local) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Let));
        if let Some(ty) = value.ty {
            ts.extend(TokenStream::from(value.pat).into_joint());
            ts.push(Token::Colon);
            ts.extend(TokenStream::from(ty));
        } else {
            ts.extend(TokenStream::from(value.pat));
        }
        ts.extend(TokenStream::from(value.kind).into_joint());
        ts.push(Token::Semi);
        ts
    }
}

impl Local {
    pub fn new(pat: impl Into<Pat>, ty: Option<Type>, kind: impl Into<LocalKind>) -> Self {
        Self {
            pat: pat.into(),
            ty,
            kind: kind.into(),
        }
    }

    pub fn simple(pat: impl Into<Pat>, expr: impl Into<Expr>) -> Self {
        Self::new(pat, None, LocalKind::Init(expr.into()))
    }

    pub fn let_else(pat: impl Into<Pat>, expr: impl Into<Expr>, block: Block) -> Self {
        Self::new(pat, None, LocalKind::InitElse(expr.into(), block))
    }

    pub fn set_ty(&mut self, ty: Type) {
        self.ty = Some(ty);
    }

    pub fn with_ty(mut self, ty: Type) -> Self {
        self.set_ty(ty);
        self
    }
}

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LocalKind {
    Decl,
    Init(Expr),
    InitElse(Expr, Block),
}

impl fmt::Display for LocalKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Decl => write!(f, ""),
            Self::Init(expr) => write!(f, " = {expr}"),
            Self::InitElse(expr, block) => {
                if OperatorPrecedence::Assign < expr.precedence() {
                    write!(f, " = ({expr}) else {block}")
                } else {
                    write!(f, " = {expr} else {block}")
                }
            }
        }
    }
}

impl<E: Into<Expr>> From<E> for LocalKind {
    fn from(expr: E) -> Self {
        Self::Init(expr.into())
    }
}

impl From<LocalKind> for TokenStream {
    fn from(value: LocalKind) -> Self {
        match value {
            LocalKind::Decl => TokenStream::new(),
            LocalKind::Init(expr) => {
                let mut ts = TokenStream::new();
                ts.push(Token::Eq);
                ts.extend(TokenStream::from(expr));
                ts
            }
            LocalKind::InitElse(expr, block) => {
                let mut ts = TokenStream::new();
                ts.push(Token::Eq);
                if OperatorPrecedence::Assign < expr.precedence() {
                    ts.push(Token::OpenDelim(Delimiter::Parenthesis).into_joint());
                    ts.extend(TokenStream::from(expr));
                    ts.push(Token::CloseDelim(Delimiter::Parenthesis));
                } else {
                    ts.extend(TokenStream::from(expr));
                }
                ts.push(Token::Keyword(KeywordToken::Else));
                ts.extend(TokenStream::from(block));
                ts
            }
        }
    }
}

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PatField {
    pub ident: String,
    pub pat: Pat,
}

impl Ident for PatField {
    fn ident(&self) -> &str {
        &self.ident
    }
}

impl fmt::Display for PatField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{ident}: {pat}", ident = self.ident, pat = self.pat)
    }
}

impl From<PatField> for TokenStream {
    fn from(value: PatField) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::ident(value.ident).into_joint());
        ts.push(Token::Colon);
        ts.extend(TokenStream::from(value.pat));
        ts
    }
}

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IdentPat {
    pub is_mut: bool,
    pub ident: String,
    pub pat: Option<Box<Pat>>,
}

impl fmt::Display for IdentPat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_mut {
            write!(f, "mut ")?;
        }
        write!(f, "{ident}", ident = self.ident)?;
        if let Some(pat) = &self.pat {
            write!(f, " @ {pat}")?;
        }
        Ok(())
    }
}

impl<S: Into<String>> From<S> for IdentPat {
    fn from(ident: S) -> Self {
        Self {
            is_mut: false,
            ident: ident.into(),
            pat: None,
        }
    }
}

impl From<IdentPat> for TokenStream {
    fn from(value: IdentPat) -> Self {
        let mut ts = TokenStream::new();
        if value.is_mut {
            ts.push(Token::Keyword(KeywordToken::Mut));
        }
        ts.push(Token::ident(value.ident));
        if let Some(pat) = value.pat {
            ts.push(Token::At);
            ts.extend(TokenStream::from(*pat));
        }
        ts
    }
}

impl IdentPat {
    pub fn new(is_mut: bool, ident: impl Into<String>, pat: Option<impl Into<Pat>>) -> Self {
        Self {
            is_mut,
            ident: ident.into(),
            pat: pat.map(|x| Box::new(x.into())),
        }
    }

    pub fn mut_(ident: impl Into<String>, pat: Option<Pat>) -> Self {
        Self {
            is_mut: true,
            ident: ident.into(),
            pat: pat.map(Box::new),
        }
    }

    pub fn simple(ident: impl Into<String>) -> Self {
        Self::from(ident.into())
    }

    pub fn set_pat(&mut self, pat: impl Into<Pat>) {
        self.pat = Some(Box::new(pat.into()));
    }

    pub fn with_pat(mut self, pat: impl Into<Pat>) -> Self {
        self.set_pat(pat);
        self
    }
}

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructPat {
    pub path: Path,
    pub fields: Vec<PatField>,
}

impl fmt::Display for StructPat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{path} {{", path = self.path)?;
        for (i, field) in self.fields.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{field}")?;
        }
        write!(f, "}}")
    }
}

impl From<StructPat> for TokenStream {
    fn from(value: StructPat) -> Self {
        let mut ts = TokenStream::new();
        ts.extend(TokenStream::from(value.path));
        ts.push(Token::OpenDelim(Delimiter::Brace));
        for (i, field) in value.fields.iter().enumerate() {
            if i != 0 {
                ts.push(Token::Comma);
            }
            if i == value.fields.len() - 1 {
                ts.extend(TokenStream::from(field.clone()));
            } else {
                ts.extend(TokenStream::from(field.clone()).into_joint());
            }
        }
        ts.push(Token::CloseDelim(Delimiter::Brace));
        ts
    }
}

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TupleStructPat {
    pub path: Path,
    pub pats: Vec<Pat>,
}

impl fmt::Display for TupleStructPat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{path}(", path = self.path)?;
        for (i, pat) in self.pats.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{pat}")?;
        }
        write!(f, ")")
    }
}

impl From<TupleStructPat> for TokenStream {
    fn from(value: TupleStructPat) -> Self {
        let mut ts = TokenStream::new();
        ts.extend(TokenStream::from(value.path).into_joint());
        ts.push(Token::OpenDelim(Delimiter::Parenthesis).into_joint());
        for (i, pat) in value.pats.iter().enumerate() {
            if i != 0 {
                ts.push(Token::Comma);
            }
            ts.extend(TokenStream::from(pat.clone()).into_joint());
        }
        ts.push(Token::CloseDelim(Delimiter::Parenthesis));
        ts
    }
}

impl TupleStructPat {
    pub fn new(path: Path, pats: Vec<Pat>) -> Self {
        Self { path, pats }
    }

    pub fn add_pat(&mut self, pat: Pat) {
        self.pats.push(pat);
    }

    pub fn with_pat(mut self, pat: Pat) -> Self {
        self.add_pat(pat);
        self
    }
}

/// `('&mut' | '&') pat`
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RefPat {
    pub is_mut: bool,
    pub pat: Box<Pat>,
}

impl fmt::Display for RefPat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_mut {
            write!(f, "&mut ")?;
        } else {
            write!(f, "&")?;
        }
        write!(f, "{}", self.pat)
    }
}

impl From<RefPat> for TokenStream {
    fn from(value: RefPat) -> Self {
        let mut ts = TokenStream::new();
        if value.is_mut {
            ts.push(Token::And.into_joint());
            ts.push(Token::Keyword(KeywordToken::Mut));
        } else {
            ts.push(Token::And);
        }
        ts.extend(TokenStream::from(*value.pat));
        ts
    }
}

impl RefPat {
    pub fn immut(pat: Pat) -> Self {
        Self {
            is_mut: false,
            pat: Box::new(pat),
        }
    }

    pub fn mut_(pat: Pat) -> Self {
        Self {
            is_mut: true,
            pat: Box::new(pat),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pat {
    Wild,
    Ident(IdentPat),
    Struct(StructPat),
    TupleStruct(TupleStructPat),
    Or(Vec<Pat>),
    Tuple(Vec<Pat>),
    Box(Box<Pat>),
    Ref(RefPat),
    Lit(Expr),
    Range(Range),
    Slice(Vec<Pat>),
    Rest,
    Paren(Box<Pat>),
    MacCall(MacCall),
}

#[cfg(feature = "fuzzing")]
impl<'a> arbitrary::Arbitrary<'a> for Pat {
    fn arbitrary(u: &mut arbitrary::Unstructured<'_>) -> arbitrary::Result<Self> {
        if crate::depth_limiter::reached() {
            return Ok(Pat::Wild);
        }
        // Generates only irrefutable patterns.
        match u.int_in_range(0..=7)? {
            0 => Ok(Self::Wild),
            1 => Ok(Self::Ident(IdentPat::arbitrary(u)?)),
            2 => Ok(Self::Struct(StructPat::arbitrary(u)?)),
            3 => Ok(Self::TupleStruct(TupleStructPat::arbitrary(u)?)),
            4 => {
                let len = u.int_in_range(1..=5)?;
                let mut pats = Vec::with_capacity(len);
                for _ in 0..len {
                    pats.push(Pat::arbitrary(u)?);
                }
                Ok(Self::Or(pats))
            }
            5 => {
                let len = u.int_in_range(1..=5)?;
                let mut pats = Vec::with_capacity(len);
                for _ in 0..len {
                    pats.push(Pat::arbitrary(u)?);
                }
                Ok(Self::Tuple(pats))
            }
            // 6 => Ok(Self::Box(Box::new(Pat::arbitrary(u)?))),
            6 => Ok(Self::Ref(RefPat::arbitrary(u)?)),
            7 => {
                let len = u.int_in_range(1..=5)?;
                let mut pats = Vec::with_capacity(len);
                for _ in 0..len {
                    pats.push(Pat::arbitrary(u)?);
                }
                Ok(Self::Slice(pats))
            }
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for Pat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Wild => write!(f, "_"),
            Self::Ident(ident) => write!(f, "{ident}"),
            Self::Tuple(pats) => {
                write!(f, "(")?;
                for (i, pat) in pats.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{pat}")?;
                }
                write!(f, ")")
            }
            Self::Struct(struct_pat) => write!(f, "{struct_pat}"),
            Self::TupleStruct(tuple_struct_pat) => write!(f, "{tuple_struct_pat}"),
            Self::Box(pat) => write!(f, "box {pat}"),
            Self::Ref(ref_pat) => write!(f, "{ref_pat}"),
            Self::Or(pats) => {
                write!(f, "(")?;
                for (i, pat) in pats.iter().enumerate() {
                    if i != 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{pat}")?;
                }
                write!(f, ")")
            }
            Self::Lit(expr) => write!(f, "{expr}"),
            Self::Range(range) => write!(f, "{range}"),
            Self::Slice(pats) => {
                write!(f, "[")?;
                for (i, pat) in pats.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{pat}")?;
                }
                write!(f, "]")
            }
            Self::Rest => write!(f, "..."),
            Self::Paren(pat) => write!(f, "({pat})"),
            Self::MacCall(mac_call) => write!(f, "{mac_call}"),
        }
    }
}

impl<I: Into<IdentPat>> From<I> for Pat {
    fn from(ident: I) -> Self {
        Self::Ident(ident.into())
    }
}

impl From<Pat> for TokenStream {
    fn from(value: Pat) -> Self {
        match value {
            Pat::Wild => TokenStream::from(vec![Token::ident("_")]),
            Pat::Ident(ident) => TokenStream::from(ident),
            Pat::Struct(struct_pat) => TokenStream::from(struct_pat),
            Pat::TupleStruct(tuple_struct_pat) => TokenStream::from(tuple_struct_pat),
            Pat::Or(pats) => {
                let mut ts = TokenStream::new();
                ts.push(Token::OpenDelim(Delimiter::Parenthesis).into_joint());
                for (i, pat) in pats.iter().enumerate() {
                    if i != 0 {
                        ts.push(Token::Or);
                    }
                    if i == pats.len() - 1 {
                        ts.extend(TokenStream::from(pat.clone()).into_joint());
                    } else {
                        ts.extend(TokenStream::from(pat.clone()));
                    }
                }
                ts.push(Token::CloseDelim(Delimiter::Parenthesis));
                ts
            }
            Pat::Tuple(pats) => {
                let mut ts = TokenStream::new();
                ts.push(Token::OpenDelim(Delimiter::Parenthesis).into_joint());
                for (i, pat) in pats.iter().enumerate() {
                    if i != 0 {
                        ts.push(Token::Comma);
                    }
                    ts.extend(TokenStream::from(pat.clone()).into_joint());
                }
                ts.push(Token::CloseDelim(Delimiter::Parenthesis));
                ts
            }
            Pat::Box(pat) => {
                let mut ts = TokenStream::new();
                ts.push(Token::Keyword(KeywordToken::Box));
                ts.extend(TokenStream::from(*pat));
                ts
            }
            Pat::Ref(ref_pat) => TokenStream::from(ref_pat),
            Pat::Lit(expr) => TokenStream::from(expr),
            Pat::Range(range) => TokenStream::from(range),
            Pat::Slice(pats) => {
                let mut ts = TokenStream::new();
                ts.push(Token::OpenDelim(Delimiter::Bracket));
                for (i, pat) in pats.iter().enumerate() {
                    if i != 0 {
                        ts.push(Token::Comma);
                    }
                    ts.extend(TokenStream::from(pat.clone()));
                }
                ts.push(Token::CloseDelim(Delimiter::Bracket));
                ts
            }
            Pat::Rest => TokenStream::from(vec![Token::DotDotDot]),
            Pat::Paren(pat) => {
                let mut ts = TokenStream::new();
                ts.push(Token::OpenDelim(Delimiter::Parenthesis));
                ts.extend(TokenStream::from(*pat));
                ts.push(Token::CloseDelim(Delimiter::Parenthesis));
                ts
            }
            Pat::MacCall(mac_call) => TokenStream::from(mac_call),
        }
    }
}

impl Pat {
    pub fn ident(ident: impl Into<String>) -> Self {
        Self::Ident(IdentPat::from(ident.into()))
    }

    pub fn slf() -> Self {
        Self::Ident(IdentPat::from("self"))
    }

    pub fn ref_self() -> Self {
        Self::Ref(RefPat::immut(Self::slf()))
    }

    pub fn ref_mut_self() -> Self {
        Self::Ref(RefPat::mut_(Self::slf()))
    }

    pub fn mut_self() -> Self {
        Self::Ident(IdentPat::mut_("self", None))
    }

    pub fn mut_(ident: impl Into<String>) -> Self {
        Self::Ident(IdentPat::mut_(ident, None))
    }

    pub fn bind(self, kind: impl Into<LocalKind>) -> Local {
        Local::new(self, None, kind)
    }

    pub fn or(self, pat: Pat) -> Self {
        match self {
            Pat::Or(mut pats) => {
                pats.push(pat);
                Pat::Or(pats)
            }
            _ => Pat::Or(vec![self, pat]),
        }
    }
}

/// `pat ':' ty`
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    pub pat: Pat,
    pub ty: Type,
}

impl fmt::Display for Param {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.ty == Type::ImplicitSelf {
            write!(f, "{pat}", pat = self.pat)
        } else {
            write!(f, "{pat}: {ty}", pat = self.pat, ty = self.ty)
        }
    }
}

impl From<Param> for TokenStream {
    fn from(value: Param) -> Self {
        let mut ts = TokenStream::new();
        ts.extend(TokenStream::from(value.pat).into_joint());
        if value.ty != Type::ImplicitSelf {
            ts.push(Token::Colon);
            ts.extend(TokenStream::from(value.ty));
        }
        ts
    }
}

impl Param {
    pub fn new(pat: Pat, ty: Type) -> Self {
        Self { pat, ty }
    }

    pub fn ident(ident: impl Into<String>, ty: Type) -> Self {
        Self::new(Pat::ident(ident), ty)
    }

    pub fn slf() -> Self {
        Self::new(Pat::slf(), Type::ImplicitSelf)
    }

    pub fn ref_self() -> Self {
        Self::new(Pat::ref_self(), Type::ImplicitSelf)
    }

    pub fn ref_mut_self() -> Self {
        Self::new(Pat::ref_mut_self(), Type::ImplicitSelf)
    }

    pub fn mut_self() -> Self {
        Self::new(Pat::mut_self(), Type::ImplicitSelf)
    }
}

/// `'(' params (, ...)? ')' ('->' output)?`
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnDecl {
    pub inputs: Vec<Param>,
    pub output: Option<Type>,
    pub is_variadic: bool,
}

impl fmt::Display for FnDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        for (i, param) in self.inputs.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{param}")?;
        }
        if self.is_variadic {
            if !self.inputs.is_empty() {
                write!(f, ", ")?;
            }
            write!(f, "...")?;
        }
        write!(f, ")")?;
        if let Some(output) = &self.output {
            write!(f, " -> {output}")?;
        }
        Ok(())
    }
}

impl From<FnDecl> for TokenStream {
    fn from(value: FnDecl) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::OpenDelim(Delimiter::Parenthesis).into_joint());
        for (i, param) in value.inputs.iter().enumerate() {
            if i != 0 {
                ts.push(Token::Comma);
            }
            ts.extend(TokenStream::from(param.clone()).into_joint());
        }
        if value.is_variadic {
            if !value.inputs.is_empty() {
                ts.push(Token::Comma);
            }
            ts.push(Token::DotDotDot);
        }
        ts.push(Token::CloseDelim(Delimiter::Parenthesis));
        if let Some(output) = value.output {
            ts.push(Token::RArrow);
            ts.extend(TokenStream::from(output));
        }
        ts
    }
}

impl FnDecl {
    pub fn new(inputs: Vec<Param>, output: Option<Type>, is_variadic: bool) -> Self {
        Self {
            inputs,
            output,
            is_variadic,
        }
    }

    /// non-variadic function declaration
    pub fn regular(inputs: Vec<Param>, output: Option<Type>) -> Self {
        Self::new(inputs, output, false)
    }

    pub fn variadic(inputs: Vec<Param>, output: Option<Type>) -> Self {
        Self::new(inputs, output, true)
    }

    pub fn empty() -> Self {
        Self::regular(Vec::new(), None)
    }

    pub fn add_input(&mut self, input: Param) {
        self.inputs.push(input);
    }

    pub fn with_input(mut self, input: Param) -> Self {
        self.add_input(input);
        self
    }

    pub fn set_output(&mut self, output: Type) {
        self.output = Some(output);
    }

    pub fn with_output(mut self, output: Type) -> Self {
        self.set_output(output);
        self
    }
}

/// `'fn' 'unsafe'? 'const'? 'async'? ('extern' "abi")? ident (<...>)? decl { ... }`
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Fn {
    pub is_unsafe: bool,
    pub is_const: bool,
    pub is_async: bool,
    pub abi: Option<String>,
    pub ident: String,
    pub generics: Vec<GenericParam>,
    pub fn_decl: FnDecl,
    pub body: Option<Block>,
}

impl_into_item!(Fn);

impl fmt::Display for RefWithInnerAttrs<'_, '_, Fn> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_const {
            write!(f, "const ")?;
        }
        if self.is_async {
            write!(f, "async ")?;
        }
        if self.is_unsafe {
            write!(f, "unsafe ")?;
        }
        if let Some(abi) = &self.abi {
            write!(f, "extern \"{abi}\" ")?;
        }
        write!(f, "fn {}", self.ident)?;
        if !self.generics.is_empty() {
            write!(f, "<")?;
            for (i, generic) in self.generics.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{generic}")?;
            }
            write!(f, ">")?;
        }
        write!(f, "{}", self.fn_decl)?;
        if let Some(body) = &self.body {
            write!(f, " ")?;
            RefWithInnerAttrs {
                item: body,
                inner: self.inner.clone(),
            }
            .fmt(f)?;
        } else if !self.inner.is_empty() {
            write!(f, " ")?;
            RefWithInnerAttrs {
                item: &Block::empty(),
                inner: self.inner.clone(),
            }
            .fmt(f)?;
        } else {
            write!(f, ";")?;
        }

        Ok(())
    }
}

impl fmt::Display for Fn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        RefWithInnerAttrs {
            item: self,
            inner: vec![],
        }
        .fmt(f)
    }
}

impl From<WithInnerAttrs<Fn>> for TokenStream {
    fn from(value: WithInnerAttrs<Fn>) -> Self {
        let inner = value.inner;
        let value = value.item;
        let mut ts = TokenStream::new();
        if value.is_const {
            ts.push(Token::Keyword(KeywordToken::Const));
        }
        if value.is_async {
            ts.push(Token::Keyword(KeywordToken::Async));
        }
        if value.is_unsafe {
            ts.push(Token::Keyword(KeywordToken::Unsafe));
        }
        if let Some(abi) = value.abi {
            ts.push(Token::Keyword(KeywordToken::Extern));
            ts.push(Token::lit(format!("\"{abi}\"")));
        }
        ts.push(Token::Keyword(KeywordToken::Fn));
        ts.push(Token::ident(value.ident).into_joint());
        if !value.generics.is_empty() {
            ts.push(Token::Lt.into_joint());
            for (i, generic) in value.generics.iter().enumerate() {
                if i != 0 {
                    ts.push(Token::Comma);
                }
                ts.extend(TokenStream::from(generic.clone()).into_joint());
            }
            ts.push(Token::Gt.into_joint());
        }
        ts.extend(TokenStream::from(value.fn_decl));
        if let Some(body) = value.body {
            ts.extend(TokenStream::from(WithInnerAttrs { item: body, inner }));
        } else if !inner.is_empty() {
            ts.extend(TokenStream::from(WithInnerAttrs {
                item: Block::empty(),
                inner,
            }));
        } else {
            ts.push(Token::Semi);
        }
        ts
    }
}

impl From<Fn> for TokenStream {
    fn from(value: Fn) -> Self {
        TokenStream::from(WithInnerAttrs {
            item: value,
            inner: Vec::new(),
        })
    }
}

impl EmptyItem for Fn {
    type Input = String;

    fn empty(ident: impl Into<Self::Input>) -> Self {
        Self::empty(ident)
    }
}

impl Ident for Fn {
    fn ident(&self) -> &str {
        &self.ident
    }
}

impl Fn {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        is_unsafe: bool,
        is_const: bool,
        is_async: bool,
        abi: Option<String>,
        ident: impl Into<String>,
        generics: Vec<GenericParam>,
        fn_decl: FnDecl,
        body: Option<Block>,
    ) -> Self {
        Self {
            is_unsafe,
            is_const,
            is_async,
            abi,
            ident: ident.into(),
            generics,
            fn_decl,
            body,
        }
    }

    pub fn simple(ident: impl Into<String>, fn_decl: FnDecl, body: Block) -> Self {
        Self {
            is_unsafe: false,
            is_const: false,
            is_async: false,
            abi: None,
            ident: ident.into(),
            generics: Vec::new(),
            fn_decl,
            body: Some(body),
        }
    }

    pub fn new_unsafe(
        ident: impl Into<String>,
        generics: Vec<GenericParam>,
        fn_decl: FnDecl,
    ) -> Self {
        Self {
            is_unsafe: true,
            is_const: false,
            is_async: false,
            abi: None,
            ident: ident.into(),
            generics,
            fn_decl,
            body: None,
        }
    }

    pub fn new_const(
        ident: impl Into<String>,
        generics: Vec<GenericParam>,
        fn_decl: FnDecl,
    ) -> Self {
        Self {
            is_unsafe: false,
            is_const: true,
            is_async: false,
            abi: None,
            ident: ident.into(),
            generics,
            fn_decl,
            body: None,
        }
    }

    pub fn new_async(
        ident: impl Into<String>,
        generics: Vec<GenericParam>,
        fn_decl: FnDecl,
    ) -> Self {
        Self {
            is_unsafe: false,
            is_const: false,
            is_async: true,
            abi: None,
            ident: ident.into(),
            generics,
            fn_decl,
            body: None,
        }
    }

    pub fn extern_c(
        ident: impl Into<String>,
        generics: Vec<GenericParam>,
        fn_decl: FnDecl,
    ) -> Self {
        Self {
            is_unsafe: false,
            is_const: false,
            is_async: false,
            abi: Some("C".into()),
            ident: ident.into(),
            generics,
            fn_decl,
            body: None,
        }
    }

    pub fn main(output: Option<Type>, body: Block) -> Self {
        Self {
            is_unsafe: false,
            is_const: false,
            is_async: false,
            abi: None,
            ident: "main".into(),
            generics: Vec::new(),
            fn_decl: FnDecl::regular(Vec::new(), output),
            body: Some(body),
        }
    }

    pub fn empty(ident: impl Into<String>) -> Self {
        Self {
            is_unsafe: false,
            is_const: false,
            is_async: false,
            abi: None,
            ident: ident.into(),
            generics: Vec::new(),
            fn_decl: FnDecl::empty(),
            body: None,
        }
    }

    pub fn empty_method(ident: impl Into<String>, self_pat: Pat) -> Self {
        Self {
            is_unsafe: false,
            is_const: false,
            is_async: false,
            abi: None,
            ident: ident.into(),
            generics: Vec::new(),
            fn_decl: FnDecl::regular(vec![Param::new(self_pat, Type::ImplicitSelf)], None),
            body: Some(Block::empty()),
        }
    }

    pub fn add_stmt(&mut self, stmt: impl Into<Stmt>) -> StmtIndex {
        self.body.get_or_insert_with(Block::empty).add_stmt(stmt)
    }

    pub fn add_semi_stmt(&mut self, expr: impl Into<Expr>) -> StmtIndex {
        self.body
            .get_or_insert_with(Block::empty)
            .add_stmt(Semi::new(expr))
    }

    pub fn with_stmt(mut self, stmt: impl Into<Stmt>) -> Self {
        self.add_stmt(stmt);
        self
    }

    pub fn with_semi_stmt(mut self, expr: impl Into<Expr>) -> Self {
        self.add_semi_stmt(expr);
        self
    }

    pub fn try_remove_stmt(&mut self, index: usize) -> Option<Stmt> {
        self.body.as_mut()?.try_remove_stmt(index)
    }

    pub fn remove_stmt(&mut self, index: StmtIndex) -> Stmt {
        self.body.as_mut().unwrap().remove_stmt(index)
    }

    pub fn add_generic_param(&mut self, param: GenericParam) {
        self.generics.push(param);
    }

    pub fn with_generic_param(mut self, param: GenericParam) -> Self {
        self.add_generic_param(param);
        self
    }
}

/// `'mod' ident { ... }`
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LoadedMod {
    pub ident: String,
    pub items: Vec<Item>,
}

impl fmt::Display for RefWithInnerAttrs<'_, '_, LoadedMod> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "mod {} {{", self.ident)?;
        let mut indent = indenter::indented(f).with_str("    ");
        for inner in &self.inner {
            writeln!(indent, "{inner}")?;
        }
        for item in self.items.iter() {
            writeln!(indent, "{item}")?;
        }
        write!(f, "}}")
    }
}

impl fmt::Display for LoadedMod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        RefWithInnerAttrs {
            inner: vec![],
            item: self,
        }
        .fmt(f)
    }
}

impl From<WithInnerAttrs<LoadedMod>> for TokenStream {
    fn from(value: WithInnerAttrs<LoadedMod>) -> Self {
        let inner = value.inner;
        let value = value.item;
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Mod));
        ts.push(Token::ident(value.ident));
        ts.push(Token::OpenDelim(Delimiter::Brace));
        for attr in inner {
            ts.extend(TokenStream::from(attr));
        }
        for item in value.items {
            ts.extend(TokenStream::from(item));
        }
        ts.push(Token::CloseDelim(Delimiter::Brace));
        ts
    }
}

impl From<LoadedMod> for TokenStream {
    fn from(value: LoadedMod) -> Self {
        TokenStream::from(WithInnerAttrs {
            item: value,
            inner: Vec::new(),
        })
    }
}

impl EmptyItem for LoadedMod {
    type Input = String;

    fn empty(ident: impl Into<Self::Input>) -> Self {
        Self::empty(ident)
    }
}

impl Ident for LoadedMod {
    fn ident(&self) -> &str {
        &self.ident
    }
}

impl HasItem for LoadedMod {
    fn items(&self) -> &[Item] {
        &self.items
    }
    fn items_mut(&mut self) -> &mut Vec<Item> {
        &mut self.items
    }
}

impl_hasitem_methods!(LoadedMod);

impl LoadedMod {
    pub fn new(ident: impl Into<String>, items: Vec<Item>) -> Self {
        Self {
            ident: ident.into(),
            items,
        }
    }

    pub fn empty(ident: impl Into<String>) -> Self {
        Self::new(ident, Vec::new())
    }

    pub fn ident(&self) -> &str {
        Ident::ident(self)
    }
}

/// `mod ident { ... }` or `mod ident;`
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Mod {
    Loaded(LoadedMod),
    Unloaded(String),
}

impl_into_item!(Mod);

impl fmt::Display for RefWithInnerAttrs<'_, '_, Mod> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.item {
            Mod::Loaded(module) => RefWithInnerAttrs {
                item: module,
                inner: self.inner.clone(),
            }
            .fmt(f),
            Mod::Unloaded(ident) => write!(f, "mod {ident};"),
        }
    }
}

impl fmt::Display for Mod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        RefWithInnerAttrs {
            item: self,
            inner: vec![],
        }
        .fmt(f)
    }
}

impl From<WithInnerAttrs<Mod>> for TokenStream {
    fn from(value: WithInnerAttrs<Mod>) -> Self {
        let inner = value.inner;
        let value = value.item;
        match value {
            Mod::Loaded(module) => TokenStream::from(WithInnerAttrs {
                item: module,
                inner,
            }),
            Mod::Unloaded(ident) => {
                let mut ts = TokenStream::new();
                ts.push(Token::Keyword(KeywordToken::Mod));
                ts.push(Token::ident(ident));
                ts.push(Token::Semi);
                ts
            }
        }
    }
}

impl From<Mod> for TokenStream {
    fn from(value: Mod) -> Self {
        TokenStream::from(WithInnerAttrs {
            item: value,
            inner: Vec::new(),
        })
    }
}

impl Ident for Mod {
    fn ident(&self) -> &str {
        match self {
            Self::Loaded(module) => &module.ident,
            Self::Unloaded(ident) => ident,
        }
    }
}

impl EmptyItem for Mod {
    type Input = String;

    fn empty(ident: impl Into<Self::Input>) -> Self {
        Self::empty(ident)
    }
}

impl Mod {
    pub fn new(ident: impl Into<String>, items: Vec<Item>) -> Self {
        Self::Loaded(LoadedMod::new(ident, items))
    }

    pub fn empty(ident: impl Into<String>) -> Self {
        Self::Loaded(LoadedMod::empty(ident))
    }

    pub fn ident(&self) -> &str {
        Ident::ident(self)
    }
}

/// This index should not be kept after the statement is removed.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct StmtIndex(usize);

/// `('label:)? { ... }`
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct LabelledBlock {
    pub label: Option<String>,
    pub block: Block,
}

#[cfg(feature = "fuzzing")]
impl LabelledBlock {
    pub fn arbitrary_no_label(u: &mut arbitrary::Unstructured<'_>) -> arbitrary::Result<Self> {
        use arbitrary::Arbitrary;
        Ok(Self {
            label: None,
            block: Block::arbitrary(u)?,
        })
    }
}

impl HasPrecedence for LabelledBlock {
    fn precedence(&self) -> OperatorPrecedence {
        OperatorPrecedence::AlwaysWrapped
    }
}

impl fmt::Display for LabelledBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(label) = &self.label {
            write!(f, "'{label}: ")?;
        }
        write!(f, "{}", self.block)?;
        Ok(())
    }
}

impl<S: Into<Stmt>> From<S> for LabelledBlock {
    fn from(stmt: S) -> Self {
        let mut block = LabelledBlock::empty();
        block.add_stmt(stmt);
        block
    }
}

impl From<LabelledBlock> for TokenStream {
    fn from(value: LabelledBlock) -> Self {
        let mut ts = TokenStream::new();
        if let Some(label) = value.label {
            ts.push(Token::lifetime(label));
            ts.push(Token::Colon);
        }
        ts.extend(TokenStream::from(value.block));
        ts
    }
}

impl Index<StmtIndex> for LabelledBlock {
    type Output = Stmt;

    fn index(&self, index: StmtIndex) -> &Self::Output {
        self.block.index(index)
    }
}

impl IndexMut<StmtIndex> for LabelledBlock {
    fn index_mut(&mut self, index: StmtIndex) -> &mut Self::Output {
        self.block.index_mut(index)
    }
}

impl HasItem<Stmt> for LabelledBlock {
    fn items(&self) -> &[Stmt] {
        self.block.items()
    }
    fn items_mut(&mut self) -> &mut Vec<Stmt> {
        self.block.items_mut()
    }
}

impl_hasitem_methods!(LabelledBlock, Stmt, Deref);

impl LabelledBlock {
    pub fn new(block: Block, label: Option<String>) -> Self {
        Self { block, label }
    }

    pub fn single(expr: impl Into<Expr>) -> Self {
        Self::new(Block::single(expr.into()), None)
    }

    pub fn empty() -> Self {
        Self {
            block: Block::empty(),
            label: None,
        }
    }

    pub fn with_stmt(mut self, stmt: impl Into<Stmt>) -> Self {
        self.add_stmt(stmt);
        self
    }

    pub fn add_stmt(&mut self, stmt: impl Into<Stmt>) -> StmtIndex {
        self.block.add_stmt(stmt)
    }

    pub fn try_remove_stmt(&mut self, index: usize) -> Option<Stmt> {
        self.block.try_remove_stmt(index)
    }

    pub fn remove_stmt(&mut self, index: StmtIndex) -> Stmt {
        self.block.remove_stmt(index)
    }

    pub fn try_remove_item_by_id(&mut self, ident: &str) -> Option<Item> {
        self.block.try_remove_item_by_id(ident)
    }

    pub fn get_stmt(&self, index: usize) -> Option<&Stmt> {
        self.block.get_stmt(index)
    }

    pub fn get_item_by_id(&self, ident: &str) -> Option<&Item> {
        self.block.get_item_by_id(ident)
    }
}

/// { ... }`
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[cfg(feature = "fuzzing")]
impl<'a> arbitrary::Arbitrary<'a> for Block {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        let i = u.int_in_range(1..=10)?;
        let mut stmts = Vec::with_capacity(i);
        for _ in 0..i {
            stmts.push(Stmt::arbitrary_not_expr(u)?);
        }
        Ok(Self { stmts })
    }
}

#[cfg(feature = "fuzzing")]
impl Block {
    pub fn arbitrary_item_block(u: &mut arbitrary::Unstructured<'_>) -> arbitrary::Result<Self> {
        let i = u.int_in_range(0..=10)?;
        let mut stmts = Vec::with_capacity(i);
        for _ in 0..i {
            stmts.push(Stmt::arbitrary_item(u)?);
        }
        Ok(Self { stmts })
    }

    pub fn arbitrary_extern_item_block(
        u: &mut arbitrary::Unstructured<'_>,
    ) -> arbitrary::Result<Self> {
        let i = u.int_in_range(0..=10)?;
        let mut stmts = Vec::with_capacity(i);
        for _ in 0..i {
            stmts.push(Stmt::arbitrary_extern_item(u)?);
        }
        Ok(Self { stmts })
    }
}

impl HasPrecedence for Block {
    fn precedence(&self) -> OperatorPrecedence {
        OperatorPrecedence::Elemental
    }
}

impl fmt::Display for RefWithInnerAttrs<'_, '_, Block> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.stmts.is_empty() && self.inner.is_empty() {
            write!(f, "{{}}")?;
        } else {
            writeln!(f, "{{")?;
            let mut indent = indenter::indented(f).with_str("    ");
            for inner in &self.inner {
                writeln!(indent, "{inner}")?;
            }
            for stmt in self.stmts.iter() {
                writeln!(indent, "{stmt}")?;
            }
            write!(f, "}}")?;
        }
        Ok(())
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        RefWithInnerAttrs {
            inner: vec![],
            item: self,
        }
        .fmt(f)
    }
}

impl<S: Into<Stmt>> From<S> for Block {
    fn from(stmt: S) -> Self {
        let mut block = Block::empty();
        block.add_stmt(stmt);
        block
    }
}

impl From<Vec<Stmt>> for Block {
    fn from(stmts: Vec<Stmt>) -> Self {
        Self { stmts }
    }
}

impl From<WithInnerAttrs<Block>> for TokenStream {
    fn from(value: WithInnerAttrs<Block>) -> Self {
        let inner = value.inner;
        let value = value.item;
        let mut ts = TokenStream::new();
        ts.push(Token::OpenDelim(Delimiter::Brace));
        for attr in inner {
            ts.extend(TokenStream::from(attr));
        }
        for stmt in value.stmts {
            ts.extend(TokenStream::from(stmt));
            // ts.push(Token::Semi);
        }
        ts.push(Token::CloseDelim(Delimiter::Brace));
        ts
    }
}

impl From<Block> for TokenStream {
    fn from(value: Block) -> Self {
        TokenStream::from(WithInnerAttrs {
            item: value,
            inner: Vec::new(),
        })
    }
}

impl Index<StmtIndex> for Block {
    type Output = Stmt;

    fn index(&self, index: StmtIndex) -> &Self::Output {
        &self.stmts[index.0]
    }
}

impl IndexMut<StmtIndex> for Block {
    fn index_mut(&mut self, index: StmtIndex) -> &mut Self::Output {
        &mut self.stmts[index.0]
    }
}

impl HasItem<Stmt> for Block {
    fn items(&self) -> &[Stmt] {
        &self.stmts
    }
    fn items_mut(&mut self) -> &mut Vec<Stmt> {
        &mut self.stmts
    }
}

impl_hasitem_methods!(Block, Stmt, Deref);

impl Block {
    pub fn new(stmts: Vec<Stmt>) -> Self {
        Self { stmts }
    }

    pub fn single(expr: impl Into<Expr>) -> Self {
        Self::new(vec![Stmt::Expr(expr.into())])
    }

    pub fn empty() -> Self {
        Self { stmts: Vec::new() }
    }

    pub fn async_(self) -> Async {
        Async::new(self)
    }

    pub fn try_(self) -> TryBlock {
        TryBlock::new(self)
    }

    pub fn unsafe_(self) -> UnsafeBlock {
        UnsafeBlock::new(self)
    }

    pub fn const_(self) -> ConstBlock {
        ConstBlock::new(self)
    }

    pub fn extern_(self, is_unsafe: bool, abi: Option<String>) -> ExternBlock {
        ExternBlock::new(is_unsafe, abi, self)
    }

    pub fn with_stmt(mut self, stmt: impl Into<Stmt>) -> Self {
        self.add_stmt(stmt);
        self
    }

    pub fn add_stmt(&mut self, stmt: impl Into<Stmt>) -> StmtIndex {
        self.stmts.push(stmt.into());
        StmtIndex(self.stmts.len() - 1)
    }

    pub fn try_remove_stmt(&mut self, index: usize) -> Option<Stmt> {
        self.stmts.get(index)?;
        Some(self.stmts.remove(index))
    }

    pub fn remove_stmt(&mut self, index: StmtIndex) -> Stmt {
        self.stmts.remove(index.0)
    }

    pub fn try_remove_item_by_id(&mut self, ident: &str) -> Option<Item> {
        let index = self
            .stmts
            .iter()
            .position(|stmt| stmt.ident() == Some(ident))?;
        let Some(Stmt::Item(item)) = self.try_remove_stmt(index) else {
            unreachable!()
        };
        Some(item)
    }

    pub fn get_stmt(&self, index: usize) -> Option<&Stmt> {
        self.stmts.get(index)
    }

    pub fn get_item_by_id(&self, ident: &str) -> Option<&Item> {
        let Stmt::Item(item) = self.stmts.iter().find(|stmt| stmt.ident() == Some(ident))? else {
            unreachable!()
        };
        Some(item)
    }
}

/// `vis (ident ':')? ty`
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldDef {
    pub attrs: Vec<Attribute>,
    pub vis: Visibility,
    pub ident: Option<String>,
    pub ty: Type,
}

impl fmt::Display for FieldDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for attr in self.attrs.iter() {
            writeln!(f, "{attr}")?;
        }
        write!(f, "{}", self.vis)?;
        if let Some(ident) = &self.ident {
            write!(f, "{ident}: ")?;
        }
        write!(f, "{ty}", ty = self.ty)
    }
}

impl From<FieldDef> for TokenStream {
    fn from(value: FieldDef) -> Self {
        let mut ts = TokenStream::new();
        for attr in value.attrs.iter() {
            ts.extend(TokenStream::from(attr.clone()));
        }
        ts.extend(TokenStream::from(value.vis));
        if let Some(ident) = value.ident {
            ts.push(Token::ident(ident).into_joint());
            ts.push(Token::Colon);
        }
        ts.extend(TokenStream::from(value.ty));
        ts
    }
}

impl MaybeIdent for FieldDef {
    fn ident(&self) -> Option<&str> {
        self.ident.as_deref()
    }
}

impl FieldDef {
    pub fn new(vis: Visibility, ident: Option<impl Into<String>>, ty: impl Into<Type>) -> Self {
        Self {
            attrs: Vec::new(),
            vis,
            ident: ident.map(Into::into),
            ty: ty.into(),
        }
    }

    pub fn anonymous(ty: impl Into<Type>) -> Self {
        Self::new(Visibility::Inherited, Option::<String>::None, ty)
    }

    pub fn inherited(ident: impl Into<String>, ty: impl Into<Type>) -> Self {
        Self::new(Visibility::Inherited, Some(ident), ty)
    }

    pub fn public(ident: impl Into<String>, ty: impl Into<Type>) -> Self {
        Self::new(Visibility::Public, Some(ident), ty)
    }

    pub fn with_attr(mut self, attr: impl Into<Attribute>) -> Self {
        self.add_attr(attr);
        self
    }

    pub fn add_attr(&mut self, attr: impl Into<Attribute>) {
        self.attrs.push(attr.into());
    }

    pub fn remove_attr(&mut self, index: usize) -> Attribute {
        self.attrs.remove(index)
    }
}

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Fields {
    Unit,
    Tuple(Vec<FieldDef>),
    Struct(Vec<FieldDef>),
}

impl fmt::Display for Fields {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => write!(f, "{{}}"),
            Self::Tuple(fields) => {
                write!(f, "(")?;
                for (i, field) in fields.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{field}")?;
                }
                write!(f, ")")
            }
            Self::Struct(fields) => {
                writeln!(f, " {{")?;
                let mut indent = indenter::indented(f).with_str("    ");
                for (i, field) in fields.iter().enumerate() {
                    if i != 0 {
                        writeln!(indent, ", ")?;
                    }
                    write!(indent, "{field}")?;
                }
                write!(f, "\n}}")
            }
        }
    }
}

impl From<Fields> for TokenStream {
    fn from(value: Fields) -> Self {
        match value {
            Fields::Unit => TokenStream::new(),
            Fields::Tuple(fields) => {
                let mut ts = TokenStream::new();
                ts.push(Token::OpenDelim(Delimiter::Parenthesis).into_joint());
                for (i, field) in fields.iter().enumerate() {
                    if i != 0 {
                        ts.push(Token::Comma);
                    }
                    ts.extend(TokenStream::from(field.clone()).into_joint());
                }
                ts.push(Token::CloseDelim(Delimiter::Parenthesis));
                ts
            }
            Fields::Struct(fields) => {
                let mut ts = TokenStream::new();
                ts.push(Token::OpenDelim(Delimiter::Brace));
                for (i, field) in fields.iter().enumerate() {
                    if i != 0 {
                        ts.push(Token::Comma);
                    }
                    if i == fields.len() - 1 {
                        ts.extend(TokenStream::from(field.clone()));
                    } else {
                        ts.extend(TokenStream::from(field.clone()).into_joint());
                    }
                }
                ts.push(Token::CloseDelim(Delimiter::Brace));
                ts
            }
        }
    }
}

impl HasItem<FieldDef> for Fields {
    fn items(&self) -> &[FieldDef] {
        match self {
            Self::Unit => &[],
            Self::Tuple(fields) => fields,
            Self::Struct(fields) => fields,
        }
    }
    fn items_mut(&mut self) -> &mut Vec<FieldDef> {
        match self {
            Self::Unit => panic!("unit variant"),
            Self::Tuple(fields) => fields,
            Self::Struct(fields) => fields,
        }
    }
}

impl_hasitem_methods!(Fields, FieldDef, Deref);

impl Fields {
    pub fn with_field(mut self, field: FieldDef) -> Self {
        self.add_field(field);
        self
    }

    pub fn add_field(&mut self, field: FieldDef) -> ItemIndex {
        match self {
            Self::Unit => {
                let new = if field.ident.is_some() {
                    Self::Struct(vec![field])
                } else {
                    Self::Tuple(vec![field])
                };
                *self = new;
            }
            Self::Tuple(fields) => fields.push(field),
            Self::Struct(fields) => fields.push(field),
        }
        ItemIndex(self.items().len() - 1)
    }

    pub fn try_remove_field(&mut self, index: usize) -> Option<FieldDef> {
        match self {
            Self::Unit => None,
            Self::Tuple(fields) => {
                fields.get(index)?;
                Some(fields.remove(index))
            }
            Self::Struct(fields) => {
                fields.get(index)?;
                Some(fields.remove(index))
            }
        }
    }

    pub fn remove_field(&mut self, index: ItemIndex) -> FieldDef {
        self.try_remove_field(index.0).expect("index out of bounds")
    }

    pub fn try_remove_field_by_id(&mut self, ident: &str) -> Option<FieldDef> {
        let index = match self {
            Self::Unit => return None,
            Self::Tuple(fields) => fields
                .iter()
                .position(|field| field.ident.as_deref() == Some(ident))?,
            Self::Struct(fields) => fields
                .iter()
                .position(|field| field.ident.as_deref() == Some(ident))?,
        };
        Some(self.try_remove_field(index).unwrap())
    }

    pub fn get_field_by_id(&self, ident: &str) -> Option<&FieldDef> {
        match self {
            Self::Unit => None,
            Self::Tuple(fields) => fields
                .iter()
                .find(|field| field.ident.as_deref() == Some(ident)),
            Self::Struct(fields) => fields
                .iter()
                .find(|field| field.ident.as_deref() == Some(ident)),
        }
    }
}

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variant {
    pub attrs: Vec<Attribute>,
    pub vis: Visibility,
    pub ident: String,
    pub fields: Fields,
    pub discriminant: Option<Expr>,
}

impl fmt::Display for Variant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for attr in self.attrs.iter() {
            writeln!(f, "{attr}")?;
        }
        write!(f, "{}{}{}", self.vis, self.ident, self.fields)?;
        if let Some(discriminant) = &self.discriminant {
            write!(f, " = {discriminant}")?;
        }
        Ok(())
    }
}

impl From<Variant> for TokenStream {
    fn from(value: Variant) -> Self {
        let mut ts = TokenStream::new();
        for attr in value.attrs.iter() {
            ts.extend(TokenStream::from(attr.clone()));
        }
        ts.extend(TokenStream::from(value.vis));
        ts.push(Token::ident(value.ident).into_joint());
        ts.extend(TokenStream::from(value.fields));
        if let Some(discriminant) = value.discriminant {
            ts.push(Token::Eq);
            ts.extend(TokenStream::from(discriminant));
        }
        ts
    }
}

impl EmptyItem for Variant {
    type Input = String;

    fn empty(ident: impl Into<Self::Input>) -> Self {
        Self::empty(ident)
    }
}

impl Ident for Variant {
    fn ident(&self) -> &str {
        &self.ident
    }
}

impl Variant {
    pub fn new(
        attrs: Vec<Attribute>,
        vis: Visibility,
        ident: impl Into<String>,
        data: Fields,
        discriminant: Option<Expr>,
    ) -> Self {
        Self {
            attrs,
            vis,
            ident: ident.into(),
            fields: data,
            discriminant,
        }
    }

    pub fn empty(ident: impl Into<String>) -> Self {
        Self::new(vec![], Visibility::Inherited, ident, Fields::Unit, None)
    }

    pub fn inherited(ident: impl Into<String>, data: Fields) -> Self {
        Self::new(vec![], Visibility::Inherited, ident, data, None)
    }

    pub fn struct_(ident: impl Into<String>, fields: Vec<FieldDef>) -> Self {
        Self::new(
            vec![],
            Visibility::Inherited,
            ident,
            Fields::Struct(fields),
            None,
        )
    }

    pub fn tuple(ident: impl Into<String>, fields: Vec<FieldDef>) -> Self {
        Self::new(
            vec![],
            Visibility::Inherited,
            ident,
            Fields::Tuple(fields),
            None,
        )
    }

    pub fn tuple1(ident: impl Into<String>, ty: impl Into<Type>) -> Self {
        Self::tuple(ident, vec![FieldDef::anonymous(ty)])
    }

    pub fn add_attr(&mut self, attr: impl Into<Attribute>) {
        self.attrs.push(attr.into());
    }

    pub fn with_attr(mut self, attr: impl Into<Attribute>) -> Self {
        self.add_attr(attr);
        self
    }

    pub fn remove_attr(&mut self, index: usize) -> Attribute {
        self.attrs.remove(index)
    }

    pub fn set_discriminant(&mut self, expr: impl Into<Expr>) {
        self.discriminant = Some(expr.into());
    }

    pub fn with_discriminant(mut self, expr: impl Into<Expr>) -> Self {
        self.set_discriminant(expr);
        self
    }
}

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumDef {
    pub ident: String,
    pub generics: Vec<GenericParam>,
    pub variants: Vec<Variant>,
}

impl_into_item!(EnumDef);

impl fmt::Display for EnumDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "enum {}", self.ident)?;
        if !self.generics.is_empty() {
            write!(f, "<")?;
            for (i, generic) in self.generics.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{generic}")?;
            }
            write!(f, ">")?;
        }
        writeln!(f, " {{")?;
        let mut indent = indenter::indented(f).with_str("    ");
        for variant in self.variants.iter() {
            writeln!(indent, "{variant},")?;
        }
        write!(f, "}}")
    }
}

impl From<EnumDef> for TokenStream {
    fn from(value: EnumDef) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Enum));
        ts.push(Token::ident(value.ident));
        if !value.generics.is_empty() {
            ts.push(Token::Lt.into_joint());
            for (i, generic) in value.generics.iter().enumerate() {
                if i != 0 {
                    ts.push(Token::Comma);
                }
                ts.extend(TokenStream::from(generic.clone()).into_joint());
            }
            ts.push(Token::Gt);
        }
        ts.push(Token::OpenDelim(Delimiter::Brace));
        for variant in value.variants.iter() {
            ts.extend(TokenStream::from(variant.clone()).into_joint());
            ts.push(Token::Comma);
        }
        ts.push(Token::CloseDelim(Delimiter::Brace));
        ts
    }
}

impl EmptyItem for EnumDef {
    type Input = String;

    fn empty(ident: impl Into<Self::Input>) -> Self {
        Self::empty(ident)
    }
}

impl Ident for EnumDef {
    fn ident(&self) -> &str {
        &self.ident
    }
}

impl HasItem<Variant> for EnumDef {
    fn items(&self) -> &[Variant] {
        &self.variants
    }
    fn items_mut(&mut self) -> &mut Vec<Variant> {
        &mut self.variants
    }
}

impl_hasitem_methods!(EnumDef, Variant);

impl EnumDef {
    pub fn new(
        ident: impl Into<String>,
        generics: Vec<GenericParam>,
        variants: Vec<Variant>,
    ) -> Self {
        Self {
            ident: ident.into(),
            generics,
            variants,
        }
    }

    pub fn empty(ident: impl Into<String>) -> Self {
        Self::new(ident, Vec::new(), Vec::new())
    }

    pub fn with_variant(mut self, item: Variant) -> Self {
        self.add_variant(item);
        self
    }

    pub fn add_variant(&mut self, item: Variant) -> ItemIndex {
        self.variants.push(item);
        ItemIndex(self.variants.len() - 1)
    }

    pub fn try_remove_variant(&mut self, index: usize) -> Option<Variant> {
        self.variants.get(index)?;
        Some(self.variants.remove(index))
    }

    pub fn remove_variant(&mut self, index: ItemIndex) -> Variant {
        self.variants.remove(index.0)
    }

    pub fn try_remove_variant_by_id(&mut self, ident: &str) -> Option<Variant> {
        let index = self.variants.iter().position(|va| va.ident == ident)?;
        self.try_remove_variant(index)
    }

    pub fn get_variant(&self, index: usize) -> Option<&Variant> {
        self.variants.get(index)
    }

    pub fn get_variant_by_id(&self, ident: &str) -> Option<&Variant> {
        self.variants.iter().find(|va| va.ident == ident)
    }

    pub fn with_generic_param(mut self, param: GenericParam) -> Self {
        self.add_generic_param(param);
        self
    }

    pub fn add_generic_param(&mut self, param: GenericParam) {
        self.generics.push(param);
    }
}

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructDef {
    pub ident: String,
    pub generics: Vec<GenericParam>,
    pub fields: Fields,
}

impl_into_item!(StructDef);

impl fmt::Display for StructDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "struct {}", self.ident)?;
        if !self.generics.is_empty() {
            write!(f, "<")?;
            for (i, generic) in self.generics.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{generic}")?;
            }
            write!(f, ">")?;
        }
        write!(f, "{}", self.fields)
    }
}

impl From<StructDef> for TokenStream {
    fn from(value: StructDef) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Struct));
        ts.push(Token::ident(value.ident));
        if !value.generics.is_empty() {
            ts.push(Token::Lt);
            for (i, generic) in value.generics.iter().enumerate() {
                if i != 0 {
                    ts.push(Token::Comma);
                }
                ts.extend(TokenStream::from(generic.clone()));
            }
            ts.push(Token::Gt);
        }
        ts.extend(TokenStream::from(value.fields));
        ts
    }
}

impl EmptyItem for StructDef {
    type Input = String;

    fn empty(ident: impl Into<Self::Input>) -> Self {
        Self::empty(ident)
    }
}

impl Ident for StructDef {
    fn ident(&self) -> &str {
        &self.ident
    }
}

impl HasItem<FieldDef> for StructDef {
    fn items(&self) -> &[FieldDef] {
        self.fields.items()
    }
    fn items_mut(&mut self) -> &mut Vec<FieldDef> {
        self.fields.items_mut()
    }
}

impl StructDef {
    pub fn new(ident: impl Into<String>, generics: Vec<GenericParam>, fields: Fields) -> Self {
        Self {
            ident: ident.into(),
            generics,
            fields,
        }
    }

    pub fn empty(ident: impl Into<String>) -> Self {
        Self::new(ident, Vec::new(), Fields::Unit)
    }

    pub fn with_field(mut self, field: FieldDef) -> Self {
        self.add_field(field);
        self
    }

    pub fn add_field(&mut self, field: FieldDef) -> ItemIndex {
        self.fields.add_field(field);
        ItemIndex(self.fields.items().len() - 1)
    }

    pub fn try_remove_field(&mut self, index: usize) -> Option<FieldDef> {
        self.fields.try_remove_field(index)
    }

    pub fn remove_field(&mut self, index: ItemIndex) -> FieldDef {
        self.fields.remove_field(index)
    }

    pub fn try_remove_field_by_id(&mut self, ident: &str) -> Option<FieldDef> {
        self.fields.try_remove_field_by_id(ident)
    }

    pub fn get_field_by_id(&self, ident: &str) -> Option<&FieldDef> {
        self.fields.get_field_by_id(ident)
    }

    pub fn with_generic_param(mut self, param: GenericParam) -> Self {
        self.add_generic_param(param);
        self
    }

    pub fn add_generic_param(&mut self, param: GenericParam) {
        self.generics.push(param);
    }
}

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnionDef {
    pub ident: String,
    pub generics: Vec<GenericParam>,
    pub fields: Fields,
}

impl_into_item!(UnionDef);

impl fmt::Display for UnionDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "union {}", self.ident)?;
        if !self.generics.is_empty() {
            write!(f, "<")?;
            for (i, generic) in self.generics.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{generic}")?;
            }
            write!(f, ">")?;
        }
        write!(f, "{}", self.fields)
    }
}

impl From<UnionDef> for TokenStream {
    fn from(value: UnionDef) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::ident("union"));
        ts.push(Token::ident(value.ident));
        if !value.generics.is_empty() {
            ts.push(Token::Lt);
            for (i, generic) in value.generics.iter().enumerate() {
                if i != 0 {
                    ts.push(Token::Comma);
                }
                ts.extend(TokenStream::from(generic.clone()));
            }
            ts.push(Token::Gt);
        }
        ts.extend(TokenStream::from(value.fields));
        ts
    }
}

impl EmptyItem for UnionDef {
    type Input = String;

    fn empty(ident: impl Into<Self::Input>) -> Self {
        Self::empty(ident)
    }
}

impl Ident for UnionDef {
    fn ident(&self) -> &str {
        &self.ident
    }
}

impl HasItem<FieldDef> for UnionDef {
    fn items(&self) -> &[FieldDef] {
        self.fields.items()
    }
    fn items_mut(&mut self) -> &mut Vec<FieldDef> {
        self.fields.items_mut()
    }
}

impl_hasitem_methods!(UnionDef, FieldDef);

impl UnionDef {
    pub fn new(ident: impl Into<String>, generics: Vec<GenericParam>, fields: Fields) -> Self {
        Self {
            ident: ident.into(),
            generics,
            fields,
        }
    }

    pub fn empty(ident: impl Into<String>) -> Self {
        Self::new(ident, Vec::new(), Fields::Unit)
    }

    pub fn with_field(mut self, field: FieldDef) -> Self {
        self.add_field(field);
        self
    }

    pub fn add_field(&mut self, field: FieldDef) -> ItemIndex {
        self.fields.add_field(field);
        ItemIndex(self.fields.items().len() - 1)
    }

    pub fn try_remove_field(&mut self, index: usize) -> Option<FieldDef> {
        self.fields.try_remove_field(index)
    }

    pub fn remove_field(&mut self, index: ItemIndex) -> FieldDef {
        self.fields.remove_field(index)
    }

    pub fn try_remove_field_by_id(&mut self, ident: &str) -> Option<FieldDef> {
        self.fields.try_remove_field_by_id(ident)
    }

    pub fn get_field_by_id(&self, ident: &str) -> Option<&FieldDef> {
        self.fields.get_field_by_id(ident)
    }

    pub fn with_generic_param(mut self, param: GenericParam) -> Self {
        self.add_generic_param(param);
        self
    }

    pub fn add_generic_param(&mut self, param: GenericParam) {
        self.generics.push(param);
    }
}

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitDef {
    pub ident: String,
    pub generics: Vec<GenericParam>,
    pub supertraits: Vec<Type>,
    pub items: Vec<AssocItem>,
}

impl_into_item!(TraitDef);

impl fmt::Display for TraitDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "trait {}", self.ident)?;
        if !self.generics.is_empty() {
            write!(f, "<")?;
            for (i, generic) in self.generics.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{generic}")?;
            }
            write!(f, ">")?;
        }
        if !self.supertraits.is_empty() {
            write!(f, ": ")?;
            for (i, sup) in self.supertraits.iter().enumerate() {
                if i != 0 {
                    write!(f, " + ")?;
                }
                write!(f, "{sup}")?;
            }
        }
        write!(f, " {{")?;
        let mut indent = indenter::indented(f).with_str("    ");
        for item in self.items.iter() {
            writeln!(indent, "{item}")?;
        }
        write!(f, "}}")
    }
}

impl From<TraitDef> for TokenStream {
    fn from(value: TraitDef) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Trait));
        ts.push(Token::ident(value.ident));
        if !value.generics.is_empty() {
            ts.push(Token::Lt.into_joint());
            for (i, generic) in value.generics.iter().enumerate() {
                if i != 0 {
                    ts.push(Token::Comma);
                }
                ts.extend(TokenStream::from(generic.clone()).into_joint());
            }
            ts.push(Token::Gt);
        }
        if !value.supertraits.is_empty() {
            ts.push(Token::Colon);
            for (i, sup) in value.supertraits.iter().enumerate() {
                if i != 0 {
                    ts.push(Token::BinOp(BinOpToken::Plus));
                }
                ts.extend(TokenStream::from(sup.clone()));
            }
        }
        ts.push(Token::OpenDelim(Delimiter::Brace));
        for item in value.items.iter() {
            ts.extend(TokenStream::from(item.clone()));
            // ts.push(Token::Semi);
        }
        ts.push(Token::CloseDelim(Delimiter::Brace));
        ts
    }
}

impl EmptyItem for TraitDef {
    type Input = String;

    fn empty(ident: impl Into<Self::Input>) -> Self {
        Self::empty(ident)
    }
}

impl Ident for TraitDef {
    fn ident(&self) -> &str {
        &self.ident
    }
}

impl HasItem<AssocItem> for TraitDef {
    fn items(&self) -> &[AssocItem] {
        &self.items
    }
    fn items_mut(&mut self) -> &mut Vec<AssocItem> {
        &mut self.items
    }
}

impl_hasitem_methods!(TraitDef, AssocItem);

impl TraitDef {
    pub fn new(
        ident: impl Into<String>,
        generics: Vec<GenericParam>,
        supertraits: Vec<Type>,
        items: Vec<AssocItem>,
    ) -> Self {
        Self {
            ident: ident.into(),
            generics,
            supertraits,
            items,
        }
    }

    pub fn simple(ident: impl Into<String>, items: Vec<AssocItem>) -> Self {
        Self::new(ident, Vec::new(), Vec::new(), items)
    }

    pub fn empty(ident: impl Into<String>) -> Self {
        Self::new(ident, Vec::new(), Vec::new(), Vec::new())
    }

    pub fn add_supertrait(&mut self, ty: impl Into<Type>) {
        self.supertraits.push(ty.into());
    }

    pub fn with_supertrait(mut self, ty: impl Into<Type>) -> Self {
        self.add_supertrait(ty);
        self
    }

    pub fn add_generic_param(&mut self, param: GenericParam) {
        self.generics.push(param);
    }

    pub fn with_generic_param(mut self, param: GenericParam) -> Self {
        self.add_generic_param(param);
        self
    }
}

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PredicateType {
    pub bounded_ty: Type,
    pub bounds: Vec<Type>,
}

impl fmt::Display for PredicateType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{bounded_ty}: ", bounded_ty = self.bounded_ty)?;
        for (i, bound) in self.bounds.iter().enumerate() {
            if i != 0 {
                write!(f, " + ")?;
            }
            write!(f, "{bound}")?;
        }
        Ok(())
    }
}

impl From<PredicateType> for TokenStream {
    fn from(value: PredicateType) -> Self {
        let mut ts = TokenStream::new();
        ts.extend(TokenStream::from(value.bounded_ty).into_joint());
        ts.push(Token::Colon);
        for (i, bound) in value.bounds.iter().enumerate() {
            if i != 0 {
                ts.push(Token::BinOp(BinOpToken::Plus));
            }
            ts.extend(TokenStream::from(bound.clone()));
        }
        ts
    }
}

impl PredicateType {
    pub fn new(bounded_ty: impl Into<Type>, bounds: Vec<Type>) -> Self {
        Self {
            bounded_ty: bounded_ty.into(),
            bounds,
        }
    }

    pub fn add_bound(&mut self, bound: impl Into<Type>) {
        self.bounds.push(bound.into());
    }

    pub fn with_bound(mut self, bound: impl Into<Type>) -> Self {
        self.add_bound(bound);
        self
    }
}

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PredicateLifetime {
    pub lifetime: String,
    pub bounds: Vec<String>,
}

impl fmt::Display for PredicateLifetime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "'{}: ", self.lifetime)?;
        for (i, bound) in self.bounds.iter().enumerate() {
            if i != 0 {
                write!(f, " + ")?;
            }
            write!(f, "'{bound}")?;
        }
        Ok(())
    }
}

impl From<PredicateLifetime> for TokenStream {
    fn from(value: PredicateLifetime) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::lifetime(value.lifetime).into_joint());
        ts.push(Token::Colon);
        for (i, bound) in value.bounds.iter().enumerate() {
            if i != 0 {
                ts.push(Token::BinOp(BinOpToken::Plus));
            }
            ts.push(Token::lifetime(bound));
        }
        ts
    }
}

impl PredicateLifetime {
    pub fn new(lifetime: impl Into<String>, bounds: Vec<String>) -> Self {
        Self {
            lifetime: lifetime.into(),
            bounds: bounds.into_iter().collect(),
        }
    }

    pub fn add_bound(&mut self, bound: impl Into<String>) {
        self.bounds.push(bound.into());
    }

    pub fn with_bound(mut self, bound: impl Into<String>) -> Self {
        self.add_bound(bound);
        self
    }
}

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum WherePredicate {
    Type(PredicateType),
    Lifetime(PredicateLifetime),
}

impl fmt::Display for WherePredicate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Type(pred) => write!(f, "{pred}"),
            Self::Lifetime(pred) => write!(f, "{pred}"),
        }
    }
}

impl From<WherePredicate> for TokenStream {
    fn from(value: WherePredicate) -> Self {
        match value {
            WherePredicate::Type(pred) => TokenStream::from(pred),
            WherePredicate::Lifetime(pred) => TokenStream::from(pred),
        }
    }
}

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Impl {
    pub generics: Vec<GenericParam>,
    pub of_trait: Option<Type>,
    pub self_ty: Type,
    pub where_clauses: Option<Vec<WherePredicate>>,
    pub items: Vec<AssocItem>,
}

impl fmt::Display for RefWithInnerAttrs<'_, '_, Impl> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "impl")?;
        if !self.generics.is_empty() {
            write!(f, "<")?;
            for (i, generic) in self.generics.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{generic}")?;
            }
            write!(f, ">")?;
        }
        write!(f, " ")?;
        if let Some(of_trait) = &self.of_trait {
            write!(f, "{of_trait} for ")?;
        }
        write!(f, "{self_ty} ", self_ty = self.self_ty)?;
        if let Some(where_clauses) = &self.where_clauses {
            write!(f, "where ")?;
            for (i, clause) in where_clauses.iter().enumerate() {
                if i != 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{clause}")?;
            }
        }
        writeln!(f, "{{")?;
        let mut indent = indenter::indented(f).with_str("    ");
        for inner in &self.inner {
            writeln!(indent, "{inner}")?;
        }
        for item in self.items.iter() {
            writeln!(indent, "{item}")?;
        }
        write!(f, "}}")
    }
}

impl fmt::Display for Impl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        RefWithInnerAttrs {
            inner: vec![],
            item: self,
        }
        .fmt(f)
    }
}

impl From<WithInnerAttrs<Impl>> for TokenStream {
    fn from(value: WithInnerAttrs<Impl>) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Impl));
        if !value.item.generics.is_empty() {
            ts.push(Token::Lt.into_joint());
            for (i, generic) in value.item.generics.iter().enumerate() {
                if i != 0 {
                    ts.push(Token::Comma);
                }
                ts.extend(TokenStream::from(generic.clone()).into_joint());
            }
            ts.push(Token::Gt);
        }
        if let Some(of_trait) = value.item.of_trait {
            ts.extend(TokenStream::from(of_trait));
            ts.push(Token::Keyword(KeywordToken::For));
        }
        ts.extend(TokenStream::from(value.item.self_ty));
        if let Some(where_clauses) = value.item.where_clauses {
            ts.push(Token::Keyword(KeywordToken::Where));
            for (i, clause) in where_clauses.into_iter().enumerate() {
                if i != 0 {
                    ts.push(Token::Comma);
                }
                ts.extend(TokenStream::from(clause).into_joint());
            }
        }
        ts.push(Token::OpenDelim(Delimiter::Brace));
        for inner in value.inner.into_iter() {
            ts.extend(TokenStream::from(inner));
        }
        for item in value.item.items.into_iter() {
            ts.extend(TokenStream::from(item));
            // ts.push(Token::Semi);
        }
        ts.push(Token::CloseDelim(Delimiter::Brace));
        ts
    }
}

impl From<Impl> for TokenStream {
    fn from(value: Impl) -> Self {
        TokenStream::from(WithInnerAttrs {
            inner: vec![],
            item: value,
        })
    }
}

impl EmptyItem for Impl {
    type Input = Type;

    fn empty(self_ty: impl Into<Self::Input>) -> Self {
        Self::new(vec![], None, self_ty.into(), None, vec![])
    }
}

impl HasItem<AssocItem> for Impl {
    fn items(&self) -> &[AssocItem] {
        &self.items
    }
    fn items_mut(&mut self) -> &mut Vec<AssocItem> {
        &mut self.items
    }
}

impl_hasitem_methods!(Impl, AssocItem);

impl Impl {
    pub fn new(
        generics: Vec<GenericParam>,
        of_trait: Option<Type>,
        self_ty: Type,
        where_clauses: Option<Vec<WherePredicate>>,
        items: Vec<AssocItem>,
    ) -> Self {
        Self {
            generics,
            of_trait,
            self_ty,
            where_clauses,
            items,
        }
    }

    pub fn trait_impl(
        generics: Vec<GenericParam>,
        self_ty: Type,
        of_trait: Type,
        where_clauses: Option<Vec<WherePredicate>>,
        items: Vec<AssocItem>,
    ) -> Self {
        Self {
            generics,
            of_trait: Some(of_trait),
            self_ty,
            where_clauses,
            items,
        }
    }

    pub fn simple(self_ty: Type, items: Vec<AssocItem>) -> Self {
        Self {
            generics: vec![],
            of_trait: None,
            self_ty,
            where_clauses: None,
            items,
        }
    }

    pub fn add_generic_param(&mut self, param: GenericParam) {
        self.generics.push(param);
    }

    pub fn with_generic_param(mut self, param: GenericParam) -> Self {
        self.add_generic_param(param);
        self
    }

    pub fn add_where_clause(&mut self, clause: WherePredicate) {
        if let Some(clauses) = &mut self.where_clauses {
            clauses.push(clause);
        } else {
            self.where_clauses = Some(vec![clause]);
        }
    }

    pub fn with_where_clause(mut self, clause: WherePredicate) -> Self {
        self.add_where_clause(clause);
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MacroDef {
    pub ident: String,
    pub args: DelimArgs,
}

#[cfg(feature = "fuzzing")]
impl<'a> arbitrary::Arbitrary<'a> for MacroDef {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        // ts: () => {}
        let mut ts = TokenStream::new();
        ts.push(Token::OpenDelim(Delimiter::Parenthesis));
        ts.push(Token::CloseDelim(Delimiter::Parenthesis));
        ts.push(Token::FatArrow);
        ts.push(Token::OpenDelim(Delimiter::Brace));
        ts.push(Token::CloseDelim(Delimiter::Brace));
        Ok(MacroDef {
            ident: String::arbitrary(u)?,
            args: DelimArgs::brace(ts),
        })
    }
}

impl fmt::Display for MacroDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "macro_rules! {} {}", self.ident, self.args)
    }
}

impl From<MacroDef> for TokenStream {
    fn from(value: MacroDef) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::ident("macro_rules").into_joint());
        ts.push(Token::Not);
        ts.push(Token::ident(value.ident).into_joint());
        ts.extend(TokenStream::from(value.args));
        ts
    }
}

impl EmptyItem for MacroDef {
    type Input = String;

    fn empty(ident: impl Into<Self::Input>) -> Self {
        Self::empty(ident)
    }
}

impl Ident for MacroDef {
    fn ident(&self) -> &str {
        &self.ident
    }
}

impl MacroDef {
    pub fn new(ident: impl Into<String>, args: DelimArgs) -> Self {
        Self {
            ident: ident.into(),
            args,
        }
    }

    pub fn empty(ident: impl Into<String>) -> Self {
        Self::new(ident, DelimArgs::default())
    }
}

/// `'extern' 'unsafe'? "abi"? { ... }`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExternBlock {
    pub is_unsafe: bool,
    pub abi: Option<String>,
    pub block: Block,
}

#[cfg(feature = "fuzzing")]
impl<'a> arbitrary::Arbitrary<'a> for ExternBlock {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        Ok(ExternBlock {
            is_unsafe: bool::arbitrary(u)?,
            abi: Option::<String>::arbitrary(u)?,
            block: Block::arbitrary_extern_item_block(u)?,
        })
    }
}

impl fmt::Display for ExternBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_unsafe {
            write!(f, "unsafe ")?;
        }
        write!(f, "extern ")?;
        if self.abi.is_some() {
            write!(f, "\"{}\" ", self.abi.as_ref().unwrap())?;
        }
        write!(f, "{}", self.block)
    }
}

impl From<ExternBlock> for TokenStream {
    fn from(value: ExternBlock) -> Self {
        let mut ts = TokenStream::new();
        if value.is_unsafe {
            ts.push(Token::Keyword(KeywordToken::Unsafe));
        }
        ts.push(Token::Keyword(KeywordToken::Extern));
        if value.abi.is_some() {
            ts.push(Token::Lit(Lit::str(value.abi.unwrap())));
        }
        ts.extend(TokenStream::from(value.block));
        ts
    }
}

impl EmptyItem for ExternBlock {
    type Input = Option<String>;

    fn empty(abi: impl Into<Self::Input>) -> Self {
        Self::safe(abi.into(), Block::empty())
    }
}

impl ExternBlock {
    pub fn new(is_unsafe: bool, abi: Option<impl Into<String>>, block: Block) -> ExternBlock {
        ExternBlock {
            is_unsafe,
            abi: abi.map(|a| a.into()),
            block,
        }
    }

    pub fn safe(abi: Option<impl Into<String>>, block: Block) -> ExternBlock {
        ExternBlock::new(false, abi, block)
    }

    pub fn unsafe_(abi: Option<impl Into<String>>, block: Block) -> ExternBlock {
        ExternBlock::new(true, abi, block)
    }

    pub fn unsafe_c(block: Block) -> ExternBlock {
        ExternBlock::unsafe_(Some("C"), block)
    }

    pub fn unsafe_cdecl(block: Block) -> ExternBlock {
        ExternBlock::unsafe_(Some("cdecl"), block)
    }

    pub fn unsafe_rust(block: Block) -> ExternBlock {
        ExternBlock::unsafe_(Some("Rust"), block)
    }

    pub fn unsafe_stdcall(block: Block) -> ExternBlock {
        ExternBlock::unsafe_(Some("stdcall"), block)
    }

    pub fn unsafe_system(block: Block) -> ExternBlock {
        ExternBlock::unsafe_(Some("system"), block)
    }

    pub fn unsafe_win64(block: Block) -> ExternBlock {
        ExternBlock::unsafe_(Some("win64"), block)
    }

    pub fn unsafe_sysv64(block: Block) -> ExternBlock {
        ExternBlock::unsafe_(Some("sysv64"), block)
    }

    pub fn unsafe_aapcs(block: Block) -> ExternBlock {
        ExternBlock::unsafe_(Some("aapcs"), block)
    }

    pub fn unsafe_thiscall(block: Block) -> ExternBlock {
        ExternBlock::unsafe_(Some("thiscall"), block)
    }

    pub fn unsafe_fastcall(block: Block) -> ExternBlock {
        ExternBlock::unsafe_(Some("fastcall"), block)
    }

    pub fn unsafe_vectorcall(block: Block) -> ExternBlock {
        ExternBlock::unsafe_(Some("vectorcall"), block)
    }
}

/// `'extern' 'crate' ident ('as' alias)?;`
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExternCrate {
    pub ident: String,
    pub alias: Option<String>,
}

impl fmt::Display for ExternCrate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "extern crate {}", self.ident)?;
        if let Some(alias) = &self.alias {
            write!(f, " as {alias}")?;
        }
        write!(f, ";")?;
        Ok(())
    }
}

impl From<ExternCrate> for TokenStream {
    fn from(value: ExternCrate) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Extern));
        ts.push(Token::Keyword(KeywordToken::Crate));

        if let Some(alias) = value.alias {
            ts.push(Token::ident(value.ident));
            ts.push(Token::Keyword(KeywordToken::As));
            ts.push(Token::ident(alias).into_joint());
        } else {
            ts.push(Token::ident(value.ident).into_joint());
        }
        ts.push(Token::Semi);
        ts
    }
}

impl ExternCrate {
    pub fn new(ident: impl Into<String>, alias: Option<impl Into<String>>) -> Self {
        Self {
            ident: ident.into(),
            alias: alias.map(|a| a.into()),
        }
    }

    pub fn simple(ident: impl Into<String>) -> Self {
        Self::new(ident, Option::<String>::None)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VisibilityScope {
    /// pub(crate)
    Crate,
    /// pub(super)
    Super,
    /// pub(self)
    Self_,
    /// pub(in path)
    Path(Path),
}

#[cfg(feature = "fuzzing")]
impl<'a> arbitrary::Arbitrary<'a> for VisibilityScope {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        match u.int_in_range(0..=3)? {
            0 => Ok(VisibilityScope::Crate),
            1 => Ok(VisibilityScope::Super),
            2 => Ok(VisibilityScope::Self_),
            3 => Ok(VisibilityScope::Path(Path::arbitrary_no_arg(u)?)),
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for VisibilityScope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Crate => write!(f, "crate"),
            Self::Super => write!(f, "super"),
            Self::Self_ => write!(f, "self"),
            Self::Path(path) => write!(f, "in {path}"),
        }
    }
}

impl From<VisibilityScope> for TokenStream {
    fn from(value: VisibilityScope) -> Self {
        match value {
            VisibilityScope::Crate => TokenStream::from(vec![Token::Keyword(KeywordToken::Crate)]),
            VisibilityScope::Super => TokenStream::from(vec![Token::Keyword(KeywordToken::Super)]),
            VisibilityScope::Self_ => TokenStream::from(vec![Token::Keyword(KeywordToken::Self_)]),
            VisibilityScope::Path(path) => {
                let mut ts = TokenStream::new();
                ts.push(Token::Keyword(KeywordToken::In));
                ts.extend(TokenStream::from(path));
                ts
            }
        }
    }
}

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub enum Visibility {
    #[default]
    Inherited,
    Public,
    Scoped(VisibilityScope),
}

impl Visibility {
    /// Creates a new `Visibility` with `pub(crate)` scope
    pub fn crate_() -> Self {
        Self::Scoped(VisibilityScope::Crate)
    }

    /// Creates a new `Visibility` with `pub(super)` scope
    pub fn super_() -> Self {
        Self::Scoped(VisibilityScope::Super)
    }

    /// Creates a new `Visibility` with `pub(self)` scope
    pub fn self_() -> Self {
        Self::Scoped(VisibilityScope::Self_)
    }

    /// Creates a new `Visibility` with `pub(in path)` scope
    pub fn in_path(path: impl Into<Path>) -> Self {
        Self::Scoped(VisibilityScope::Path(path.into()))
    }
}

impl fmt::Display for Visibility {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Inherited => write!(f, ""),
            Self::Public => write!(f, "pub "),
            Self::Scoped(scope) => write!(f, "pub({scope}) "),
        }
    }
}

impl From<Visibility> for TokenStream {
    fn from(value: Visibility) -> Self {
        match value {
            Visibility::Inherited => TokenStream::new(),
            Visibility::Public => TokenStream::from(vec![Token::Keyword(KeywordToken::Pub)]),
            Visibility::Scoped(scope) => {
                let mut ts = TokenStream::new();
                ts.push(Token::Keyword(KeywordToken::Pub).into_joint());
                ts.push(Token::OpenDelim(Delimiter::Parenthesis).into_joint());
                ts.extend(TokenStream::from(scope).into_joint());
                ts.push(Token::CloseDelim(Delimiter::Parenthesis));
                ts
            }
        }
    }
}

pub trait HasVisibility {
    fn has_visibility(&self) -> bool;
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Item<K = ItemKind> {
    pub attrs: Vec<Attribute>,
    pub vis: Visibility,
    pub kind: K,
}

pub type ModuleItem = Item<ItemKind>;
pub type ScopeItem = Item<ItemKind>;

#[cfg(feature = "fuzzing")]
impl<'a, K: arbitrary::Arbitrary<'a> + HasVisibility> arbitrary::Arbitrary<'a> for Item<K> {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        let kind = K::arbitrary(u)?;
        let vis = if kind.has_visibility() {
            Visibility::arbitrary(u)?
        } else {
            Visibility::Inherited
        };
        Ok(Self {
            attrs: Vec::<Attribute>::arbitrary(u)?,
            vis,
            kind,
        })
    }
}

#[cfg(feature = "fuzzing")]
impl Item {
    pub fn arbitrary_extern_item(u: &mut arbitrary::Unstructured<'_>) -> arbitrary::Result<Self> {
        use arbitrary::Arbitrary;

        Ok(Self {
            attrs: Vec::<Attribute>::arbitrary(u)?,
            vis: Visibility::arbitrary(u)?,
            kind: ItemKind::arbitrary_extern_item(u)?,
        })
    }
}

impl<I: Into<ItemKind>> From<I> for Item<ItemKind> {
    fn from(item: I) -> Self {
        Self::inherited(item)
    }
}
impl<I: Into<AssocItemKind>> From<I> for Item<AssocItemKind> {
    fn from(item: I) -> Self {
        Self::inherited(item)
    }
}

impl<K> AddVisibility<K> for Item<K> {
    fn inherited(item: impl Into<K>) -> Self {
        Self::inherited(item)
    }
    fn public(item: impl Into<K>) -> Self {
        Self::public(item)
    }
}

impl fmt::Display for ScopeItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (inner, outer): (Vec<_>, Vec<_>) = self.attrs.iter().partition(|attr| attr.is_inner());
        for attr in outer {
            writeln!(f, "{attr}")?;
        }
        write!(f, "{}", self.vis)?;
        match &self.kind {
            ItemKind::Fn(func) => RefWithInnerAttrs { inner, item: func }.fmt(f),
            ItemKind::Impl(imp) => RefWithInnerAttrs { inner, item: imp }.fmt(f),
            ItemKind::Mod(m) => RefWithInnerAttrs { inner, item: m }.fmt(f),
            _ => self.kind.fmt(f),
        }
    }
}

impl fmt::Display for AssocItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (inner, outer): (Vec<_>, Vec<_>) = self.attrs.iter().partition(|attr| attr.is_inner());
        for attr in outer {
            writeln!(f, "{attr}")?;
        }
        write!(f, "{}", self.vis)?;
        match &self.kind {
            AssocItemKind::Fn(func) => RefWithInnerAttrs { inner, item: func }.fmt(f),
            _ => self.kind.fmt(f),
        }
    }
}

impl From<ScopeItem> for TokenStream {
    fn from(value: ScopeItem) -> Self {
        let mut ts = TokenStream::new();
        let (inner, outer): (Vec<_>, Vec<_>) =
            value.attrs.into_iter().partition(|attr| attr.is_inner());
        for attr in outer {
            ts.extend(TokenStream::from(attr))
        }
        ts.extend(TokenStream::from(value.vis));
        match value.kind {
            ItemKind::Fn(func) => {
                ts.extend(TokenStream::from(WithInnerAttrs { inner, item: func }));
            }
            ItemKind::Impl(imp) => {
                ts.extend(TokenStream::from(WithInnerAttrs { inner, item: imp }));
            }
            ItemKind::Mod(m) => {
                ts.extend(TokenStream::from(WithInnerAttrs { inner, item: m }));
            }
            kind => {
                ts.extend(TokenStream::from(kind));
            }
        }
        ts
    }
}

impl From<AssocItem> for TokenStream {
    fn from(value: Item<AssocItemKind>) -> Self {
        let mut ts = TokenStream::new();
        let (inner, outer): (Vec<_>, Vec<_>) =
            value.attrs.into_iter().partition(|attr| attr.is_inner());
        for attr in outer {
            ts.extend(TokenStream::from(attr))
        }
        ts.extend(TokenStream::from(value.vis));
        match value.kind {
            AssocItemKind::Fn(func) => {
                ts.extend(TokenStream::from(WithInnerAttrs { inner, item: func }));
            }
            kind => {
                ts.extend(TokenStream::from(kind));
            }
        }
        ts
    }
}

impl<K: MaybeIdent> MaybeIdent for Item<K> {
    fn ident(&self) -> Option<&str> {
        self.kind.ident()
    }
}

impl<K> Item<K> {
    pub fn inherited(item: impl Into<K>) -> Self {
        Self {
            attrs: Vec::new(),
            vis: Visibility::Inherited,
            kind: item.into(),
        }
    }

    pub fn public(item: impl Into<K>) -> Self {
        Self {
            attrs: Vec::new(),
            vis: Visibility::Public,
            kind: item.into(),
        }
    }

    pub fn new(vis: Visibility, item: impl Into<K>) -> Self {
        Self {
            attrs: Vec::new(),
            vis,
            kind: item.into(),
        }
    }

    pub fn with_attr(mut self, attr: Attribute) -> Self {
        self.add_attr(attr);
        self
    }

    pub fn add_attr(&mut self, attr: Attribute) {
        self.attrs.push(attr);
    }

    pub fn remove_attr(&mut self, index: usize) -> Attribute {
        self.attrs.remove(index)
    }
}

impl<K: MaybeIdent> Item<K> {
    pub fn ident(&self) -> Option<&str> {
        self.kind.ident()
    }
}

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ItemKind {
    Use(Use),
    StaticItem(StaticItem),
    ConstItem(ConstItem),
    Fn(Fn),
    Mod(Mod),
    TyAlias(TyAlias),
    EnumDef(EnumDef),
    StructDef(StructDef),
    UnionDef(UnionDef),
    TraitDef(TraitDef),
    Impl(Impl),
    MacCallWithSemi(MacCallWithSemi),
    MacroDef(MacroDef),
    ExternBlock(ExternBlock),
    ExternCrate(ExternCrate),
}

impl HasVisibility for ItemKind {
    fn has_visibility(&self) -> bool {
        !matches!(
            self,
            Self::Impl(_) | Self::MacCallWithSemi(_) | Self::MacroDef(_)
        )
    }
}

#[cfg(feature = "fuzzing")]
impl ItemKind {
    pub fn arbitrary_extern_item(u: &mut arbitrary::Unstructured<'_>) -> arbitrary::Result<Self> {
        use arbitrary::Arbitrary;

        match u.int_in_range(0..=2)? {
            0 => Ok(ItemKind::Fn(Fn::arbitrary(u)?)),
            1 => Ok(ItemKind::TyAlias(TyAlias::arbitrary(u)?)),
            2 => Ok(ItemKind::StaticItem(StaticItem::arbitrary(u)?)),
            _ => unreachable!(),
        }
    }
}

impl_obvious_conversion!(ItemKind; Use, StaticItem, ConstItem, Fn, Mod, TyAlias, EnumDef, StructDef, UnionDef, TraitDef, Impl, MacroDef, MacCallWithSemi, ExternBlock, ExternCrate);
impl_display_for_enum!(ItemKind; Use, StaticItem, ConstItem, Fn, Mod, TyAlias, EnumDef, StructDef, UnionDef, TraitDef, Impl, MacroDef, MacCallWithSemi, ExternBlock, ExternCrate);

impl MaybeIdent for ItemKind {
    fn ident(&self) -> Option<&str> {
        match self {
            Self::Use(_) => None,
            Self::StaticItem(item) => Some(&item.ident),
            Self::ConstItem(item) => Some(&item.ident),
            Self::Fn(item) => Some(&item.ident),
            Self::Mod(module) => Some(module.ident()),
            Self::TyAlias(item) => Some(&item.ident),
            Self::EnumDef(item) => Some(&item.ident),
            Self::StructDef(item) => Some(&item.ident),
            Self::UnionDef(item) => Some(&item.ident),
            Self::TraitDef(item) => Some(&item.ident),
            Self::Impl(_) => None,
            Self::MacCallWithSemi(_) => None,
            Self::MacroDef(item) => Some(&item.ident),
            Self::ExternBlock(_) => None,
            Self::ExternCrate(item) => Some(&item.ident),
        }
    }
}

impl ItemKind {
    pub fn ident(&self) -> Option<&str> {
        MaybeIdent::ident(self)
    }
}

/// `ident '::' tree`
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UsePath {
    ident: String,
    tree: Box<UseTree>,
}

impl fmt::Display for UsePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}::{}", self.ident, self.tree)
    }
}

impl From<UsePath> for TokenStream {
    fn from(value: UsePath) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::ident(value.ident).into_joint());
        ts.push(Token::ModSep.into_joint());
        ts.extend(TokenStream::from(*value.tree));
        ts
    }
}

impl From<Path> for UsePath {
    fn from(value: Path) -> Self {
        let mut iter = value.segments.into_iter().rev();
        let ident = iter
            .next()
            .expect("Path must have at least one segment")
            .ident;
        let next = iter.next().expect("Path must have at least two segments");
        iter.fold(
            UsePath::new(next.ident, UseTree::name(ident)),
            |acc, path| UsePath::new(path.ident, UseTree::Path(acc)),
        )
    }
}

impl UsePath {
    pub fn new(ident: impl Into<String>, tree: UseTree) -> Self {
        Self {
            ident: ident.into(),
            tree: Box::new(tree),
        }
    }

    pub fn ident(&self) -> &str {
        &self.ident
    }

    pub fn tree(&self) -> &UseTree {
        &self.tree
    }
}

/// `ident 'as' alias`
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UseRename {
    pub ident: String,
    pub alias: String,
}

impl fmt::Display for UseRename {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} as {}", self.ident, self.alias)
    }
}

impl From<UseRename> for TokenStream {
    fn from(value: UseRename) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::ident(value.ident));
        ts.push(Token::Keyword(KeywordToken::As));
        ts.push(Token::ident(value.alias));
        ts
    }
}

impl UseRename {
    pub fn new(ident: impl Into<String>, alias: impl Into<String>) -> Self {
        Self {
            ident: ident.into(),
            alias: alias.into(),
        }
    }
}

/// `path | use_rename | '*' | '{' (use_tree ',')+ '}'`
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UseTree {
    Name(String),
    Path(UsePath),
    Rename(UseRename),
    Glob,
    Group(Vec<UseTree>),
}

impl fmt::Display for UseTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Name(name) => write!(f, "{name}"),
            Self::Path(path) => path.fmt(f),
            Self::Rename(rename) => rename.fmt(f),
            Self::Glob => write!(f, "*"),
            Self::Group(trees) => {
                write!(f, "{{")?;
                for (i, tree) in trees.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    tree.fmt(f)?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl From<UseTree> for TokenStream {
    fn from(value: UseTree) -> Self {
        match value {
            UseTree::Name(name) => TokenStream::from(vec![Token::ident(name)]),
            UseTree::Path(path) => TokenStream::from(path),
            UseTree::Rename(rename) => TokenStream::from(rename),
            UseTree::Glob => TokenStream::from(vec![Token::BinOp(BinOpToken::Star)]),
            UseTree::Group(trees) => {
                let mut ts = TokenStream::new();
                ts.push(Token::OpenDelim(Delimiter::Brace).into_joint());
                for (i, tree) in trees.iter().enumerate() {
                    if i != 0 {
                        ts.push(Token::Comma);
                    }
                    ts.extend(TokenStream::from(tree.clone()).into_joint());
                }
                ts.push(Token::CloseDelim(Delimiter::Brace));
                ts
            }
        }
    }
}

impl From<Path> for UseTree {
    fn from(value: Path) -> Self {
        let mut iter = value.segments.into_iter().rev();
        let init = UseTree::Name(
            iter.next()
                .expect("Path must have at least one segment")
                .ident,
        );
        iter.fold(init, |acc, segment| {
            UseTree::Path(UsePath::new(segment.ident, acc))
        })
    }
}

impl UseTree {
    pub fn name(path: impl Into<String>) -> Self {
        Self::Name(path.into())
    }

    pub fn path(path: UsePath) -> Self {
        Self::Path(path)
    }

    pub fn rename(rename: UseRename) -> Self {
        Self::Rename(rename)
    }

    pub fn group(trees: Vec<UseTree>) -> Self {
        Self::Group(trees)
    }

    pub fn add_group_element(&mut self, tree: UseTree) {
        if let Self::Group(trees) = self {
            trees.push(tree);
        } else {
            *self = Self::Group(vec![self.clone(), tree]);
        }
    }

    pub fn with_group_element(mut self, tree: UseTree) -> Self {
        self.add_group_element(tree);
        self
    }
}

/// `'use' use_tree;`
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Use(pub UseTree);

impl_into_item!(Use);

impl fmt::Display for Use {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "use {};", self.0)
    }
}

impl From<Use> for TokenStream {
    fn from(value: Use) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Use));
        ts.extend(TokenStream::from(value.0));
        ts
    }
}

impl From<Path> for Use {
    fn from(value: Path) -> Self {
        Self(UseTree::from(value))
    }
}

impl From<UseTree> for Use {
    fn from(tree: UseTree) -> Self {
        Self(tree)
    }
}

impl Use {
    pub fn name(name: impl Into<String>) -> Self {
        Self(UseTree::Name(name.into()))
    }

    pub fn path(path: UsePath) -> Self {
        Self(UseTree::Path(path))
    }

    pub fn tree(tree: UseTree) -> Self {
        Self(tree)
    }

    pub fn rename(rename: UseRename) -> Self {
        Self(UseTree::Rename(rename))
    }

    pub fn group(trees: Vec<UseTree>) -> Self {
        Self(UseTree::Group(trees))
    }
}

/// `'static' ident: ty (= expr)?;`
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StaticItem {
    pub mutability: Mutability,
    pub ident: String,
    pub ty: Type,
    pub expr: Option<Expr>,
}

impl_into_item!(StaticItem);

impl fmt::Display for StaticItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "static {mutability} {ident}: {ty}",
            ident = self.ident,
            mutability = self.mutability,
            ty = self.ty
        )?;
        if let Some(expr) = &self.expr {
            write!(f, " = {expr}")?;
        }
        write!(f, ";")?;
        Ok(())
    }
}

impl From<StaticItem> for TokenStream {
    fn from(value: StaticItem) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Static));
        if value.mutability.is_mut() {
            ts.push(Token::Keyword(KeywordToken::Mut));
        }
        ts.push(Token::ident(value.ident).into_joint());
        ts.push(Token::Colon);
        ts.extend(TokenStream::from(value.ty));
        if let Some(expr) = value.expr {
            ts.push(Token::Eq);
            ts.extend(TokenStream::from(expr));
        }
        ts
    }
}

impl Ident for StaticItem {
    fn ident(&self) -> &str {
        &self.ident
    }
}

/// `'const' ident: ty (= expr)?;`
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstItem {
    pub ident: String,
    pub ty: Type,
    pub expr: Option<Expr>,
}

impl_into_item!(ConstItem);

impl fmt::Display for ConstItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "const {ident}: {ty}", ident = self.ident, ty = self.ty)?;
        if let Some(expr) = &self.expr {
            write!(f, " = {expr}")?;
        }
        write!(f, ";")?;
        Ok(())
    }
}

impl From<ConstItem> for TokenStream {
    fn from(value: ConstItem) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Const));
        ts.push(Token::ident(value.ident).into_joint());
        ts.push(Token::Colon);
        ts.extend(TokenStream::from(value.ty));
        if let Some(expr) = value.expr {
            ts.push(Token::Eq);
            ts.extend(TokenStream::from(expr));
        }
        ts
    }
}

impl Ident for ConstItem {
    fn ident(&self) -> &str {
        &self.ident
    }
}

impl ConstItem {
    pub fn new(ident: impl Into<String>, ty: impl Into<Type>, expr: Option<Expr>) -> Self {
        Self {
            ident: ident.into(),
            ty: ty.into(),
            expr,
        }
    }
}

/// `'type' ident = ty;`
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyAlias {
    pub ident: String,
    pub ty: Option<Type>,
}

impl_into_item!(TyAlias);

impl fmt::Display for TyAlias {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "type {ident}", ident = self.ident)?;
        if let Some(ty) = &self.ty {
            write!(f, " = {ty}")?;
        }
        write!(f, ";")?;
        Ok(())
    }
}

impl From<TyAlias> for TokenStream {
    fn from(value: TyAlias) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Type));
        ts.push(Token::ident(value.ident));
        if let Some(ty) = value.ty {
            ts.push(Token::Eq);
            ts.extend(TokenStream::from(ty));
        }
        ts
    }
}

impl Ident for TyAlias {
    fn ident(&self) -> &str {
        &self.ident
    }
}

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AssocItemKind {
    ConstItem(ConstItem),
    Fn(Fn),
    TyAlias(TyAlias),
    MacCall(MacCall),
}

impl HasVisibility for AssocItemKind {
    fn has_visibility(&self) -> bool {
        !matches!(self, Self::MacCall(_))
    }
}

impl_display_for_enum!(AssocItemKind; ConstItem, Fn, TyAlias, MacCall);
impl_obvious_conversion!(AssocItemKind; ConstItem, Fn, TyAlias, MacCall);

impl MaybeIdent for AssocItemKind {
    fn ident(&self) -> Option<&str> {
        match self {
            Self::ConstItem(item) => Some(&item.ident),
            Self::Fn(item) => Some(&item.ident),
            Self::TyAlias(item) => Some(&item.ident),
            Self::MacCall(_) => None,
        }
    }
}

impl AssocItemKind {
    pub fn ident(&self) -> Option<&str> {
        MaybeIdent::ident(self)
    }
}

pub type AssocItem = Item<AssocItemKind>;

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Empty {}

impl fmt::Display for Empty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "")
    }
}

impl From<Empty> for TokenStream {
    fn from(_: Empty) -> Self {
        TokenStream::new()
    }
}

/// `expr;`
#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Semi<E = Expr>(pub E);

impl<E: fmt::Display> fmt::Display for Semi<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{};", self.0)
    }
}

impl<E: Into<TokenStream>> From<Semi<E>> for TokenStream {
    fn from(value: Semi<E>) -> Self {
        let mut ts = TokenStream::new();
        ts.extend(value.0.into().into_joint());
        ts.push(Token::Semi);
        ts
    }
}

impl Semi {
    pub fn new(expr: impl Into<Expr>) -> Self {
        Self(expr.into())
    }
}

pub type MacCallWithSemi = Semi<MacCall>;

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
    Local(Local),
    Item(Item),
    Expr(Expr),
    Semi(Semi),
    Empty(Empty),
    MacCallWithSemi(MacCallWithSemi),
}

impl_obvious_conversion!(Stmt; Local, Item, Expr, Semi, Empty, MacCallWithSemi);

#[cfg(feature = "fuzzing")]
impl Stmt {
    pub fn arbitrary_not_expr(u: &mut arbitrary::Unstructured<'_>) -> arbitrary::Result<Self> {
        use arbitrary::Arbitrary;

        match u.int_in_range(0..=3)? {
            0 => Ok(Stmt::Local(Local::arbitrary(u)?)),
            1 => Ok(Stmt::Item(Item::arbitrary(u)?)),
            2 => Ok(Stmt::Semi(Semi::arbitrary(u)?)),
            3 => Ok(Stmt::MacCallWithSemi(MacCallWithSemi::arbitrary(u)?)),
            _ => unreachable!(),
        }
    }

    pub fn arbitrary_item(u: &mut arbitrary::Unstructured<'_>) -> arbitrary::Result<Self> {
        use arbitrary::Arbitrary;

        Ok(Stmt::Item(Item::arbitrary(u)?))
    }

    pub fn arbitrary_extern_item(u: &mut arbitrary::Unstructured<'_>) -> arbitrary::Result<Self> {
        Ok(Stmt::Item(Item::arbitrary_extern_item(u)?))
    }
}

impl From<Use> for Stmt {
    fn from(item: Use) -> Self {
        Self::Item(Item::inherited(item))
    }
}
impl From<StaticItem> for Stmt {
    fn from(item: StaticItem) -> Self {
        Self::Item(Item::inherited(item))
    }
}
impl From<ConstItem> for Stmt {
    fn from(item: ConstItem) -> Self {
        Self::Item(Item::inherited(item))
    }
}
impl From<Fn> for Stmt {
    fn from(item: Fn) -> Self {
        Self::Item(Item::inherited(item))
    }
}
impl From<Mod> for Stmt {
    fn from(item: Mod) -> Self {
        Self::Item(Item::inherited(item))
    }
}
impl From<TyAlias> for Stmt {
    fn from(item: TyAlias) -> Self {
        Self::Item(Item::inherited(item))
    }
}
impl From<EnumDef> for Stmt {
    fn from(item: EnumDef) -> Self {
        Self::Item(Item::inherited(item))
    }
}
impl From<StructDef> for Stmt {
    fn from(item: StructDef) -> Self {
        Self::Item(Item::inherited(item))
    }
}
impl From<UnionDef> for Stmt {
    fn from(item: UnionDef) -> Self {
        Self::Item(Item::inherited(item))
    }
}
impl From<TraitDef> for Stmt {
    fn from(item: TraitDef) -> Self {
        Self::Item(Item::inherited(item))
    }
}
impl From<Impl> for Stmt {
    fn from(item: Impl) -> Self {
        Self::Item(Item::inherited(item))
    }
}
impl From<MacroDef> for Stmt {
    fn from(item: MacroDef) -> Self {
        Self::Item(Item::inherited(item))
    }
}
impl From<Call> for Stmt {
    fn from(item: Call) -> Self {
        Self::Expr(item.into())
    }
}
impl From<MethodCall> for Stmt {
    fn from(item: MethodCall) -> Self {
        Self::Expr(item.into())
    }
}
impl From<ForLoop> for Stmt {
    fn from(item: ForLoop) -> Self {
        Self::Expr(item.into())
    }
}
impl From<UnsafeBlock> for Stmt {
    fn from(item: UnsafeBlock) -> Self {
        Self::Expr(item.into())
    }
}
impl From<ExternBlock> for Stmt {
    fn from(item: ExternBlock) -> Self {
        Self::Item(Item::inherited(item))
    }
}
impl From<ExternCrate> for Stmt {
    fn from(item: ExternCrate) -> Self {
        Self::Item(Item::inherited(item))
    }
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Local(local) => write!(f, "{local}"),
            Self::Item(item) => write!(f, "{item}"),
            Self::Expr(expr) => write!(f, "{expr}"),
            Self::Semi(semi) => write!(f, "{semi}"),
            Self::Empty(_) => write!(f, ""),
            Self::MacCallWithSemi(mac_call) => write!(f, "{mac_call}"),
        }
    }
}

impl MaybeIdent for Stmt {
    fn ident(&self) -> Option<&str> {
        match self {
            Self::Item(item) => item.ident(),
            _ => None,
        }
    }
}

impl Stmt {
    pub fn ident(&self) -> Option<&str> {
        MaybeIdent::ident(self)
    }
}
