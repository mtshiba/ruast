use core::fmt::Write;
use std::fmt;
use std::hash::Hash;

use crate::expr::{
    Async, Attribute, Call, ConstBlock, DelimArgs, Expr, GenericArg, MacCall, MethodCall, Path,
    Range, TryBlock, UnsafeBlock,
};
use crate::token::{BinOpToken, Delimiter, KeywordToken, Token, TokenStream};
use crate::ty::Type;
use crate::{
    impl_display_for_enum, impl_hasitem_methods, impl_obvious_conversion, ForLoop, GenericParam,
    Lit,
};

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
    VariantData,
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
    fn add_item(&mut self, item: impl Into<Itm>) {
        self.items_mut().push(item.into());
    }
    fn add_pub_item<K>(&mut self, item: impl Into<K>)
    where
        Itm: AddVisibility<K>,
    {
        let item = Itm::public(item);
        self.items_mut().push(item);
    }
    fn with_pub_item<K>(mut self, item: impl Into<K>) -> Self
    where
        Self: Sized,
        Itm: AddVisibility<K>,
    {
        self.add_pub_item(item);
        self
    }
    fn remove_item(&mut self, index: usize) -> Option<Itm> {
        self.items_mut().get(index)?;
        Some(self.items_mut().remove(index))
    }
    fn remove_item_by_id(&mut self, ident: &str) -> Option<Itm> {
        let index = self
            .items()
            .iter()
            .position(|item| item.ident() == Some(ident))?;
        Some(self.remove_item(index).unwrap())
    }
    fn get_item(&self, index: usize) -> Option<&Itm> {
        self.items().get(index)
    }
    fn get_item_by_id(&self, ident: &str) -> Option<&Itm> {
        self.items().iter().find(|item| item.ident() == Some(ident))
    }
}

pub trait Semicolon {
    fn semi(self) -> Semi;
}

impl<E: Into<Expr>> Semicolon for E {
    fn semi(self) -> Semi {
        Semi::new(self)
    }
}

/// `let pat (:ty)? (= expr)?;`
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
        ts.extend(TokenStream::from(value.pat));
        if let Some(ty) = value.ty {
            ts.push(Token::Colon);
            ts.extend(TokenStream::from(ty));
        }
        ts.extend(TokenStream::from(value.kind));
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
}

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
            Self::InitElse(expr, block) => write!(f, " = {expr} else {block}"),
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
                ts.extend(TokenStream::from(expr));
                ts.push(Token::Keyword(KeywordToken::Else));
                ts.extend(TokenStream::from(block));
                ts
            }
        }
    }
}

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
        ts.push(Token::ident(value.ident));
        ts.push(Token::Colon);
        ts.extend(TokenStream::from(value.pat));
        ts
    }
}

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
}

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
            ts.extend(TokenStream::from(field.clone()));
        }
        ts.push(Token::CloseDelim(Delimiter::Brace));
        ts
    }
}

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
        ts.extend(TokenStream::from(value.path));
        ts.push(Token::OpenDelim(Delimiter::Parenthesis));
        for (i, pat) in value.pats.iter().enumerate() {
            if i != 0 {
                ts.push(Token::Comma);
            }
            ts.extend(TokenStream::from(pat.clone()));
        }
        ts.push(Token::CloseDelim(Delimiter::Parenthesis));
        ts
    }
}

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
        ts.push(Token::BinOp(BinOpToken::And));
        if value.is_mut {
            ts.push(Token::Keyword(KeywordToken::Mut));
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
                    write!(f, "{pat}", pat = pat)?;
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
                    write!(f, "{pat}", pat = pat)?;
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
                    write!(f, "{pat}", pat = pat)?;
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
                ts.push(Token::OpenDelim(Delimiter::Parenthesis));
                for (i, pat) in pats.iter().enumerate() {
                    if i != 0 {
                        ts.push(Token::BinOp(BinOpToken::Or));
                    }
                    ts.extend(TokenStream::from(pat.clone()));
                }
                ts.push(Token::CloseDelim(Delimiter::Parenthesis));
                ts
            }
            Pat::Tuple(pats) => {
                let mut ts = TokenStream::new();
                ts.push(Token::OpenDelim(Delimiter::Parenthesis));
                for (i, pat) in pats.iter().enumerate() {
                    if i != 0 {
                        ts.push(Token::Comma);
                    }
                    ts.extend(TokenStream::from(pat.clone()));
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
}

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
        ts.extend(TokenStream::from(value.pat));
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FnDecl {
    pub inputs: Vec<Param>,
    pub output: Option<Type>,
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
        ts.push(Token::OpenDelim(Delimiter::Parenthesis));
        for (i, param) in value.inputs.iter().enumerate() {
            if i != 0 {
                ts.push(Token::Comma);
            }
            ts.extend(TokenStream::from(param.clone()));
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
    pub fn new(inputs: Vec<Param>, output: Option<Type>) -> Self {
        Self { inputs, output }
    }

    pub fn empty() -> Self {
        Self::new(Vec::new(), None)
    }
}

/// `fn (unsafe)? (const)? (async)? (extern "abi")? ident (<...>)? decl { ... }`
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

impl fmt::Display for Fn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_unsafe {
            write!(f, "unsafe ")?;
        }
        if self.is_const {
            write!(f, "const ")?;
        }
        if self.is_async {
            write!(f, "async ")?;
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
            write!(f, " {body}")?;
        } else {
            write!(f, ";")?;
        }

        Ok(())
    }
}

impl From<Fn> for TokenStream {
    fn from(value: Fn) -> Self {
        let mut ts = TokenStream::new();
        if value.is_unsafe {
            ts.push(Token::Keyword(KeywordToken::Unsafe));
        }
        if value.is_const {
            ts.push(Token::Keyword(KeywordToken::Const));
        }
        if value.is_async {
            ts.push(Token::Keyword(KeywordToken::Async));
        }
        if let Some(abi) = value.abi {
            ts.push(Token::Keyword(KeywordToken::Extern));
            ts.push(Token::lit(format!("\"{abi}\"")));
        }
        ts.push(Token::Keyword(KeywordToken::Fn));
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
        ts.extend(TokenStream::from(value.fn_decl));
        if let Some(body) = value.body {
            ts.extend(TokenStream::from(body));
        }
        ts
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
            abi: Some("C".to_string()),
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
            ident: "main".to_string(),
            generics: Vec::new(),
            fn_decl: FnDecl::new(Vec::new(), output),
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
            fn_decl: FnDecl::new(vec![Param::new(self_pat, Type::ImplicitSelf)], None),
            body: Some(Block::empty()),
        }
    }

    pub fn add_stmt(&mut self, stmt: impl Into<Stmt>) {
        self.body.get_or_insert_with(Block::empty).add_stmt(stmt);
    }

    pub fn add_semi_stmt(&mut self, expr: impl Into<Expr>) {
        self.body
            .get_or_insert_with(Block::empty)
            .add_stmt(Semi::new(expr));
    }

    pub fn with_stmt(mut self, stmt: impl Into<Stmt>) -> Self {
        self.add_stmt(stmt);
        self
    }

    pub fn with_semi_stmt(mut self, expr: impl Into<Expr>) -> Self {
        self.add_semi_stmt(expr);
        self
    }

    pub fn remove_stmt(&mut self, index: usize) -> Option<Stmt> {
        self.body.as_mut()?.remove_stmt(index)
    }
}

/// `mod ident { ... }`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LoadedMod {
    pub ident: String,
    pub items: Vec<Item>,
}

impl fmt::Display for LoadedMod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "mod {} {{", self.ident)?;
        let mut indent = indenter::indented(f).with_str("    ");
        for item in self.items.iter() {
            writeln!(indent, "{item}")?;
        }
        write!(f, "}}")
    }
}

impl From<LoadedMod> for TokenStream {
    fn from(value: LoadedMod) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Mod));
        ts.push(Token::ident(value.ident));
        ts.push(Token::OpenDelim(Delimiter::Brace));
        for item in value.items.iter() {
            ts.extend(TokenStream::from(item.clone()));
        }
        ts.push(Token::CloseDelim(Delimiter::Brace));
        ts
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Mod {
    Loaded(LoadedMod),
    Unloaded(String),
}

impl fmt::Display for Mod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Loaded(module) => write!(f, "{module}"),
            Self::Unloaded(ident) => write!(f, "mod {ident};"),
        }
    }
}

impl From<Mod> for TokenStream {
    fn from(value: Mod) -> Self {
        match value {
            Mod::Loaded(module) => TokenStream::from(module),
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

/// `('label:)? { ... }`
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Block {
    pub label: Option<String>,
    pub stmts: Vec<Stmt>,
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(label) = &self.label {
            write!(f, "'{label}: ")?;
        }
        if self.stmts.is_empty() {
            write!(f, "{{}}")?;
        } else {
            writeln!(f, "{{")?;
            let mut indent = indenter::indented(f).with_str("    ");
            for stmt in self.stmts.iter() {
                writeln!(indent, "{stmt}")?;
            }
            write!(f, "}}")?;
        }
        Ok(())
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
        Self { label: None, stmts }
    }
}

impl From<Block> for TokenStream {
    fn from(value: Block) -> Self {
        let mut ts = TokenStream::new();
        if let Some(label) = value.label {
            ts.push(Token::lifetime(label));
            ts.push(Token::Colon);
        }
        ts.push(Token::OpenDelim(Delimiter::Brace));
        for stmt in value.stmts.iter() {
            ts.extend(TokenStream::from(stmt.clone()));
            // ts.push(Token::Semi);
        }
        ts.push(Token::CloseDelim(Delimiter::Brace));
        ts
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
    pub fn new(stmts: Vec<Stmt>, label: Option<String>) -> Self {
        Self { stmts, label }
    }

    pub fn single(expr: impl Into<Expr>) -> Self {
        Self::new(vec![Stmt::Expr(expr.into())], None)
    }

    pub fn empty() -> Self {
        Self {
            stmts: Vec::new(),
            label: None,
        }
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

    pub fn add_stmt(&mut self, stmt: impl Into<Stmt>) {
        self.stmts.push(stmt.into());
    }

    pub fn remove_stmt(&mut self, index: usize) -> Option<Stmt> {
        self.stmts.get(index)?;
        Some(self.stmts.remove(index))
    }

    pub fn remove_item_by_id(&mut self, ident: &str) -> Option<Item> {
        let index = self
            .stmts
            .iter()
            .position(|stmt| stmt.ident() == Some(ident))?;
        let Some(Stmt::Item(item)) = self.remove_stmt(index) else {
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

/// `vis (ident:)? ty`
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
            ts.push(Token::ident(ident));
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VariantData {
    Unit,
    Tuple(Vec<FieldDef>),
    Struct(Vec<FieldDef>),
}

impl fmt::Display for VariantData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => write!(f, ""),
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

impl From<VariantData> for TokenStream {
    fn from(value: VariantData) -> Self {
        match value {
            VariantData::Unit => TokenStream::new(),
            VariantData::Tuple(fields) => {
                let mut ts = TokenStream::new();
                ts.push(Token::OpenDelim(Delimiter::Parenthesis));
                for (i, field) in fields.iter().enumerate() {
                    if i != 0 {
                        ts.push(Token::Comma);
                    }
                    ts.extend(TokenStream::from(field.clone()));
                }
                ts.push(Token::CloseDelim(Delimiter::Parenthesis));
                ts
            }
            VariantData::Struct(fields) => {
                let mut ts = TokenStream::new();
                ts.push(Token::OpenDelim(Delimiter::Brace));
                for (i, field) in fields.iter().enumerate() {
                    if i != 0 {
                        ts.push(Token::Comma);
                    }
                    ts.extend(TokenStream::from(field.clone()));
                }
                ts.push(Token::CloseDelim(Delimiter::Brace));
                ts
            }
        }
    }
}

impl HasItem<FieldDef> for VariantData {
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

impl_hasitem_methods!(VariantData, FieldDef, Deref);

impl VariantData {
    pub fn with_field(mut self, field: FieldDef) -> Self {
        self.add_field(field);
        self
    }

    pub fn add_field(&mut self, field: FieldDef) {
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
    }

    pub fn remove_field(&mut self, index: usize) -> Option<FieldDef> {
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

    pub fn remove_field_by_id(&mut self, ident: &str) -> Option<FieldDef> {
        let index = match self {
            Self::Unit => return None,
            Self::Tuple(fields) => fields
                .iter()
                .position(|field| field.ident.as_deref() == Some(ident))?,
            Self::Struct(fields) => fields
                .iter()
                .position(|field| field.ident.as_deref() == Some(ident))?,
        };
        Some(self.remove_field(index).unwrap())
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variant {
    pub vis: Visibility,
    pub ident: String,
    pub data: VariantData,
    pub disr_expr: Option<Expr>,
}

impl fmt::Display for Variant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}{}", self.vis, self.ident, self.data)?;
        if let Some(disr_expr) = &self.disr_expr {
            write!(f, " = {disr_expr}")?;
        }
        Ok(())
    }
}

impl From<Variant> for TokenStream {
    fn from(value: Variant) -> Self {
        let mut ts = TokenStream::new();
        ts.extend(TokenStream::from(value.vis));
        ts.push(Token::ident(value.ident));
        ts.extend(TokenStream::from(value.data));
        if let Some(disr_expr) = value.disr_expr {
            ts.push(Token::Eq);
            ts.extend(TokenStream::from(disr_expr));
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
        vis: Visibility,
        ident: impl Into<String>,
        data: VariantData,
        disr_expr: Option<Expr>,
    ) -> Self {
        Self {
            vis,
            ident: ident.into(),
            data,
            disr_expr,
        }
    }

    pub fn empty(ident: impl Into<String>) -> Self {
        Self::new(Visibility::Inherited, ident, VariantData::Unit, None)
    }

    pub fn inherited(ident: impl Into<String>, data: VariantData) -> Self {
        Self::new(Visibility::Inherited, ident, data, None)
    }

    pub fn struct_(ident: impl Into<String>, fields: Vec<FieldDef>) -> Self {
        Self::new(
            Visibility::Inherited,
            ident,
            VariantData::Struct(fields),
            None,
        )
    }

    pub fn tuple(ident: impl Into<String>, fields: Vec<FieldDef>) -> Self {
        Self::new(
            Visibility::Inherited,
            ident,
            VariantData::Tuple(fields),
            None,
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumDef {
    pub ident: String,
    pub generics: Vec<GenericArg>,
    pub variants: Vec<Variant>,
}

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
            ts.push(Token::Lt);
            for (i, generic) in value.generics.iter().enumerate() {
                if i != 0 {
                    ts.push(Token::Comma);
                }
                ts.extend(TokenStream::from(generic.clone()));
            }
            ts.push(Token::Gt);
        }
        ts.push(Token::OpenDelim(Delimiter::Brace));
        for variant in value.variants.iter() {
            ts.extend(TokenStream::from(variant.clone()));
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
        generics: Vec<GenericArg>,
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

    pub fn add_variant(&mut self, item: Variant) {
        self.variants.push(item);
    }

    pub fn remove_variant(&mut self, index: usize) -> Variant {
        self.variants.remove(index)
    }

    pub fn remove_variant_by_id(&mut self, ident: &str) -> Option<Variant> {
        let index = self.variants.iter().position(|va| va.ident == ident)?;
        Some(self.remove_variant(index))
    }

    pub fn get_variant(&self, index: usize) -> Option<&Variant> {
        self.variants.get(index)
    }

    pub fn get_variant_by_id(&self, ident: &str) -> Option<&Variant> {
        self.variants.iter().find(|va| va.ident == ident)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructDef {
    pub ident: String,
    pub generics: Vec<GenericArg>,
    pub variant: VariantData,
}

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
        write!(f, "{}", self.variant)
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
        ts.extend(TokenStream::from(value.variant));
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
        self.variant.items()
    }
    fn items_mut(&mut self) -> &mut Vec<FieldDef> {
        self.variant.items_mut()
    }
}

impl StructDef {
    pub fn new(ident: impl Into<String>, generics: Vec<GenericArg>, variant: VariantData) -> Self {
        Self {
            ident: ident.into(),
            generics,
            variant,
        }
    }

    pub fn empty(ident: impl Into<String>) -> Self {
        Self::new(ident, Vec::new(), VariantData::Unit)
    }

    pub fn with_field(mut self, field: FieldDef) -> Self {
        self.add_field(field);
        self
    }

    pub fn add_field(&mut self, field: FieldDef) {
        self.variant.add_field(field);
    }

    pub fn remove_field(&mut self, index: usize) -> Option<FieldDef> {
        self.variant.remove_field(index)
    }

    pub fn remove_field_by_id(&mut self, ident: &str) -> Option<FieldDef> {
        self.variant.remove_field_by_id(ident)
    }

    pub fn get_field_by_id(&self, ident: &str) -> Option<&FieldDef> {
        self.variant.get_field_by_id(ident)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnionDef {
    pub ident: String,
    pub generics: Vec<GenericArg>,
    pub variants: VariantData,
}

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
        write!(f, "{}", self.variants)
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
        ts.extend(TokenStream::from(value.variants));
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

impl UnionDef {
    pub fn new(ident: impl Into<String>, generics: Vec<GenericArg>, variants: VariantData) -> Self {
        Self {
            ident: ident.into(),
            generics,
            variants,
        }
    }

    pub fn empty(ident: impl Into<String>) -> Self {
        Self::new(ident, Vec::new(), VariantData::Unit)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitDef {
    pub ident: String,
    pub generics: Vec<GenericArg>,
    pub supertraits: Vec<Type>,
    pub items: Vec<AssocItem>,
}

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
            ts.push(Token::Lt);
            for (i, generic) in value.generics.iter().enumerate() {
                if i != 0 {
                    ts.push(Token::Comma);
                }
                ts.extend(TokenStream::from(generic.clone()));
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
        generics: Vec<GenericArg>,
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
}

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
        ts.extend(TokenStream::from(value.bounded_ty));
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
}

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
        ts.push(Token::lifetime(value.lifetime));
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
}

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Impl {
    pub generics: Vec<GenericParam>,
    pub of_trait: Option<Type>,
    pub self_ty: Type,
    pub where_clauses: Option<Vec<WherePredicate>>,
    pub items: Vec<AssocItem>,
}

impl fmt::Display for Impl {
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
        for item in self.items.iter() {
            writeln!(indent, "{item}")?;
        }
        write!(f, "}}")
    }
}

impl From<Impl> for TokenStream {
    fn from(value: Impl) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Impl));
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
        if let Some(of_trait) = value.of_trait {
            ts.extend(TokenStream::from(of_trait));
            ts.push(Token::Keyword(KeywordToken::For));
        }
        ts.extend(TokenStream::from(value.self_ty));
        if let Some(where_clauses) = value.where_clauses {
            ts.push(Token::Keyword(KeywordToken::Where));
            for (i, clause) in where_clauses.iter().enumerate() {
                if i != 0 {
                    ts.push(Token::Comma);
                }
                ts.extend(TokenStream::from(clause.clone()));
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
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MacroDef {
    pub ident: String,
    pub args: DelimArgs,
}

impl fmt::Display for MacroDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "macro_rules! {} {}", self.ident, self.args)
    }
}

impl From<MacroDef> for TokenStream {
    fn from(value: MacroDef) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::ident("macro_rules"));
        ts.push(Token::Not);
        ts.push(Token::ident(value.ident));
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

/// `extern unsafe? "abi"? { ... }`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExternBlock {
    pub is_unsafe: bool,
    pub abi: Option<String>,
    pub block: Block,
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

/// `extern crate ident (as alias)?;`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExternCrate {
    pub ident: String,
    pub alias: Option<String>,
}

impl fmt::Display for ExternCrate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "extern crate {}", self.ident)?;
        if let Some(alias) = &self.alias {
            write!(f, " as {}", alias)?;
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
        ts.push(Token::ident(value.ident));
        if let Some(alias) = value.alias {
            ts.push(Token::Keyword(KeywordToken::As));
            ts.push(Token::ident(alias));
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub enum Visibility {
    #[default]
    Inherited,
    Public,
}

impl fmt::Display for Visibility {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Inherited => write!(f, ""),
            Self::Public => write!(f, "pub "),
        }
    }
}

impl From<Visibility> for TokenStream {
    fn from(value: Visibility) -> Self {
        match value {
            Visibility::Inherited => TokenStream::new(),
            Visibility::Public => TokenStream::from(vec![Token::Keyword(KeywordToken::Pub)]),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Item<K = ItemKind> {
    pub attrs: Vec<Attribute>,
    pub vis: Visibility,
    pub kind: K,
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

impl<K: fmt::Display> fmt::Display for Item<K> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.vis, self.kind)
    }
}

impl<K: Into<TokenStream>> From<Item<K>> for TokenStream {
    fn from(value: Item<K>) -> Self {
        let mut ts = TokenStream::new();
        ts.extend(TokenStream::from(value.vis));
        ts.extend(value.kind.into());
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
    MacCall(MacCall),
    MacroDef(MacroDef),
    ExternBlock(ExternBlock),
    ExternCrate(ExternCrate),
}

impl_obvious_conversion!(ItemKind; Use, StaticItem, ConstItem, Fn, Mod, TyAlias, EnumDef, StructDef, UnionDef, TraitDef, Impl, MacroDef, MacCall, ExternBlock, ExternCrate);
impl_display_for_enum!(ItemKind; Use, StaticItem, ConstItem, Fn, Mod, TyAlias, EnumDef, StructDef, UnionDef, TraitDef, Impl, MacroDef, MacCall, ExternBlock, ExternCrate);

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
            Self::MacCall(_) => None,
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

/// `use path;`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Use(pub Path);

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

impl Use {
    pub fn new(path: impl Into<Path>) -> Self {
        Self(path.into())
    }
}

/// `static ident: ty (= expr)?;`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StaticItem {
    pub ident: String,
    pub ty: Type,
    pub expr: Option<Expr>,
}

impl fmt::Display for StaticItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "static {ident}: {ty}", ident = self.ident, ty = self.ty)?;
        if let Some(expr) = &self.expr {
            write!(f, " = {expr}", expr = expr)?;
        }
        write!(f, ";")?;
        Ok(())
    }
}

impl From<StaticItem> for TokenStream {
    fn from(value: StaticItem) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Static));
        ts.push(Token::ident(value.ident));
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

/// `const ident: ty (= expr)?;`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstItem {
    pub ident: String,
    pub ty: Type,
    pub expr: Option<Expr>,
}

impl fmt::Display for ConstItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "const {ident}: {ty}", ident = self.ident, ty = self.ty)?;
        if let Some(expr) = &self.expr {
            write!(f, " = {expr}", expr = expr)?;
        }
        write!(f, ";")?;
        Ok(())
    }
}

impl From<ConstItem> for TokenStream {
    fn from(value: ConstItem) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Const));
        ts.push(Token::ident(value.ident));
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

/// `type ident = ty;`
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyAlias {
    pub ident: String,
    pub ty: Option<Type>,
}

impl fmt::Display for TyAlias {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "type {ident}", ident = self.ident)?;
        if let Some(ty) = &self.ty {
            write!(f, " = {ty}", ty = ty)?;
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AssocItemKind {
    ConstItem(ConstItem),
    Fn(Fn),
    TyAlias(TyAlias),
    MacCall(MacCall),
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Semi(pub Expr);

impl fmt::Display for Semi {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{};", self.0)
    }
}

impl From<Semi> for TokenStream {
    fn from(value: Semi) -> Self {
        let mut ts = TokenStream::new();
        ts.extend(TokenStream::from(value.0));
        ts.push(Token::Semi);
        ts
    }
}

impl Semi {
    pub fn new(expr: impl Into<Expr>) -> Self {
        Self(expr.into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
    Local(Local),
    Item(Item),
    Expr(Expr),
    Semi(Semi),
    Empty(Empty),
    MacCall(MacCall),
}

impl_obvious_conversion!(Stmt; Local, Item, Expr, Semi, Empty, MacCall);

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
            Self::MacCall(mac_call) => write!(f, "{mac_call}"),
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
