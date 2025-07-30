use std::fmt;

use crate::expr::{Const, GenericArg, Lit, MacCall, Path, PathSegment};
use crate::stmt::Param;
use crate::token::{BinOpToken, Delimiter, KeywordToken, Token, TokenStream};
use crate::{impl_display_for_enum, impl_obvious_conversion, EmptyItem};

#[cfg(feature = "tokenize")]
crate::impl_to_tokens!(
    MutTy,
    Ptr,
    Ref,
    BareFn,
    PolyTraitRef,
    GenericBound,
    TraitObject,
    ImplTrait,
    Type,
);

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

impl MutTy {
    pub fn new(mutable: bool, ty: impl Into<Type>) -> Self {
        Self {
            mutable,
            ty: Box::new(ty.into()),
        }
    }

    pub fn immut(ty: impl Into<Type>) -> Self {
        Self::new(false, ty)
    }

    pub fn mut_(ty: impl Into<Type>) -> Self {
        Self::new(true, ty)
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
        ts.push(Token::And.into_joint());
        if let Some(lifetime) = value.lifetime {
            ts.push(Token::Lifetime(lifetime));
        }
        ts.extend(TokenStream::from(value.ty));
        ts
    }
}

impl Ref {
    pub fn new(lifetime: Option<impl Into<String>>, ty: MutTy) -> Self {
        Self {
            lifetime: lifetime.map(|l| l.into()),
            ty,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PtrKind {
    Const,
    Mut,
}

impl fmt::Display for PtrKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PtrKind::Const => write!(f, "const"),
            PtrKind::Mut => write!(f, "mut"),
        }
    }
}

impl From<PtrKind> for TokenStream {
    fn from(value: PtrKind) -> Self {
        let mut ts = TokenStream::new();
        ts.push(match value {
            PtrKind::Mut => Token::Keyword(KeywordToken::Mut),
            PtrKind::Const => Token::Keyword(KeywordToken::Const),
        });
        ts
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ptr {
    pub ty: Box<Type>,
    pub kind: PtrKind,
}

impl fmt::Display for Ptr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "*{} {}", self.kind, self.ty)
    }
}

impl From<Ptr> for TokenStream {
    fn from(value: Ptr) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::BinOp(BinOpToken::Star).into_joint());
        ts.extend(TokenStream::from(value.kind));
        ts.extend(TokenStream::from(*value.ty));
        ts
    }
}

impl Ptr {
    pub fn new(kind: PtrKind, ty: impl Into<Type>) -> Self {
        Self {
            kind,
            ty: Box::new(ty.into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BareFn {
    pub generic_params: Vec<GenericParam>,
    pub inputs: Vec<Param>,
    pub output: Box<Type>,
    pub is_unsafe: bool,
    pub abi: Option<String>,
}

impl fmt::Display for BareFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_unsafe {
            write!(f, "unsafe ")?;
        }

        if let Some(abi) = &self.abi {
            write!(f, "extern \"{abi}\" ")?;
        }

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

        if value.is_unsafe {
            ts.push(Token::Keyword(KeywordToken::Unsafe));
        }

        if let Some(abi) = value.abi {
            ts.push(Token::Keyword(KeywordToken::Extern));
            ts.push(Token::Lit(Lit::str(abi)));
        }

        ts.push(Token::Keyword(KeywordToken::Fn).into_joint());
        ts.push(Token::OpenDelim(Delimiter::Parenthesis).into_joint());
        for (i, param) in value.inputs.iter().enumerate() {
            if i > 0 {
                ts.push(Token::Comma);
            }
            ts.extend(TokenStream::from(param.clone()).joint_last());
        }
        ts.push(Token::CloseDelim(Delimiter::Parenthesis));
        ts.push(Token::RArrow);
        ts.extend(TokenStream::from(*value.output));
        ts
    }
}

impl BareFn {
    pub fn new(
        generic_params: Vec<GenericParam>,
        inputs: Vec<Param>,
        output: impl Into<Type>,
        abi: Option<String>,
        is_unsafe: bool,
    ) -> Self {
        Self {
            generic_params,
            inputs,
            output: Box::new(output.into()),
            abi,
            is_unsafe,
        }
    }

    pub fn safe(
        generic_params: Vec<GenericParam>,
        inputs: Vec<Param>,
        output: impl Into<Type>,
    ) -> Self {
        BareFn::new(generic_params, inputs, output, None, false)
    }

    pub fn add_input(&mut self, input: Param) {
        self.inputs.push(input);
    }

    pub fn with_input(mut self, input: Param) -> Self {
        self.add_input(input);
        self
    }

    pub fn add_generic_param(&mut self, param: GenericParam) {
        self.generic_params.push(param);
    }

    pub fn with_generic_param(mut self, param: GenericParam) -> Self {
        self.add_generic_param(param);
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeParam {
    pub ident: String,
    pub bounds: Vec<GenericBound>,
}

impl fmt::Display for TypeParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.ident)?;
        if !self.bounds.is_empty() {
            write!(
                f,
                ": {}",
                self.bounds
                    .iter()
                    .map(|b| format!("{b}"))
                    .collect::<Vec<_>>()
                    .join(" + ")
            )?;
        }
        Ok(())
    }
}

impl From<TypeParam> for TokenStream {
    fn from(value: TypeParam) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::ident(value.ident).into_joint());
        if !value.bounds.is_empty() {
            ts.push(Token::Colon);
            for (i, bound) in value.bounds.into_iter().enumerate() {
                if i > 0 {
                    ts.push(Token::BinOp(BinOpToken::Plus));
                }
                ts.extend(TokenStream::from(bound));
            }
        }
        ts
    }
}

impl TypeParam {
    pub fn new(ident: impl Into<String>, bounds: Vec<GenericBound>) -> Self {
        Self {
            ident: ident.into(),
            bounds,
        }
    }

    pub fn simple(ident: impl Into<String>) -> Self {
        Self {
            ident: ident.into(),
            bounds: vec![],
        }
    }

    pub fn add_bound(&mut self, bound: GenericBound) {
        self.bounds.push(bound);
    }

    pub fn with_bound(mut self, bound: GenericBound) -> Self {
        self.add_bound(bound);
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstParam {
    pub ident: String,
    pub ty: Type,
}

impl fmt::Display for ConstParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "const {}: {}", self.ident, self.ty)
    }
}

impl From<ConstParam> for TokenStream {
    fn from(value: ConstParam) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Const));
        ts.push(Token::ident(value.ident).into_joint());
        ts.push(Token::Colon);
        ts.extend(TokenStream::from(value.ty));
        ts
    }
}

impl ConstParam {
    pub fn new(ident: impl Into<String>, ty: Type) -> Self {
        Self {
            ident: ident.into(),
            ty,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GenericParam {
    TypeParam(TypeParam),
    ConstParam(ConstParam),
}
impl_display_for_enum!(GenericParam; TypeParam, ConstParam);
impl_obvious_conversion!(GenericParam; TypeParam, ConstParam);

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
        write!(f, "{}", self.trait_ref)
    }
}

impl From<PolyTraitRef> for TokenStream {
    fn from(value: PolyTraitRef) -> Self {
        TokenStream::from(value.trait_ref)
    }
}

impl PolyTraitRef {
    pub fn new(bound_generic_params: Vec<GenericParam>, trait_ref: impl Into<Path>) -> Self {
        Self {
            bound_generic_params,
            trait_ref: trait_ref.into(),
        }
    }

    pub fn simple(trait_ref: impl Into<Path>) -> Self {
        Self {
            bound_generic_params: vec![],
            trait_ref: trait_ref.into(),
        }
    }

    pub fn add_bound_generic_param(&mut self, param: GenericParam) {
        self.bound_generic_params.push(param);
    }

    pub fn with_bound_generic_param(mut self, param: GenericParam) -> Self {
        self.add_bound_generic_param(param);
        self
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
        write!(
            f,
            "{bounds}",
            bounds = self
                .bounds
                .iter()
                .map(|b| format!("{b}"))
                .collect::<Vec<_>>()
                .join(" + ")
        )
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

impl TraitObject {
    pub fn new(is_dyn: bool, bounds: Vec<GenericBound>) -> Self {
        Self { is_dyn, bounds }
    }

    pub fn dyn_(bounds: Vec<GenericBound>) -> Self {
        Self::new(true, bounds)
    }

    pub fn static_(bounds: Vec<GenericBound>) -> Self {
        Self::new(false, bounds)
    }

    pub fn add_bound(&mut self, bound: GenericBound) {
        self.bounds.push(bound);
    }

    pub fn with_bound(mut self, bound: GenericBound) -> Self {
        self.add_bound(bound);
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImplTrait {
    pub bounds: Vec<GenericBound>,
}

impl fmt::Display for ImplTrait {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "impl {bounds}",
            bounds = self
                .bounds
                .iter()
                .map(|b| format!("{b}"))
                .collect::<Vec<_>>()
                .join(" + ")
        )
    }
}

impl EmptyItem for ImplTrait {
    type Input = ();
    fn empty(_: impl Into<Self::Input>) -> Self {
        Self { bounds: vec![] }
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

impl ImplTrait {
    pub fn new(bounds: Vec<GenericBound>) -> Self {
        Self { bounds }
    }

    pub fn add_bound(&mut self, bound: GenericBound) {
        self.bounds.push(bound);
    }

    pub fn with_bound(mut self, bound: GenericBound) -> Self {
        self.add_bound(bound);
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// `[T]`
    Slice(Box<Type>),
    /// `[T; N]`
    Array(Box<Type>, Box<Const>),
    Ptr(Ptr),
    Ref(Ref),
    BareFn(BareFn),
    Macro(MacCall),
    /// `!`
    Never,
    Tuple(Vec<Type>),
    Path(Path),
    TraitObject(TraitObject),
    ImplTrait(ImplTrait),
    /// `_`
    Infer,
    ImplicitSelf,
    Err,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Slice(ty) => write!(f, "[{ty}]"),
            Self::Array(ty, len) => write!(f, "[{ty}; {len}]"),
            Self::Ref(r) => r.fmt(f),
            Self::Ptr(p) => p.fmt(f),
            Self::BareFn(bare_fn) => bare_fn.fmt(f),
            Self::Macro(mac) => mac.fmt(f),
            Self::Never => write!(f, "!"),
            Self::Tuple(tys) => write!(
                f,
                "({})",
                tys.iter()
                    .map(|ty| format!("{ty}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Path(path) => path.fmt(f),
            Self::TraitObject(trait_object) => trait_object.fmt(f),
            Self::ImplTrait(impl_trait) => impl_trait.fmt(f),
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

impl From<Ptr> for Type {
    fn from(ptr: Ptr) -> Self {
        Self::Ptr(ptr)
    }
}
impl From<Ref> for Type {
    fn from(ref_: Ref) -> Self {
        Self::Ref(ref_)
    }
}
impl From<BareFn> for Type {
    fn from(bare_fn: BareFn) -> Self {
        Self::BareFn(bare_fn)
    }
}
impl From<MacCall> for Type {
    fn from(mac: MacCall) -> Self {
        Self::Macro(mac)
    }
}
impl From<TraitObject> for Type {
    fn from(trait_object: TraitObject) -> Self {
        Self::TraitObject(trait_object)
    }
}
impl From<ImplTrait> for Type {
    fn from(impl_trait: ImplTrait) -> Self {
        Self::ImplTrait(impl_trait)
    }
}

impl From<Type> for TokenStream {
    fn from(value: Type) -> Self {
        match value {
            Type::Slice(ty) => {
                let mut ts = TokenStream::new();
                ts.push(Token::OpenDelim(Delimiter::Bracket).into_joint());
                ts.extend(TokenStream::from(*ty).joint_last());
                ts.push(Token::CloseDelim(Delimiter::Bracket));
                ts
            }
            Type::Array(ty, len) => {
                let mut ts = TokenStream::new();
                ts.push(Token::OpenDelim(Delimiter::Bracket).into_joint());
                ts.extend(TokenStream::from(*ty).joint_last());
                ts.push(Token::Semi);
                ts.extend(TokenStream::from(*len).joint_last());
                ts.push(Token::CloseDelim(Delimiter::Bracket));
                ts
            }
            Type::Ptr(ptr) => TokenStream::from(ptr),
            Type::Ref(ref_) => TokenStream::from(ref_),
            Type::BareFn(bare_fn) => TokenStream::from(bare_fn),
            Type::Macro(mac) => TokenStream::from(mac),
            Type::Never => TokenStream::from(vec![Token::Not]),
            Type::Tuple(tys) => {
                let mut ts = TokenStream::new();
                ts.push(Token::OpenDelim(Delimiter::Parenthesis).into_joint());
                for (i, ty) in tys.into_iter().enumerate() {
                    if i > 0 {
                        ts.push(Token::Comma);
                    }
                    ts.extend(TokenStream::from(ty).joint_last())
                }
                ts.push(Token::CloseDelim(Delimiter::Parenthesis));
                ts
            }
            Type::Path(path) => TokenStream::from(path),
            Type::TraitObject(trait_object) => TokenStream::from(trait_object),
            Type::ImplTrait(impl_trait) => TokenStream::from(impl_trait),
            Type::Infer => TokenStream::from(vec![Token::ident("_")]),
            Type::ImplicitSelf => TokenStream::new(),
            Type::Err => TokenStream::from(vec![Token::ident("<Err>")]),
        }
    }
}

impl Type {
    pub fn unit() -> Type {
        Self::Tuple(vec![])
    }
    pub fn usize() -> Type {
        Self::Path(Path::single("usize"))
    }
    pub fn isize() -> Type {
        Self::Path(Path::single("isize"))
    }
    pub fn bool() -> Type {
        Self::Path(Path::single("bool"))
    }
    pub fn char() -> Type {
        Self::Path(Path::single("char"))
    }
    pub fn str() -> Type {
        Self::Path(Path::single("str"))
    }
    pub fn f32() -> Type {
        Self::Path(Path::single("f32"))
    }
    pub fn f64() -> Type {
        Self::Path(Path::single("f64"))
    }
    pub fn i8() -> Type {
        Self::Path(Path::single("i8"))
    }
    pub fn i16() -> Type {
        Self::Path(Path::single("i16"))
    }
    pub fn i32() -> Type {
        Self::Path(Path::single("i32"))
    }
    pub fn i64() -> Type {
        Self::Path(Path::single("i64"))
    }
    pub fn i128() -> Type {
        Self::Path(Path::single("i128"))
    }
    pub fn u8() -> Type {
        Self::Path(Path::single("u8"))
    }
    pub fn u16() -> Type {
        Self::Path(Path::single("u16"))
    }
    pub fn u32() -> Type {
        Self::Path(Path::single("u32"))
    }
    pub fn u64() -> Type {
        Self::Path(Path::single("u64"))
    }
    pub fn u128() -> Type {
        Self::Path(Path::single("u128"))
    }

    pub fn into_ref(self) -> Type {
        Type::ref_(self)
    }
    pub fn into_ref_mut(self) -> Type {
        Type::ref_mut(self)
    }
    pub fn into_static_ref(self) -> Type {
        Type::static_ref(self)
    }
    pub fn into_mut_ptr(self) -> Type {
        Type::mut_ptr(self)
    }
    pub fn into_const_ptr(self) -> Type {
        Type::const_ptr(self)
    }

    pub fn ref_(ty: impl Into<Type>) -> Type {
        Type::Ref(Ref::new(Option::<String>::None, MutTy::immut(ty)))
    }

    pub fn ref_mut(ty: impl Into<Type>) -> Type {
        Type::Ref(Ref::new(Option::<String>::None, MutTy::mut_(ty)))
    }

    pub fn static_ref(ty: impl Into<Type>) -> Type {
        Type::Ref(Ref::new(Some("static"), MutTy::immut(ty)))
    }

    pub fn simple_path(ident: impl Into<String>) -> Type {
        Type::Path(Path::single(PathSegment::simple(ident)))
    }

    pub fn poly_path(ident: impl Into<String>, args: Vec<GenericArg>) -> Type {
        Type::Path(Path::single(PathSegment::new(ident, Some(args))))
    }

    pub fn const_ptr(ty: impl Into<Type>) -> Type {
        Type::Ptr(Ptr::new(PtrKind::Const, ty))
    }

    pub fn mut_ptr(ty: impl Into<Type>) -> Type {
        Type::Ptr(Ptr::new(PtrKind::Mut, ty))
    }

    pub fn box_(inner: impl Into<Type>) -> Type {
        Type::poly_path("Box", vec![GenericArg::Type(inner.into())])
    }

    pub fn vec(inner: impl Into<Type>) -> Type {
        Type::poly_path("Vec", vec![GenericArg::Type(inner.into())])
    }
}
