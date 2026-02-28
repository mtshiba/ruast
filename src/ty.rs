use std::fmt;

use crate::expr::{Const, Expr, GenericArg, Lit, MacCall, Path, PathSegment};
use crate::token::{BinOpToken, Delimiter, KeywordToken, Token, TokenStream};
use crate::EmptyItem;

#[cfg(feature = "fuzzing")]
use crate::token::String;

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

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
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
        if self.ty.should_wrap() {
            write!(f, "({})", self.ty)
        } else {
            write!(f, "{}", self.ty)
        }
    }
}

impl From<MutTy> for TokenStream {
    fn from(value: MutTy) -> Self {
        let mut ts = TokenStream::new();
        if value.mutable {
            ts.push(Token::Keyword(KeywordToken::Mut));
        }
        if value.ty.should_wrap() {
            ts.push(Token::OpenDelim(Delimiter::Parenthesis).into_joint());
            ts.extend(TokenStream::from(*value.ty).into_joint());
            ts.push(Token::CloseDelim(Delimiter::Parenthesis));
        } else {
            ts.extend(TokenStream::from(*value.ty));
        }
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

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
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

#[cfg(feature = "syn")]
impl From<syn::TypeReference> for Ref {
    fn from(value: syn::TypeReference) -> Self {
        let lifetime = value.lifetime.map(|l| l.ident.to_string().into());
        let mutable = value.mutability.is_some();
        let mut_ty = MutTy::new(mutable, *value.elem);
        Self {
            lifetime,
            ty: mut_ty,
        }
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

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
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

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ptr {
    pub ty: Box<Type>,
    pub kind: PtrKind,
}

impl fmt::Display for Ptr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "*{} ", self.kind)?;
        if self.ty.should_wrap() {
            write!(f, "({})", self.ty)
        } else {
            write!(f, "{}", self.ty)
        }
    }
}

#[cfg(feature = "syn")]
impl From<syn::TypePtr> for Ptr {
    fn from(value: syn::TypePtr) -> Self {
        let kind = if value.mutability.is_some() {
            PtrKind::Mut
        } else {
            PtrKind::Const
        };
        Self {
            ty: Box::new(Type::from(*value.elem)),
            kind,
        }
    }
}

impl From<Ptr> for TokenStream {
    fn from(value: Ptr) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::BinOp(BinOpToken::Star).into_joint());
        ts.extend(TokenStream::from(value.kind));
        if value.ty.should_wrap() {
            ts.push(Token::OpenDelim(Delimiter::Parenthesis).into_joint());
            ts.extend(TokenStream::from(*value.ty).into_joint());
            ts.push(Token::CloseDelim(Delimiter::Parenthesis));
        } else {
            ts.extend(TokenStream::from(*value.ty));
        }
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

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BareFnArg {
    name: Option<String>,
    ty: Type,
}

impl fmt::Display for BareFnArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(name) = &self.name {
            write!(f, "{name}: ")?;
        }
        write!(f, "{}", self.ty)
    }
}

#[cfg(feature = "syn")]
impl From<syn::BareFnArg> for BareFnArg {
    fn from(value: syn::BareFnArg) -> Self {
        let name = value.name.map(|(name, _)| name.to_string().into());
        let ty = Type::from(value.ty);
        Self { name, ty }
    }
}

impl From<BareFnArg> for TokenStream {
    fn from(value: BareFnArg) -> Self {
        let mut ts = TokenStream::new();
        if let Some(name) = value.name {
            ts.push(Token::ident(name).into_joint());
            ts.push(Token::Colon);
        }
        ts.extend(TokenStream::from(value.ty));
        ts
    }
}

impl BareFnArg {
    pub fn new(name: Option<impl Into<String>>, ty: impl Into<Type>) -> Self {
        Self {
            name: name.map(|n| n.into()),
            ty: ty.into(),
        }
    }

    pub fn simple(ty: impl Into<Type>) -> Self {
        Self::new(None::<String>, ty)
    }
}

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BareFn {
    pub generic_params: Vec<GenericParam>,
    pub inputs: Vec<BareFnArg>,
    pub output: Option<Box<Type>>,
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
        write!(f, ")")?;
        if let Some(output) = &self.output {
            write!(f, " -> {}", output)?;
        }
        Ok(())
    }
}

#[cfg(feature = "syn")]
impl From<syn::TypeBareFn> for BareFn {
    fn from(value: syn::TypeBareFn) -> Self {
        let inputs = value.inputs.into_iter().map(BareFnArg::from).collect();
        let output = match value.output {
            syn::ReturnType::Default => None,
            syn::ReturnType::Type(_, ty) => Some(Box::new(Type::from(*ty))),
        };
        let is_unsafe = value.unsafety.is_some();
        let abi = value.abi.map(|a| {
            a.name
                .map(|n| n.value())
                .unwrap_or_else(|| "C".to_string())
                .into()
        });
        let generic_params = value
            .lifetimes
            .map(|bl| {
                bl.lifetimes
                    .into_iter()
                    .map(GenericParam::from)
                    .collect()
            })
            .unwrap_or_default();
        Self {
            generic_params,
            inputs,
            output,
            is_unsafe,
            abi,
        }
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
            ts.extend(TokenStream::from(param.clone()).into_joint());
        }
        ts.push(Token::CloseDelim(Delimiter::Parenthesis));
        if let Some(output) = value.output {
            ts.push(Token::RArrow);
            ts.extend(TokenStream::from(*output));
        }
        ts
    }
}

impl BareFn {
    pub fn new(
        generic_params: Vec<GenericParam>,
        inputs: Vec<BareFnArg>,
        output: Option<impl Into<Type>>,
        abi: Option<String>,
        is_unsafe: bool,
    ) -> Self {
        Self {
            generic_params,
            inputs,
            output: output.map(|o| Box::new(o.into())),
            abi,
            is_unsafe,
        }
    }

    pub fn safe(
        generic_params: Vec<GenericParam>,
        inputs: Vec<BareFnArg>,
        output: Option<impl Into<Type>>,
    ) -> Self {
        BareFn::new(generic_params, inputs, output, None, false)
    }

    pub fn add_input(&mut self, input: BareFnArg) {
        self.inputs.push(input);
    }

    pub fn with_input(mut self, input: BareFnArg) -> Self {
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

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeParam {
    pub ident: String,
    pub bounds: Vec<GenericBound>,
    pub default: Option<Type>,
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
        if let Some(default) = &self.default {
            write!(f, " = {default}")?;
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
        if let Some(default) = value.default {
            ts.push(Token::Eq);
            ts.extend(TokenStream::from(default));
        }
        ts
    }
}

impl TypeParam {
    pub fn new(ident: impl Into<String>, bounds: Vec<GenericBound>) -> Self {
        Self {
            ident: ident.into(),
            bounds,
            default: None,
        }
    }

    pub fn simple(ident: impl Into<String>) -> Self {
        Self {
            ident: ident.into(),
            bounds: vec![],
            default: None,
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

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstParam {
    pub ident: String,
    pub ty: Type,
    pub default: Option<Expr>,
}

impl fmt::Display for ConstParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "const {}: {}", self.ident, self.ty)?;
        if let Some(default) = &self.default {
            write!(f, " = {default}")?;
        }
        Ok(())
    }
}

impl From<ConstParam> for TokenStream {
    fn from(value: ConstParam) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Keyword(KeywordToken::Const));
        ts.push(Token::ident(value.ident).into_joint());
        ts.push(Token::Colon);
        ts.extend(TokenStream::from(value.ty));
        if let Some(default) = value.default {
            ts.push(Token::Eq);
            ts.extend(TokenStream::from(default));
        }
        ts
    }
}

impl ConstParam {
    pub fn new(ident: impl Into<String>, ty: Type) -> Self {
        Self {
            ident: ident.into(),
            ty,
            default: None,
        }
    }
}

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LifetimeParam {
    pub name: String,
    pub bounds: Vec<String>,
}

impl fmt::Display for LifetimeParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "'{}", self.name)?;
        if !self.bounds.is_empty() {
            write!(f, ": ")?;
            for (i, bound) in self.bounds.iter().enumerate() {
                if i > 0 {
                    write!(f, " + ")?;
                }
                write!(f, "'{bound}")?;
            }
        }
        Ok(())
    }
}

impl From<LifetimeParam> for TokenStream {
    fn from(value: LifetimeParam) -> Self {
        let mut ts = TokenStream::new();
        ts.push(Token::Lifetime(value.name).into_joint());
        if !value.bounds.is_empty() {
            ts.push(Token::Colon);
            for (i, bound) in value.bounds.iter().enumerate() {
                if i > 0 {
                    ts.push(Token::BinOp(BinOpToken::Plus));
                }
                ts.push(Token::Lifetime(bound.clone()));
            }
        }
        ts
    }
}

impl LifetimeParam {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            bounds: vec![],
        }
    }

    pub fn with_bounds(name: impl Into<String>, bounds: Vec<String>) -> Self {
        Self {
            name: name.into(),
            bounds,
        }
    }
}

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GenericParam {
    TypeParam(TypeParam),
    ConstParam(ConstParam),
    Lifetime(LifetimeParam),
}

impl fmt::Display for GenericParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TypeParam(tp) => write!(f, "{tp}"),
            Self::ConstParam(cp) => write!(f, "{cp}"),
            Self::Lifetime(lt) => write!(f, "{lt}"),
        }
    }
}

impl From<GenericParam> for TokenStream {
    fn from(item: GenericParam) -> Self {
        match item {
            GenericParam::TypeParam(v) => v.into(),
            GenericParam::ConstParam(v) => v.into(),
            GenericParam::Lifetime(lt) => lt.into(),
        }
    }
}

impl From<TypeParam> for GenericParam {
    fn from(item: TypeParam) -> Self {
        Self::TypeParam(item)
    }
}

impl From<ConstParam> for GenericParam {
    fn from(item: ConstParam) -> Self {
        Self::ConstParam(item)
    }
}

impl From<LifetimeParam> for GenericParam {
    fn from(item: LifetimeParam) -> Self {
        Self::Lifetime(item)
    }
}

#[cfg(feature = "syn")]
impl From<syn::GenericParam> for GenericParam {
    fn from(value: syn::GenericParam) -> Self {
        match value {
            syn::GenericParam::Type(tp) => GenericParam::TypeParam(TypeParam {
                ident: tp.ident.to_string().into(),
                bounds: tp.bounds.into_iter().map(GenericBound::from).collect(),
                default: tp.default.map(Type::from),
            }),
            syn::GenericParam::Const(cp) => GenericParam::ConstParam(ConstParam {
                ident: cp.ident.to_string().into(),
                ty: Type::from(cp.ty),
                default: cp.default.map(Expr::from),
            }),
            syn::GenericParam::Lifetime(lt) => {
                let bounds = lt.bounds.iter().map(|b| b.ident.to_string().into()).collect();
                GenericParam::Lifetime(LifetimeParam {
                    name: lt.lifetime.ident.to_string().into(),
                    bounds,
                })
            }
        }
    }
}

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PolyTraitRef {
    pub bound_generic_params: Vec<GenericParam>,
    pub trait_ref: Path,
}

impl fmt::Display for PolyTraitRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.bound_generic_params.is_empty() {
            write!(f, "for<")?;
            for (i, param) in self.bound_generic_params.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{param}")?;
            }
            write!(f, "> ")?;
        }
        write!(f, "{}", self.trait_ref)
    }
}

#[cfg(feature = "syn")]
impl From<syn::TraitBound> for PolyTraitRef {
    fn from(value: syn::TraitBound) -> Self {
        let bound_generic_params = value
            .lifetimes
            .map(|bl| {
                bl.lifetimes
                    .into_iter()
                    .map(GenericParam::from)
                    .collect()
            })
            .unwrap_or_default();
        let trait_ref = Path::from(value.path);
        Self {
            bound_generic_params,
            trait_ref,
        }
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

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GenericBound {
    Trait(PolyTraitRef),
    Outlives(String),
}

impl From<PolyTraitRef> for GenericBound {
    fn from(value: PolyTraitRef) -> Self {
        Self::Trait(value)
    }
}

impl fmt::Display for GenericBound {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Trait(trait_ref) => write!(f, "{trait_ref}"),
            Self::Outlives(lifetime) => write!(f, "'{lifetime}"),
        }
    }
}

#[cfg(feature = "syn")]
impl From<syn::TypeParamBound> for GenericBound {
    fn from(value: syn::TypeParamBound) -> Self {
        match value {
            syn::TypeParamBound::Trait(trait_bound) => {
                GenericBound::Trait(PolyTraitRef::from(trait_bound))
            }
            syn::TypeParamBound::Lifetime(lifetime) => {
                GenericBound::Outlives(lifetime.ident.to_string().into())
            }
            _ => unimplemented!("unsupported TypeParamBound variant"),
        }
    }
}

impl From<PolyTraitRef> for TokenStream {
    fn from(value: PolyTraitRef) -> Self {
        let mut ts = TokenStream::new();
        if !value.bound_generic_params.is_empty() {
            ts.push(Token::Keyword(KeywordToken::For));
            ts.push(Token::Lt);
            for (i, param) in value.bound_generic_params.iter().enumerate() {
                if i != 0 {
                    ts.push(Token::Comma);
                }
                ts.extend(TokenStream::from(param.clone()));
            }
            ts.push(Token::Gt);
        }
        ts.extend(TokenStream::from(value.trait_ref));
        ts
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

#[cfg(feature = "fuzzing")]
impl<'a> arbitrary::Arbitrary<'a> for TraitObject {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        let is_dyn = u.arbitrary()?;
        let len = u.int_in_range(0..=5)?;
        let mut bounds = Vec::with_capacity(len);
        bounds.push(GenericBound::Trait(PolyTraitRef::arbitrary(u)?));
        for _ in 0..len {
            bounds.push(GenericBound::arbitrary(u)?);
        }
        Ok(Self { is_dyn, bounds })
    }
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

#[cfg(feature = "syn")]
impl From<syn::TypeTraitObject> for TraitObject {
    fn from(value: syn::TypeTraitObject) -> Self {
        let is_dyn = value.dyn_token.is_some();
        let bounds = value.bounds.into_iter().map(GenericBound::from).collect();
        Self { is_dyn, bounds }
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

#[cfg(feature = "fuzzing")]
impl<'a> arbitrary::Arbitrary<'a> for ImplTrait {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        let len = u.int_in_range(0..=5)?;
        let mut bounds = Vec::with_capacity(len);
        bounds.push(GenericBound::Trait(PolyTraitRef::arbitrary(u)?));
        for _ in 0..len {
            bounds.push(GenericBound::arbitrary(u)?);
        }
        Ok(Self { bounds })
    }
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

#[cfg(feature = "syn")]
impl From<syn::TypeImplTrait> for ImplTrait {
    fn from(value: syn::TypeImplTrait) -> Self {
        let bounds = value.bounds.into_iter().map(GenericBound::from).collect();
        Self { bounds }
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

#[cfg(feature = "fuzzing")]
impl<'a> arbitrary::Arbitrary<'a> for Type {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        if crate::depth_limiter::reached() {
            return Ok(Type::Never);
        }
        match u.int_in_range(0..=10)? {
            0 => Ok(Type::Slice(Box::new(Type::arbitrary(u)?))),
            1 => Ok(Type::Array(
                Box::new(Type::arbitrary(u)?),
                Box::new(Const::arbitrary(u)?),
            )),
            2 => Ok(Type::Ptr(Ptr::arbitrary(u)?)),
            3 => Ok(Type::Ref(Ref::arbitrary(u)?)),
            4 => Ok(Type::BareFn(BareFn::arbitrary(u)?)),
            5 => Ok(Type::Macro(MacCall::arbitrary(u)?)),
            6 => Ok(Type::Never),
            7 => {
                let len = u.int_in_range(0..=5)?;
                let mut types = Vec::with_capacity(len);
                for _ in 0..len {
                    types.push(Type::arbitrary(u)?);
                }
                Ok(Type::Tuple(types))
            }
            8 => Ok(Type::Path(Path::arbitrary(u)?)),
            9 => Ok(Type::TraitObject(TraitObject::arbitrary(u)?)),
            10 => Ok(Type::ImplTrait(ImplTrait::arbitrary(u)?)),
            _ => unreachable!(),
        }
    }
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

impl From<PathSegment> for Type {
    fn from(p: PathSegment) -> Self {
        Self::Path(Path::single(p))
    }
}
impl From<Path> for Type {
    fn from(path: Path) -> Self {
        Self::Path(path)
    }
}
impl From<&str> for Type {
    fn from(path: &str) -> Self {
        Self::Path(Path::single(path))
    }
}
impl From<String> for Type {
    fn from(path: String) -> Self {
        Self::Path(Path::single(path))
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

#[cfg(feature = "syn")]
impl From<syn::Type> for Type {
    fn from(value: syn::Type) -> Self {
        match value {
            syn::Type::Slice(ty) => Type::Slice(Box::new(Type::from(*ty.elem))),
            syn::Type::Array(ty) => Type::Array(
                Box::new(Type::from(*ty.elem)),
                Box::new(Const::from(ty.len)),
            ),
            syn::Type::Ptr(ptr) => Type::Ptr(Ptr::from(ptr)),
            syn::Type::Reference(ref_) => Type::Ref(Ref::from(ref_)),
            syn::Type::BareFn(bare_fn) => Type::BareFn(BareFn::from(bare_fn)),
            syn::Type::Path(path) => Type::Path(Path::from(path)),
            syn::Type::TraitObject(trait_object) => {
                Type::TraitObject(TraitObject::from(trait_object))
            }
            syn::Type::ImplTrait(impl_trait) => Type::ImplTrait(ImplTrait::from(impl_trait)),
            syn::Type::Tuple(ty) => Type::Tuple(ty.elems.into_iter().map(Type::from).collect()),
            syn::Type::Macro(mac) => Type::Macro(MacCall::from(mac.mac)),
            syn::Type::Never(_) => Type::Never,
            syn::Type::Infer(_) => Type::Infer,
            syn::Type::Group(g) => Type::from(*g.elem),
            syn::Type::Paren(p) => Type::from(*p.elem),
            // non-exhaustive
            _ => unimplemented!("unsupported syn::Type variant"),
        }
    }
}

impl From<Type> for TokenStream {
    fn from(value: Type) -> Self {
        match value {
            Type::Slice(ty) => {
                let mut ts = TokenStream::new();
                ts.push(Token::OpenDelim(Delimiter::Bracket).into_joint());
                ts.extend(TokenStream::from(*ty).into_joint());
                ts.push(Token::CloseDelim(Delimiter::Bracket));
                ts
            }
            Type::Array(ty, len) => {
                let mut ts = TokenStream::new();
                ts.push(Token::OpenDelim(Delimiter::Bracket).into_joint());
                ts.extend(TokenStream::from(*ty).into_joint());
                ts.push(Token::Semi);
                ts.extend(TokenStream::from(*len).into_joint());
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
                    ts.extend(TokenStream::from(ty).into_joint())
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
    pub fn string() -> Type {
        Self::Path(Path::single("String"))
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

    pub fn should_wrap(&self) -> bool {
        matches!(self, Type::ImplTrait(_) | Type::TraitObject(_))
    }
}
