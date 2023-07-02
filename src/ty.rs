use std::fmt;

use crate::{Path, Param, Const, PathSegment};

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Ref {
    pub lifetime: Option<String>,
    pub ty: MutTy,
}

impl fmt::Display for Ref {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "&")?;
        match &self.lifetime {
            Some(lifetime) => write!(f, "'{lifetime} ")?,
            None => (),
        }
        write!(f, "{ty}", ty = self.ty)
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImplTrait {
    pub bounds: Vec<GenericBound>,
}

impl fmt::Display for ImplTrait {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "impl {bounds}", bounds = self.bounds.iter().map(|b| format!("{b}")).collect::<Vec<_>>().join(" + "))
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
