use std::fmt;

use crate::{expr::{Expr, MacCall, Path}, GenericArg, Type, impl_obvious_conversion, impl_display_for_enum, Attribute};

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
        write!(f, "{kind}", kind = self.kind)
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PatField {
    pub ident: String,
    pub pat: Pat,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructPat {
    path: Path,
    fields: Vec<PatField>,
}

impl fmt::Display for StructPat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{path} {{", path = self.path)?;
        for (i, field) in self.fields.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{ident}: {pat}", ident = field.ident, pat = field.pat)?;
        }
        write!(f, "}}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pat {
    Wild,
    Ident(String),
    Tuple(Vec<Pat>),
    Struct(StructPat),
    Or(Vec<Pat>),
    Lit(Expr),
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
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    pub pat: Pat,
    pub ty: Type,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Fn {
    pub ident: String,
    pub generics: Vec<GenericArg>,
    pub inputs: Vec<Param>,
    pub output: Option<Type>,
    pub body: Option<Block>,
}

impl fmt::Display for Fn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
        write!(f, "(")?;
        for (i, param) in self.inputs.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{pat}: {ty}", pat = param.pat, ty = param.ty)?;
        }
        write!(f, ")")?;
        if let Some(output) = &self.output {
            write!(f, " -> {ty}", ty = output)?;
        }
        if let Some(body) = &self.body {
            write!(f, " {body}")?;
        }
        Ok(())
    }
}

impl Fn {
    pub fn main(output: Option<Type>, body: Block) -> Self {
        Self {
            ident: "main".to_string(),
            generics: Vec::new(),
            inputs: Vec::new(),
            output,
            body: Some(body),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{{")?;
        for stmt in self.stmts.iter() {
            writeln!(f, "{stmt};")?;
        }
        write!(f, "}}")
    }
}

impl<S: Into<Stmt>> From<S> for Block {
    fn from(stmt: S) -> Self {
        let mut block = Block::new();
        block.add_stmt(stmt);
        block
    }
}

impl Block {
    pub fn new() -> Self {
        Self { stmts: Vec::new() }
    }

    pub fn add_stmt(&mut self, stmt: impl Into<Stmt>) {
        self.stmts.push(stmt.into());
    }
}

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
                write!(f, "{{")?;
                for (i, field) in fields.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{field}")?;
                }
                write!(f, "}}")
            }
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumDef {
    pub ident: String,
    pub generics: Vec<GenericArg>,
    pub variants: Vec<Variant>,
}

impl fmt::Display for EnumDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "enum {} {{", self.ident)?;
        for (i, variant) in self.variants.iter().enumerate() {
            if i != 0 {
                writeln!(f)?;
            }
            writeln!(f, "{variant},")?;
        }
        write!(f, "}}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructDef {
    pub ident: String,
    pub generics: Vec<GenericArg>,
    pub fields: Vec<FieldDef>,
}

impl fmt::Display for StructDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "struct {} {{", self.ident)?;
        for (i, field) in self.fields.iter().enumerate() {
            if i != 0 {
                writeln!(f)?;
            }
            writeln!(f, "{field},")?;
        }
        write!(f, "}}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnionDef {
    pub ident: String,
}

impl fmt::Display for UnionDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "union {} {{}}", self.ident)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TraitDef {
    pub ident: String,
    pub generics: Vec<GenericArg>,
    pub items: Vec<AssocItem>,
}

impl fmt::Display for TraitDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "trait {} {{", self.ident)?;
        for item in self.items.iter() {
            writeln!(f, "{item};")?;
        }
        write!(f, "}}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Impl {
    of_trait: Option<Type>,
    self_ty: Type,
    items: Vec<AssocItem>,
}

impl fmt::Display for Impl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "impl ")?;
        if let Some(of_trait) = &self.of_trait {
            write!(f, "{of_trait} for ", of_trait = of_trait)?;
        }
        write!(f, "{self_ty} {{", self_ty = self.self_ty)?;
        for item in self.items.iter() {
            writeln!(f, "{item};")?;
        }
        write!(f, "}}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MacroDef {
    pub ident: String,
}

impl fmt::Display for MacroDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "macro_rules! {} {{}}", self.ident)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Visibility {
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Item<K = ItemKind> {
    pub vis: Visibility,
    pub kind: K,
}

impl<I: Into<ItemKind>> From<I> for Item {
    fn from(item: I) -> Self {
        Self::inherited(item.into())
    }
}

impl<K: fmt::Display> fmt::Display for Item<K> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.vis, self.kind)
    }
}

impl Item {
    pub fn inherited(item: impl Into<ItemKind>) -> Self {
        Self {
            vis: Visibility::Inherited,
            kind: item.into(),
        }
    }

    pub fn public(item: impl Into<ItemKind>) -> Self {
        Self {
            vis: Visibility::Public,
            kind: item.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ItemKind {
    Use(Path),
    Fn(Fn),
    EnumDef(EnumDef),
    StructDef(StructDef),
    UnionDef(UnionDef),
    TraitDef(TraitDef),
    Impl(Impl),
    MacCall(MacCall),
    MacroDef(MacroDef),
}

impl_obvious_conversion!(ItemKind; Fn, EnumDef, StructDef, UnionDef, TraitDef, Impl, MacroDef, MacCall);
impl_display_for_enum!(ItemKind; Use, Fn, EnumDef, StructDef, UnionDef, TraitDef, Impl, MacroDef, MacCall);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstItem {
    ident: String,
    ty: Type,
    expr: Option<Expr>,
}

impl fmt::Display for ConstItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "const {ident}: {ty}", ident = self.ident, ty = self.ty)?;
        if let Some(expr) = &self.expr {
            write!(f, " = {expr}", expr = expr)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyAlias {
    ident: String,
    ty: Option<Type>,
}

impl fmt::Display for TyAlias {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "type {ident}", ident = self.ident)?;
        if let Some(ty) = &self.ty {
            write!(f, " = {ty}", ty = ty)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AssocItemKind {
    ConstItem(ConstItem),
    Fn(Fn),
    TyAlias(TyAlias),
    MacroDef(MacroDef),
}

impl_display_for_enum!(AssocItemKind; ConstItem, Fn, TyAlias, MacroDef);

pub type AssocItem = Item<AssocItemKind>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
    Local(Local),
    Item(Item),
    Expr(Expr),
    Semi(Expr),
    Empty,
    MacCall(MacCall),
}

impl_obvious_conversion!(Stmt; Local, Item, MacCall);

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Local(local) => write!(f, "{local}"),
            Self::Item(item) => write!(f, "{item}"),
            Self::Expr(expr) => write!(f, "{expr}"),
            Self::Semi(expr) => write!(f, "{expr};"),
            Self::Empty => write!(f, ""),
            Self::MacCall(mac_call) => write!(f, "{mac_call}"),
        }
    }
}

impl<E: Into<Expr>> From<E> for Stmt {
    fn from(expr: E) -> Self {
        Self::Expr(expr.into())
    }
}
