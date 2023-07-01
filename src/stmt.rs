use std::fmt;

use crate::{expr::{Expr, MacCall, Path}, GenericArg, Type, impl_obvious_conversion, impl_display_for_enum, Attribute, DelimArgs};

pub trait Empty {
    type Input;
    fn empty(ident: impl Into<Self::Input>) -> Self;
}
pub trait HasItem<Itm = Item> {
    fn with_item(mut self, item: impl Into<Itm>) -> Self
    where Self: Sized {
        self.add_item(item);
        self
    }
    fn add_item(&mut self, item: impl Into<Itm>);
    fn remove_item(&mut self, index: usize) -> Option<Itm>;
    fn remove_item_by_ident(&mut self, ident: &str) -> Option<Itm>;
}

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
            Self::InitElse(expr, block) => write!(f, " = {expr} else {{ {block} }}"),
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

impl Empty for Fn {
    type Input = String;

    fn empty(ident: impl Into<Self::Input>) -> Self {
        Self::empty(ident)
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

    pub fn empty(ident: impl Into<String>) -> Self {
        Self {
            ident: ident.into(),
            generics: Vec::new(),
            inputs: Vec::new(),
            output: None,
            body: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Mod {
    Loaded { ident: String, items: Vec<Item> },
    Unloaded(String),
}

impl fmt::Display for Mod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Loaded { ident, items } => {
                writeln!(f, "mod {} {{", ident)?;
                for item in items.iter() {
                    writeln!(f, "{item}")?;
                }
                write!(f, "}}")
            }
            Self::Unloaded(ident) => write!(f, "mod {};", ident),
        }
    }
}

impl Mod {
    pub fn ident(&self) -> &str {
        match self {
            Self::Loaded { ident, .. } => ident,
            Self::Unloaded(ident) => ident,
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

    pub fn with_stmt(mut self, stmt: impl Into<Stmt>) -> Self {
        self.add_stmt(stmt);
        self
    }

    pub fn add_stmt(&mut self, stmt: impl Into<Stmt>) {
        self.stmts.push(stmt.into());
    }

    pub fn remove_stmt(&mut self, index: usize) -> Stmt {
        self.stmts.remove(index)
    }

    pub fn remove_item_by_ident(&mut self, ident: &str) -> Option<Item> {
        let index = self.stmts.iter().position(|stmt| stmt.ident() == Some(ident))?;
        let Stmt::Item(item) = self.remove_stmt(index) else { unreachable!() };
        Some(item)
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

impl FieldDef {
    pub fn new(vis: Visibility, ident: Option<impl Into<String>>, ty: impl Into<Type>) -> Self {
        Self {
            attrs: Vec::new(),
            vis,
            ident: ident.map(Into::into),
            ty: ty.into(),
        }
    }

    pub fn simple(ident: impl Into<String>, ty: impl Into<Type>) -> Self {
        Self::new(Visibility::Inherited, Some(ident), ty)
    }

    pub fn with_attr(mut self, attr: impl Into<Attribute>) -> Self {
        self.add_attr(attr);
        self
    }

    pub fn add_attr(&mut self, attr: impl Into<Attribute>) {
        self.attrs.push(attr.into());
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

impl VariantData {
    pub fn add_field(&mut self, field: FieldDef) {
        match self {
            Self::Unit => {
                let new = if field.ident.is_some() {
                    Self::Struct(vec![field])
                } else {
                    Self::Tuple(vec![field])
                };
                *self = new;
            },
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

    pub fn remove_field_by_ident(&mut self, ident: &str) -> Option<FieldDef> {
        let index = match self {
            Self::Unit => return None,
            Self::Tuple(fields) => fields.iter().position(|field| field.ident.as_deref() == Some(ident))?,
            Self::Struct(fields) => fields.iter().position(|field| field.ident.as_deref() == Some(ident))?,
        };
        Some(self.remove_field(index).unwrap())
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

impl Empty for Variant {
    type Input = String;

    fn empty(ident: impl Into<Self::Input>) -> Self {
        Self::empty(ident)
    }
}

impl Variant {
    pub fn new(vis: Visibility, ident: impl Into<String>, data: VariantData, disr_expr: Option<Expr>) -> Self {
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

impl Empty for EnumDef {
    type Input = String;

    fn empty(ident: impl Into<Self::Input>) -> Self {
        Self::empty(ident)
    }
}

impl EnumDef {
    pub fn new(ident: impl Into<String>, generics: Vec<GenericArg>, variants: Vec<Variant>) -> Self {
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

    pub fn remove_variant_by_ident(&mut self, ident: &str) -> Option<Variant> {
        let index = self.variants.iter().position(|va| va.ident == ident)?;
        Some(self.remove_variant(index))
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
        writeln!(f, "struct {} {}", self.ident, self.variant)
    }
}

impl Empty for StructDef {
    type Input = String;

    fn empty(ident: impl Into<Self::Input>) -> Self {
        Self::empty(ident)
    }
}

impl StructDef {
    pub fn new(ident: impl Into<String>, generics: Vec<GenericArg>, variant: VariantData) -> Self {
        Self {
            ident: ident.into(),
            generics,
            variant
        }
    }

    pub fn empty(ident: impl Into<String>) -> Self {
        Self::new(ident, Vec::new(), VariantData::Unit)
    }

    pub fn with_item(mut self, item: FieldDef) -> Self {
        self.add_field(item);
        self
    }

    pub fn add_field(&mut self, item: FieldDef) {
        self.variant.add_field(item);
    }

    pub fn remove_field(&mut self, index: usize) -> Option<FieldDef> {
        self.variant.remove_field(index)
    }

    pub fn remove_field_by_ident(&mut self, ident: &str) -> Option<FieldDef> {
        self.variant.remove_field_by_ident(ident)
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
        write!(f, "union {} {{}}", self.ident)
    }
}

impl Empty for UnionDef {
    type Input = String;

    fn empty(ident: impl Into<Self::Input>) -> Self {
        Self::empty(ident)
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

impl Empty for TraitDef {
    type Input = String;

    fn empty(ident: impl Into<Self::Input>) -> Self {
        Self::empty(ident)
    }
}

impl TraitDef {
    pub fn new(ident: impl Into<String>, generics: Vec<GenericArg>, items: Vec<AssocItem>) -> Self {
        Self {
            ident: ident.into(),
            generics,
            items,
        }
    }

    pub fn empty(ident: impl Into<String>) -> Self {
        Self::new(ident, Vec::new(), Vec::new())
    }

    pub fn with_item(mut self, item: impl Into<AssocItem>) -> Self {
        self.add_item(item);
        self
    }

    pub fn add_item(&mut self, item: impl Into<AssocItem>) {
        self.items.push(item.into());
    }

    pub fn remove_item(&mut self, index: usize) -> AssocItem {
        self.items.remove(index)
    }

    pub fn remove_item_by_ident(&mut self, ident: &str) -> Option<AssocItem> {
        let index = self.items.iter().position(|item| item.ident() == Some(ident))?;
        Some(self.remove_item(index))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Impl {
    pub of_trait: Option<Type>,
    pub self_ty: Type,
    pub items: Vec<AssocItem>,
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

impl Empty for Impl {
    type Input = Type;

    fn empty(self_ty: impl Into<Self::Input>) -> Self {
        Self::new(self_ty.into(), vec![])
    }
}

impl Impl {
    pub fn new(self_ty: Type, items: Vec<AssocItem>) -> Self {
        Self {
            of_trait: None,
            self_ty,
            items,
        }
    }

    pub fn trait_impl(self_ty: Type, of_trait: Type, items: Vec<AssocItem>) -> Self {
        Self {
            of_trait: Some(of_trait),
            self_ty,
            items,
        }
    }

    pub fn with_item(mut self, item: impl Into<AssocItem>) -> Self {
        self.add_item(item);
        self
    }

    pub fn add_item(&mut self, item: impl Into<AssocItem>) {
        self.items.push(item.into());
    }

    pub fn remove_item(&mut self, index: usize) -> AssocItem {
        self.items.remove(index)
    }

    pub fn remove_item_by_ident(&mut self, ident: &str) -> Option<AssocItem> {
        let index = self.items.iter().position(|item| item.ident() == Some(ident))?;
        Some(self.remove_item(index))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MacroDef {
    pub ident: String,
    pub args: DelimArgs,
}

impl fmt::Display for MacroDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "macro_rules! {} {{}}", self.ident)
    }
}

impl Empty for MacroDef {
    type Input = String;

    fn empty(ident: impl Into<Self::Input>) -> Self {
        Self::empty(ident)
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

    pub fn ident(&self) -> Option<&str> {
        self.kind.ident()
    }
}

impl Item<AssocItemKind> {
    pub fn ident(&self) -> Option<&str> {
        self.kind.ident()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ItemKind {
    Use(Path),
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
}

impl_obvious_conversion!(ItemKind; Fn, Mod, TyAlias, EnumDef, StructDef, UnionDef, TraitDef, Impl, MacroDef, MacCall);
impl_display_for_enum!(ItemKind; Use, Fn, Mod, TyAlias, EnumDef, StructDef, UnionDef, TraitDef, Impl, MacroDef, MacCall);

impl ItemKind {
    pub fn ident(&self) -> Option<&str> {
        match self {
            Self::Use(_) => None,
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
        }
    }
}

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
        Ok(())
    }
}

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
        Ok(())
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

impl AssocItemKind {
    pub fn ident(&self) -> Option<&str> {
        match self {
            Self::ConstItem(item) => Some(&item.ident),
            Self::Fn(item) => Some(&item.ident),
            Self::TyAlias(item) => Some(&item.ident),
            Self::MacCall(_) => None,
        }
    }
}

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

impl Stmt {
    pub fn ident(&self) -> Option<&str> {
        match self {
            Self::Item(item) => item.ident(),
            _ => None,
        }
    }
}