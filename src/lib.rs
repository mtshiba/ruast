#[cfg(feature = "tokenize")]
mod conversion;
mod expr;
mod stmt;
mod token;
mod ty;

use std::fmt;
use std::fs::File;
use std::io::Write;
use std::path::Path as Pt;

pub use expr::*;
pub use stmt::*;
pub use token::*;
pub use ty::*;

pub mod traits {
    pub use crate::{
        Accessible, AddVisibility, Assignable, Awaitable, BinaryOperable, Callable, Castable,
        EmptyItem, HasItem, HasPrecedence, HasVisibility, Ident, Indexable, IntoConst, IntoItem,
        IntoTokens, IntoTryBlock, IntoUnsafe, MaybeIdent, MethodCallable, Parenthesize, Returnable,
        Semicolon, Tryable, UnaryOperable, Yieldable,
    };
}

macro_rules! impl_obvious_conversion {
    ($Enum: ident; $($Variant: ident $(,)?)*) => {
        $(
            impl From<$Variant> for $Enum {
                fn from(item: $Variant) -> Self {
                    Self::$Variant(item)
                }
            }
        )*
        impl From<$Enum> for $crate::TokenStream {
            fn from(item: $Enum) -> Self {
                match item {
                    $($Enum::$Variant(v) => v.into(),)*
                }
            }
        }
    };
}
pub(crate) use impl_obvious_conversion;

macro_rules! impl_display_for_enum {
    ($Enum: ident; $($Variant: ident $(,)?)*) => {
        impl std::fmt::Display for $Enum {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        Self::$Variant(item) => write!(f, "{item}"),
                    )*
                }
            }
        }
    };
}
pub(crate) use impl_display_for_enum;

macro_rules! impl_hasitem_methods {
    ($Ty: ident) => {
        impl $Ty {
            pub fn with_item(self, item: impl Into<Item>) -> Self {
                HasItem::with_item(self, item)
            }

            pub fn add_item(&mut self, item: impl Into<Item>) -> $crate::ItemIndex {
                HasItem::add_item(self, item)
            }

            pub fn try_remove_item(&mut self, index: usize) -> Option<Item> {
                HasItem::try_remove_item(self, index)
            }

            pub fn remove_item(&mut self, index: $crate::ItemIndex) -> Item {
                HasItem::remove_item(self, index)
            }

            pub fn try_remove_item_by_id(&mut self, ident: &str) -> Option<Item> {
                HasItem::try_remove_item_by_id(self, ident)
            }

            pub fn get_item(&self, index: usize) -> Option<&Item> {
                HasItem::get_item(self, index)
            }

            pub fn get_item_by_id(&self, ident: &str) -> Option<&Item> {
                HasItem::get_item_by_id(self, ident)
            }
        }
        impl std::ops::Deref for $Ty {
            type Target = [Item];

            fn deref(&self) -> &Self::Target {
                self.items()
            }
        }
        impl std::ops::DerefMut for $Ty {
            fn deref_mut(&mut self) -> &mut Self::Target {
                self.items_mut()
            }
        }
        impl std::ops::Index<$crate::ItemIndex> for $Ty {
            type Output = Item;

            fn index(&self, index: $crate::ItemIndex) -> &Self::Output {
                self.get_item(index.0).expect("index out of bounds")
            }
        }
        impl std::ops::IndexMut<$crate::ItemIndex> for $Ty {
            fn index_mut(&mut self, index: $crate::ItemIndex) -> &mut Self::Output {
                self.get_item_mut(index.0).expect("index out of bounds")
            }
        }
    };
    ($Ty: ident, $Item: ident) => {
        impl $Ty {
            pub fn with_item(self, item: impl Into<$Item>) -> Self {
                HasItem::with_item(self, item)
            }

            pub fn add_item(&mut self, item: impl Into<$Item>) -> $crate::ItemIndex {
                HasItem::add_item(self, item)
            }

            pub fn try_remove_item(&mut self, index: usize) -> Option<$Item> {
                HasItem::try_remove_item(self, index)
            }

            pub fn remove_item(&mut self, index: $crate::ItemIndex) -> $Item {
                HasItem::remove_item(self, index)
            }

            pub fn try_remove_item_by_id(&mut self, ident: &str) -> Option<$Item> {
                HasItem::try_remove_item_by_id(self, ident)
            }

            pub fn get_item(&self, index: usize) -> Option<&$Item> {
                HasItem::get_item(self, index)
            }

            pub fn get_item_by_id(&self, ident: &str) -> Option<&$Item> {
                HasItem::get_item_by_id(self, ident)
            }
        }
        impl std::ops::Deref for $Ty {
            type Target = [$Item];

            fn deref(&self) -> &Self::Target {
                self.items()
            }
        }
        impl std::ops::DerefMut for $Ty {
            fn deref_mut(&mut self) -> &mut Self::Target {
                self.items_mut()
            }
        }
        impl std::ops::Index<$crate::ItemIndex> for $Ty {
            type Output = $Item;

            fn index(&self, index: $crate::ItemIndex) -> &Self::Output {
                self.get_item(index.0).expect("index out of bounds")
            }
        }
        impl std::ops::IndexMut<$crate::ItemIndex> for $Ty {
            fn index_mut(&mut self, index: $crate::ItemIndex) -> &mut Self::Output {
                self.get_item_mut(index.0).expect("index out of bounds")
            }
        }
    };
    ($Ty: ident, $Item: ident, Deref) => {
        impl std::ops::Deref for $Ty {
            type Target = [$Item];

            fn deref(&self) -> &Self::Target {
                self.items()
            }
        }
        impl std::ops::DerefMut for $Ty {
            fn deref_mut(&mut self) -> &mut Self::Target {
                self.items_mut()
            }
        }
    };
}
pub(crate) use impl_hasitem_methods;

#[cfg_attr(feature = "fuzzing", derive(arbitrary::Arbitrary))]
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Crate {
    pub attrs: Vec<Attribute>,
    pub items: Vec<Item>,
}

impl EmptyItem for Crate {
    type Input = ();
    fn empty(_: impl Into<()>) -> Self {
        Self::new()
    }
}

impl HasItem for Crate {
    fn items(&self) -> &[Item] {
        &self.items
    }
    fn items_mut(&mut self) -> &mut Vec<Item> {
        &mut self.items
    }
}

impl_hasitem_methods!(Crate);

impl fmt::Display for Crate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Only inner attributes (#![...]) are valid at crate level.
        // Outer attributes (#[...]) must be attached to items and are
        // rendered by Item::Display, not here.
        for attr in self.attrs.iter() {
            if attr.is_inner() {
                writeln!(f, "{attr}")?;
            }
        }
        if !self.items.is_empty() {
            writeln!(f)?;
        }
        for item in self.items.iter() {
            writeln!(f, "{item}")?;
        }
        Ok(())
    }
}

impl From<Crate> for TokenStream {
    fn from(value: Crate) -> Self {
        let mut ts = TokenStream::new();
        for attr in value.attrs {
            if attr.is_inner() {
                ts.extend(TokenStream::from(attr));
            }
        }
        for item in value.items {
            ts.extend(TokenStream::from(item));
        }
        ts
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct CompileOptions {
    pub allow: Option<String>,
    pub cap_lints: Option<String>,
    pub cfg: Option<String>,
    pub check_cfg: Option<String>,
    pub crate_name: Option<String>,
    pub crate_type: Option<String>,
    pub deny: Option<String>,
    pub edition: Option<String>,
    pub emit: Option<String>,
    pub explain: Option<String>,
    pub forbid: Option<String>,
    pub force_warn: Option<String>,
    pub g: bool,
    pub o: bool,
    pub print: Option<String>,
    pub target: Option<String>,
    pub test: bool,
    pub out_dir: Option<String>,
    pub verbose: bool,
    pub warn: Option<String>,
}

impl Crate {
    pub fn new() -> Self {
        Self {
            attrs: Vec::new(),
            items: Vec::new(),
        }
    }

    pub fn dump(self, path: impl AsRef<Pt>) -> Result<(), std::io::Error> {
        let mut file = File::create(path)?;
        write!(file, "{self}")?;
        Ok(())
    }

    pub fn compile(
        self,
        rs_path: impl AsRef<Pt>,
        options: CompileOptions,
    ) -> Result<(), std::io::Error> {
        let rs_path = rs_path.as_ref();
        let mut file = File::create(rs_path)?;
        write!(file, "{self}")?;
        drop(file);
        let mut cmd = std::process::Command::new("rustc");
        if let Some(allow) = options.allow {
            cmd.arg("--allow").arg(allow);
        }
        if let Some(cap_lints) = options.cap_lints {
            cmd.arg("--cap-lints").arg(cap_lints);
        }
        if let Some(cfg) = options.cfg {
            cmd.arg("--cfg").arg(cfg);
        }
        if let Some(check_cfg) = options.check_cfg {
            cmd.arg("--check-cfg").arg(check_cfg);
        }
        if let Some(crate_name) = options.crate_name {
            cmd.arg("--crate-name").arg(crate_name);
        }
        if let Some(crate_type) = options.crate_type {
            cmd.arg("--crate-type").arg(crate_type);
        }
        if let Some(deny) = options.deny {
            cmd.arg("--deny").arg(deny);
        }
        if let Some(edition) = options.edition {
            cmd.arg("--edition").arg(edition);
        }
        if let Some(emit) = options.emit {
            cmd.arg("--emit").arg(emit);
        }
        if let Some(explain) = options.explain {
            cmd.arg("--explain").arg(explain);
        }
        if let Some(forbid) = options.forbid {
            cmd.arg("--forbid").arg(forbid);
        }
        if let Some(force_warn) = options.force_warn {
            cmd.arg("--force-warn").arg(force_warn);
        }
        if options.g {
            cmd.arg("-g");
        }
        if options.o {
            cmd.arg("-O");
        }
        if let Some(print) = options.print {
            cmd.arg("--print").arg(print);
        }
        if let Some(target) = options.target {
            cmd.arg("--target").arg(target);
        }
        if options.test {
            cmd.arg("--test");
        }
        if let Some(out_dir) = options.out_dir {
            cmd.arg("--out-dir").arg(out_dir);
        }
        if options.verbose {
            cmd.arg("--verbose");
        }
        if let Some(warn) = options.warn {
            cmd.arg("--warn").arg(warn);
        }
        cmd.arg(rs_path).output()?;
        Ok(())
    }
}

#[cfg(feature = "tokenize")]
impl_to_tokens!(Crate,);

#[cfg(feature = "syn")]
impl From<syn::File> for Crate {
    fn from(value: syn::File) -> Self {
        let attrs = value
            .attrs
            .into_iter()
            .map(|a| Attribute::from(AttributeItem::from(a)))
            .collect();
        let items = value.items.into_iter().map(Item::from).collect();
        Self { attrs, items }
    }
}

// =====================================================================
// Parser API (requires `syn` feature)
// =====================================================================

/// Error type for parse operations.
#[cfg(feature = "syn")]
#[derive(Debug)]
pub struct ParseError(syn::Error);

#[cfg(feature = "syn")]
impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[cfg(feature = "syn")]
impl std::error::Error for ParseError {}

#[cfg(feature = "syn")]
impl From<syn::Error> for ParseError {
    fn from(e: syn::Error) -> Self {
        Self(e)
    }
}

#[cfg(feature = "syn")]
impl Crate {
    /// Parse a Rust source string into a [`Crate`].
    ///
    /// This is a convenience wrapper around `syn::parse_file` followed by
    /// `Crate::from`.
    ///
    /// # Example
    ///
    /// ```
    /// # #[cfg(feature = "syn")]
    /// # {
    /// let krate = ruast::Crate::parse("fn main() {}").unwrap();
    /// assert_eq!(krate.items.len(), 1);
    /// # }
    /// ```
    pub fn parse(source: &str) -> Result<Self, ParseError> {
        let file = syn::parse_file(source)?;
        Ok(Self::from(file))
    }
}

#[cfg(feature = "syn")]
impl Expr {
    /// Parse a Rust expression string into an [`Expr`].
    ///
    /// # Example
    ///
    /// ```
    /// # #[cfg(feature = "syn")]
    /// # {
    /// let expr = ruast::Expr::parse("1 + 2 * 3").unwrap();
    /// assert_eq!(expr.to_string(), "1 + 2 * 3");
    /// # }
    /// ```
    pub fn parse(source: &str) -> Result<Self, ParseError> {
        let expr: syn::Expr = syn::parse_str(source)?;
        Ok(Self::from(expr))
    }
}

#[cfg(feature = "syn")]
impl Type {
    /// Parse a Rust type string into a [`Type`].
    ///
    /// # Example
    ///
    /// ```
    /// # #[cfg(feature = "syn")]
    /// # {
    /// let ty = ruast::Type::parse("Vec<(i32, String)>").unwrap();
    /// assert_eq!(ty.to_string(), "Vec<(i32, String)>");
    /// # }
    /// ```
    pub fn parse(source: &str) -> Result<Self, ParseError> {
        let ty: syn::Type = syn::parse_str(source)?;
        Ok(Self::from(ty))
    }
}

#[cfg(feature = "syn")]
impl Item {
    /// Parse a Rust item string into an [`Item`].
    ///
    /// # Example
    ///
    /// ```
    /// # #[cfg(feature = "syn")]
    /// # {
    /// let item = ruast::Item::parse("struct Foo { x: i32 }").unwrap();
    /// assert!(item.ident() == Some("Foo"));
    /// # }
    /// ```
    pub fn parse(source: &str) -> Result<Self, ParseError> {
        let item: syn::Item = syn::parse_str(source)?;
        Ok(Self::from(item))
    }
}

#[cfg(feature = "syn")]
impl Stmt {
    /// Parse a Rust statement string into a [`Stmt`].
    ///
    /// # Example
    ///
    /// ```
    /// # #[cfg(feature = "syn")]
    /// # {
    /// let stmt = ruast::Stmt::parse("let x: i32 = 42;").unwrap();
    /// assert_eq!(stmt.to_string(), "let x: i32 = 42;");
    /// # }
    /// ```
    pub fn parse(source: &str) -> Result<Self, ParseError> {
        let stmt: syn::Stmt = syn::parse_str(source)?;
        Ok(Self::from(stmt))
    }
}

#[cfg(feature = "syn")]
impl Block {
    /// Parse a Rust block string into a [`Block`].
    ///
    /// # Example
    ///
    /// ```
    /// # #[cfg(feature = "syn")]
    /// # {
    /// let block = ruast::Block::parse("{ let x = 1; x + 1 }").unwrap();
    /// assert_eq!(block.stmts.len(), 2);
    /// # }
    /// ```
    pub fn parse(source: &str) -> Result<Self, ParseError> {
        let block: syn::ExprBlock = syn::parse_str(source)?;
        Ok(Self::from(block.block))
    }
}

#[cfg(feature = "syn")]
impl Pat {
    /// Parse a Rust pattern string into a [`Pat`].
    ///
    /// # Example
    ///
    /// ```
    /// # #[cfg(feature = "syn")]
    /// # {
    /// let pat = ruast::Pat::parse("Some(x)").unwrap();
    /// assert_eq!(pat.to_string(), "Some(x)");
    /// # }
    /// ```
    pub fn parse(source: &str) -> Result<Self, ParseError> {
        // syn::Pat doesn't implement Parse directly; parse via a let binding
        let stmt: syn::Stmt = syn::parse_str(&format!("let {source} = ();"))?;
        match stmt {
            syn::Stmt::Local(local) => Ok(Self::from(local.pat)),
            _ => Err(
                syn::Error::new(syn::spanned::Spanned::span(&stmt), "expected a pattern").into(),
            ),
        }
    }
}
