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

            pub fn add_item(&mut self, item: impl Into<Item>) {
                HasItem::add_item(self, item);
            }

            pub fn remove_item(&mut self, index: usize) -> Option<Item> {
                HasItem::remove_item(self, index)
            }

            pub fn remove_item_by_id(&mut self, ident: &str) -> Option<Item> {
                HasItem::remove_item_by_id(self, ident)
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
    };
    ($Ty: ident, $Item: ident) => {
        impl $Ty {
            pub fn with_item(self, item: impl Into<$Item>) -> Self {
                HasItem::with_item(self, item)
            }

            pub fn add_item(&mut self, item: impl Into<$Item>) {
                HasItem::add_item(self, item);
            }

            pub fn remove_item(&mut self, index: usize) -> Option<$Item> {
                HasItem::remove_item(self, index)
            }

            pub fn remove_item_by_id(&mut self, ident: &str) -> Option<$Item> {
                HasItem::remove_item_by_id(self, ident)
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
            ts.extend(TokenStream::from(attr));
        }
        for item in value.items {
            ts.extend(TokenStream::from(item));
        }
        ts
    }
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
        write!(file, "{}", self)?;
        Ok(())
    }
}

#[cfg(feature = "tokenize")]
impl_to_tokens!(Crate,);
