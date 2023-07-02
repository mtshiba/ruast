mod expr;
mod stmt;
mod token;
mod ty;
#[cfg(feature = "conversion")]
mod conversion;

use std::io::Write;
use std::path::Path as Pt;
use std::fs::File;
use std::fmt;
use std::ops::{Index, IndexMut};

pub use expr::*;
pub use stmt::*;
pub use token::*;
pub use ty::*;

#[macro_export]
macro_rules! impl_obvious_conversion {
    ($Enum: ident; $($Variant: ident $(,)?)*) => {
        $(
            impl From<$Variant> for $Enum {
                fn from(item: $Variant) -> Self {
                    Self::$Variant(item)
                }
            }
        )*
    };
}

#[macro_export]
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

#[macro_export]
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

            pub fn get_item(&self, index: usize) -> Option<&Item> {
                HasItem::get_item(self, index)
            }

            pub fn get_item_by_id(&self, ident: &str) -> Option<&Item> {
                HasItem::get_item_by_id(self, ident)
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

            pub fn get_item(&self, index: usize) -> Option<&$Item> {
                HasItem::get_item(self, index)
            }

            pub fn get_item_by_id(&self, ident: &str) -> Option<&$Item> {
                HasItem::get_item_by_id(self, ident)
            }
        }
    };
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Crate {
    pub attrs: Vec<Attribute>,
    pub items: Vec<Item>,
}

impl Index<usize> for Crate {
    type Output = Item;

    fn index(&self, index: usize) -> &Self::Output {
        &self.items[index]
    }
}

impl IndexMut<usize> for Crate {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.items[index]
    }
}

impl Empty for Crate {
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
