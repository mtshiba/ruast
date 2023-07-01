use proc_macro2::{TokenStream, TokenTree, Literal};
use quote::ToTokens;

use crate::{Lit, LitKind};

impl ToTokens for Lit {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self.kind {
            LitKind::Str => tokens.extend([TokenTree::Literal(Literal::string(&self.symbol))]),
            _ => todo!(),
        }
    }
}
