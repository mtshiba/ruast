//! WIP: implement conversions between `rast::*` and `proc_macro2::TokenStream`

use proc_macro2::{TokenStream, TokenTree, Literal, Punct, Spacing, Ident, Span};
use quote::ToTokens;

use crate::{Lit, LitKind, Token};

impl ToTokens for Token {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Eq => tokens.extend([TokenTree::Punct(Punct::new('=', Spacing::Joint)), TokenTree::Punct(Punct::new('=', Spacing::Alone))]),
            Self::Plus => tokens.extend([TokenTree::Punct(Punct::new('+', Spacing::Alone))]),
            Self::Comma => tokens.extend([TokenTree::Punct(Punct::new(',', Spacing::Alone))]),
            Self::Semi => tokens.extend([TokenTree::Punct(Punct::new(';', Spacing::Alone))]),
            Self::Lit(lit) => lit.to_tokens(tokens),
            Self::Ident(ident) => tokens.extend([TokenTree::Ident(Ident::new(ident, Span::call_site()))]),
        }
    }
}

impl ToTokens for Lit {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self.kind {
            LitKind::Str => tokens.extend([TokenTree::Literal(Literal::string(&self.symbol))]),
            _ => todo!(),
        }
    }
}
