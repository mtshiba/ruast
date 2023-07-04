//! WIP: implement conversions between `rast::*` and `proc_macro2::TokenStream`

use proc_macro2::{TokenStream, TokenTree, Literal, Punct, Spacing, Ident, Span};
use quote::ToTokens;

use crate::{Lit, LitKind, Token, BinOpToken, Delimiter};

impl BinOpToken {
    fn to_tokens_spacing(&self, spacing: Spacing, tokens: &mut TokenStream) {
        match self {
            Self::Plus => tokens.extend([TokenTree::Punct(Punct::new('+', spacing))]),
            Self::Minus => tokens.extend([TokenTree::Punct(Punct::new('-', spacing))]),
            Self::Star => tokens.extend([TokenTree::Punct(Punct::new('*', spacing))]),
            Self::Slash => tokens.extend([TokenTree::Punct(Punct::new('/', spacing))]),
            Self::Percent => tokens.extend([TokenTree::Punct(Punct::new('%', spacing))]),
            Self::Caret => tokens.extend([TokenTree::Punct(Punct::new('^', spacing))]),
            Self::And => tokens.extend([TokenTree::Punct(Punct::new('&', spacing))]),
            Self::Or => tokens.extend([TokenTree::Punct(Punct::new('|', spacing))]),
            Self::Shl => tokens.extend([TokenTree::Punct(Punct::new('<', Spacing::Joint)), TokenTree::Punct(Punct::new('<', spacing))]),
            Self::Shr => tokens.extend([TokenTree::Punct(Punct::new('>', Spacing::Joint)), TokenTree::Punct(Punct::new('>', spacing))]),
        }
    }
}

impl ToTokens for BinOpToken {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.to_tokens_spacing(Spacing::Alone, tokens)
    }
}

impl ToTokens for Token {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Eq => tokens.extend([TokenTree::Punct(Punct::new('=', Spacing::Alone))]),
            Self::Lt => tokens.extend([TokenTree::Punct(Punct::new('<', Spacing::Alone))]),
            Self::Le => tokens.extend([TokenTree::Punct(Punct::new('<', Spacing::Joint)), TokenTree::Punct(Punct::new('=', Spacing::Alone))]),
            Self::EqEq => tokens.extend([TokenTree::Punct(Punct::new('=', Spacing::Joint)), TokenTree::Punct(Punct::new('=', Spacing::Alone))]),
            Self::Ne => tokens.extend([TokenTree::Punct(Punct::new('!', Spacing::Joint)), TokenTree::Punct(Punct::new('=', Spacing::Alone))]),
            Self::Gt => tokens.extend([TokenTree::Punct(Punct::new('>', Spacing::Alone))]),
            Self::Ge => tokens.extend([TokenTree::Punct(Punct::new('>', Spacing::Joint)), TokenTree::Punct(Punct::new('=', Spacing::Alone))]),
            Self::AndAnd => tokens.extend([TokenTree::Punct(Punct::new('&', Spacing::Joint)), TokenTree::Punct(Punct::new('&', Spacing::Alone))]),
            Self::OrOr => tokens.extend([TokenTree::Punct(Punct::new('|', Spacing::Joint)), TokenTree::Punct(Punct::new('|', Spacing::Alone))]),
            Self::Not => tokens.extend([TokenTree::Punct(Punct::new('!', Spacing::Alone))]),
            Self::Tilde => tokens.extend([TokenTree::Punct(Punct::new('~', Spacing::Alone))]),
            Self::BinOp(bin) => bin.to_tokens(tokens),
            Self::BinOpEq(bin) => {
                bin.to_tokens_spacing(Spacing::Joint, tokens);
                tokens.extend([TokenTree::Punct(Punct::new('=', Spacing::Alone))])
            },
            Self::At => tokens.extend([TokenTree::Punct(Punct::new('@', Spacing::Alone))]),
            Self::Dot => tokens.extend([TokenTree::Punct(Punct::new('.', Spacing::Alone))]),
            Self::DotDot => tokens.extend([TokenTree::Punct(Punct::new('.', Spacing::Joint)), TokenTree::Punct(Punct::new('.', Spacing::Alone))]),
            Self::DotDotEq => tokens.extend([TokenTree::Punct(Punct::new('.', Spacing::Joint)), TokenTree::Punct(Punct::new('.', Spacing::Joint)), TokenTree::Punct(Punct::new('=', Spacing::Alone))]),
            Self::Comma => tokens.extend([TokenTree::Punct(Punct::new(',', Spacing::Alone))]),
            Self::Semi => tokens.extend([TokenTree::Punct(Punct::new(';', Spacing::Alone))]),
            Self::Colon => tokens.extend([TokenTree::Punct(Punct::new(':', Spacing::Alone))]),
            Self::ModSep => tokens.extend([TokenTree::Punct(Punct::new(':', Spacing::Joint)), TokenTree::Punct(Punct::new(':', Spacing::Alone))]),
            Self::LArrow => tokens.extend([TokenTree::Punct(Punct::new('<', Spacing::Joint)), TokenTree::Punct(Punct::new('-', Spacing::Alone))]),
            Self::RArrow => tokens.extend([TokenTree::Punct(Punct::new('-', Spacing::Joint)), TokenTree::Punct(Punct::new('>', Spacing::Alone))]),
            Self::FatArrow => tokens.extend([TokenTree::Punct(Punct::new('=', Spacing::Joint)), TokenTree::Punct(Punct::new('>', Spacing::Alone))]),
            Self::Pound => tokens.extend([TokenTree::Punct(Punct::new('#', Spacing::Alone))]),
            Self::Dollar => tokens.extend([TokenTree::Punct(Punct::new('$', Spacing::Alone))]),
            Self::Question => tokens.extend([TokenTree::Punct(Punct::new('?', Spacing::Alone))]),
            Self::SingleQuote => tokens.extend([TokenTree::Punct(Punct::new('\'', Spacing::Alone))]),
            Self::OpenDelim(delim) => match delim {
                Delimiter::Parenthesis => tokens.extend([TokenTree::Punct(Punct::new('(', Spacing::Alone))]),
                Delimiter::Brace => tokens.extend([TokenTree::Punct(Punct::new('{', Spacing::Alone))]),
                Delimiter::Bracket => tokens.extend([TokenTree::Punct(Punct::new('[', Spacing::Alone))]),
                _ => todo!(),
            },
            Self::CloseDelim(delim) => match delim {
                Delimiter::Parenthesis => tokens.extend([TokenTree::Punct(Punct::new(')', Spacing::Alone))]),
                Delimiter::Brace => tokens.extend([TokenTree::Punct(Punct::new('}', Spacing::Alone))]),
                Delimiter::Bracket => tokens.extend([TokenTree::Punct(Punct::new(']', Spacing::Alone))]),
                _ => todo!(),
            },
            Self::Lit(lit) => lit.to_tokens(tokens),
            Self::Ident(ident) => tokens.extend([TokenTree::Ident(Ident::new(ident, Span::call_site()))]),
            Self::Lifetime(lifetime) => tokens.extend([TokenTree::Ident(Ident::new(lifetime, Span::call_site()))]),
            Self::Eof => {},
        }
    }
}

impl ToTokens for Lit {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self.kind {
            LitKind::Integer => tokens.extend([TokenTree::Literal(Literal::isize_unsuffixed(self.symbol.parse().unwrap()))]),
            LitKind::Float => tokens.extend([TokenTree::Literal(Literal::f64_unsuffixed(self.symbol.parse().unwrap()))]),
            LitKind::Char => tokens.extend([TokenTree::Literal(Literal::character(self.symbol.chars().next().unwrap()))]),
            LitKind::Str => tokens.extend([TokenTree::Literal(Literal::string(&self.symbol))]),
            _ => todo!(),
        }
    }
}
