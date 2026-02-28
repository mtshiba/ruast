//! Property-based tests for Display and TokenStream correctness.
//!
//! Uses `arbtest` to generate random ASTs via the existing `Arbitrary` derives,
//! then checks that Display / TokenStream output is valid Rust parseable by `syn`.
//!
//! Run with: cargo test --test test_properties --features "fuzzing,tokenize,syn" -- --ignored

use arbitrary::Arbitrary;

// ---------------------------------------------------------------------------
// P1: Display → valid Rust
// ---------------------------------------------------------------------------

#[test]
#[ignore] // Run explicitly: cargo test -- --ignored
fn prop_expr_display_is_valid_rust() {
    arbtest::arbtest(|u| {
        ruast::depth_limiter::set(50);
        let expr = ruast::Expr::arbitrary(u)?;
        let src = expr.to_string();
        if let Err(e) = syn::parse_str::<syn::Expr>(&src) {
            panic!("Expr Display output is not valid Rust:\n  src: {src}\n  err: {e}");
        }
        Ok(())
    });
}

#[test]
#[ignore] // Run explicitly: cargo test -- --ignored
fn prop_type_display_is_valid_rust() {
    arbtest::arbtest(|u| {
        ruast::depth_limiter::set(50);
        let ty = ruast::Type::arbitrary(u)?;
        let src = ty.to_string();
        if let Err(e) = syn::parse_str::<syn::Type>(&src) {
            panic!("Type Display output is not valid Rust:\n  src: {src}\n  err: {e}");
        }
        Ok(())
    });
}

#[test]
#[ignore] // Run explicitly: cargo test -- --ignored
fn prop_crate_display_is_valid_rust() {
    arbtest::arbtest(|u| {
        ruast::depth_limiter::set(50);
        let krate = ruast::Crate::arbitrary(u)?;
        let src = krate.to_string();
        if let Err(e) = syn::parse_file(&src) {
            panic!("Crate Display output is not valid Rust:\n  src: {src}\n  err: {e}");
        }
        Ok(())
    });
}

// ---------------------------------------------------------------------------
// P2: TokenStream → valid Rust
// ---------------------------------------------------------------------------

#[test]
#[ignore] // Run explicitly: cargo test -- --ignored
fn prop_expr_tokenstream_is_valid_rust() {
    arbtest::arbtest(|u| {
        ruast::depth_limiter::set(50);
        let expr = ruast::Expr::arbitrary(u)?;
        let ts = ruast::TokenStream::from(expr);
        let src = ts.to_string();
        if let Err(e) = syn::parse_str::<syn::Expr>(&src) {
            panic!("Expr TokenStream output is not valid Rust:\n  src: {src}\n  err: {e}");
        }
        Ok(())
    });
}

#[test]
#[ignore] // Run explicitly: cargo test -- --ignored
fn prop_type_tokenstream_is_valid_rust() {
    arbtest::arbtest(|u| {
        ruast::depth_limiter::set(50);
        let ty = ruast::Type::arbitrary(u)?;
        let ts = ruast::TokenStream::from(ty);
        let src = ts.to_string();
        if let Err(e) = syn::parse_str::<syn::Type>(&src) {
            panic!("Type TokenStream output is not valid Rust:\n  src: {src}\n  err: {e}");
        }
        Ok(())
    });
}

// ---------------------------------------------------------------------------
// P3: Display ↔ TokenStream semantic consistency
//
// Both paths should parse to equivalent syn ASTs.
// We normalize via `quote::quote!` to compare.
// ---------------------------------------------------------------------------

#[test]
#[ignore] // Run explicitly: cargo test -- --ignored
fn prop_expr_display_and_tokenstream_are_consistent() {
    arbtest::arbtest(|u| {
        ruast::depth_limiter::set(50);
        let expr = ruast::Expr::arbitrary(u)?;
        let display_src = expr.to_string();
        let ts_src = ruast::TokenStream::from(expr).to_string();

        let display_parsed = syn::parse_str::<syn::Expr>(&display_src);
        let ts_parsed = syn::parse_str::<syn::Expr>(&ts_src);

        match (display_parsed, ts_parsed) {
            (Ok(d), Ok(t)) => {
                let d_norm = quote::quote!(#d).to_string();
                let t_norm = quote::quote!(#t).to_string();
                assert_eq!(
                    d_norm, t_norm,
                    "Display and TokenStream diverge:\n  display: {display_src}\n  tokens:  {ts_src}"
                );
            }
            (Err(e), _) => {
                panic!("Display parse failed: \"{display_src}\"\n{e}");
            }
            (_, Err(e)) => {
                panic!("TokenStream parse failed: \"{ts_src}\"\n{e}");
            }
        }
        Ok(())
    });
}

#[test]
#[ignore] // Run explicitly: cargo test -- --ignored
fn prop_type_display_and_tokenstream_are_consistent() {
    arbtest::arbtest(|u| {
        ruast::depth_limiter::set(50);
        let ty = ruast::Type::arbitrary(u)?;
        let display_src = ty.to_string();
        let ts_src = ruast::TokenStream::from(ty).to_string();

        let display_parsed = syn::parse_str::<syn::Type>(&display_src);
        let ts_parsed = syn::parse_str::<syn::Type>(&ts_src);

        match (display_parsed, ts_parsed) {
            (Ok(d), Ok(t)) => {
                let d_norm = quote::quote!(#d).to_string();
                let t_norm = quote::quote!(#t).to_string();
                assert_eq!(
                    d_norm, t_norm,
                    "Display and TokenStream diverge:\n  display: {display_src}\n  tokens:  {ts_src}"
                );
            }
            (Err(e), _) => {
                panic!("Display parse failed: \"{display_src}\"\n{e}");
            }
            (_, Err(e)) => {
                panic!("TokenStream parse failed: \"{ts_src}\"\n{e}");
            }
        }
        Ok(())
    });
}
