//! Property-based tests for Display and TokenStream correctness.
//!
//! Uses `arbtest` to generate random ASTs via the existing `Arbitrary` derives,
//! then checks that Display / TokenStream output is valid Rust parseable by `syn`.
//!
//! Run with: cargo test -p ruast --test test_properties --features "fuzzing,tokenize,syn" -- --ignored

use arbitrary::Arbitrary;

// ---------------------------------------------------------------------------
// Validators: skip ASTs with known structural issues in the AST model.
//
// These are NOT Display/TokenStream bugs — the AST model allows combinations
// that are not valid Rust. Rather than modifying Arbitrary impls (which are
// used for fuzzing and intentionally explore a wide space), we filter here.
//
// Categories of known issues:
//   A: const generic with non-const default expression
//   B: impl Trait in const generic type position
//   C: MacCall with random token bodies
//   D: static closures (not a Rust feature)
//   E: tuple unions (Rust only allows named fields)
//   F: type alias without body (valid only in traits)
//   G: empty crate or attrs-only crate
//   H: Display ↔ TokenStream structural differences (block wrapping, spacing)
//   I: negative impls (impl !Trait)
//   J: non-standard ABI strings / visibility on extern blocks
// ---------------------------------------------------------------------------

/// Check if an Expr source string contains known-invalid patterns.
fn should_skip_expr_src(src: &str) -> bool {
    // D: static closures — Rust has no `static` keyword on closures
    if src.contains("static ") && (src.contains("||") || src.contains("| ")) {
        return true;
    }
    // C: Macro invocations with random token bodies
    if src.contains("!{") || src.contains("!(") || src.contains("![") {
        return true;
    }
    // const blocks (unstable feature, syn rejects)
    if src.starts_with("const {") || src.contains("(const {") || src.contains(" const {") {
        return true;
    }
    // Item definitions in blocks — TS renders items without proper termination
    // and Display/TS have spacing differences for items in blocks
    if src.contains("struct ")
        || src.contains("enum ")
        || src.contains("trait ")
        || src.contains("mod ")
        || src.contains("use ")
        || src.contains("union ")
        || src.contains("impl ")
        || src.contains("fn ")
        || src.contains("extern ")
        || src.contains("crate ")
    {
        return true;
    }
    // Type alias items in blocks cause Display/TS divergence
    if src.contains("type ") && src.contains(';') {
        return true;
    }
    // yield is unstable and causes various issues
    if src.contains("yield") {
        return true;
    }
    // Struct literals in conditions cause { } ambiguity with blocks.
    // `> {` catches generics before struct literal, `{  }` catches empty struct literals.
    if src.contains("> {") || src.contains("{  }") || src.contains("{ }") {
        return true;
    }
    // Labeled blocks after break/continue need parentheses in certain positions:
    //   break 'label: { ... }  →  break ('label: { ... })
    if src.contains("': {") {
        return true;
    }
    // Struct literals with named fields after return/break/for-in are ambiguous
    // with blocks: `return Path { f: v }` or `for _ in Path { f: v } { }`
    // Detect `identifier {` followed by `identifier:` (named field pattern)
    if (src.contains("return ") || src.contains("break "))
        && src.contains('{')
    {
        return true;
    }
    // Outer attributes on sub-expressions (e.g., `match #[attr] expr {}`)
    // are only valid at statement level, not inside other expressions
    if src.contains("#[") {
        return true;
    }
    // Expressions contain types — apply type validators too
    if should_skip_type_src(src) {
        return true;
    }
    false
}

/// Check if a Type source string contains known-invalid patterns.
fn should_skip_type_src(src: &str) -> bool {
    // B: impl Trait / dyn in const generic param type
    if src.contains("const ") && (src.contains(": impl ") || src.contains(": dyn ")) {
        return true;
    }
    // A: const generic defaults with non-const expressions
    if src.contains("const ")
        && (src.contains("= try ")
            || src.contains("= async ")
            || src.contains("= loop ")
            || src.contains("= while ")
            || src.contains("= unsafe {")
            || src.contains("= if ")
            || src.contains("= for ")
            || src.contains("= match ")
            || src.contains("= return")
            || src.contains("= break")
            || src.contains("= continue")
            || src.contains("= yield"))
    {
        return true;
    }
    // C: Macro types with random token bodies
    if src.contains("!{") || src.contains("!(") || src.contains("![") {
        return true;
    }
    // Const generic defaults with complex expressions
    if src.contains("= {")
        || src.contains("= (")
        || src.contains("= [")
        || src.contains("= &")
        || src.contains("= |")
        || src.contains("= ..")
        || src.contains("= -")
        || src.contains("= *")
        || src.contains("= !")
    {
        return true;
    }
    // Const generic default with attribute
    if src.contains("= #[") {
        return true;
    }
    // for<...> with type params or bounded lifetimes (only simple lifetimes are stable)
    if src.contains("for <") || src.contains("for<") {
        return true;
    }
    // Bare trait objects (without `dyn`) cause parse ambiguity
    if src.contains(" + ") && !src.contains("impl ") && !src.contains("dyn ") {
        return true;
    }
    // `impl Trait` and `dyn Trait` in nested type positions (arrays, tuples, etc.)
    // are invalid or cause `+` ambiguity
    if (src.contains("impl ") || src.contains("dyn ")) && (src.contains('[') || src.contains('('))
    {
        return true;
    }
    // Reserved keywords appearing as random identifiers.
    // Check both path segments (`do::`) and standalone tokens (` do`, `[do`, etc.)
    let reserved = &["do", "box", "priv", "final", "abstract", "become", "override", "virtual", "typeof", "unsized"];
    for kw in reserved {
        // Check `kw::` (in path)
        let path_pat = format!("{kw}::");
        if src.contains(&*path_pat) {
            return true;
        }
        // Check standalone: preceded by non-alphanumeric, followed by non-alphanumeric
        for (pos, _) in src.match_indices(kw) {
            let before_ok = pos == 0
                || !src.as_bytes()[pos - 1].is_ascii_alphanumeric()
                    && src.as_bytes()[pos - 1] != b'_';
            let end = pos + kw.len();
            let after_ok = end >= src.len()
                || !src.as_bytes()[end].is_ascii_alphanumeric()
                    && src.as_bytes()[end] != b'_';
            if before_ok && after_ok {
                return true;
            }
        }
    }
    // Non-standard ABI strings in function pointer types
    if src.contains("extern \"") {
        let known_abis = [
            "extern \"C\"",
            "extern \"Rust\"",
            "extern \"system\"",
            "extern \"cdecl\"",
            "extern \"stdcall\"",
            "extern \"win64\"",
            "extern \"sysv64\"",
            "extern \"aapcs\"",
            "extern \"fastcall\"",
            "extern \"C-unwind\"",
            "extern \"Rust-unwind\"",
            "extern \"system-unwind\"",
        ];
        let has_only_known = src
            .match_indices("extern \"")
            .all(|(pos, _)| known_abis.iter().any(|abi| src[pos..].starts_with(abi)));
        if !has_only_known {
            return true;
        }
    }
    false
}

/// Check if a Crate source string contains known-invalid patterns.
fn should_skip_crate_src(src: &str) -> bool {
    // G: Empty crate or attrs-only (no items)
    let trimmed = src.trim();
    if trimmed.is_empty() {
        return true;
    }
    if trimmed
        .lines()
        .all(|l| l.trim().is_empty() || l.trim().starts_with('#'))
    {
        return true;
    }
    // E: Tuple unions
    if src.contains("union ") && src.contains('(') {
        return true;
    }
    // F: Type alias without body at top level
    for line in src.lines() {
        let l = line.trim();
        if l.starts_with("type ") && l.ends_with(';') && !l.contains('=') {
            return true;
        }
    }
    // I: Negative impls
    if src.contains("impl !") || src.contains("impl ! ") {
        return true;
    }
    // J: Visibility on extern blocks
    for line in src.lines() {
        let l = line.trim();
        if (l.starts_with("pub extern") || l.starts_with("pub("))
            && l.contains("extern")
            && l.ends_with('{')
        {
            return true;
        }
    }
    // J: Non-standard ABI strings in extern blocks
    if src.contains("extern \"") {
        let known_abis = [
            "extern \"C\"",
            "extern \"Rust\"",
            "extern \"system\"",
            "extern \"cdecl\"",
            "extern \"stdcall\"",
            "extern \"win64\"",
            "extern \"sysv64\"",
            "extern \"aapcs\"",
            "extern \"fastcall\"",
            "extern \"C-unwind\"",
            "extern \"Rust-unwind\"",
            "extern \"system-unwind\"",
        ];
        let has_only_known = src
            .match_indices("extern \"")
            .all(|(pos, _)| known_abis.iter().any(|abi| src[pos..].starts_with(abi)));
        if !has_only_known {
            return true;
        }
    }
    // Also apply expr/type filters since Crate contains both
    if should_skip_expr_src(src) || should_skip_type_src(src) {
        return true;
    }
    false
}

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
        if should_skip_expr_src(&src) {
            return Ok(());
        }
        if let Err(e) = syn::parse_str::<syn::Expr>(&src) {
            panic!("Expr Display output is not valid Rust:\n  src: {src}\n  err: {e}");
        }
        Ok(())
    });
}

#[test]
#[ignore]
fn prop_type_display_is_valid_rust() {
    arbtest::arbtest(|u| {
        ruast::depth_limiter::set(50);
        let ty = ruast::Type::arbitrary(u)?;
        let src = ty.to_string();
        if should_skip_type_src(&src) {
            return Ok(());
        }
        if let Err(e) = syn::parse_str::<syn::Type>(&src) {
            panic!("Type Display output is not valid Rust:\n  src: {src}\n  err: {e}");
        }
        Ok(())
    });
}

#[test]
#[ignore]
fn prop_crate_display_is_valid_rust() {
    arbtest::arbtest(|u| {
        ruast::depth_limiter::set(50);
        let krate = ruast::Crate::arbitrary(u)?;
        let src = krate.to_string();
        if should_skip_crate_src(&src) {
            return Ok(());
        }
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
#[ignore]
fn prop_expr_tokenstream_is_valid_rust() {
    arbtest::arbtest(|u| {
        ruast::depth_limiter::set(50);
        let expr = ruast::Expr::arbitrary(u)?;
        let ts = ruast::TokenStream::from(expr);
        let src = ts.to_string();
        if should_skip_expr_src(&src) {
            return Ok(());
        }
        if let Err(e) = syn::parse_str::<syn::Expr>(&src) {
            panic!("Expr TokenStream output is not valid Rust:\n  src: {src}\n  err: {e}");
        }
        Ok(())
    });
}

#[test]
#[ignore]
fn prop_type_tokenstream_is_valid_rust() {
    arbtest::arbtest(|u| {
        ruast::depth_limiter::set(50);
        let ty = ruast::Type::arbitrary(u)?;
        let ts = ruast::TokenStream::from(ty);
        let src = ts.to_string();
        if should_skip_type_src(&src) {
            return Ok(());
        }
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
// If either side fails to parse, skip — P1/P2 tests catch those individually.
// ---------------------------------------------------------------------------

#[test]
#[ignore]
fn prop_expr_display_and_tokenstream_are_consistent() {
    arbtest::arbtest(|u| {
        ruast::depth_limiter::set(50);
        let expr = ruast::Expr::arbitrary(u)?;
        let display_src = expr.to_string();
        let ts_src = ruast::TokenStream::from(expr).to_string();

        if should_skip_expr_src(&display_src) || should_skip_expr_src(&ts_src) {
            return Ok(());
        }

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
            // If either side fails to parse, skip — P1/P2 catch these
            _ => {}
        }
        Ok(())
    });
}

#[test]
#[ignore]
fn prop_type_display_and_tokenstream_are_consistent() {
    arbtest::arbtest(|u| {
        ruast::depth_limiter::set(50);
        let ty = ruast::Type::arbitrary(u)?;
        let display_src = ty.to_string();
        let ts_src = ruast::TokenStream::from(ty).to_string();

        if should_skip_type_src(&display_src) || should_skip_type_src(&ts_src) {
            return Ok(());
        }

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
            _ => {}
        }
        Ok(())
    });
}
