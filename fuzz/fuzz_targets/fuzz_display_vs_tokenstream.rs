#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|bytes: &[u8]| {
    use arbitrary::Arbitrary;

    let mut u = arbitrary::Unstructured::new(bytes);
    ruast::depth_limiter::set(50);
    let expr = match ruast::Expr::arbitrary(&mut u) {
        Ok(expr) => expr,
        Err(_) => return,
    };

    let display_src = expr.to_string();
    let ts_src = ruast::TokenStream::from(expr).to_string();

    let display_parsed = syn::parse_str::<syn::Expr>(&display_src);
    let ts_parsed = syn::parse_str::<syn::Expr>(&ts_src);

    match (display_parsed, ts_parsed) {
        (Ok(d), Ok(t)) => {
            let d_norm = quote::quote!(#d).to_string();
            let t_norm = quote::quote!(#t).to_string();
            if d_norm != t_norm {
                panic!(
                    "Display and TokenStream produce different ASTs:\n\
                     display: {display_src}\n\
                     tokens:  {ts_src}\n\
                     display_norm: {d_norm}\n\
                     tokens_norm:  {t_norm}"
                );
            }
        }
        (Err(e), _) => {
            panic!("Display output failed to parse: \"{display_src}\"\n{e}");
        }
        (_, Err(e)) => {
            panic!("TokenStream output failed to parse: \"{ts_src}\"\n{e}");
        }
    }
});
