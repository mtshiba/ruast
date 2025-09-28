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
    let src = expr.to_string();

    match syn::parse_str::<syn::Expr>(src.as_str()) {
        Ok(_expr) => {}
        Err(err) => {
            panic!("failed to parse generated expr code: \"{src}\"\nexpr: {expr:?}\nerror: {err}")
        }
    }
});
