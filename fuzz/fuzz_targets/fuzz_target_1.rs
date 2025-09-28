#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|expr: ruast::Expr| {
    let src = expr.to_string();

    match syn::parse_str::<syn::Expr>(src.as_str()) {
        Ok(_expr) => {}
        Err(err) => panic!("failed to parse generated expr code: \"{src}\"\nerror: {err}"),
    }
});
