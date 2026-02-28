#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|bytes: &[u8]| {
    use arbitrary::Arbitrary;

    let mut u = arbitrary::Unstructured::new(bytes);
    ruast::depth_limiter::set(50);
    let ty = match ruast::Type::arbitrary(&mut u) {
        Ok(ty) => ty,
        Err(_) => return,
    };
    let src = ty.to_string();

    match syn::parse_str::<syn::Type>(src.as_str()) {
        Ok(_) => {}
        Err(err) => {
            panic!("failed to parse generated type code: \"{src}\"\ntype: {ty:?}\nerror: {err}")
        }
    }
});
