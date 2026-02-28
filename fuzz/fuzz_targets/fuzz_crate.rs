#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|bytes: &[u8]| {
    use arbitrary::Arbitrary;

    let mut u = arbitrary::Unstructured::new(bytes);
    ruast::depth_limiter::set(50);
    let krate = match ruast::Crate::arbitrary(&mut u) {
        Ok(k) => k,
        Err(_) => return,
    };
    let src = krate.to_string();

    match syn::parse_file(src.as_str()) {
        Ok(_) => {}
        Err(err) => {
            panic!("failed to parse generated crate code: \"{src}\"\ncrate: {krate:?}\nerror: {err}")
        }
    }
});
