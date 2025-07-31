use std::process::Command;

fn check_example(name: &'static str) {
    let output = Command::new("cargo")
        .args(["run", "--example", name])
        .output();
    let content = String::from_utf8(output.unwrap().stdout).unwrap();
    syn::parse_file(&content).unwrap();
}

#[test]
fn test_enum() {
    check_example("enum");
}

#[test]
fn test_hello() {
    check_example("hello");
}

#[test]
fn test_operation() {
    check_example("operation");
}

#[test]
fn test_struct() {
    check_example("struct");
}

#[test]
fn test_trait() {
    check_example("trait");
}

#[test]
fn test_use() {
    check_example("use");
}

#[cfg(feature = "tokenize")]
#[test]
fn test_tokenize() {
    check_example("tokenize");
}
