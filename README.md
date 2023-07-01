# `rast`

Zero-dependency formattable Rust AST library

## Basic usage

```rust
use rast::*;

let mut krate = Crate::new();
krate.add_item(Fn {
    ident: "main".to_string(),
    generics: vec![],
    inputs: vec![],
    output: None,
    body: Some(Block::from(
        Stmt::Expr(Expr::new(MacCall {
            path: Path::single("println!"),
            args: DelimArgs::from(vec![Token::lit("Hello, world!")]),
        })),
    )),
});
println!("crate:\n{krate}");
println!("main:\n{}", krate[0]);
```

more simply:

```rust
use rast::*;

let mut krate = Crate::new();
let def = Fn::main(
    None,
    Block::from(MacCall::new(
        Path::single("println"),
        vec![Token::lit("Hello, world!")],
    ))
);
krate.add_item(def);
println!("{krate}");
println!("{}", krate[0]);
```

## Why this is needed?

[The Rust project](https://github.com/rust-lang/rust) has a submodule called [`rustc_ast`](https://github.com/rust-lang/rust/tree/master/compiler/rustc_ast) that defines an AST, but it is not published on crates.io and requires a huge build of `rust` itself. Also, `rustc_ast` is not designed for third parties to build ASTs by hand.

There is a [`codegen`](https://github.com/carllerche/codegen) crate for Rust code generation, but this crate has not been maintained for some time and only supports basic syntax elements.

## Goals

The goal of this project is to provide a simple and portable Rust AST building/Rust code generation library.

## Non-goals

This library is not directly related to the Rust compiler AST, and ASTs built with this library cannot be directly given as input to the compiler.
