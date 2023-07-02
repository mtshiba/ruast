# `rast`

This crate provides a formattable & modifiable Rust AST.

## Basic usage

### Hello world

```rust
use rast::*;

let mut krate = Crate::new();
krate.add_item(Fn {
    ident: "main".to_string(),
    generics: vec![],
    fn_decl: FnDecl::new(vec![], None),
    body: Some(Block::from(
        Stmt::Expr(Expr::new(MacCall {
            path: Path::single("println!"),
            args: DelimArgs::from(vec![Token::lit("Hello, world!")]),
        })),
    )),
});
println!("{krate}");
krate.dump("test.rs")?;
krate.remove_item_by_id("main");
assert!(krate.is_empty());
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
krate.dump("test.rs")?;
krate.remove_item_by_id("main");
assert!(krate.is_empty());
```

### Building struct, enum, and impl

```rust
use rast::*;

let mut krate = Crate::new();
let def = StructDef::empty("Foo")
    .with_field(FieldDef::inherited("foo", Type::from("u32")))
    .with_field(FieldDef::inherited("bar", Type::from("u32")));
krate.add_item(def);
let imp = Impl::empty("Foo")
    .with_item(Fn::empty_method("test", Pat::ref_self()));
krate.add_item(imp);
println!("{krate}");
```

```rust
use rast::*;

let mut krate = Crate::new();
let def = EnumDef::empty("Foo")
    .with_variant(Variant::empty("Bar"))
    .with_variant(Variant::tuple("Baz", vec![FieldDef::anonymous("u32")]));
krate.add_item(def);
let imp = Impl::empty("Foo")
    .with_item(Fn::empty_method("test", Pat::ref_self()));
krate.add_item(imp);
println!("{krate}");
```

## Why this is needed?

[The Rust project](https://github.com/rust-lang/rust) has a submodule called [`rustc_ast`](https://github.com/rust-lang/rust/tree/master/compiler/rustc_ast) that defines an AST, but it is not published on crates.io and requires a huge build of `rust` itself. Also, `rustc_ast` is not designed for third parties to build ASTs by hand.

There is a [`codegen`](https://github.com/carllerche/codegen) crate for Rust code generation, but this crate has not been maintained for some time and only supports basic syntax elements.

There is also a [`syn`](https://github.com/dtolnay/syn) crate that can parse `proc_macro::TokenStream` into an AST, but its AST elements don't implement `Display` trait and are not designed for direct manipulation.

## Goals

The goal of this project is to provide a simple and portable Rust AST building/Rust code generation library.

## Non-goals

This library is not directly related to the Rust compiler AST, and ASTs built with this library cannot be directly given as input to the compiler.

## License

This project is licensed under either of [Apache license version 2.0](./LICENSE-APACHE) or [MIT license](./LICENSE-MIT) at your option.
