# `ruast`

This crate provides a printable & modifiable Rust AST.

## Basic usage

### Hello world

```rust
use ruast::*;

let mut krate = Crate::new();
let def = Fn::main(
    None,
    Block::from(
        Path::single("println")
            .mac_call(vec![Token::lit("Hello, world!")])
            .semi(),
    ),
);
krate.add_item(def);
println!("{krate}");
// krate.dump("test.rs")?;
// krate.compile("test.rs", CompileOptions::default())?;
krate.remove_item_by_id("main");
assert!(krate.is_empty());
```

This is equivalent to:

```rust
use ruast::*;

let mut krate = Crate::new();
krate.add_item(Fn {
    is_unsafe: false,
    is_const: false,
    is_async: false,
    abi: None,
    ident: "main".to_string(),
    generics: vec![],
    fn_decl: FnDecl::new(vec![], None),
    body: Some(Block::from(Stmt::Semi(Semi::new(Expr::new(MacCall {
        path: Path::single("println"),
        args: DelimArgs::from(vec![Token::lit("Hello, world!")]),
    }))))),
});
println!("{krate}");
// krate.dump("test.rs")?;
// krate.compile("test.rs", CompileOptions::default())?;
krate.remove_item_by_id("main");
assert!(krate.is_empty());
```

```rust
> cargo run --example hello

fn main() {
    println!("Hello, world!");
}
```

### Building struct, enum, trait and impl

The source code is available in the [`examples`](https://github.com/mtshiba/ruast/tree/main/examples) directory.

```rust
use ruast::*;

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
use ruast::*;

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

```rust
use ruast::*;

let mut krate = Crate::new();

let partial_eq = Type::simple_path("PartialEq");
let trait_def = TraitDef::new("Eq", vec![], vec![partial_eq], vec![]);
krate.add_item(trait_def);
let arg_t = GenericArg::Type(Type::simple_path("T"));
let eq_bound = GenericBound::Trait(PolyTraitRef::simple("Eq"));
let eq = Type::simple_path("Eq");
let param_t = GenericParam::new("T", vec![eq_bound]);
let self_ty = Type::poly_path("Vec", vec![arg_t]);
let imp = Impl::trait_impl(vec![param_t], self_ty, eq, None, vec![]);
krate.add_item(imp);
println!("{krate}");
```

### Convert to `proc_macro2::TokenStream`

By enabling a feature `tokenize`, you can convert `ruast` ASTs to `proc_macro2::TokenStream`.

You can build ASTs systematically without using `syn` or `quote` macros.

```rust
use ruast::*;

let mut krate = Crate::new();
let def = Fn::main(
    None,
    Block::from(Path::single("println").mac_call(vec![Token::lit("Hello, world!")])),
);
krate.add_item(def);
let tokens = krate.to_token_stream();
println!("{krate}");
println!("{tokens}");
```

You can also find examples on how to create a proc macro using this crate in [`examples/proc_macro_example`](https://github.com/mtshiba/ruast/tree/main/examples/proc_macro_example).

## Why this is needed?

[The Rust project](https://github.com/rust-lang/rust) has a submodule called [`rustc_ast`](https://github.com/rust-lang/rust/tree/master/compiler/rustc_ast) that defines an AST, but it is not published on crates.io and requires a huge build of `rust` itself. Also, `rustc_ast` is not designed for third parties to build ASTs by hand.

There is a [`codegen`](https://github.com/carllerche/codegen) crate for Rust code generation, but this crate has not been maintained for some time and only supports basic syntax elements.

There is also a [`syn`](https://github.com/dtolnay/syn) crate that can parse `proc_macro::TokenStream` into an AST, but its AST elements don't implement `Display` trait and are not designed for direct construction & modification.

## Goals

The goal of this project is to provide a simple and portable Rust AST building/Rust code generation library.

## Non-goals

This library is not directly related to the Rust compiler AST, and ASTs built with this library cannot be directly given as input to the compiler.

## License

This project is licensed under either of [Apache license version 2.0](./LICENSE-APACHE) or [MIT license](./LICENSE-MIT) at your option.
