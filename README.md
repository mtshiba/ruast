# `ruast`

This crate provides a printable & modifiable Rust AST.

## Features

* [x] Indentation-aware pretty-printing
* [x] Operator precedence-aware pretty-printing
* [x] Fuzzable AST nodes (with `arbitrary` crate)

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
krate.try_remove_item_by_id("main");
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
krate.try_remove_item_by_id("main");
assert!(krate.is_empty());
```

```rust
> cargo run --example hello

fn main() {
    println!("Hello, world!");
}
```

### Operations

```rust
use ruast::*;

let lhs = Lit::int("1");
let rhs = Lit::int("2");
let add = lhs.clone().add(rhs.clone());
assert_snapshot!(add, @"1 + 2");

let add_add = add.clone().add(Lit::int("3"));
assert_snapshot!(add_add, @"1 + 2 + 3");

let mul_add = add.mul(Lit::int("3"));
assert_snapshot!(mul_add, @"(1 + 2) * 3");

let mul = lhs.clone().mul(rhs.clone());
let add_mul = mul.add(Lit::int("3"));
assert_snapshot!(add_mul, @"1 * 2 + 3");

let add = lhs.neg().add(rhs.neg());
assert_snapshot!(add, @"-1 + -2");
```

### Building struct, enum, trait and impl

The source code is available in the [`examples`](https://github.com/mtshiba/ruast/tree/main/examples) directory.

```rust
use ruast::*;

let mut krate = Crate::new();
let def = StructDef::empty("Foo")
    .with_field(FieldDef::inherited("foo", Type::u32()))
    .with_field(FieldDef::inherited("bar", Type::u32()));
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
    .with_variant(Variant::tuple("Baz", vec![FieldDef::anonymous(Type::u32())]));
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

### Fuzzing

By enabling a feature `fuzzing`, you can use [`arbitrary`](https://docs.rs/arbitrary/latest/arbitrary/) crate to generate random AST nodes for fuzz testing.

Note: The generated AST nodes are entirely random and may not be compilable.

```rust
use arbitrary::Arbitrary;

let mut u = arbitrary::Unstructured::new(&[0u8; 10]);
ruast::depth_limiter::set(10);
let expr = ruast::Expr::arbitrary(&mut u).unwrap();
println!("{expr}");
```

## Feature flags

* `tokenize`: Enables conversion to `proc_macro2::TokenStream`.
* `checked-ident`: Enables `check_ident`, `Identifier`, etc.
* `fuzzing`: Enables `arbitrary` implementations for AST nodes for fuzz testing.

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
