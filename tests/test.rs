use insta::assert_snapshot;
use ruast::*;

#[test]
fn test() {
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
    assert_snapshot!(krate, @r###"
    fn main() {
        println!("Hello, world!");
    }
    "###);
    krate.try_remove_item_by_id("main");
    assert!(krate.is_empty());
    assert_snapshot!(krate, @"");
}

#[test]
fn test_general() {
    let mut krate = Crate::new();
    let i = krate.add_item(Fn {
        is_unsafe: false,
        is_const: false,
        is_async: false,
        abi: None,
        ident: "main".into(),
        generics: vec![],
        fn_decl: FnDecl::regular(vec![], None),
        body: Some(Block::from(Stmt::Semi(Semi::new(Expr::new(MacCall {
            path: Path::single("println"),
            args: DelimArgs::from(vec![Token::lit("Hello, world!")]),
        }))))),
    });
    assert_snapshot!(krate, @r###"
    fn main() {
        println!("Hello, world!");
    }
    "###);
    assert_snapshot!(krate[i], @r###"
    fn main() {
        println!("Hello, world!");
    }
    "###);
}

#[test]
fn test_blocks() {
    let block = Block::from(Stmt::Expr(Expr::new(Lit::int("17"))));
    assert_snapshot!(block, @"
    {
        17
    }");

    assert_snapshot!(Block::empty(), @"{}");
}

#[test]
fn test_call() {
    let call = Path::single("foo").call(vec![Lit::int("42").into()]);
    assert_snapshot!(call, @"foo(42)");

    let chained_call = call.clone().call(vec![Lit::int("7").into()]);
    assert_snapshot!(chained_call, @"foo(42)(7)");

    let method_call = Path::single("foo").field("bar").call(vec![]);
    assert_snapshot!(method_call, @"foo.bar()");

    let ptr_call = Path::single("foo").field("bar").paren().call(vec![]);
    assert_snapshot!(ptr_call, @"(foo.bar)()");
}

#[test]
fn test_if_else() {
    let if_else = If::new(
        Expr::new(Lit::bool("true")),
        Block::from(Stmt::Expr(Expr::new(Lit::int("17")))),
        Some(Expr::from(Block::from(Stmt::Expr(Expr::new(Lit::int(
            "39",
        )))))),
    );
    assert_snapshot!(if_else, @"
    if true {
        17
    } else {
        39
    }");
    let if_elseif_else = If::new(
        Expr::new(Lit::bool("false")),
        Block::from(Stmt::Expr(Expr::new(Lit::int("1")))),
        Some(Expr::from(if_else)),
    );
    assert_snapshot!(if_elseif_else, @"
    if false {
        1
    } else if true {
        17
    } else {
        39
    }");
}

#[test]
fn test_binop() {
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
}

#[test]
fn test_try() {
    let x = Path::single("x");
    let try_ = x.clone().try_();
    assert_snapshot!(try_, @"x?");

    let try_add = try_.clone().add(Lit::int("42"));
    assert_snapshot!(try_add, @"x? + 42");

    let neg_try = x.neg().try_();
    assert_snapshot!(neg_try, @"(-x)?");

    let try_neg = try_.clone().neg();
    assert_snapshot!(try_neg, @"-x?");

    let try_call = try_.call(vec![Lit::int("42").into()]);
    assert_snapshot!(try_call, @"(x?)(42)");
}

#[test]
fn test_addrof() {
    let x = Path::single("x");
    let add = x.clone().add(Path::single("y"));
    let ref_ = add.clone().ref_immut();
    assert_snapshot!(ref_, @"&(x + y)");
    let ref_mut = add.ref_mut();
    assert_snapshot!(ref_mut, @"&mut (x + y)");

    let addr_deref = x.ref_immut().deref();
    assert_snapshot!(addr_deref, @"*&x");
}

#[test]
fn test_field() {
    let x = Path::single("x");
    let add = x.clone().add(Path::single("y"));
    let field = add.field("z");
    assert_snapshot!(field, @"(x + y).z");
    let ref_ = x.clone().ref_immut();
    let ref_field = ref_.field("z");
    assert_snapshot!(ref_field, @"(&x).z");
    let field_ref = x.clone().field("z").ref_immut();
    assert_snapshot!(field_ref, @"&x.z");

    let field_raw_ref = x
        .clone()
        .field("z")
        .addr_of(BorrowKind::Raw, Mutability::Mut);
    assert_snapshot!(field_raw_ref, @"&raw mut x.z");
    let raw_ref_field = x.addr_of(BorrowKind::Raw, Mutability::Mut).field("z");
    assert_snapshot!(raw_ref_field, @"(&raw mut x).z");
}

#[test]
fn test_cast() {
    let cast = Field::new(Path::single("x"), "y")
        .addr_of(BorrowKind::Raw, Mutability::Mut)
        .cast(Type::mut_ptr(Type::i8()))
        .cast(Type::mut_ptr(Type::unit()));

    assert_snapshot!(cast, @"&raw mut x.y as *mut i8 as *mut ()");

    let cast = Path::single("foo")
        .cast(Type::u32())
        .bin_op(BinOpKind::Lt, Lit::int("42"));
    assert_snapshot!(cast, @"(foo as u32) < 42");

    let cast = Path::single("foo")
        .cast(Type::u32())
        .bin_op(BinOpKind::Gt, Lit::int("42"));
    assert_snapshot!(cast, @"foo as u32 > 42");
}

#[test]
fn test_closure() {
    let x = Path::single("x");
    let decl = FnDecl::regular(vec![Param::ident("a", Type::Infer)], None);
    let closure = Closure::simple(decl, x);
    assert_snapshot!(closure, @"|a: _| { x }");

    let call = closure.call(vec![Lit::int("42").into()]);
    assert_snapshot!(call, @"(|a: _| { x })(42)");
}

#[test]
fn test_tuple() {
    let x = Path::single("x");
    let tuple = Tuple::new(vec![
        Lit::int("1").into(),
        x.clone().into(),
        Lit::int("3").into(),
    ]);
    assert_snapshot!(tuple, @"(1, x, 3)");

    let single_tuple = Tuple::new(vec![x.clone().into()]);
    assert_snapshot!(single_tuple, @"(x,)");

    let empty_tuple = Tuple::new(vec![]);
    assert_snapshot!(empty_tuple, @"()");
}

#[test]
fn test_struct() {
    let x = Path::single("x");
    let field1 = ExprField::new("a", Lit::int("1"));
    let field2 = ExprField::new("b", x.clone());
    let field3 = ExprField::new("c", Lit::int("3"));
    let struct_ = Struct::new("MyStruct", vec![field1, field2, field3], None);
    assert_snapshot!(struct_, @"MyStruct { a: 1, b: x, c: 3 }");
}

#[test]
fn test_return() {
    let x = Path::single("x");
    let return_ = x.return_();
    assert_snapshot!(return_, @"return x");

    let call_return = return_.call(vec![]);
    assert_snapshot!(call_return, @"(return x)()");
}

#[test]
fn test_match() {
    let x = Path::single("x");
    let arm1 = Arm::new(Pat::Lit(Lit::int("1").into()), None, Lit::int("1"));
    let arm2 = Arm::new(Pat::Lit(Lit::int("2").into()), None, Lit::int("2"));
    let default_arm = Arm::new(Pat::Wild, None, Lit::int("0"));
    let match_ = Match::new(x, vec![arm1, arm2, default_arm]);
    assert_snapshot!(match_, @"match x {
    1 => 1,
    2 => 2,
    _ => 0,
}");
}

#[test]
fn test_use() {
    let path = Path::single("foo").chain("bar").chain("baz");
    let use_ = Use::from(path.clone());

    assert_snapshot!(use_, @"use foo::bar::baz;");

    let use_tree = UseTree::path(UsePath::from(path));
    let use_ = Use::from(use_tree);
    assert_snapshot!(use_, @"use foo::bar::baz;");

    let baz = UseTree::name("baz");
    let qux = UseTree::name("qux");
    let group = vec![baz, qux];
    let items = UseTree::Group(group.clone());
    let items = UseTree::Path(UsePath::new("bar", items));
    let tree = UseTree::Path(UsePath::new("foo", items));
    let use_ = Use::tree(tree);
    assert_snapshot!(use_, @"use foo::bar::{baz, qux};");

    let tree = Path::single("foo").chain("bar").chain_use_group(group);
    let use_ = Use::from(tree);
    assert_snapshot!(use_, @"use foo::bar::{baz, qux};");

    let tree = Path::single("foo").chain("bar").chain_use_glob();
    let use_ = Use::from(tree);
    assert_snapshot!(use_, @"use foo::bar::*;");
}

#[test]
fn test_visibility_scope() {
    let vis_crate = Visibility::crate_();
    assert_snapshot!(vis_crate, @"pub(crate) ");

    let vis_super = Visibility::super_();
    assert_snapshot!(vis_super, @"pub(super) ");

    let vis_self = Visibility::self_();
    assert_snapshot!(vis_self, @"pub(self) ");

    let path = Path::single("my_module");
    let vis_path = Visibility::in_path(path);
    assert_snapshot!(vis_path, @"pub(in my_module) ");

    let nested_path = Path::single("crate").chain("module").chain("submodule");
    let vis_nested_path = Visibility::in_path(nested_path);
    assert_snapshot!(vis_nested_path, @"pub(in crate::module::submodule) ");

    let vis_inherited = Visibility::default();
    assert_snapshot!(vis_inherited, @"");

    let vis_public = Visibility::Public;
    assert_snapshot!(vis_public, @"pub ");
}

#[test]
fn test_macro_call() {
    let tokens = vec![
        Token::Keyword(KeywordToken::Let),
        Token::Keyword(KeywordToken::Mut),
        Token::Ident("x".into()),
        Token::Eq,
        Token::Lit(Lit::int("42")),
    ];
    let mac_call =
        Path::single("assign").mac_call(DelimArgs::new(MacDelimiter::Parenthesis, tokens.into()));

    assert_snapshot!(mac_call, @"assign!(let mut x = 42)");
}

#[test]
fn test_joint_token() {
    let ts = TokenStream::from(vec![
        Token::Ident("foo".into()),
        Token::Dot,
        Token::Ident("bar".into()),
    ]);
    assert_snapshot!(ts, @"foo . bar");

    let ts = TokenStream::from(vec![
        Token::Ident("foo".into()),
        Token::Dot.into_joint(),
        Token::Ident("bar".into()),
    ]);
    assert_snapshot!(ts, @"foo .bar");

    let ts = TokenStream::from(vec![
        Token::Ident("foo".into()).into_joint(),
        Token::Dot.into_joint(),
        Token::Ident("bar".into()),
    ]);
    assert_snapshot!(ts, @"foo.bar");
}

#[test]
fn test_attribute() {
    let fun = Fn::empty("foo");
    let item = ModuleItem::inherited(ItemKind::Fn(fun))
        .with_attr(Attribute::inner("foo", AttrArgs::Empty))
        .with_attr(Attribute::outer("bar", AttrArgs::Empty));

    assert_snapshot!(item, @r###"
    #[bar]
    fn foo() {
        #![foo]
    }"###
    );
}

#[cfg(feature = "syn")]
mod syn_conversion {
    use ruast::*;

    fn parse_and_convert(code: &str) -> Crate {
        let file: syn::File = syn::parse_str(code).unwrap();
        Crate::from(file)
    }

    /// Helper to trim the leading/trailing whitespace from Crate display
    fn display(krate: &Crate) -> String {
        format!("{krate}").trim().to_string()
    }

    #[test]
    fn test_simple_fn() {
        let krate = parse_and_convert("fn main() {}");
        assert_eq!(display(&krate), "fn main() {}");
    }

    #[test]
    fn test_fn_with_args_and_return() {
        let krate = parse_and_convert("fn add(x: i32, y: i32) -> i32 { x + y }");
        let output = display(&krate);
        assert!(output.contains("fn add(x: i32, y: i32) -> i32"));
        assert!(output.contains("x + y"));
    }

    #[test]
    fn test_struct_named_fields() {
        let krate = parse_and_convert("struct Point { x: f64, y: f64 }");
        let output = display(&krate);
        assert!(output.contains("struct Point"));
        assert!(output.contains("x: f64"));
        assert!(output.contains("y: f64"));
    }

    #[test]
    fn test_struct_tuple() {
        let krate = parse_and_convert("struct Wrapper(i32);");
        let output = display(&krate);
        assert!(output.contains("struct Wrapper(i32)"));
    }

    #[test]
    fn test_struct_unit() {
        let krate = parse_and_convert("struct Unit;");
        let output = display(&krate);
        assert!(output.contains("struct Unit"));
    }

    #[test]
    fn test_enum() {
        let krate = parse_and_convert(
            "enum Color { Red, Green, Blue(u8, u8, u8), Named { name: String } }",
        );
        let output = display(&krate);
        assert!(output.contains("enum Color"));
        assert!(output.contains("Red"));
        assert!(output.contains("Green"));
        assert!(output.contains("Blue(u8, u8, u8)"));
        assert!(output.contains("name: String"));
    }

    #[test]
    fn test_trait_def() {
        let krate = parse_and_convert(
            "trait Summary { fn summarize(&self) -> String; }",
        );
        let output = display(&krate);
        assert!(output.contains("trait Summary"));
        assert!(output.contains("fn summarize(&self) -> String;"));
    }

    #[test]
    fn test_impl_block() {
        let krate = parse_and_convert(
            "impl Point { fn new(x: f64, y: f64) -> Self { Self { x, y } } }",
        );
        let output = display(&krate);
        assert!(output.contains("impl Point"));
        assert!(output.contains("fn new(x: f64, y: f64) -> Self"));
    }

    #[test]
    fn test_use_statement() {
        let krate = parse_and_convert("use std::collections::HashMap;");
        assert_eq!(display(&krate), "use std::collections::HashMap;");
    }

    #[test]
    fn test_use_group() {
        let krate = parse_and_convert("use std::io::{Read, Write};");
        assert_eq!(display(&krate), "use std::io::{Read, Write};");
    }

    #[test]
    fn test_use_glob() {
        let krate = parse_and_convert("use std::collections::*;");
        assert_eq!(display(&krate), "use std::collections::*;");
    }

    #[test]
    fn test_const_item() {
        let krate = parse_and_convert("const MAX: u32 = 100;");
        assert_eq!(display(&krate), "const MAX: u32 = 100;");
    }

    #[test]
    fn test_static_item() {
        let krate = parse_and_convert("static mut COUNT: u32 = 0;");
        assert_eq!(display(&krate), "static mut COUNT: u32 = 0;");
    }

    #[test]
    fn test_type_alias() {
        let krate = parse_and_convert("type Result<T> = std::result::Result<T, Error>;");
        let output = display(&krate);
        assert!(output.contains("type Result<T>"));
        assert!(output.contains("std::result::Result"));
        assert!(output.contains("T, Error"));
    }

    #[test]
    fn test_extern_crate() {
        let krate = parse_and_convert("extern crate alloc;");
        assert_eq!(display(&krate), "extern crate alloc;");
    }

    #[test]
    fn test_closure() {
        let krate = parse_and_convert("fn f() { let c = |x: i32| x + 1; }");
        let output = display(&krate);
        assert!(output.contains("|x: i32|"));
        assert!(output.contains("x + 1"));
    }

    #[test]
    fn test_match_expr() {
        let krate = parse_and_convert(
            r#"fn f(x: i32) -> &'static str {
                match x {
                    1 => "one",
                    2 => "two",
                    _ => "other",
                }
            }"#,
        );
        let output = display(&krate);
        assert!(output.contains("match x"));
        assert!(output.contains(r#"1 => "one""#));
        assert!(output.contains(r#"_ => "other""#));
    }

    #[test]
    fn test_generics() {
        let krate = parse_and_convert(
            "fn first<T: Clone>(items: &[T]) -> T { items[0].clone() }",
        );
        let output = display(&krate);
        assert!(output.contains("fn first<T: Clone>"));
        assert!(output.contains("items: &[T]"));
        assert!(output.contains("items[0]"));
        assert!(output.contains(".clone()"));
    }

    #[test]
    fn test_lifetime() {
        let krate = parse_and_convert(
            "fn longest<'a>(x: &'a str, y: &'a str) -> &'a str { if x.len() > y.len() { x } else { y } }",
        );
        let output = display(&krate);
        assert!(output.contains("fn longest<'a>"));
        assert!(output.contains("x: &'a str"));
        assert!(output.contains("-> &'a str"));
    }

    #[test]
    fn test_visibility() {
        let krate = parse_and_convert("pub fn public() {} pub(crate) fn crate_fn() {}");
        let output = display(&krate);
        assert!(output.contains("pub fn public()"));
        assert!(output.contains("pub(crate) fn crate_fn()"));
    }

    #[test]
    fn test_async_fn() {
        let krate = parse_and_convert("async fn fetch() -> Result<()> { Ok(()) }");
        let output = display(&krate);
        assert!(output.contains("async fn fetch()"));
        assert!(output.contains("Result"));
        assert!(output.contains("Ok(())"));
    }

    #[test]
    fn test_unsafe_fn() {
        let krate = parse_and_convert("unsafe fn dangerous() {}");
        assert_eq!(display(&krate), "unsafe fn dangerous() {}");
    }

    #[test]
    fn test_trait_impl() {
        let krate = parse_and_convert(
            "impl Display for Point { fn fmt(&self, f: &mut Formatter) -> Result { write!(f, \"({}, {})\", self.x, self.y) } }",
        );
        let output = display(&krate);
        assert!(output.contains("impl Display for Point"));
        assert!(output.contains("fn fmt(&self, f: &mut Formatter) -> Result"));
        assert!(output.contains("write!"));
    }

    #[test]
    fn test_module() {
        let krate = parse_and_convert("mod inner { fn secret() {} }");
        let output = display(&krate);
        assert!(output.contains("mod inner"));
        assert!(output.contains("fn secret()"));
    }

    #[test]
    fn test_union() {
        let krate = parse_and_convert("union MyUnion { i: i32, f: f32 }");
        let output = display(&krate);
        assert!(output.contains("union MyUnion"));
        assert!(output.contains("i: i32"));
        assert!(output.contains("f: f32"));
    }

    #[test]
    fn test_attributes() {
        let krate = parse_and_convert("#[derive(Debug, Clone)] struct Foo { x: i32 }");
        let output = display(&krate);
        assert!(output.contains("#[derive("));
        assert!(output.contains("Debug"));
        assert!(output.contains("Clone"));
        assert!(output.contains("struct Foo"));
        assert!(output.contains("x: i32"));
    }

    #[test]
    fn test_let_with_type_annotation() {
        let krate = parse_and_convert("fn f() { let x: i32 = 42; }");
        let output = display(&krate);
        assert!(output.contains("let x: i32 = 42;"));
    }

    #[test]
    fn test_let_without_type_annotation() {
        let krate = parse_and_convert("fn f() { let x = 42; }");
        let output = display(&krate);
        assert!(output.contains("let x = 42;"));
        assert!(!output.contains("let x:"));
    }

    /// Test that the conversion doesn't panic on complex real-world code
    #[test]
    fn test_complex_code() {
        let code = r#"
            use std::fmt;

            #[derive(Debug)]
            pub struct Config {
                pub name: String,
                pub value: i32,
            }

            impl fmt::Display for Config {
                fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                    write!(f, "{}: {}", self.name, self.value)
                }
            }

            pub fn process(configs: &[Config]) -> Vec<String> {
                configs.iter().map(|c| c.name.clone()).collect()
            }
        "#;
        let krate = parse_and_convert(code);
        let output = display(&krate);
        assert!(output.contains("use std::fmt;"));
        assert!(output.contains("struct Config"));
        assert!(output.contains("impl fmt::Display for Config"));
        assert!(output.contains("fn process"));
    }
}
