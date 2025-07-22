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
    krate.remove_item_by_id("main");
    assert!(krate.is_empty());
    assert_snapshot!(krate, @"");
}

#[test]
fn test_general() {
    let mut krate = Crate::new();
    krate.add_item(Fn {
        is_unsafe: false,
        is_const: false,
        is_async: false,
        abi: None,
        ident: "main".to_string(),
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
    assert_snapshot!(krate[0], @r###"
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
fn test_closure() {
    let x = Path::single("x");
    let decl = FnDecl::regular(vec![Param::ident("a", Type::Infer)], None);
    let closure = Closure::simple(decl, x);
    assert_snapshot!(closure, @"|a: _| { x }");

    let call = closure.call(vec![Lit::int("42").into()]);
    assert_snapshot!(call, @"(|a: _| { x })(42)");
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
