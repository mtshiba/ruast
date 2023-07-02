use rast::*;

#[test]
fn test() -> Result<(), ()> {
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
    krate.remove_item_by_id("main");
    assert!(krate.is_empty());
    Ok(())
}

#[test]
fn test_general() -> Result<(), ()> {
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
    println!("{}", krate[0]);
    Ok(())
}
