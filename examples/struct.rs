fn main() -> Result<(), Box<dyn std::error::Error>> {
    use ruast::*;

    let mut krate = Crate::new();
    let def = StructDef::empty("Foo")
        .with_field(FieldDef::inherited("foo", Type::from("u32")))
        .with_field(FieldDef::inherited("bar", Type::from("u32")));
    krate.add_item(def);
    let fn_decl = FnDecl::new(vec![Param::ref_self(), Param::ident("x", Type::usize())], Some(Type::usize()));
    let add = Lit::int("1").bin_op(BinOpKind::Add, Path::single("x"));
    let body = Block::new(vec![Stmt::Expr(add.into())], None);
    let f = Fn::simple("foo", fn_decl, body);
    let imp = Impl::empty("Foo")
        .with_item(Fn::empty_method("test", Pat::ref_self()))
        .with_item(f);
    krate.add_item(imp);
    println!("{krate}");
    Ok(())
}
