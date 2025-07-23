fn main() -> Result<(), Box<dyn std::error::Error>> {
    use ruast::*;

    let mut krate = Crate::new();
    let def = EnumDef::empty("Foo")
        .with_variant(Variant::empty("Bar"))
        .with_variant(Variant::tuple(
            "Baz",
            vec![FieldDef::anonymous(Type::u32())],
        ))
        .with_variant(Variant::struct_(
            "Qux",
            vec![FieldDef::inherited("x", Type::u32())],
        ));
    krate.add_item(def);
    let imp = Impl::empty("Foo").with_item(Fn::empty_method("test", Pat::ref_self()));
    krate.add_item(imp);
    println!("{krate}");
    Ok(())
}
