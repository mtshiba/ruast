fn main() -> Result<(), Box<dyn std::error::Error>> {
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
    Ok(())
}
