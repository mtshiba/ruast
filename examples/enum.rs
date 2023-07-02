fn main() -> Result<(), Box<dyn std::error::Error>> {
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
    Ok(())
}
