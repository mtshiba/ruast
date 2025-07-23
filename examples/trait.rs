fn main() -> Result<(), Box<dyn std::error::Error>> {
    use ruast::*;

    let mut krate = Crate::new();

    let partial_eq = Type::simple_path("PartialEq");
    let trait_def = TraitDef::new("Eq", vec![], vec![partial_eq], vec![]);
    krate.add_item(trait_def);
    let eq_bound = GenericBound::Trait(PolyTraitRef::simple("Eq"));
    let eq = Type::simple_path("Eq");
    let param_t = GenericParam::TypeParam(TypeParam::new("T", vec![eq_bound]));
    let arg_t = GenericArg::Type(Type::simple_path("T"));
    let self_ty = Type::poly_path("Vec", vec![arg_t]);
    // or
    let _self_ty = Type::vec("T");
    let imp = Impl::trait_impl(vec![param_t], self_ty, eq, None, vec![]);
    krate.add_item(imp);
    println!("{krate}");
    Ok(())
}
