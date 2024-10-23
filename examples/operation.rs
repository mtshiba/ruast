fn main() -> Result<(), Box<dyn std::error::Error>> {
    use ruast::*;

    let mut krate = Crate::new();
    let mut def = Fn::empty("foo");
    let x = Path::single("x");
    let y = Path::single("y");
    def.add_stmt(x.clone().add(y.clone()).semi());
    def.add_stmt(x.clone().neg().semi());
    def.add_stmt(x.clone().field("y").semi());
    def.add_stmt(x.clone().call(vec![y.clone().into()]).semi());
    def.add_stmt(x.clone().index(y.clone()).semi());
    def.add_stmt(x.clone().ref_immut().semi());
    def.add_stmt(x.clone().cast("u32").semi());
    krate.add_item(def);
    println!("{krate}");
    Ok(())
}
