fn main() -> Result<(), Box<dyn std::error::Error>> {
    use rast::*;

    let mut krate = Crate::new();
    let def = Fn::main(
        Some(Type::from("Result<(), Box<dyn std::error::Error>>")),
        Block::from(vec![
            Use::new("rast::*").into(),
            Pat::mut_("krate").bind(Path::from("Crate::new").call(vec![])).into(),
            Pat::from("def").bind(
                Path::from("Fn::main").call(
                    vec![
                        Path::from("Some").call(vec![Path::from("Type::from").call(vec![Lit::from("Result<(), Box<dyn std::error::Error>>").into()]).into()]).into(),
                        Path::from("Block::new").call(vec![MacCall::bracket(Path::from("vec"), vec![
                            Path::from("Use::new").call(vec![Lit::from("rast::*").into()]),
                            Path::from("Local::simple").call(vec![]),
                            Path::from("Local::simple").call(vec![]),
                        ]).into()]).into(),
                    ],
                ),
            ).into(),
        ])
    );
    krate.add_item(def);
    println!("{krate}");
    // krate.dump("hello.rs")?;
    Ok(())
}
