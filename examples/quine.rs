fn main() -> Result<(), Box<dyn std::error::Error>> {
    use rast::*;

    let mut krate = Crate::new();
    let def = Fn::main(
        Some(Type::from("Result<(), Box<dyn std::error::Error>>")),
        Block::new(vec![
            Use::new("rast::*").into(),
            Local::simple(
                Pat::mut_("krate"),
                Call::new(Path::from("Crate::new"), vec![]),
            ).into(),
            Local::simple(
                Pat::from("def"),
                Call::new(
                    Path::from("Fn::main"),
                    vec![
                        Call::new(Path::from("Some"), vec![Call::new(Path::from("Type::from"), vec![Lit::from("Result<(), Box<dyn std::error::Error>>").into()]).into()]).into(),
                        Call::new(Path::from("Block::new"), vec![MacCall::bracket(Path::from("vec"), vec![
                            Call::new(Path::from("Use::new"), vec![Lit::from("rast::*").into()]),
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
