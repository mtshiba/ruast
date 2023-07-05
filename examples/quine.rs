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
                        Path::from("Some").call1(Path::from("Type::from").call1(Lit::from("Result<(), Box<dyn std::error::Error>>"))).into(),
                        Path::from("Block::from").call1(MacCall::bracket(Path::from("vec"), vec![
                            Path::from("Use::new").call1(Lit::from("rast::*")).method_call("into", vec![]).into_tokens(),
                            Path::from("Pat::mut_").call1(Lit::from("krate")).method_call("bind", vec![]).into_tokens(),
                            Path::from("Pat::from").call1(Lit::from("def")).method_call("bind", vec![
                                Path::from("Path::from").call1(Lit::from("Fn::main")).into(),
                            ]).into_tokens(),
                        ])).into(),
                    ],
                ),
            ).into(),
            Path::single("krate").method_call1("add_item", Path::single("def")).into(),
            Path::single("println").mac_call(vec![Token::lit("{krate}")]).into(),
            Path::single("Ok").call(vec![Tuple::unit().into()]).into(),
        ])
    );
    krate.add_item(def);
    println!("{krate}");
    // krate.dump("hello.rs")?;
    Ok(())
}
