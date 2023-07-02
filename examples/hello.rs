fn main() -> Result<(), Box<dyn std::error::Error>> {
    use rast::*;

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
    krate.dump("hello.rs")?;
    Ok(())
}
