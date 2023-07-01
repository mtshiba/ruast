fn main() -> Result<(), ()> {
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
    Ok(())
}
