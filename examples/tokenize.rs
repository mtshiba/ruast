#[cfg(feature = "tokenize")]
fn main() -> Result<(), Box<dyn std::error::Error>> {
    use ruast::*;

    let mut krate = Crate::new();
    let def = Fn::main(
        None,
        Block::from(
            Path::single("println")
                .mac_call(vec![Token::lit("Hello, world!")])
                .semi(),
        ),
    );
    krate.add_item(def);
    let tokens = krate.to_token_stream();
    println!("{krate}");
    println!("{tokens}");
    Ok(())
}

#[cfg(not(feature = "tokenize"))]
fn main() {}
