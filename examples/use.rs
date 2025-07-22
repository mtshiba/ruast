fn main() -> Result<(), Box<dyn std::error::Error>> {
    use ruast::*;

    let mut krate = Crate::new();

    let path = Path::single("foo").chain("bar");
    krate.add_item(Use::path(UsePath::from(path)));

    let tree = Path::single("foo").chain("bar").chain_use_rename("baz");
    krate.add_item(Use::from(tree));

    let tree = Path::single("foo")
        .chain("bar")
        .chain_use_group(vec![UseTree::name("baz"), UseTree::name("qux")]);
    krate.add_item(Use::from(tree));

    let tree = Path::single("foo").chain("bar").chain_use_glob();
    krate.add_item(Use::from(tree));

    println!("{krate}");
    Ok(())
}
