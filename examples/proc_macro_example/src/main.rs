use ruast_proc_macro_example::{gen_foo, Getter};

#[derive(Getter)]
struct Foo {
    bar: usize,
}

fn main() {
    let f = Foo { bar: 42 };
    assert_eq!(f.get_bar(), &42);

    gen_foo!();
    println!("{}", foo());
}
