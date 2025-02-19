extern crate proc_macro;

use proc_macro::TokenStream;

#[proc_macro]
pub fn gen_foo(_input: TokenStream) -> TokenStream {
    use ruast::*;
    let lhs = Lit::int("1");
    let rhs = Lit::int("2");
    let bin = Binary::new(lhs, BinOpKind::Add, rhs);
    let bin = Binary::new(bin, BinOpKind::Mul, Lit::int("3"));
    let f = Fn::simple("foo", FnDecl::new(vec![], Some(Type::usize())), Block::single(bin));
    f.to_token_stream().into()
}

#[proc_macro_derive(Getter)]
pub fn derive_getter(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    let syn::Data::Struct(struct_) = input.data else {
        panic!("expected struct");
    };

    use ruast::{EmptyItem, Addressable};

    let mut impls = ruast::Impl::empty(input.ident.to_string());
    for field in struct_.fields {
        let slf = ruast::Path::from("self");
        let name = field.ident.unwrap().to_string();
        let syn::Type::Path(ty) = field.ty else { todo!() };
        let fn_name = format!("get_{name}");
        let ret = ruast::Type::ref_(ruast::Type::from(ty.path.get_ident().unwrap().to_string()));
        let fn_decl = ruast::FnDecl::new(vec![ruast::Param::ref_self()], Some(ret));
        let block = ruast::Block::single(ruast::Field::new(slf, name).ref_immut());
        let fn_ = ruast::Fn::simple(fn_name, fn_decl, block);
        impls.add_item(fn_);
    }
    impls.to_token_stream().into()
}
