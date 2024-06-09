fn main() {
    let result = mif::parse_mif(include_str!("charmap.mif"));
    dbg!(result);
}
