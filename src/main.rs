mod types;
use std::fs::read;
use types::Data;

fn main() {
    let file_bytes = read("data.toml").unwrap();
    let raw_data = String::from_utf8(file_bytes).unwrap();
    let data: Data = toml::from_str(&raw_data).unwrap();
    println!("{}", data.find_free_slices());
}
