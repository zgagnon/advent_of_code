use std::fmt::Display;
use std::str::FromStr;
pub trait MyInt: FromStr + Copy + Display {}
impl MyInt for i32 {}
impl MyInt for i64 {}
impl MyInt for u32 {}
impl MyInt for u64 {}

pub fn string_to_digits<T: MyInt>(str: &str) -> Vec<T> {
    str.split("")
        .filter(|s| s.to_string() != "")
        .map(|s| {
            s.parse::<T>()
                .unwrap_or_else(|_| panic!("Not a number: {}", s))
        })
        .collect::<Vec<_>>()
}

pub fn digits_to_string<'a, F: MyInt + 'a, T: IntoIterator<Item = &'a F>>(dig: T) -> String {
    dig.into_iter()
        .map(|x| x.to_string())
        .collect::<Vec<_>>()
        .join("")
}

pub fn digits_to_digit<'a, F: MyInt + 'a, T: IntoIterator<Item = &'a F>>(digs: T) -> F {
    digits_to_string(digs)
        .parse::<F>()
        .unwrap_or_else(|_| panic!("Not able to number the numbers!!"))
}
