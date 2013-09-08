use std::os::args;
use std::iterator::range;

fn main() {
    let input = args()[1];
    for e in range(1,26) {
        println(fmt!("%?: %?", e, input.iter().map(|c| substitute(e as u8, c)).collect::<~str>()));
    };
}

fn substitute(shift : u8, c : char) -> char {
    let c_int = c as u8;
    let A_int = 'A' as u8;
    if c_int < A_int || c_int > ('Z' as u8) {
        fail!("Character out of range. Input should be all caps ASCII.");
    }
    ((((c_int) - (A_int) + shift) % 26) + (A_int)) as char
}