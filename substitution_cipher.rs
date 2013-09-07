use std::os::args;
use std::vec;

fn main() {
    let input = args()[1];
    let shifts = build_vec(26, ~[]);
    do shifts.map |e| {
        println(fmt!("%?: %?", e, input.map_chars(substitute(*e))));
    };
}

fn substitute(shift : int) -> @fn(char) -> char {
    |c:char| {
        ((((c as int) - ('A' as int) + shift) % 26) + ('A' as int)) as char
    }
}

fn build_vec(n : int, v : ~[int]) -> ~[int] {
    match n {
        0 => v,
        _ => {
            let nv = vec::append(v, ~[n]);
            build_vec(n-1, nv)
        }
    }
}