use std::os::args;
use std::from_str::FromStr;

fn main() {
    let a : int = FromStr::from_str(args()[1]).unwrap();
    let b : int = FromStr::from_str(args()[2]).unwrap();

    println(fmt!("gcd(%d, %d)", a, b));
    println(fmt!("= %d", gcd(a, b)));
}

fn gcd(a : int, b : int) -> int {
    let r = b % a;
    println(fmt!("%d = %d * %d + %d", b, b / a, a, r));
    if r == 0 {
        a
    } else {
        gcd(r, a)
    }
}