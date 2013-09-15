use std::os::args;
use std::from_str::FromStr;

fn main() {
    let a : int = FromStr::from_str(args()[1]).unwrap();
    let b : int = FromStr::from_str(args()[2]).unwrap();

    let (g, u, v) = eea(a, b, 1, a, 0, b);
    println(fmt!("%d (a) * %d (u) + %d (b) * %d (v) = %d (gcd(a,b))", a, u, b, v, g));
}

fn eea(a : int, b : int, u : int, g : int, x : int, y : int) -> (int,int,int) {
    if y == 0 {
        return (g, u, (g - a * u)/b);
    }
    let q = g % y;
    let t = g % y;
    let s = u - q * x;
    eea(a, b, x, y, s, t)
}