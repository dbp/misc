use std::os::args;
use std::from_str::FromStr;

fn main() {
    let p : int = FromStr::from_str(args()[1]).unwrap();
    let n : int = FromStr::from_str(args()[2]).unwrap();

    println(fmt!("ord_%d(%d) = %d", p, n, ord(p,n,0)));
}

fn ord(p : int, n : int, q : int) -> int {
    if n % p != 0 {
        q
    } else {
        ord(p, n / p, q + 1)
    }
}
