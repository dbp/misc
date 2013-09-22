use std::os::args;
use std::from_str::FromStr;
use std::int;
use std::iterator::range;

fn main() {
    let g : int = FromStr::from_str(args()[1]).unwrap();
    let n : int = FromStr::from_str(args()[2]).unwrap();
    let m : int = FromStr::from_str(args()[3]).unwrap();

    let gn = fpa(g, n, m);
    println(fmt!("%d^%d = %d (mod %d)", g, n, gn, m));
}

fn fpa(g : int, n : int, m : int) -> int {
    let r = (int::bits as int) - (n.leading_zeros() as int);
    let mut a = g;
    let mut gn = 1;
    for i in range(0,r) {
        if (n & (1 << i)) != 0 {
            gn = (gn * a) % m;
        }
        a = (a * a) % m;
    }
    return gn;
}