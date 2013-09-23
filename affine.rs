use std::os::args;
use std::from_str::FromStr;

mod fpa;

fn main() {
    let mode = args()[1];
    let p : int = FromStr::from_str(args()[2]).unwrap();
    let k1 : int = FromStr::from_str(args()[3]).unwrap();
    let k2 : int = FromStr::from_str(args()[4]).unwrap();
    let m : int = FromStr::from_str(args()[5]).unwrap();

    if mode == ~"e" {
        println(fmt!("%d", (k1*m + k2) % p));
    } else if mode == ~"d" {
        let k1inv = fpa::fpa(k1, p-2, p);
        println(fmt!("%d", (k1inv*(m - k2)) % p));
    } else {
        fail!("mode must be 'e' or 'd'");
    }
}
