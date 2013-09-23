use std::os::args;
use std::from_str::FromStr;
use std::iterator::range;
use std::vec;

fn main() {
    let p : int = FromStr::from_str(args()[1]).unwrap();

    let roots = prs(p);
    for root in roots.iter() {
        println(fmt!("%d", *root));
    }
}

fn prs(p : int) -> ~[int] {
    let mut roots = ~[];
    for e in range(2, p-1) {
        // NOTE(dbp 2013-09-22): ignoring the first element so we can use base-1
        let mut marked = vec::from_elem(p as uint, 0);
        let mut g = 1;
        for _ in range(0, p-1) {
            if marked[g] == 0 {
                marked[g] = 1;
            }
            g = (g * e) % p;
        }
        let mut is_root = true;
        for g in range(1, p) {
            if marked[g] == 0 {
                is_root = false;
            }
        }
        if is_root {
            roots.push(e);
        }
    }
    return roots;
}