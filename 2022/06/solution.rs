use std::fs;
use std::collections::HashSet;

fn main() {
    let input = fs::read_to_string("./input.txt").expect("Could not read file");

    let chars: Vec<_> = input.chars().collect();
    let mut count = 0;
    for i in 0..(input.len() - 3) {
        let set = HashSet::from([chars[i], chars[i+1], chars[i+2], chars[i+3]]);
        if set.len() == 4 {
            count = i + 4;
            break;
        }

    }
    println!("Part 1: {}", count);

    for i in 0..(input.len() - 13) {
        let set: HashSet<char> = (i..(i+14)).map(|n| chars[n]).collect();
        if set.len() == 14 {
            count = i + 14;
            break;
        }
    }
    println!("Part 2: {}", count);
}

