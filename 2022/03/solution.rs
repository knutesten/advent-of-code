use std::fs;
use std::collections::HashSet;

fn priority(a: char) -> i32 {
    if a.is_uppercase() {
        return a as i32 - 38;
    } else {
        return a as i32 - 96;
    }
}

fn main() {
    let input = fs::read_to_string("./input.txt")
        .expect("Reading file failed");

    let part_1: i32 = input
        .split("\n")
        .map(|l| {
            let half = l.len() / 2;
            let first_half: HashSet<char> = (&l[..half]).chars().collect();
            let second_half: HashSet<char> = (&l[half..]).chars().collect();
            let intersection = first_half.intersection(&second_half).next().unwrap();
            priority(*intersection)
        })
        .sum();

    println!("Part 1: {part_1}");

    let part_2: i32 = input.split("\n")
        .collect::<Vec<_>>()
        .chunks(3)
        .map(|c| {
            let set = c.iter()
                .map(|l| l.chars().collect::<HashSet<_>>())
                .reduce(|acc, s| acc.intersection(&s).copied().collect())
                .unwrap();
            priority(*set.iter().next().unwrap())
        })
        .sum();

    println!("Part 2: {part_2}");
}
