use std::fs;
use std::collections::HashMap;

fn main() {
    let input = fs::read_to_string("./input.txt")
        .expect("Reading file failed");

    let scores = HashMap::from([
        ("A X", 4),
        ("A Y", 8),
        ("A Z", 3),
        ("B X", 1),
        ("B Y", 5),
        ("B Z", 9),
        ("C X", 7),
        ("C Y", 2),
        ("C Z", 6),
    ]);

    let part_1: i32 = input
        .split("\n")
        .map(|m| scores[m])
        .sum();

    println!("Part 1: {part_1}");

    let scores = HashMap::from([
        ("A X", 3),
        ("A Y", 4),
        ("A Z", 8),
        ("B X", 1),
        ("B Y", 5),
        ("B Z", 9),
        ("C X", 2),
        ("C Y", 6),
        ("C Z", 7),
    ]);

    let part_2: i32 = input
        .split("\n")
        .map(|m| scores[m])
        .sum();

    println!("Part 2: {part_2}");
}
