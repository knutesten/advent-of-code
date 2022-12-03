use std::fs;

fn main() {
    let input = fs::read_to_string("./input.txt")
        .expect("Reading file failed");

    let mut sums: Vec<i32> = input
        .split("\n\n")
        .map(|x| x.split("\n").map(|n| n.parse::<i32>().unwrap()).sum::<i32>())
        .collect();
    sums.sort_by(|a,b| b.cmp(a));

    let part_1 = sums[0];
    println!("Part 1: {part_1}");

    let part_2 = sums[0] + sums[1] + sums[2];
    println!("Part 2: {part_2}");
}
