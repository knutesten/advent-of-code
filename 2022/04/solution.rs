use std::fs;

fn includes(a: &Vec<i32>, b: &Vec<i32>) -> bool {
    let a1 = a[0];
    let a2 = a[1];
    let b1 = b[0];
    let b2 = b[1];

    if a1 <= b1 && b2 <= a2 || b1 <= a1 && a2 <= b2 {
        return true;
    } else {
        return false;
    }
}

fn overlaps(a: &Vec<i32>, b: &Vec<i32>) -> bool {
    let a1 = a[0];
    let a2 = a[1];
    let b1 = b[0];
    let b2 = b[1];

    if a1 <= b2 && b2 <= a2 || b1 <= a2 && a2 <= b2 {
        return true
    } else {
        return false;
    }
}

fn main() {
    let input = fs::read_to_string("./input.txt")
        .expect("Reading file failed");

    let pairs: Vec<_> = input
        .split("\n")
        .map(|l| l
             .split(",")
             .map(|x| x
                  .split("-")
                  .map(|ns| ns.parse::<i32>().unwrap())
                  .collect::<Vec<_>>())
             .collect::<Vec<_>>())
        .collect();

    let part_1 = pairs
        .iter()
        .filter(|ranges| includes(&ranges[0], &ranges[1]))
        .count();

    println!("Part 1: {part_1}");

    let part_2 = pairs
        .iter()
        .filter(|ranges| overlaps(&ranges[0], &ranges[1]))
        .count();

    println!("Part 2: {part_2}");

}
