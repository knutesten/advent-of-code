use std::fs;
use std::collections::{HashSet};

fn manhattan_distance(a: &(i32, i32), b: &(i32, i32)) -> i32 {
    return (b.0 - a.0).abs() + (b.1 - a.1).abs();
}

fn main() {
    let input = fs::read_to_string("./input.txt")
        .expect("Could not read file");

    let sensors: Vec<((i32, i32), (i32, i32))> = input.split("\n")
        .map(|l| {
            let s: Vec<Vec<_>>= l.split(", ")
                .map(|l2| l2.split("=").collect())
                .collect();
            ((s[0][1].parse().unwrap(),
              s[1][1].split(":").next().unwrap().parse().unwrap()),
             (s[1][2].parse().unwrap(),
              s[2][1].parse().unwrap()))
        })
        .collect();
    let beacons: HashSet<_> = sensors.iter()
        .map(|(_, b)| b)
        .collect();

    let r = 2000000;
    let beacons_on_line = beacons.iter().filter(|(_, y)| y == &r).count() as i32;
    'outer: for y in 0..r*2 {
        let mut lines: Vec<_> = sensors
            .iter()
            .flat_map(|(s, b)| {
                let l = manhattan_distance(&s, &b);
                let dx = l - (y - s.1).abs();
                if dx <= 0 {
                    vec![]
                } else {
                    vec![(s.0 - dx, s.0 + dx)]
                }
            })
            .collect();
        lines.sort();
        let mut curr = lines[0];
        let mut sum = 0;
        for i in 1..lines.len() {
            if curr.0.max(lines[i].0) - 1 <= curr.1.min(lines[i].1) {
                curr = (curr.0.min(lines[i].0), curr.1.max(lines[i].1));
            } else {
                let x: u128 = curr.1 as u128 + 1;
                println!("Part 2: {}", x * 2 * r as u128 + y as u128);
                break 'outer;
            }
        }
        sum = sum + 1 + curr.1 - curr.0 - beacons_on_line;
        if y == r {
            println!("Part 1: {}", sum);
        }
    }
}
