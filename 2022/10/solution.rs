use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt")
        .expect("Could not read file");

    let instructions = input
        .split("\n")
        .map(|s| {
            let split: Vec<_> = s.split(" ").collect();
            if split.len() == 1 {
                (split[0], 0, 1)
            } else {
                (split[0], split[1].parse().unwrap(), 2)
            }
        });

    let mut score = 0;
    let mut cycle = 0;
    let mut reg = 1;
    println!("Part 2:");
    for instruction in instructions {
        let (op, val, cyc) = instruction;

        for _i in 1..=cyc {
            if reg - 1 <= cycle % 40 && cycle % 40 <= reg + 1 {
                print!("#");
            } else {
                print!(".");
            }

            cycle = cycle + 1;
            if cycle % 40 == 20 {
                score = score + cycle * reg;
            }


            if cycle % 40 == 0 {
                println!();
            }
        }

        if op == "addx" {
            reg = reg + val;
        }

        if cycle >= 240 {
            break;
        }
    }

    println!("Part 1: {}", score);
}
