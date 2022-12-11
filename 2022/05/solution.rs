use std::fs;

fn main() {
    let input = fs::read_to_string("./input.txt")
        .expect("Reading file failed");

    let rows_and_instructions: Vec<&str> = input
        .split("\n\n")
        .collect();

    let size = (rows_and_instructions[0].split("\n").next().unwrap().len() + 1) /4;
    let mut crates: Vec<Vec<char>> = vec![vec![]; size];
    for row in rows_and_instructions[0].split("\n") {
        for (i, c) in row.chars().enumerate() {
            if 65 <= c as i32 && c as i32 <= 90 {
                let _ = &crates[(i - 1)/4].insert(0, c);
            }
        }
    }

    let mut part_1_crates = crates.clone();
    for inst_str in rows_and_instructions[1].split("\n") {
        let inst: Vec<_> = inst_str.split(" ").collect();
        for _i in 0..inst[1].parse::<usize>().unwrap() {
            let to_move = part_1_crates[inst[3].parse::<usize>().unwrap() - 1].pop().unwrap().clone();
            let _ = &part_1_crates[inst[5].parse::<usize>().unwrap() - 1].push(to_move);
        }
    }


    let part_1: String = part_1_crates.iter().map(|c| c.last().unwrap()).collect();
    println!("Part 1: {}", part_1);

    let mut part_2_crates = crates.clone();
    for inst_str in rows_and_instructions[1].split("\n") {
        let inst: Vec<_> = inst_str.split(" ").collect();
        let mut to_move: Vec<char> = vec![];
        for _i in 0..inst[1].parse::<usize>().unwrap() {
            let _ = to_move.insert(0, part_2_crates[inst[3].parse::<usize>().unwrap() - 1].pop().unwrap().clone());
        }

        for c in to_move {
            let _ = &part_2_crates[inst[5].parse::<usize>().unwrap() - 1].push(c);
        }
    }

    let part_2: String = part_2_crates.iter().map(|c| c.last().unwrap()).collect();
    println!("Part 2: {}", part_2);
}
