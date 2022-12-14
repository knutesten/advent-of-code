use std::fs;
use std::collections::HashMap;
use std::collections::HashSet;

fn main() {
    let input = fs::read_to_string("input.txt")
        .expect("Could not read file");

    let instructions = input
        .split("\n")
        .map(|s| {
            let dir_nr: Vec<&str> = s.split(" ").collect();
            (dir_nr[0], dir_nr[1].parse::<usize>().unwrap())
        });

    let directions = HashMap::from([
        ("R", ( 1,  0)),
        ("L", (-1,  0)),
        ("U", ( 0,  1)),
        ("D", ( 0, -1)),
    ]);

    let mut pos: Vec<(i32, i32)> = vec!((0,0);10);
    let mut visited_part_1 = HashSet::new();
    let mut visited_part_2 = HashSet::new();
    for (dir, nr) in instructions {
        for _i in 0..nr {
            let (x, y) = pos[0];
            let (dx, dy) = directions[dir];
            pos[0] = (x+dx, y+dy);

            for i in 1..pos.len() {
                let (tx, ty) = pos[i];
                let (tdx, tdy) = (pos[i-1].0 - tx, pos[i-1].1 - ty);

                if tdx.abs() > 1 || tdy.abs() > 1 {
                    pos[i] = (tx + (tdx/1.max(tdx.abs())),
                              ty + (tdy/1.max(tdy.abs())));
                }
            }

            visited_part_1.insert(pos[1]);
            visited_part_2.insert(pos[9]);
        }
    }

    println!("Part 1: {}", visited_part_1.len());
    println!("Part 2: {}", visited_part_2.len());
}
