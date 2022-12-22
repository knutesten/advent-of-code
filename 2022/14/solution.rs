use std::fs;
use std::collections::HashSet;

fn main() {
    let input = fs::read_to_string("input.txt")
        .expect("Could not read file");

    let wall: HashSet<_> = input.split("\n")
        .flat_map(|l| {
            let lines: Vec<_> = l.split(" -> ")
                .map(|c| {
                    let coor: Vec<i32> = c.split(",").map(|n| n.parse().unwrap()).collect();
                    (coor[0], coor[1])
                })
                .collect();
            let mut coors = vec![];
            for i in 0..lines.len()-1 {
                let (x1, y1) = lines[i];
                let (x2, y2) = lines[i+1];
                for x in x1.min(x2)as usize..=x1.max(x2)as usize {
                    for y in y1.min(y2) as usize..=y1.max(y2)as usize {
                        coors.push((x as i32, y as i32));
                    }
                }
            }
            coors
        })
        .collect();

    let dirs: Vec<(i32, i32)> = vec![(0, 1), (-1, 1), (1, 1)];

    let max_y = wall
        .iter()
        .map(|(_, y)| y)
        .max()
        .unwrap();

    let mut sand = HashSet::new();
    let mut prev_size: i32 = -1;
    let mut part_1 = 0;
    while sand.len() as i32 != prev_size {
        let mut grain = (500, 0);
        prev_size = sand.len() as i32;
        loop {
            let possible_dirs: Vec<_> = dirs.iter()
                .filter(|(dx, dy)| {
                    let c = (grain.0 + dx, grain.1 + dy);
                    !sand.contains(&c) && !wall.contains(&c) && c.1 < *max_y + 2
                })
                .collect();

            if grain.1 >= *max_y && part_1 == 0 {
                part_1 = sand.len();
            }

            if possible_dirs.is_empty() {
                sand.insert(grain);
                break;
            }

            let (dx, dy) = possible_dirs[0];
            grain = (grain.0 + dx, grain.1 + dy);

        }
    }

    println!("Part 1: {}", part_1);
    println!("Part 2: {}", sand.len());
}
