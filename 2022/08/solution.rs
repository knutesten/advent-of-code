use std::fs;
use std::collections::HashSet;

fn main() {
    let input = fs::read_to_string("./input.txt")
        .expect("Could not read file");

    let map: Vec<Vec<u32>> = input
        .split("\n")
        .map(|l| l.chars()
             .map(|c| c.to_digit(10).unwrap())
             .collect::<Vec<u32>>())
        .collect();


    let mut visible = HashSet::new();
    let mut max_score = 0;
    for y in 0..map.len() {
        for x in 0..map[y].len() {
            let seen_left = &map[y][0..x].iter().rev().take_while(|t| t < &&map[y][x]).count();
            let seen_right = &map[y][x+1..map[y].len()].iter().take_while(|t| t < &&map[y][x]).count();
            let seen_up = (0..y).rev().take_while(|ya| map[ya.clone()][x] < map[y][x]).count();
            let seen_down = (y+1..map.len()).take_while(|ya| map[ya.clone()][x] < map[y][x]).count();

            let end_left = seen_left == &x;
            let end_right = seen_right == &(map[y].len()-1-x);
            let end_up = seen_up == y;
            let end_down = seen_down == map.len()-1-y;

            if end_left || end_right || end_up || end_down {
                visible.insert((x, y));
            }

            let score = (seen_left + if end_left {0} else {1}) *
                (seen_right + if end_right {0} else {1}) *
                (seen_up + if end_up {0} else {1}) *
                (seen_down + if end_down {0} else {1});
            if score > max_score {
                max_score = score;
            }
        }
    }


    println!("Part 1: {}", visible.len());
    println!("Part 2: {}", max_score);
}
