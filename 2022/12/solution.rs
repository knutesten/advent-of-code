use std::fs;
use std::collections::{BTreeMap,HashMap};

fn neighbours(map: &Vec<Vec<char>>, pos: &(usize, usize)) -> Vec<(usize, usize)> {
    let dirs: [(i32, i32);4] = [(0, 1), (0, -1), (1, 0), (-1, 0)];

    return dirs.iter()
        .fold(vec![], |mut acc, (dx, dy)| {
            let (x, y) = (dx + pos.0 as i32, dy + pos.1 as i32);
            if 0 <= x && x < map[0].len() as i32 && 0 <= y && y < map.len() as i32 {
                let a = match map[pos.1][pos.0] as i32 {
                    83 => 97,
                    a => a
                };
                let b = match map[y as usize][x as usize] as i32 {
                    69 => 122,
                    83 => 97,
                    a => a
                };
                if a >= b-1 {
                    acc.push((x as usize, y as usize));
                }
            }
            acc
        });
}

fn find_path_len(map: &Vec<Vec<char>>, start: (usize, usize), goal: (usize, usize)) -> usize {
    let mut open_set = BTreeMap::new();
    let mut came_from = HashMap::new();
    let mut g_score = HashMap::new();
    let mut path = vec![];

    open_set.insert(format!("{:06}_{:?}", 0, start), start);
    g_score.insert(start, 0);

    while !open_set.is_empty() {
        let (key, current) = open_set.iter().next().unwrap();
        let current = current.clone();

        if current == goal {
            let mut c = current;
            while came_from.contains_key(&c) {
                let nc = came_from.get(&c).unwrap();
                path.insert(0, *nc);
                c = *nc;
            }
            return path.len();
        }

        open_set.remove(&key.clone());
        for neigh in neighbours(&map, &current) {
            let score = g_score.get(&current).unwrap() + 1;
            let shorter_path = match g_score.get(&neigh) {
                Some(s) => &score < s,
                None => true
            };

            if shorter_path {
                came_from.insert(neigh, current);
                g_score.insert(neigh, score);
                open_set.insert(format!("{:06}_{:?}", score, neigh), neigh);
            }
        }
    }

    return 100000;
}

fn main() {
    let input = fs::read_to_string("./input.txt")
        .expect("Could not read file");

    let map: Vec<_> = input
        .split("\n")
        .map(|l| l.chars().collect::<Vec<_>>())
        .collect();
    let mut start = (0,0);
    let mut goal = (0,0);
    let mut starts = vec![];

    for y in 0..map.len() {
        for x in 0..map[0].len() {
            if map[y][x] == 'S' {
                start = (x, y);
                starts.push((x,y));
            }

            if map[y][x] == 'a' {
                starts.push((x, y));
            }

            if map[y][x] == 'E' {
                goal = (x, y);
            }
        }
    }

    println!("Part 1: {}", find_path_len(&map, start, goal));

    let mut lengths: Vec<_> = starts.iter()
        .map(|s| find_path_len(&map, *s, goal))
        .collect();
    lengths.sort();
    println!("Part 2: {}", lengths[0]);
}

