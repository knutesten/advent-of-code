use std::fs;
use std::collections::HashMap;

fn folder_size<'a>(folder: &'a Vec<&str>,
                   file_system: &'a HashMap<Vec<&str>, (usize, Vec<Vec<&str>>)>) -> usize {
    let (size, sub_dirs) = file_system.get(folder).unwrap();
    if sub_dirs.len() == 0 {
        return size.clone();
    }

    return size + sub_dirs.iter().fold(0, |acc, x| acc + folder_size(x, file_system));
}

fn main() {
    let input = fs::read_to_string("./input.txt")
        .expect("Could not read file");

    let mut current_dir: Vec<&str> = vec![];
    let mut dirs: Vec<Vec<&str>> = vec![];
    let mut size = 0;
    let mut file_system = HashMap::new();
    let mut is_ls = false;
    for l in input.split("\n") {
        if is_ls && l.starts_with("$") {
            is_ls = false;
            file_system.insert(current_dir.clone(), (size, dirs.clone()));
        }

        if l.starts_with("$ ls") {
            is_ls = true;
        }

        if l.ends_with("..") {
            current_dir = current_dir.clone();
            current_dir.pop();
        } else if l.starts_with("$ cd") {
            current_dir = current_dir.clone();
            current_dir.push(&l[5..]);
            dirs = vec![];
            size = 0;
        }

        if l.starts_with("dir") {
            let mut subdir = current_dir.clone();
            subdir.push(&l[4..]);
            dirs.push(subdir);
        } else if !l.starts_with("$") {
            let num = l.split(" ").next().unwrap();
            size = size + num.parse::<usize>().unwrap();
        }
    }
    file_system.insert(current_dir.clone(), (size, dirs));


    let part_1: usize = file_system
        .keys()
        .map(|f| folder_size(f, &file_system))
        .filter(|s| s <= &100000)
        .sum();

    println!("Part 1: {}", part_1);

    let root = vec!["/"];
    let used_space: usize = folder_size(&root, &file_system);
    let total_size: usize = 70000000;
    let size_needed: usize = 30000000;
    let size_to_free = size_needed - (total_size - used_space);

    let mut sizes: Vec<usize> = file_system
        .keys()
        .map(|f| folder_size(f, &file_system))
        .collect();

    sizes.sort();

    let part_2 = sizes.iter().find(|s| s.clone() > &size_to_free).unwrap();

    println!("Part 2: {:?}", part_2);
}
