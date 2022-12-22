use std::fs;
use std::cmp::Ordering::{Less,Equal,Greater};

#[derive(Debug,PartialEq)]
struct Signal {
    children: Option<Vec<Signal>>,
    value: Option<usize>
}


impl Clone for Signal {
    fn clone(&self) -> Signal {
        return Signal {
            children: match &self.children {
                Some(c) => Some(c.clone()),
                None => None
            },
            value: match &self.value {
                Some(v) => Some(v.clone()),
                None => None
            }
        }
    }
}

fn parse(x: &str) -> Signal {
    if x == "[]" {
        return Signal{children: Some(vec![]), value: None};
    }

    if !x.starts_with("[") {
        return Signal{children: None, value: Some(x.parse().unwrap())};
    }

    let mut children = vec![];
    let mut word = vec![];
    let mut depth = 0;
    for c in x[1..x.len()-1].chars() {
        if c == '[' {
            depth = depth + 1;
        }

        if c == ']' {
            depth = depth - 1;
        }

        if c == ',' && depth == 0 {
            children.push(parse(&word.iter().collect::<String>()[..]));
            word = vec![];
            continue;
        }

        word.push(c);
    }
    children.push(parse(&word.iter().collect::<String>()[..]));

    return Signal{children: Some(children), value: None}
}

fn compare(a: &Signal, b: &Signal) -> i32 {
    if !a.value.is_none() && !b.value.is_none() {
        if a.value.unwrap() < b.value.unwrap() {
            return -1
        } if a.value.unwrap() == b.value.unwrap() {
            return 0
        } else {
            return 1
        }
    }
    let a_children = match &a.children {
        Some(c) => c.clone(),
        None => vec![Signal{children: None, value: a.value}].to_owned()
    };
    let b_children = match &b.children {
        Some(c) => c.clone(),
        None => vec![Signal{children: None, value: b.value}].to_owned()
    };
    for i in 0..a_children.len() {
        if i >= b_children.len() {
            return 1;
        }

        let result = compare(&a_children[i], &b_children[i]);
        if result == 0 {
            continue;
        } else {
            return result;
        }

    }

    if b_children.len() == a_children.len() {
        return 0;
    } else {
        return -1;
    }
}

fn main() {
    let input = fs::read_to_string("./input.txt")
        .expect("Could not read file");

    let pairs: usize = input.split("\n\n")
        .map(|l| l.split("\n").map(|p| parse(p)).collect::<Vec<Signal>>())
        .enumerate()
        .map(|(i, x)| {
            let c = compare(&x[0], &x[1]);
                if c == -1 || c == 0 {
                    i+1
                } else {
                    0
                }
        })
        .sum();

    println!("Part 1: {}", pairs) ;

    let mut packets: Vec<Signal> = input.split("\n\n")
        .flat_map(|l| l.split("\n"))
        .map(|s| parse(s))
        .collect();

    let div_1 = parse("[[2]]");
    let div_2 = parse("[[6]]");
    packets.push(div_1.clone());
    packets.push(div_2.clone());

    packets.sort_by(|a, b| match compare(&a, &b) {
        -1 => Less,
        0 => Equal,
        _ => Greater
    });

    let mut index_div_1 = 0;
    let mut index_div_2 = 0;
    for i in 0..packets.len() {
        if div_1 == packets[i] {
            index_div_1 = i + 1;
        }

        if div_2 == packets[i] {
            index_div_2 = i + 1
        }
    }

    println!("Part 2: {}", index_div_1 * index_div_2);
}
