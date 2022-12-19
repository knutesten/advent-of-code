use std::fs;

fn calc_monkey_business(
    turns: usize,
    divide_by: u128,
    monkeys: &Vec<(Vec<u128>, Vec<&str>, u128, usize, usize, u128)>
) -> u128 {

    let mut monkeys = monkeys.clone();

    let product = monkeys.iter().fold(1, |acc, x| acc * &x.2);

    for _x in 0..turns {
        for i in 0..monkeys.len() {
            let (items, oper, test, if_true, if_false, _ins) = monkeys[i].clone();

            for j in 0..items.len() {
                let item = items[j];

                let a = if oper[0] == "old" { item.clone() } else { oper[0].parse().unwrap() };
                let b = if oper[2] == "old" { item.clone() } else { oper[2].parse().unwrap() };
                let new = (match oper[1] {
                    "*" => a * b,
                    _ => a + b,
                } / divide_by) % product;

                if new % test == 0 {
                    monkeys[if_true].0.push(new);
                } else {
                    monkeys[if_false].0.push(new);
                }

                monkeys[i].5 = &monkeys[i].5 + 1;
                monkeys[i].0 = monkeys[i].0[1..].to_vec();
            }
        }
    }

    monkeys.sort_by(|a, b| b.5.cmp(&a.5));
    return monkeys[0].5 * monkeys[1].5;
}

fn main() {
    let input = fs::read_to_string("./input.txt")
        .expect("Could not read file");

    let monkeys: Vec<_> = input
        .split("\n\n")
        .map(|s| {
            let lines: Vec<_> = s.split("\n").collect();
            let items: Vec<u128> = lines[1][18..].split(", ")
                .map(|s| s.parse().unwrap()).collect();
            let oper: Vec<_> = lines[2][19..].split(" ").collect();
            let test: u128 = lines[3][21..].parse().unwrap();
            let if_true: usize = lines[4][29..].parse().unwrap();
            let if_false: usize = lines[5][30..].parse().unwrap();
            (items, oper, test, if_true, if_false, 0u128)
        })
        .collect();

    let part_1 = calc_monkey_business(20, 3, &monkeys);
    let part_2 = calc_monkey_business(10000, 1, &monkeys);

    println!("Part 1: {}", part_1);
    println!("Part 2: {}", part_2);
}
