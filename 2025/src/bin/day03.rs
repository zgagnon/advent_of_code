use advent_of_code_2025::advent;
type Bank = Vec<u64>;
type Mask = Vec<u64>;

fn main() {
    let input = include_str!("../../inputs/day03.txt");
    println!("Part 1: {}", part1(input));
    println!("Part 2: {}", part2(input));
}

fn part1(input: &str) -> u64 {
    input
        .lines()
        .map(advent::string_to_digits)
        .map(|line| walk_the_line(line))
        .sum()
}

fn part2(input: &str) -> u64 {
    input
        .lines()
        .map(advent::string_to_digits)
        .map(|line| lower_the_ceiling(line))
        .sum()
}
fn n_long_of_y(n: usize, y: u64) -> Mask {
    advent::string_to_digits(y.to_string().repeat(n).as_str())
}

fn lower_the_ceiling(line: Bank) -> u64 {
    let len = line.len();
    let masks = (1..=9).rev().map(|x| n_long_of_y(len, x));

    let tallest =
        masks
            .map(|mask| and(line.clone(), mask))
            .fold(n_long_of_y(len, 0), |masked, mask| {
                let length = length(&masked);

                if length >= 12 {
                    println!("Have twelve! {}", advent::digits_to_string(&masked));
                    masked
                } else {
                    let or = or(masked, mask);
                    println!("Adding more! {}", advent::digits_to_string(&or));
                    or
                }
            });
    cut_down(tallest)
}

fn and(line: Bank, mask: Mask) -> Mask {
    let result = line
        .iter()
        .zip(mask.iter())
        .map(|item| match item {
            (a, b) if a == b => a,
            _ => &0,
        })
        .copied()
        .collect();
    // println!(
    //     "{}\n{}\n{}\n{}",
    //     advent::digits_to_string(&line),
    //     advent::digits_to_string(&mask),
    //     advent::digits_to_string(&result),
    //     "*".repeat(line.len())
    result
}

fn length(line: &Mask) -> usize {
    line.iter()
        .filter(|&&x| x != 0)
        .copied()
        .collect::<Mask>()
        .len()
}

fn cut_down(line: Bank) -> u64 {
    let mut smallest = 1;
    let mut fin = line.clone();
    while length(&fin) > 12 {
        let mut findable = fin.iter().copied();
        let found = findable.position(|x| x == smallest);
        match found {
            Some(e) => {
                fin[e] = 0;
                fin = fin.iter().filter(|&&x| x != 0).copied().collect::<Vec<_>>();
                println!("Cut down to {}", advent::digits_to_string(&fin));
            }
            _ => {
                smallest = smallest + 1;
                println!("Smallest now {}", smallest);
            }
        }
    }
    advent::digits_to_digit(&fin)
}

fn or(a: Mask, b: Mask) -> Mask {
    a.iter()
        .zip(b.iter())
        .map(|(x, y)| if x > y { x } else { y })
        .copied()
        .collect()
}

fn walk_the_line(line: Vec<u64>) -> u64 {
    let mut forward = line.clone();
    forward.pop();
    let (tens_index, tens) = forward
        .iter()
        .enumerate()
        .reduce(|(max_index, max), (next_index, next)| {
            if next > max {
                (next_index, next)
            } else {
                (max_index, max)
            }
        })
        .expect("line should not be empty");

    let ones = line[tens_index + 1..line.len()]
        .iter()
        .max()
        .expect("line slice shouldn't be empty");
    tens * 10 + ones
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        let input = include_str!("../../inputs/day03_example.txt");
        assert_eq!(part1(input), 357);
    }

    #[test]
    fn test_part2() {
        let input = include_str!("../../inputs/day03_example.txt");
        assert_eq!(part2(input), 3121910778619);
    }
}
