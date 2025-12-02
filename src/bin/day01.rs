fn main() {
    let input = include_str!("../../inputs/day01.txt");
    println!("Part 1: {}", part1(input, 50));
    println!("Part 2: {}", part2(input, 50));
}

fn part1(input: &str, start: i32) -> i32 {
    input
        .lines()
        .map(leading_zero)
        .map(to_tuple)
        .map(|t| {
            let (a, b) = t;
            (a, parse(b))
        })
        .fold((start, 0_i32), |acc, x| {
            let (current, count) = acc;
            let (direction, value) = x;
            let result = match direction.as_str() {
                "L" => left(current, value),
                "R" => right(current, value),
                &_ => 0,
            };

            // println!(
            //     "{}: {}{} => {} | {}",
            //     current, direction, value, result, count
            // );
            (result, if result == 0 { count + 1 } else { count })
        })
        .1
}

fn part2(input: &str, start: i32) -> i32 {
    println!("YAY Part 2!");
    input
        .lines()
        .map(leading_zero)
        .map(to_tuple)
        .map(|t| {
            let (a, b) = t;
            (a, parse(b))
        })
        .fold((start, 0_i32), |acc, x| {
            let (current, count) = acc;
            let (direction, value) = x;
            let go_rounds = value.abs() / 100;
            let result = match direction.as_str() {
                "L" => current - (value % 100),
                "R" => current + (value % 100),
                &_ => 0,
            };
            let new_count = count_em(current, result, go_rounds) + count;
            (result.rem_euclid(100), new_count)
        })
        .1
}

fn count_em(start: i32, end: i32, go_rounds: i32) -> i32 {
    let result = match (start, end) {
        (0, _) => go_rounds,
        (100, _) => go_rounds,
        (_, b) if b > 0 && b < 100 => go_rounds,
        _ => go_rounds + 1,
    };
    result
}
fn left(start: i32, numbers: i32) -> i32 {
    (start - numbers).rem_euclid(100_i32)
}
fn right(start: i32, numbers: i32) -> i32 {
    (start + numbers).rem_euclid(100_i32)
}

fn leading_zero(string: &str) -> String {
    match string.len() {
        2 => format!("{}00{}", &string[0..1], &string[1..2]),
        3 => format!("{}0{}", &string[0..1], &string[1..3]),
        4 => string.to_string(),
        _ => panic!("wrong length {}", string),
    }
}

fn to_tuple(s: String) -> (String, String) {
    (s[0..1].to_string(), s[1..4].to_string())
}

fn parse(s: String) -> i32 {
    match s.parse::<i32>() {
        Ok(num) => num,
        Err(e) => panic!("Got a not-number{}\n{}", s, e),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        let input = include_str!("../../inputs/day01_example.txt");
        assert_eq!(part1(input, 50), 3);
    }

    #[test]
    fn test_part2() {
        let input = include_str!("../../inputs/day01_example.txt");
        assert_eq!(part2(input, 50), 6);
    }
}
