#![feature(array_zip)]

use std::fmt;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Digit {
    pub states: [bool; 9],
}
impl Digit {
    pub fn empty() -> Self { Self { states: [true; 9] } }
    pub fn contradiction() -> Self { Self { states: [false; 9] } }
    pub fn from_number(mut number: i32) -> Digit {
        let mut out = Digit::empty();
        while number != 0 {
            out = out.remove(number % 10);
            number /= 10;
        }
        out
    }
    pub fn add(mut self, other: i32) -> Digit {
        self.states[other as usize - 1] = true;
        self
    }
    pub fn remove(mut self, other: i32) -> Digit {
        self.states[other as usize - 1] = false;
        self
    }
    pub fn and(self, other: Digit) -> Digit {
        Digit { states: self.states.zip(other.states).map(|(a,b)| a&b) }
    }
    pub fn digits(self) -> impl Iterator<Item=i32> {
        self.states.into_iter().enumerate().filter(|(i,c)| *c).map(|c| c.0 as i32 + 1)
    }
}
impl fmt::Display for Digit {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[")?;
        for i in self.digits() {
            write!(f, "{}", i)?;
        }
        write!(f, "]")?;
        Ok(())
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Op { Add, Sub, Mul }

#[derive(Debug, Clone, PartialEq)]
pub enum Cell {
    Digit(Digit),
    Op(Op)
}

#[derive(Debug)]
pub struct Board {
    width: usize,
    data: Vec<Cell>,
    row_clues: Vec<Clue>,
    col_clues: Vec<Clue>,
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (i,c) in self.data.iter().enumerate() {
            match c {
                Cell::Digit(c) => write!(f, "{}", c)?,
                Cell::Op(op) => write!(f, " {} ", match op {
                    Op::Add => "+", Op::Sub => "-", Op::Mul => "*"
                })?,
            }
            if i % self.width == self.width-1 { writeln!(f); }
        }
        Ok(())
    }
}

impl Board {
    pub fn parse(table: &str, width: usize, row_clues: Vec<Clue>, col_clues: Vec<Clue>) -> Self {
        let data = table.chars().filter(|c| !c.is_whitespace()).map(|c| match c {
            '.' => Cell::Digit(Digit::empty()),
            '+' => Cell::Op(Op::Add),
            '-' => Cell::Op(Op::Sub),
            '*' => Cell::Op(Op::Mul),
            _ => todo!()
        }).collect();
        Board {
            width,
            data,
            row_clues,
            col_clues
        }
    }
    pub fn line_for_row(&mut self, r: usize) -> Line<'_> {
        let mut start = r * self.width;
        let mut end = start + self.width;
        while matches!(self.data[start], Cell::Op(_)) { start += 1; }
        while matches!(self.data[end-1], Cell::Op(_)) { end -= 1; }
        Line {
            clue: &self.row_clues[r],
            expr: self.data[start..end].iter_mut().collect()
        }
    }
    pub fn line_for_col(&mut self, r: usize) -> Line<'_> {
        let mut start = r;
        let mut end = self.data.len() - self.width + r;
        while matches!(self.data[start], Cell::Op(_)) { start += self.width; }
        while matches!(self.data[end], Cell::Op(_)) { end -= self.width; }
        let mut expr = self.data[start..=end].iter_mut().step_by(self.width).collect::<Vec<_>>();
        Line {
            clue: &self.col_clues[r],
            expr
        }
    }
    pub fn step(&mut self) -> bool {
        let backup = self.data.clone();
        for i in 0..self.data.len()/self.width {
            let mut line = self.line_for_row(i);
            //if !matches!(line.clue, Clue::None) {
                line.apply_step();
            //}
        }
        println!("{}", self);
        for i in 0..self.width {
            let mut line = self.line_for_col(i);
            //if !matches!(line.clue, Clue::None) {
                line.apply_step();
            //}
        }
        println!("{}", self);
        self.data != backup
    }
}

#[derive(Debug)]
pub enum Clue {
    None,
    Exact(i32),
    GreaterThan(i32),
    Custom
}
impl Clue {
    pub fn check(&self, a: i32) -> bool {
        match self {
            Clue::None => true,
            Clue::Exact(b) => a == *b,
            Clue::GreaterThan(b) => a > *b,
            Clue::Custom => todo!()
        }
    }
}

#[derive(Debug)]
pub struct Line<'a> {
    pub clue: &'a Clue,
    pub expr: Vec<&'a mut Cell>
}
impl fmt::Display for Line<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for i in self.expr.iter() {
            match i {
                Cell::Digit(c) => write!(f, "{}", c)?,
                Cell::Op(op) => write!(f, "{}", match op {
                    Op::Add => "+", Op::Sub => "-", Op::Mul => "*"
                })?,
            }
        }
        Ok(())
    }
}

impl Line<'_> {
    pub fn enumerate(&self) -> Vec<LineSolution> {
        let mut out = vec![];
        enumerate_line(&self.expr, &mut out, LineSolution::default(), Digit::empty(), &self.clue);
        out
    }
    pub fn apply_step(&mut self) {
        let ls = self.enumerate();
        for i in self.expr.iter_mut() {
            match i {
                Cell::Digit(ref mut c) => *c = Digit::contradiction(),
                _ => {}
            }
        }
        for i in ls {
            for (i,c) in i.cells.iter().zip(self.expr.iter_mut()) {
                match (i,c) {
                    (CellSolution::Digit(i), Cell::Digit(c)) => *c = c.add(*i),
                    _ => {}
                }
            }
        }
    }
}

pub fn enumerate_line(
    cells: &[&mut Cell],
    out: &mut Vec<LineSolution>,
    partial: LineSolution,
    constraint: Digit,
    clue: &Clue
) {
    let (c, rest) = cells.split_first().unwrap();
    let c = match c {
        Cell::Op(op) => {
            let mut partial = partial.clone();
            partial.cells.push(CellSolution::Op(op.clone()));
            return enumerate_line(rest, out, partial, constraint, clue);
        },
        Cell::Digit(c) => c,
    };
    for i in c.and(constraint).digits() {
        let mut partial = partial.clone();
        partial.cells.push(CellSolution::Digit(i));
        if rest.len() == 0 {
            if clue.check(partial.value()) {
                out.push(partial);
            }
        } else {
            let constraint = constraint.remove(i);
            enumerate_line(rest, out, partial, constraint, clue);
        }
    }
}

#[derive(Debug, Clone)]
pub enum CellSolution {
    Digit(i32),
    Op(Op),
}
impl CellSolution {
    pub fn as_digit(&self) -> Option<i32> {
        match self {
            CellSolution::Digit(c) => Some(*c),
            _ => None
        }
    }
    pub fn as_op(&self) -> Option<Op> {
        match self {
            CellSolution::Op(c) => Some(*c),
            _ => None
        }
    }
}
#[derive(Debug, Default, Clone)]
pub struct LineSolution {
    pub cells: Vec<CellSolution>
}

impl fmt::Display for LineSolution {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for i in self.cells.iter() {
            match i {
                CellSolution::Digit(c) => write!(f, "{}", c)?,
                CellSolution::Op(op) => write!(f, "{}", match op {
                    Op::Add => "+", Op::Sub => "-", Op::Mul => "*"
                })?,
            }
        }
        Ok(())
    }
}

impl LineSolution {
    pub fn value(&self) -> i32 {
        if self.cells.iter().any(|c| c.as_op() == Some(Op::Sub) || c.as_op() == Some(Op::Mul)) {
            return self.value_mul();
        }
        //todo: order of operations
        let mut sum = 0;
        let mut cur = 0;
        for i in self.cells.iter() {
            match i {
                CellSolution::Digit(c) => { cur *= 10; cur += c; },
                CellSolution::Op(op) => { sum += cur; cur = 0; },
            }
        }
        sum+cur
    }
    pub fn value_mul(&self) -> i32 {
        let mut expr = vec![];
        let mut cur = 0;
        for i in self.cells.iter() {
            match i {
                CellSolution::Digit(c) => { cur *= 10; cur += c; },
                CellSolution::Op(op) => {
                    if cur == 0 {
                        let c = expr.last_mut().unwrap();
                        if matches!(c, CellSolution::Op(Op::Add)) {
                            *c = CellSolution::Op(*op);
                        }
                    } else {
                        expr.push(CellSolution::Digit(cur));
                        expr.push(CellSolution::Op(*op));
                        cur = 0;
                    }
                }
            }
        }
        expr.push(CellSolution::Digit(cur));
        //println!("{:?}", expr);
        fn parse_muldiv(tokens: &mut &[CellSolution]) -> i32 {
            let mut prod = tokens[0].as_digit().unwrap();
            *tokens = &tokens[1..];
            while tokens.len() > 0 && tokens[0].as_op() == Some(Op::Mul) {
                *tokens = &tokens[1..];
                prod *= tokens[0].as_digit().unwrap();
                *tokens = &tokens[1..];
            }
            prod
        }
        fn parse_addsub(tokens: &mut &[CellSolution]) -> i32 {
            let mut sum = parse_muldiv(tokens);
            while tokens.len() > 0 {
                match tokens[0].as_op() {
                    Some(Op::Add) => {
                        *tokens = &tokens[1..];
                        sum += parse_muldiv(tokens);
                    },
                    Some(Op::Sub) => {
                        *tokens = &tokens[1..];
                        sum -= parse_muldiv(tokens);
                    },
                    _ => break,
                }
            }
            sum
        }
        parse_addsub(&mut &expr[..])
    }
}



fn main() {
    use std::fmt::Write;
/*
    let board = r"
..++
.+..
+.++
....
".trim();
    let row_clues = vec![Clue::Exact(23), Clue::Exact(43), Clue::None, Clue::None];
    let col_clues = vec![Clue::Exact(24), Clue::Exact(45), Clue::Exact(9), Clue::Exact(11)];
    let mut board = Board::parse(board, 4, row_clues, col_clues);
*/
/*
    let board = r"
.+..+
++...
.....
.+.++
...+.
".trim();
    use Clue::Exact as E;
    use Clue::None as N;
    let row_clues = vec![E(75), E(514), N, E(15), E(525)];
    let col_clues = vec![E(367), E(3), N, E(312), E(49)];
    let mut board = Board::parse(board, 5, row_clues, col_clues);
*/
/*
    let board = r"
...+...
.+.....
..+.++.
+....++
..++...
.+.....
..+...+
".trim();
    use Clue::Exact as E;
    use Clue::None as N;
    let row_clues = vec![E(1369), N, E(30),E(4356), E(438), N, E(937)];
    let col_clues = vec![E(1553),E(855),E(28),E(464),E(6393),E(841),E(761)];
    let mut board = Board::parse(board, 7, row_clues, col_clues);
*/
/*
    let board = r"
...++..
+....*.
.+++..+
...+...
....+..
+...++.
...+...
".trim();
    use Clue::Exact as E;
    use Clue::None as N;
    let row_clues = vec![E(329),E(9516),N,E(1806),E(4820),E(178),E(581)];
    let col_clues = vec![E(597),N,E(5721),N,E(982),E(1937),E(1317)];
    let mut board = Board::parse(board, 7, row_clues, col_clues);
*/
/*
    let board = r"
...+...
..+..++
+++.+*.
.......
.+.*..+
+.+..+.
...+...
".trim();
    use Clue::Exact as E;
    use Clue::None as N;
    let row_clues = vec![E(416), E(95),N,N,Clue::GreaterThan(500),E(56),E(765)];
    let col_clues = vec![E(56),E(84),E(34),E(3460),N,E(332),E(63)];
    let mut board = Board::parse(board, 7, row_clues, col_clues);
*/
/*
    let board = r"
..+...+..
.........
..*.++...
+...+...+
.+.-..+..
.++.+++..
++....+..
....+...*
..+...+..
".trim();
    use Clue::Exact as E;
    use Clue::None as N;
    let row_clues = vec![E(641), N, E(1035), E(1493), E(41), N, E(4844), E(3317), E(212)];
    let col_clues = vec![E(397), E(1314), E(515), E(518), N, E(1100), N, N, E(3099)];
    let mut board = Board::parse(board, 9, row_clues, col_clues);
    */
    /*
    let board = r"
+....+...
+...+....
...+.+...
.+..+..+.
..+...+..
+....+...
.*+++..+.
.........
+...+....
".trim();
    use Clue::Exact as E;
    use Clue::None as N;
    let row_clues = vec![N, E(7680),E(875), E(81), E(544), E(3583), N, N, E(5459)];
    let col_clues = vec![E(461), E(1810),N,E(793),E(110),E(833),E(7678),N,N];
    let mut board = Board::parse(board, 9, row_clues, col_clues);
*/
    let board = r"
...+..+
..+....
+.+.++.
.+..+..
...+..+
+...+..
..+..+.
".trim();
    use Clue::Exact as E;
    use Clue::None as N;
    let row_clues = vec![E(542),E(4756),N,E(137),E(948),E(249),E(105)];
    let col_clues = vec![E(120),E(844),N,E(474),N,E(855),E(645)];
    let mut board = Board::parse(board, 7, row_clues, col_clues);

    while board.step() {
    }

/*
    let mut cells = vec![
        Cell::Digit(Digit::empty()),
        Cell::Op(Op::Add),
        Cell::Digit(Digit::empty()),
        Cell::Op(Op::Sub),
        Cell::Digit(Digit::empty()),
        Cell::Digit(Digit::empty()),
        Cell::Op(Op::Add),
        Cell::Digit(Digit::empty()),
    ];
    let mut line1 = Line {
        clue: &Clue::Exact(1937),
        expr: cells.iter_mut().collect()
    };
    line1.apply_step();
    println!("{}", line1);
    */
}
