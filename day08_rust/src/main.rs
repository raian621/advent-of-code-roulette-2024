use std::{fs::read_to_string, collections::{HashSet, HashMap}};


type Point = (usize, usize);


fn main() {
  let grid = read_input("input.txt");

  println!("Part 1:");
  solve_part_1(&grid);
  println!("Part 2:");
  solve_part_2(&grid);
}


fn read_input(filepath: &str) -> Vec<Vec<char>> {
  let mut grid = Vec::new();

  for line in read_to_string(filepath).unwrap().lines() {
    grid.push(line.chars().collect())
  }

  grid
}


fn insert_position_if_in_bounds(
  positions: &mut HashSet<Point>,
  row: isize,
  col: isize,
  rows: isize,
  cols: isize
) {
  if row >= 0 && row < rows && col >= 0 && col < cols {
    positions.insert((row as usize, col as usize));
  }
}


fn get_antenna_positions(grid: &[Vec<char>]) -> HashMap<char, Vec<Point>> {
  let mut antenna_positions = HashMap::<char, Vec<Point>>::new();

  for row in 0..grid.len() {
    for col in 0..grid[0].len() {
      let c = grid[row][col];
      if c.is_alphanumeric() {
        antenna_positions
          .entry(c)
          .and_modify(|positions| positions.push((row, col)))
          .or_insert(vec![(row, col)]);
      }
    }
  }

  antenna_positions
}


fn get_antinode_positions_p1(
  antenna_positions: &HashMap<char, Vec<Point>>,
  rows: isize,
  cols: isize
) -> HashSet<Point>{
  let mut antinode_positions = HashSet::new();

  for (_, positions) in antenna_positions.iter(){
    for i in 0..positions.len() {
      for j in (i+1)..positions.len() {
        let (row1, col1) = positions[i];
        let (row2, col2) = positions[j];
        let (d_row, d_col) = (row2 as isize - row1 as isize, col2 as isize - col1 as isize);
        let (anode_row_1, anode_col_1) = (row1 as isize - d_row, col1 as isize - d_col);
        let (anode_row_2, anode_col_2) = (row2 as isize + d_row, col2 as isize + d_col);

        insert_position_if_in_bounds(
          &mut antinode_positions, anode_row_1, anode_col_1, rows, cols);
        insert_position_if_in_bounds(
          &mut antinode_positions, anode_row_2, anode_col_2, rows, cols);
      }
    }
  }

  antinode_positions
}


fn insert_positions_in_bounds(
  positions: &mut HashSet<Point>,
  rows: isize,
  cols: isize,
  p1: Point,
  p2: Point
) {
  positions.insert(p1);
  positions.insert(p1);

  let (row1, col1) = p1;
  let (row2, col2) = p2;
  let d_row = row2 as isize - row1 as isize;
  let d_col = col2 as isize - col1 as isize;

  // behind p1
  let (mut row, mut col) = (row1 as isize - d_row, col1 as isize - d_col);
  while row >= 0 && row < rows && col >= 0 && col < cols {
    positions.insert((row as usize, col as usize));
    row -= d_row;
    col -= d_col;
  }

  // after p2
  let (mut row, mut col) = (row2 as isize + d_row, col2 as isize + d_col);
  while row >= 0 && row < rows && col >= 0 && col < cols {
    positions.insert((row as usize, col as usize));
    row += d_row;
    col += d_col;
  }
}


fn get_antinode_positions_p2(
  antenna_positions: &HashMap<char, Vec<Point>>,
  rows: isize,
  cols: isize
) -> HashSet<Point>{
  let mut antinode_positions = HashSet::new();

  for (_, positions) in antenna_positions.iter(){
    for i in 0..positions.len() {
      for j in (i+1)..positions.len() {
        insert_positions_in_bounds(&mut antinode_positions, rows, cols, positions[i], positions[j]);
      }
    }
  }

  antinode_positions
}


fn solve_part_1(grid: &[Vec<char>]) {
  let rows = grid.len() as isize;
  let cols = grid[0].len() as isize;
  let antenna_positions = get_antenna_positions(grid);
  let antinode_positions = get_antinode_positions_p1(
    &antenna_positions, rows, cols);

  println!("{}", antinode_positions.len());
}

fn solve_part_2(grid: &[Vec<char>]) {
  let rows = grid.len() as isize;
  let cols = grid[0].len() as isize;
  let antenna_positions = get_antenna_positions(grid);
  let antinode_positions = get_antinode_positions_p2(
    &antenna_positions, rows, cols);

  println!("{}", antinode_positions.len());
}