
class Solution {
  static void main(String[] args) {
    GridState gs = readInput("input.txt");
    println("Part 1:");
    solvePart1(gs.grid, gs.row, gs.col, gs.direction);
    println("Part 2:");
    solvePart2(gs.grid, gs.row, gs.col, gs.direction);
  }


  static GridState readInput(String filepath) {
    GridState gs = new GridState();
    File file = new File(filepath);

    file.eachLine { line -> gs.grid.add(line.toCharArray()) };
    for (def row = 0; row < gs.grid.size(); row++) {
      for (def col = 0; col < gs.grid[0].length; col++) {
        switch (gs.grid[row][col]) {
          case '^':
            gs.direction = Directions.UP;
            gs.row = row;
            gs.col = col;
            return gs;
          case '>':
            gs.direction = Directions.RIGHT;
            gs.row = row;
            gs.col = col;
            return gs;
          case 'v':
            gs.direction = Directions.DOWN;
            gs.row = row;
            gs.col = col;
            return gs;
          case '<':
            gs.direction = Directions.LEFT;
            gs.row = row;
            gs.col = col;
            return gs;
        }
      }
    }

    return gs;
  }


  static void solvePart1(ArrayList<char[]> grid, int row, int col, int direction) {
    def tilesCovered = 0;
    def rows = grid.size();
    def cols = grid[0].length;

    while (row >= 0 && row < rows && col >= 0 && col < cols) {
      if (grid[row][col] != '*') {
        grid[row][col] = '*';
        tilesCovered++;
      }
      switch (direction) {
        case Directions.UP:
          row--;
          if (row > 0 && (grid[row - 1][col]) == "#") {
            direction = (direction + 1) % 4;
          }
          break;
        case Directions.RIGHT:
          col++;
          if (col < cols - 1 && (grid[row][col + 1]) == "#") {
            direction = (direction + 1) % 4;
          }
          break;
        case Directions.DOWN:
          row++;
          if (row < rows - 1 && (grid[row + 1][col]) == "#") {
            direction = (direction + 1) % 4;
          }
          break;
        case Directions.LEFT:
          col--;
          if (col > 0 && (grid[row][col - 1]) == "#") {
            direction = (direction + 1) % 4;
          }
      }
    }

    println(tilesCovered);
  }


  static void solvePart2(ArrayList<char[]> grid, int row, int col, int direction) {
    def obstaclePositions = 0;
    def rows = grid.size();
    def cols = grid[0].length;

    for (def r = 0; r < rows; r++) {
      for (def c = 0; c < cols; c++) {
        if (grid[r][c] != '#') {
          grid[r][c] = '#';
          Guard fast = new Guard(row, col, direction);
          Guard slow = new Guard(row, col, direction);
          if (detectCycle(grid, slow, fast)) {
            obstaclePositions++;
          }
          grid[r][c] = '.';
        }
      }
    }

    println(obstaclePositions);
  }


  // Check if the guard enters a cycle using Floyd's cycle detection algorithm
  static boolean detectCycle(ArrayList<char[]> grid, Guard fast, Guard slow) {
    for (;;) {
      // guard exits grid, no cycle
      if (!fast.advance(grid)) {
        return false;
      }
      if (!fast.advance(grid)) {
        return false;
      }
      slow.advance(grid);

      if (slow.equals(fast)) {
        return true;
      }
    }
    return false; // dead code
  }
}


class Directions {
  public static final int UP = 0;
  public static final int RIGHT = 1;
  public static final int DOWN = 2;
  public static final int LEFT = 3;

  public static final int[][] direction = [
    [-1,  0],
    [ 0,  1],
    [ 1,  0],
    [ 0, -1]
  ];
}


class GridState {
  public ArrayList<char[]> grid;
  public int row;
  public int col;
  public int direction;

  public GridState() {
    grid = new ArrayList<>();
  }
}


class Guard {
  private int row;
  private int col;
  private int direction;

  public Guard(int row, int col, int direction) {
    this.row = row;
    this.col = col;
    this.direction = direction;
  }

  // Advance the guard to the next obstacle.
  // Returns false if the guard moves outside of the grid, true otherwise.
  public boolean advance(ArrayList<char[]> grid) {
    def rows = grid.size();
    def cols = grid[0].length;

    while (row >= 0 && row < rows && col >= 0 && col < cols) {
      def nextRow = row + Directions.direction[direction][0];
      def nextCol = col + Directions.direction[direction][1];
      
      if (nextRow < 0 || nextRow >= rows || nextCol < 0 || nextCol >= cols) {
        return false;
      }

      if (grid[nextRow][nextCol] == '#') {
        direction = (direction + 1) % 4;  
        return true;
      }

      row = nextRow;
      col = nextCol;
    }

    return false;
  }

  public boolean equals(Guard other) {
    return row == other.row && col == other.col && direction == other.direction;    
  }

  public String toString() {
    return "Guard{ row = ${row}, col = ${col}, direction = ${direction} }";
  }
}