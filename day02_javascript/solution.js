const fs = require("fs");
const readline = require("readline");

const FILENAME = "input.txt";


// utility functions start ----------------------------------------------------

async function readInput() {
  const rl = readline.createInterface({
    input: fs.createReadStream(FILENAME),
    crlfDelay: Infinity
  });
  const reports = [];

  for await (const line of rl) {
    reports.push(line.split(' ').map(x => parseInt(x)));
  }

  return reports;
}


/**
 * Check if the previous level and the current level are violating any
 * rules
 *  
 * @param {number} prev 
 * @param {number} curr 
 * @param {{increasing: boolean, decreasing: boolean}} state
 */
function isViolation(prev, curr, state) {
  let diff = Math.abs(prev - curr);
  if (diff > 3 || diff == 0) {
    return true;
  }
  if (prev > curr) {
    state.decreasing = false;
  }
  if (prev < curr) {
    state.increasing = false;
  }

  return false;
}


/** @param {int[]} levels */
function isSafe(levels) {
  let prev = levels[0];
  const state = {
    increasing: true,
    decreasing: true
  };

  for (let level of levels.slice(1)) {
    if (isViolation(prev, level, state)) {
      return false;
    }
    prev = level;
  }

  return state.decreasing || state.increasing;
}


/**
 * Check if a report is safe when using the "Problem Dampener"
 * 
 * @param {int[]} levels 
 */
function isSafeWithDampener(levels) {
  if (isSafe(levels)) {
    return true;
  }
  
  return levels.find(
    (_, idx) => isSafe(levels.slice(0, idx).concat(levels.slice(idx+1)))
  ) != undefined
}

// utility functions end ------------------------------------------------------

/** @param {int[][]} reports */
function solvePart1(reports) {
  const safeLevels = reports
    .map(levels => isSafe(levels))
    .reduce((acc, val) => acc + val);

  console.log(safeLevels)
}


/** @param {int[][]} reports */
function solvePart2(reports) {
  const safeLevels = reports
    .map(levels => isSafeWithDampener(levels))
    .reduce((acc, val) => acc + val);

  console.log(safeLevels);
}


async function main() {
	const reports = await readInput();

	console.log("Part 1:");
	solvePart1(reports);
	console.log("Part 2:");
	solvePart2(reports);
}

main()
