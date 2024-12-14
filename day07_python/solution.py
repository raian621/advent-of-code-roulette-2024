from typing import Tuple, List

def read_input(filepath: str) -> Tuple[List[int], List[List[int]]]:
  results, expressions = [], []
  with open(filepath) as file:
    for line in file:
      results_and_numbers = line.rstrip().split(": ")
      results.append(int(results_and_numbers[0]))
      expressions.append([int(numStr) for numStr in results_and_numbers[1].split(' ')])
  return results, expressions


operators = [
  lambda x, y: x + y,         # add
  lambda x, y: x * y,         # multiply
  lambda x, y: int(f"{x}{y}") # digit concatenation
]

def can_be_evaluated_as_result(expression: List[int], result: int, acc: int, i: int, op: int, op_count: int) -> bool:
  new_acc = operators[op](acc, expression[i])

  if i == len(expression) - 1:
    return new_acc == result
  for j in range(op_count):
    op_result = can_be_evaluated_as_result(expression, result, new_acc, i+1, j, op_count)
    if op_result:
      return True

  return False 


def solve_part_1(results: List[int], expressions: List[List[int]], op_count: int = 2):
  result_sum = 0

  for expression, result in zip(expressions, results):
    if can_be_evaluated_as_result(expression, result, 0, 0, 0, op_count):
      result_sum += result

  print(result_sum)


def solve_part_2(results: List[int], expressions: List[List[int]]):
  solve_part_1(results, expressions, 3)


results, expressions = read_input("input.txt")
print("Part 1:")
solve_part_1(results, expressions)
print("Part 2:")
solve_part_2(results, expressions)