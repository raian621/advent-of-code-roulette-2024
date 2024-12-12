function read_input(filename::String)
  lines = readlines(filename)
  i = 1

  # read ordering
  ordering = Dict{Int64, Set{Int64}}()
  while length(lines[i]) > 0
    from_str, to_str = split(lines[i], "|")
    from_num, to_num = parse(Int64, from_str), parse(Int64, to_str)
    if haskey(ordering, from_num)
      push!(ordering[from_num], to_num)
    else
      ordering = merge(ordering, Dict(from_num => Set([to_num])))
    end
    i += 1
  end
  i += 1

  # read list
  updates = Vector{Int64}[]
  while i <= length(lines)
    push!(updates, map((x) -> parse(Int64, x), split(lines[i], ",")))
    i += 1
  end

  return ordering, updates
end


# get a "less than" function based on the ordering constraints of the `ordering` Dict
function get_lt_callback(ordering::Dict{Int64, Set{Int64}})
  return (x, y) -> begin
    if haskey(ordering, x)
      return y in ordering[x]
    end
    return false
  end
end


function solve_part_1(
  ordering::Dict{Int64, Set{Int64}},
  updates::Vector{Vector{Int64}}
)
  sum = 0
  lt = get_lt_callback(ordering)

  for update in updates
    if issorted(update, lt = lt)
      sum += update[Int(ceil(length(update) // 2))]
    end
  end

  println(sum)
end


function solve_part_2(
  ordering::Dict{Int64, Set{Int64}},
  updates::Vector{Vector{Int64}}
)
  sum = 0
  lt = get_lt_callback(ordering)

  for update in updates
    if !issorted(update, lt = lt)
      update = sort(update, lt = lt)
      sum += update[Int(ceil(length(update) // 2))]
    end
  end

  println(sum)
end


ordering, updates = read_input("input.txt")

print("Part 1:\n")
solve_part_1(ordering, updates)
print("Part 2:\n")
solve_part_2(ordering, updates)