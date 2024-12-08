let solve_day_1 list1 list2 =
	List.combine (List.sort compare list1) (List.sort compare list2)
		|> List.fold_left (fun acc (val1, val2) -> acc + abs(val1 - val2)) 0
		|> Printf.printf "%d\n";;

let solve_day_2 list1 list2 =
	List.fold_left
		(fun sim x ->
			let count = List.fold_left
				(fun acc y -> if x = y then acc + 1 else acc) 0 list2 in
			sim + x * count) 0 list1
		|> Printf.printf "%d\n"

let read_input file =
    let ic = open_in file in
    let try_read_line () =
			try Some (input_line ic) with End_of_file -> None in
		let parse_line line =
			String.split_on_char ' ' line
				|> List.filter (fun s -> s <> "") in
    let rec loop tuple_list =
			match try_read_line () with
				| Some (line) -> (
					match parse_line line with
						| [v1; v2] -> loop ((int_of_string v1, int_of_string v2) :: tuple_list)
						| _ -> failwith "uh oh")
            | None ->
							close_in ic; List.rev tuple_list in
    loop []
		|> List.split

let (list1, list2) = read_input "input.txt";;

print_endline "Part 1 Solution:";;
solve_day_1 list1 list2;;
print_endline "Part 2 Solution:";;
solve_day_2 list1 list2;;