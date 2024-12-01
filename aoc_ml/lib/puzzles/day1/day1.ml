let file = "day1.txt"

let split_inputs file =
  let ic = open_in file in
  let rec read_lines left right =
    try
      input_line ic
      |> fun line ->
      Scanf.sscanf line "%d %d" (fun l r -> (l, r))
      |> fun (l, r) -> read_lines (l :: left) (r :: right)
    with End_of_file ->
      close_in ic ;
      (left |> List.rev, right |> List.rev)
  in
  read_lines [] []

let sorted lst = List.sort (fun a b -> compare a b) lst

let part1 =
  split_inputs file
  |> fun (left, right) ->
  let sorted_left = sorted left in
  let sorted_right = sorted right in
  List.map2 (fun x y -> abs (x - y)) sorted_right sorted_left
  |> List.fold_left ( + ) 0

let part2 =
  split_inputs file
  |> fun (left, right) ->
  let count x lst = lst |> List.filter (fun y -> y == x) |> List.length in
  let count_occurrences left right =
    left |> List.map (fun x -> count x right)
  in
  let scores =
    count_occurrences left right |> List.map2 (fun x y -> x * y) left
  in
  scores |> List.fold_left ( + ) 0

let () =
  Printf.printf "Part 1: %d\n" part1 ;
  Printf.printf "Part 2: %d\n" part2
