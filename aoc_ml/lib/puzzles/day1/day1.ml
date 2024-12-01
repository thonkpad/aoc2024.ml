let file = "day1.txt"

let split_inputs file =
  let ic = open_in file in
  let rec read_lines left right =
    try
      let line = input_line ic in
      let pair = Scanf.sscanf line "%d %d" (fun l r -> (l, r)) in
      read_lines (fst pair :: left) (snd pair :: right)
    with End_of_file ->
      close_in ic ;
      (List.rev left, List.rev right)
  in
  read_lines [] []

let sorted lst = List.sort (fun a b -> compare a b) lst

let part1 =
  let left, right = split_inputs file in
  let sorted_left = sorted left in
  let sorted_right = sorted right in
  let distance_pairs =
    List.map2 (fun x y -> abs (x - y)) sorted_right sorted_left
  in
  List.fold_left ( + ) 0 distance_pairs

let part2 =
  let left, right = split_inputs file in
  let count x lst = List.length (List.filter (fun y -> y == x) lst) in
  let count_occurrences left right = List.map (fun x -> count x right) left in
  let scores =
    List.map2 (fun x y -> x * y) left (count_occurrences left right)
  in
  List.fold_left ( + ) 0 scores

let () =
  Printf.printf "Part 1: %d\n" part1 ;
  Printf.printf "Part 2: %d\n" part2
