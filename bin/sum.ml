open Base
open Stdio

let read_lines () =
  In_channel.fold_lines In_channel.stdin ~init:[] ~f:(fun acc line ->
      match Int.of_string_opt @@ String.strip line with
      | None -> acc
      | Some int -> int :: acc)

let rec sum_lines = function [] -> 0 | hd :: tl -> hd + sum_lines tl

let find_max_3 lst =
  let sorted = List.sort lst ~compare:Int.compare in
  match sorted with
  | [] -> (0, 0, 0)
  | [ a ] -> (a, 0, 0)
  | [ a; b ] -> (a, b, 0)
  | a :: b :: c :: _tl -> (a, b, c)

let () =
  let int_lst = read_lines () in
  let sum = sum_lines int_lst in
  let a, b, c = find_max_3 int_lst in
  printf "sum: %d\n" sum;
  printf "max 3: %d %d %d\n" a b c;
  printf "max 3 sum: %d\n" (a + b + c)
