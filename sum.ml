open Base
open Stdio

let rec sum_lines accum =
    match In_channel.input_line In_channel.stdin with
    | Some "" -> accum
    | Some value ->
        sum_lines (Int.of_string value + accum)
    | None -> accum

let max_3 x (a, b, c) =
    if x > a then
        (x, a, b)
    else if x > b then
        (a, x, b)
    else if x > c then
        (a, b, x)
    else
        (a, b, c)

let rec find_max (a, b, c) =
    match sum_lines 0 with
    | 0 -> (a, b, c)
    | x -> find_max (max_3 x (a, b, c))

let () =
    let (a, b, c) = find_max (0, 0, 0) in
    printf "%d\n" (a + b + c)

