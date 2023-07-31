open Base
open Stdio

exception InvalidString of string

let read_line () =
    match In_channel.input_line stdin with
    | Some line -> Some line
    | None -> None

let score (line) =
    match line with
    | "A X" -> 1 + 3
    | "A Y" -> 2 + 6
    | "A Z" -> 3 + 0
    | "B X" -> 1 + 0
    | "B Y" -> 2 + 3
    | "B Z" -> 3 + 6
    | "C X" -> 1 + 6
    | "C Y" -> 2 + 0
    | "C Z" -> 3 + 3
    (* pretty print line with an error "Line ${line} is invalid" *)
    | _ -> raise (InvalidString ("Line " ^ line ^ " is invalid"))

let rec sum_score (sum) =
    match read_line () with
    | Some "" -> sum_score (sum)
    | Some line -> sum_score (sum + score line)
    | None -> sum

let () =
    printf "%d\n" (sum_score 0)


