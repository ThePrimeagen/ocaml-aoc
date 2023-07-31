module CharMap = Map.Make(Char)

let split_once str point =
    let first = String.sub str 0 point in
    let second = String.sub str point (String.length str - point) in
    (first, second)

let get_value k =
    match k with
    | 'a'..'z' ->
        Char.code k - Char.code 'a' + 1
    | 'A'..'Z' ->
        Char.code k - Char.code 'A' + 1 + 26
    | _ -> 0

let get_priority str =
    let (first, second) = split_once str (String.length str / 2) in
    let first = String.fold_left (fun map c -> CharMap.add c 1 map) CharMap.empty first in
    let second = String.fold_left (fun map c -> CharMap.add c 1 map) CharMap.empty second in
    let doubles = CharMap.merge (fun _ a b -> match a, b with
        | Some a, Some b -> Some (a + b)
        | Some a, None -> Some a
        | None, Some b -> Some b
        | None, None -> None
    ) first second
    |> CharMap.filter (fun _ v -> v >= 2) in

    CharMap.fold (fun k _ acc -> acc + get_value k) doubles 0

let read_line () =
    match In_channel.input_line stdin with
    | Some line -> Some line
    | None -> None

let rec sum acc =
    match read_line () with
    | Some "" -> sum acc
    | Some line -> sum (acc + get_priority line)
    | None -> acc

let () =
    Printf.printf "%d\n" (sum 0)

