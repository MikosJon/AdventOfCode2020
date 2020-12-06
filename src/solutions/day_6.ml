open Utils.Signature
open Utils.List_utils
open Utils.String_utils

module Solver : Solver = struct
  let ascii_list =
    [
      'a';
      'b';
      'c';
      'd';
      'e';
      'f';
      'g';
      'h';
      'i';
      'j';
      'k';
      'l';
      'm';
      'n';
      'o';
      'p';
      'q';
      'r';
      's';
      't';
      'u';
      'v';
      'w';
      'x';
      'y';
      'z';
    ]

  let process_line acc = function
    | "" -> acc
    | s ->
        let li = String.explode s in
        acc @ List.filter (fun x -> not (List.mem x acc)) li

  let process_line2 acc line =
    match acc with
    | [] -> []
    | _ -> List.filter (fun x -> String.contains line x) acc

  let rec get_answers li current = function
    | [] -> current :: li
    | x :: xs ->
        if x = "" then get_answers (current :: li) [] xs
        else get_answers li (process_line current x) xs

  let rec get_answers2 li current = function
    | [] -> current :: li
    | x :: xs ->
        if x = "" then get_answers2 (current :: li) ascii_list xs
        else get_answers2 li (process_line2 current x) xs

  let naloga1 vsebina_datoteke =
    vsebina_datoteke |> List.lines |> get_answers [] []
    |> List.map (fun x -> List.length x)
    |> List.sum |> string_of_int

  let naloga2 vsebina_datoteke _part1 =
    vsebina_datoteke |> List.lines |> get_answers2 [] ascii_list
    |> List.map (fun x -> List.length x)
    |> List.sum |> string_of_int
end
