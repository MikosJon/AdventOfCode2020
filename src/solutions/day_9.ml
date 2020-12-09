open Utils.Signature
open Utils.List_utils

module Solver : Solver = struct
  let map_contains x mem_li = List.exists (fun (_, li) -> List.mem x li) mem_li

  let rec map_update x = function
    | [] -> [ (x, []) ]
    | (t, li) :: ts when List.length ts < 24 ->
        (t, (x + t) :: li) :: map_update x ts
    | _ :: ts when List.length ts = 24 -> map_update x ts
    | _ -> failwith "mem_list too long"

  let rec get_odd_one_out prev_li = function
    | [] -> failwith "No bad numbers"
    | x :: xs ->
        (* print_mem prev_li; *)
        if map_contains x prev_li then get_odd_one_out (map_update x prev_li) xs
        else x

  let rec make_start_mem acc = function
    | [] -> List.rev acc
    | x :: xs ->
        let new_acc = List.map (fun (num, li) -> (num, (num + x) :: li)) acc in
        make_start_mem new_acc xs

  let make_mem_li preamble =
    make_start_mem (List.map (fun x -> (x, [])) preamble) preamble

  let rec find_partial_sum goal acc = function
    | [] -> failwith "No partial sums match goal!"
    | x :: xs -> (
        match List.find_opt (fun (n, _) -> n = goal - x) acc with
        | None ->
            let new_acc =
              (x, [ x ]) :: List.map (fun (n, li) -> (n + x, x :: li)) acc
            in
            find_partial_sum goal new_acc xs
        | Some (_, li) -> li)

  let naloga1 vsebina_datoteke =
    let lines = vsebina_datoteke |> List.lines |> List.map int_of_string in
    let preamble = List.filteri (fun i _ -> i < 25) lines
    and values = List.filteri (fun i _ -> i >= 25) lines in
    values |> get_odd_one_out (make_mem_li preamble) |> string_of_int

  let naloga2 vsebina_datoteke part1 =
    let lines = vsebina_datoteke |> List.lines |> List.map int_of_string in
    lines |> find_partial_sum (int_of_string part1) [] |> fun li ->
    List.max li + List.min li |> string_of_int
end
