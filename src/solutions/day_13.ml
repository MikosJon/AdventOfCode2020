open Utils.Signature
open Utils.List_utils

module Solver : Solver = struct
  let get_data1 lines =
    let timestamp = int_of_string (List.nth lines 0)
    and cycles =
      List.filter
        (fun n -> n <> "x")
        (String.split_on_char ',' (List.nth lines 1))
    in
    (timestamp, List.map int_of_string cycles)

  let solve1 (start, cycles) =
    let id = List.nth cycles 0 in
    let first_min = (id - (start mod id), id) in
    let m, bus =
      List.fold_left
        (fun (min, min_bus) bus ->
          let r = bus - (start mod bus) in
          if r < min then (r, bus) else (min, min_bus))
        first_min cycles
    in
    m * bus

  let naloga1 vsebina_datoteke =
    vsebina_datoteke |> List.lines |> get_data1 |> solve1 |> string_of_int

  let get_data2 lines =
    let modified =
      List.mapi
        (fun i x -> (x, i))
        (String.split_on_char ',' (List.nth lines 1))
    in
    List.map
      (fun (x, i) -> (int_of_string x, i))
      (List.filter (fun (x, _) -> x <> "x") modified)

  let get_constraints buses =
    List.map
      (fun (id, pos) ->
        ( List.fold_left
            (fun acc (bus, _) ->
              if bus = id then acc else bus mod id * acc mod id)
            1 buses,
          (if pos = 0 then 0
          else if id > pos then id - pos
          else id - (pos mod id)),
          id ))
      buses

  let rec solve_equations acc eqs =
    let rec aux (a, b, c) = function
      | x when x > c ->
          failwith
            ("No solution to equation " ^ string_of_int a ^ " * x = "
           ^ string_of_int b ^ " (" ^ string_of_int c ^ ")")
      | x -> if a * x mod c = b then x else aux (a, b, c) (x + 1)
    in
    match eqs with
    | [] -> List.rev acc
    | (a, b, c) :: xs -> solve_equations (aux (a, b, c) 0 :: acc) xs

  let solve2 buses =
    let total = List.prod (List.map fst buses) in
    buses |> get_constraints |> solve_equations []
    |> List.mapi (fun i x -> x * total / fst (List.nth buses i))
    |> List.fold_left (fun acc x -> (acc + x) mod total) 0

  let naloga2 vsebina_datoteke _part1 =
    vsebina_datoteke |> List.lines |> get_data2 |> solve2 |> string_of_int
end
