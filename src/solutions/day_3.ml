open Utils.Signature
open Utils.List_utils

module Solver : Solver = struct
  let r_step1 = 3

  let rec count_trees1 col = function
    | [] -> 0
    | x :: xs ->
        (if Char.equal x.[col] '#' then 1 else 0)
        + count_trees1 ((col + r_step1) mod String.length x) xs

  let rec count_trees2 col (r_step, d_step) = function
    | [] -> 0
    | x :: xs -> (
        let tree = if Char.equal x.[col] '#' then 1 else 0 in
        if d_step = 1 then
          tree
          + count_trees2
              ((col + r_step) mod String.length x)
              (r_step, d_step) xs
        else
          match xs with
          | _ :: ys ->
              tree
              + count_trees2
                  ((col + r_step) mod String.length x)
                  (r_step, d_step) ys
          | _ -> tree)

  let naloga1 vsebina_datoteke =
    let rows = List.lines vsebina_datoteke in
    string_of_int (count_trees1 0 rows)

  let naloga2 vsebina_datoteke _part1 =
    let steps = [ (1, 1); (3, 1); (5, 1); (7, 1); (1, 2) ] in
    let path_functions = List.map (count_trees2 0) steps
    and forest = List.lines vsebina_datoteke in
    string_of_int (List.prod (List.apply forest path_functions))
end
