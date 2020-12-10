open Utils.Signature
open Utils.List_utils
include Map

module Solver : Solver = struct
  module MemoMap = Map.Make (Int)

  let naloga1 vsebina_datoteke =
    let lines =
      vsebina_datoteke |> List.lines |> List.int_list
      |> List.sort Stdlib.compare
    in
    let one_gaps =
      lines
      |> List.filteri (fun i x ->
             if i = 0 then x = 1 else x - List.nth lines (i - 1) = 1)
      |> List.length
    and three_gaps =
      1
      + (lines
        |> List.filteri (fun i x ->
               if i = 0 then x = 3 else x - List.nth lines (i - 1) = 3)
        |> List.length)
    in
    string_of_int (one_gaps * three_gaps)

  let memo = ref MemoMap.empty

  let rec step goal start = function
    | [] -> if goal = start then 1 else 0
    | x :: xs -> (
        if x - start > 3 then 0
        else
          match MemoMap.find_opt start !memo with
          | Some number -> number
          | None ->
              let paths = step goal start xs + step goal x xs in
              memo := MemoMap.add start paths !memo;
              paths)

  let naloga2 vsebina_datoteke _part1 =
    let lines =
      vsebina_datoteke |> List.lines |> List.int_list
      |> List.sort Stdlib.compare
    in
    lines |> step (List.max lines) 0 |> string_of_int
end
