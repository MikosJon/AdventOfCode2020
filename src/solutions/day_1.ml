open Utils.Signature
open Utils.List_utils

module Solver : Solver = struct
  let get_int_list_from_contents vsebina_datoteke =
    List.int_list (List.non_empty_lines vsebina_datoteke)

  let rec knapsack (sum_left, num_left) acc = function
    | [] -> if sum_left = 0 && num_left = 0 then acc else 1
    | x :: xs ->
        let take =
          if x <= sum_left && num_left > 0 then
            knapsack (sum_left - x, num_left - 1) (acc * x) xs
          else 1
        and not_take = knapsack (sum_left, num_left) acc xs in
        max take not_take

  let naloga1 vsebina_datoteke =
    let int_list = get_int_list_from_contents vsebina_datoteke in
    string_of_int (knapsack (2020, 2) 1 int_list)

  let naloga2 vsebina_datoteke _part1 =
    let int_list = get_int_list_from_contents vsebina_datoteke in
    string_of_int (knapsack (2020, 3) 1 int_list)
end
