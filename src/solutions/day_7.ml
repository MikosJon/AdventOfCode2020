open Utils.Signature
open Utils.List_utils
include Map
include Set

module Solver : Solver = struct
  module BagMap = Map.Make (String)
  module SeenSet = Set.Make (String)

  let rec group = function
    | [ n; desc; col; _ ] -> [ (desc ^ " " ^ col, int_of_string n) ]
    | n :: desc :: col :: _ :: xs ->
        [ (desc ^ " " ^ col, int_of_string n) ] @ group xs
    | _ -> failwith "bad line!"

  let get_connections color li =
    if List.nth li 0 = "no" then (color, ([] : (string * int) list))
    else (color, group li)

  let parse line =
    let s = String.split_on_char ' ' line in
    let color = List.nth s 0 ^ " " ^ List.nth s 1 in
    get_connections color (List.filteri (fun i _ -> i >= 4) s)

  let rec update m = function
    | [] -> m
    | (color, connections) :: pairs ->
        let rec aux s = function
          | [] -> s
          | (x, v) :: xs ->
              let current_li =
                match BagMap.find_opt x s with None -> [] | Some li -> li
              in
              aux (BagMap.add x ((color, v) :: current_li) s) xs
        in
        update (aux m connections) pairs

  let rec get_connected_nodes seen start graph =
    let neighbours =
      match BagMap.find_opt start graph with
      | None -> []
      | Some li -> List.map fst li
    in
    let s = List.fold_right SeenSet.add neighbours seen in
    List.fold_right SeenSet.union
      (List.map (fun x -> get_connected_nodes s x graph) neighbours)
      s

  let rec update2 m = function
    | [] -> m
    | (color, connections) :: pairs ->
        update2 (BagMap.add color connections m) pairs

  let rec get_num_containing_bags start graph =
    let neighbours = BagMap.find start graph in
    if neighbours = [] then 1
    else
      let li =
        List.map (fun (x, v) -> v * get_num_containing_bags x graph) neighbours
      in
      1 + List.sum li

  let naloga1 vsebina_datoteke =
    let graph =
      vsebina_datoteke |> List.lines |> List.map parse |> update BagMap.empty
    and ss = SeenSet.empty in
    let bags = SeenSet.elements (get_connected_nodes ss "shiny gold" graph) in
    string_of_int (List.length bags)

  let naloga2 vsebina_datoteke _part1 =
    let graph =
      vsebina_datoteke |> List.lines |> List.map parse |> update2 BagMap.empty
    in
    string_of_int (get_num_containing_bags "shiny gold" graph - 1)
end
