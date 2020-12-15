open Utils.Signature
open Utils.List_utils
include Map

module Solver : Solver = struct
  module Memory = Map.Make (Int)

  let memory = ref Memory.empty

  let last_num = ref 0

  let update_memory idx =
    memory :=
      Memory.update !last_num
        (fun m ->
          match m with None -> Some (idx, -1) | Some (p2, _) -> Some (idx, p2))
        !memory

  let naloga1 vsebina_datoteke =
    let initial =
      vsebina_datoteke |> String.split_on_char ',' |> List.map int_of_string
    in
    let _ =
      List.mapi (fun i x -> memory := Memory.add x (i, -1) !memory) initial
    in
    last_num := List.nth initial (List.length initial - 1);
    for idx = List.length initial to 2020 - 1 do
      match Memory.find_opt !last_num !memory with
      | None ->
          last_num := 0;
          update_memory idx
      | Some (p, -1) ->
          if idx - p = 1 then last_num := 0 else last_num := idx - p;
          update_memory idx
      | Some (p2, p1) ->
          last_num := p2 - p1;
          update_memory idx
    done;
    string_of_int !last_num

  let naloga2 vsebina_datoteke _part1 =
    memory := Memory.empty;
    let initial =
      vsebina_datoteke |> String.split_on_char ',' |> List.map int_of_string
    in
    let _ =
      List.mapi (fun i x -> memory := Memory.add x (i, -1) !memory) initial
    in
    last_num := List.nth initial (List.length initial - 1);
    for idx = List.length initial to 30000000 - 1 do
      match Memory.find_opt !last_num !memory with
      | None ->
          last_num := 0;
          update_memory idx
      | Some (p, -1) ->
          if idx - p = 1 then last_num := 0 else last_num := idx - p;
          update_memory idx
      | Some (p2, p1) ->
          last_num := p2 - p1;
          update_memory idx
    done;
    string_of_int !last_num
end
