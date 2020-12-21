open Utils.Signature
open Utils.List_utils
open Utils.String_utils
include Hashtbl

module Solver : Solver = struct
  let neigbours1 (x, y, z, w) =
    let out = ref [] in
    for dx = -1 to 1 do
      for dy = -1 to 1 do
        for dz = -1 to 1 do
          if dx <> 0 || dy <> 0 || dz <> 0 then
            out := (x + dx, y + dy, z + dz, w) :: !out
        done
      done
    done;
    !out

  let neigbours2 (x, y, z, w) =
    let out = ref [] in
    for dx = -1 to 1 do
      for dy = -1 to 1 do
        for dz = -1 to 1 do
          for dw = -1 to 1 do
            if dx <> 0 || dy <> 0 || dz <> 0 || dw <> 0 then
              out := (x + dx, y + dy, z + dz, w + dw) :: !out
          done
        done
      done
    done;
    !out

  let rec count_active part acc = function
    | [] -> acc
    | cell :: cells ->
        if part = 1 then
          count_active part (List.rev_append (neigbours1 cell) acc) cells
        else count_active part (List.rev_append (neigbours2 cell) acc) cells

  let rec loop part board = function
    | 0 -> List.length board
    | n ->
        let next = Hashtbl.create 1000 in
        let neighbouring = count_active part [] board in
        let _ =
          List.map
            (fun (x, y, z, w) ->
              match Hashtbl.find_opt next (x, y, z, w) with
              | None -> Hashtbl.add next (x, y, z, w) 1
              | Some c -> Hashtbl.replace next (x, y, z, w) (c + 1))
            neighbouring
        in
        let new_board =
          Hashtbl.fold
            (fun cell number acc ->
              if List.mem cell board then
                if number = 2 || number = 3 then cell :: acc else acc
              else if number = 3 then cell :: acc
              else acc)
            next []
        in
        loop part new_board (n - 1)

  let get_starting lines =
    let rec by_x acc (x, y) = function
      | [] -> acc
      | c :: cs ->
          let new_acc = if c = '#' then (x, y, 0, 0) :: acc else acc in
          by_x new_acc (x + 1, y) cs
    in
    let rec by_y acc y = function
      | [] -> acc
      | l :: ls -> by_y (by_x [] (0, y) l @ acc) (y + 1) ls
    in
    by_y [] 0 lines

  let naloga1 vsebina_datoteke =
    let starting =
      vsebina_datoteke |> List.lines |> List.map String.explode |> get_starting
    in
    loop 1 starting 6 |> string_of_int

  let naloga2 vsebina_datoteke _part1 =
    let starting =
      vsebina_datoteke |> List.lines |> List.map String.explode |> get_starting
    in
    loop 2 starting 6 |> string_of_int
end
