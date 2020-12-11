open Utils.Signature
open Utils.List_utils

module Solver : Solver = struct
  let surrounding =
    [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]

  let neighbours list row col =
    let row_len = List.length list
    and col_len = String.length (List.nth list 0) in
    let valid_moves =
      List.filter
        (fun (dx, dy) ->
          row + dx >= 0
          && row + dx < row_len
          && col + dy >= 0
          && col + dy < col_len)
        surrounding
    in
    List.map (fun (dx, dy) -> (List.nth list (row + dx)).[col + dy]) valid_moves

  let update list row col cell =
    if cell = '.' then '.'
    else if cell = '#' then
      if
        List.fold_left
          (fun acc chr -> if chr = '#' then acc + 1 else acc)
          0 (neighbours list row col)
        >= 4
      then 'L'
      else '#'
    else if List.mem '#' (neighbours list row col) then 'L'
    else '#'

  let rec loop list =
    let new_list =
      List.mapi
        (fun i row -> String.mapi (fun j cell -> update list i j cell) row)
        list
    in
    if new_list = list then
      List.sum
        (List.map (fun s -> List.length (String.split_on_char '#' s) - 1) list)
    else loop new_list

  let naloga1 vsebina_datoteke =
    vsebina_datoteke |> List.lines |> loop |> string_of_int

  let rec find_seat list row col (dx, dy) =
    let row_len = List.length list
    and col_len = String.length (List.nth list 0)
    and new_row = row + dx
    and new_col = col + dy in
    if new_row < 0 || new_row >= row_len || new_col < 0 || new_col >= col_len
    then '.'
    else
      match (List.nth list new_row).[new_col] with
      | '.' -> find_seat list new_row new_col (dx, dy)
      | x -> x

  let neighbours2 list row col = List.map (find_seat list row col) surrounding

  let update2 list row col cell =
    if cell = '.' then '.'
    else if cell = '#' then
      if
        List.fold_left
          (fun acc chr -> if chr = '#' then acc + 1 else acc)
          0 (neighbours2 list row col)
        >= 5
      then 'L'
      else '#'
    else if List.mem '#' (neighbours2 list row col) then 'L'
    else '#'

  let rec loop2 list =
    let new_list =
      List.mapi
        (fun i row -> String.mapi (fun j cell -> update2 list i j cell) row)
        list
    in
    if new_list = list then
      List.sum
        (List.map (fun s -> List.length (String.split_on_char '#' s) - 1) list)
    else loop2 new_list

  let naloga2 vsebina_datoteke _part1 =
    vsebina_datoteke |> List.lines |> loop2 |> string_of_int
end
