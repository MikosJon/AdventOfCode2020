open Utils.Signature
open Utils.List_utils

module Solver : Solver = struct
  type direction = North | South | East | West

  let get_instructions lines =
    List.map
      (fun s ->
        (String.sub s 0 1, int_of_string (String.sub s 1 (String.length s - 1))))
      lines

  let rotate_left amount direction =
    let rotate_left' = function
      | North -> West
      | West -> South
      | South -> East
      | East -> North
    in
    match amount with
    | 90 -> rotate_left' direction
    | 180 -> rotate_left' (rotate_left' direction)
    | 270 -> rotate_left' (rotate_left' (rotate_left' direction))
    | _ -> failwith "bad rotation left"

  let rotate_right amount direction =
    let rotate_right' = function
      | North -> East
      | East -> South
      | South -> West
      | West -> North
    in
    match amount with
    | 90 -> rotate_right' direction
    | 180 -> rotate_right' (rotate_right' direction)
    | 270 -> rotate_right' (rotate_right' (rotate_right' direction))
    | _ -> failwith "bad rotation right"

  let rec move (x_pos, y_pos) direction = function
    | [] -> (x_pos, y_pos)
    | ("N", value) :: xs -> move (x_pos, y_pos + value) direction xs
    | ("S", value) :: xs -> move (x_pos, y_pos - value) direction xs
    | ("E", value) :: xs -> move (x_pos + value, y_pos) direction xs
    | ("W", value) :: xs -> move (x_pos - value, y_pos) direction xs
    | ("L", value) :: xs -> move (x_pos, y_pos) (rotate_left value direction) xs
    | ("R", value) :: xs ->
        move (x_pos, y_pos) (rotate_right value direction) xs
    | ("F", value) :: xs -> (
        match direction with
        | North -> move (x_pos, y_pos + value) direction xs
        | South -> move (x_pos, y_pos - value) direction xs
        | East -> move (x_pos + value, y_pos) direction xs
        | West -> move (x_pos - value, y_pos) direction xs)
    | _ -> failwith "bad command"

  let naloga1 vsebina_datoteke =
    let final_x, final_y =
      vsebina_datoteke |> List.lines |> get_instructions |> move (0, 0) East
    in
    abs final_x + abs final_y |> string_of_int

  let rotate_left2 (way_x, way_y) = function
    | 90 -> (-way_y, way_x)
    | 180 -> (-way_x, -way_y)
    | 270 -> (way_y, -way_x)
    | _ -> failwith "bad rotate left2"

  let rotate_right2 (way_x, way_y) = function
    | 90 -> (way_y, -way_x)
    | 180 -> (-way_x, -way_y)
    | 270 -> (-way_y, way_x)
    | _ -> failwith "bad rotate right2"

  let rec move2 (x_pos, y_pos) (way_x, way_y) = function
    | [] -> (x_pos, y_pos)
    | ("N", value) :: xs -> move2 (x_pos, y_pos) (way_x, way_y + value) xs
    | ("S", value) :: xs -> move2 (x_pos, y_pos) (way_x, way_y - value) xs
    | ("E", value) :: xs -> move2 (x_pos, y_pos) (way_x + value, way_y) xs
    | ("W", value) :: xs -> move2 (x_pos, y_pos) (way_x - value, way_y) xs
    | ("L", value) :: xs ->
        move2 (x_pos, y_pos) (rotate_left2 (way_x, way_y) value) xs
    | ("R", value) :: xs ->
        move2 (x_pos, y_pos) (rotate_right2 (way_x, way_y) value) xs
    | ("F", value) :: xs ->
        move2
          (x_pos + (value * way_x), y_pos + (value * way_y))
          (way_x, way_y) xs
    | _ -> failwith "bad command"

  let naloga2 vsebina_datoteke _part1 =
    let final_x, final_y =
      vsebina_datoteke |> List.lines |> get_instructions |> move2 (0, 0) (10, 1)
    in
    abs final_x + abs final_y |> string_of_int
end
