open Utils.Signature
open Utils.List_utils
open Utils.String_utils
include Map

module Solver : Solver = struct
  module Memory = Map.Make (Int)

  type instruction = Mask | Mem

  let int_pow base exp = int_of_float (float_of_int base ** float_of_int exp)

  let num_to_padded_bin_string x =
    let rec aux acc n = function
      | 0 -> acc
      | shifts_left ->
          let bit = n mod 2 in
          aux (string_of_int bit ^ acc) (Int.shift_right n 1) (shifts_left - 1)
    in
    let length = 36 in
    aux "" x length

  let bit_string_to_num s =
    let rec aux acc pow = function
      | [] -> acc
      | x :: xs ->
          if x = '1' then aux (acc + int_pow 2 pow) (pow - 1) xs
          else aux acc (pow - 1) xs
    in
    aux 0 (String.length s - 1) (String.explode s)

  let get_int_result value mask =
    let rec aux acc idx value_li = function
      | [] -> List.rev acc
      | x :: xs ->
          let new_acc =
            if x = 'X' then List.nth value_li idx :: acc else x :: acc
          in
          aux new_acc (idx + 1) value_li xs
    in
    let char_li = aux [] 0 (String.explode value) (String.explode mask) in
    bit_string_to_num (String.implode char_li)

  let get_instructions li =
    let rec aux acc = function
      | [] -> List.rev acc
      | x :: xs -> (
          let inp_li = String.split_on_char '=' x in
          let arg = List.nth inp_li 1 in
          let argument = String.sub arg 1 (String.length arg - 1) in
          let left_inp = List.nth inp_li 0 in
          match String.sub left_inp 0 3 with
          | "mas" -> aux ((Mask, None, argument) :: acc) xs
          | "mem" ->
              let cell =
                int_of_string
                  (String.sub left_inp 4 (String.length left_inp - 6))
              in
              aux
                (( Mem,
                   Some cell,
                   num_to_padded_bin_string (int_of_string argument) )
                :: acc)
                xs
          | _ -> failwith "bad instruction")
    in
    aux [] li

  let execute_instructions1 li =
    let memory = ref Memory.empty and mask = ref "" in
    let rec aux = function
      | [] -> ()
      | (Mask, None, argument) :: xs ->
          mask := argument;
          aux xs
      | (Mem, Some cell, argument) :: xs ->
          memory := Memory.add cell (get_int_result argument !mask) !memory;
          aux xs
      | _ -> failwith "bad instruction parsing"
    in
    aux li;
    !memory

  let naloga1 vsebina_datoteke =
    vsebina_datoteke |> List.lines |> get_instructions |> execute_instructions1
    |> Memory.bindings
    |> List.fold_left (fun acc (_, value) -> acc + value) 0
    |> string_of_int

  let merge cell mask =
    let rec aux acc idx cell_li = function
      | [] -> List.rev acc
      | x :: xs ->
          let new_acc =
            if x = '0' then List.nth cell_li idx :: acc else x :: acc
          in
          aux new_acc (idx + 1) cell_li xs
    in
    aux [] 0 (String.explode cell) (String.explode mask)

  let get_cells cell mask =
    let rec aux acc pow = function
      | [] -> acc
      | '0' :: xs -> aux acc (pow - 1) xs
      | '1' :: xs ->
          aux (List.map (fun x -> x + int_pow 2 pow) acc) (pow - 1) xs
      | 'X' :: xs ->
          aux (acc @ List.map (fun x -> x + int_pow 2 pow) acc) (pow - 1) xs
      | _ -> failwith "bad mask"
    in
    let template = merge (num_to_padded_bin_string cell) mask in
    aux [ 0 ] (String.length mask - 1) template

  let execute_instructions2 li =
    let memory = ref Memory.empty and mask = ref "" in
    let rec aux = function
      | [] -> ()
      | (Mask, None, argument) :: xs ->
          mask := argument;
          aux xs
      | (Mem, Some cell, argument) :: xs ->
          let value = bit_string_to_num argument
          and cells = get_cells cell !mask in
          let _ =
            List.map
              (fun c -> memory := Memory.update c (fun _ -> Some value) !memory)
              cells
          in
          aux xs
      | _ -> failwith "bad instruction parsing"
    in
    aux li;
    !memory

  let naloga2 vsebina_datoteke _part1 =
    vsebina_datoteke |> List.lines |> get_instructions |> execute_instructions2
    |> Memory.bindings
    |> List.fold_left (fun acc (_, value) -> acc + value) 0
    |> string_of_int
end
