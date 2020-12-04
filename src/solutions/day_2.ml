open Utils.Signature
open Utils.List_utils

module Solver : Solver = struct
  let get_conditions policy =
    match String.split_on_char ' ' policy with
    | [ bounds; chr ] -> (
        match String.split_on_char '-' bounds with
        | [ lower; upper ] -> (int_of_string lower, int_of_string upper, chr.[0])
        | _ -> failwith "Napacen format vrste!!")
    | _ -> failwith "Napacen format vrste!!"

  let count_letter_in_string character str =
    List.length (String.split_on_char character str) - 1

  let num_valid_passwords validate_fun =
    let rec num_valid_aux acc = function
      | [] -> acc
      | x :: xs -> num_valid_aux (acc + if validate_fun x then 1 else 0) xs
    in
    num_valid_aux 0

  let validate1 = function
    | [ policy; password ] ->
        let lower, upper, character = get_conditions policy in
        let num_occurences = count_letter_in_string character password in
        num_occurences >= lower && num_occurences <= upper
    | _ -> failwith "Napacen format vrste!!"

  let validate2 = function
    | [ policy; password ] ->
        let first, second, character = get_conditions policy in
        let first_char = password.[first] and second_char = password.[second] in
        Char.equal first_char character <> Char.equal second_char character
    | _ -> failwith "Napacen format vrste!!"

  let naloga1 vsebina_datoteke =
    vsebina_datoteke |> List.lines
    |> List.map (String.split_on_char ':')
    |> num_valid_passwords validate1
    |> string_of_int

  let naloga2 vsebina_datoteke _part1 =
    vsebina_datoteke |> List.lines
    |> List.map (String.split_on_char ':')
    |> num_valid_passwords validate2
    |> string_of_int
end
