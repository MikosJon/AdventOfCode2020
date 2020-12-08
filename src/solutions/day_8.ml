open Utils.Signature
open Utils.List_utils
include Set

module Solver : Solver = struct
  module PosSet = Set.Make (Int)

  let rec get_instruction_list acc = function
    | [] -> List.rev acc
    | x :: xs ->
        let inp = String.split_on_char ' ' x in
        get_instruction_list
          ((List.nth inp 0, int_of_string (List.nth inp 1)) :: acc)
          xs

  let execute_instruction acc pos instruction_list =
    let instruction, argument = List.nth instruction_list pos in
    match instruction with
    | "nop" -> (acc, pos + 1, instruction_list)
    | "acc" -> (acc + argument, pos + 1, instruction_list)
    | "jmp" -> (acc, pos + argument, instruction_list)
    | _ -> failwith "Instruction not supported!"

  let rec find_loop seen acc pos instruction_list =
    if pos >= List.length instruction_list || pos < 0 then (false, acc)
    else if PosSet.mem pos seen then (true, acc)
    else
      let new_seen = PosSet.add pos seen
      and new_acc, new_pos, new_instruction_list =
        execute_instruction acc pos instruction_list
      in
      find_loop new_seen new_acc new_pos new_instruction_list

  let rec terminate pos instruction_list =
    let l = List.length instruction_list in
    if pos = l then failwith "All programs loop"
    else
      let modified_instruction_list =
        List.mapi
          (fun i (instruction, argument) ->
            if i <> pos then (instruction, argument)
            else
              match instruction with
              | "acc" -> (instruction, argument)
              | "nop" -> ("jmp", argument)
              | "jmp" -> ("nop", argument)
              | _ -> failwith "Unrecognized instruction")
          instruction_list
      in
      let loops, acc = find_loop PosSet.empty 0 0 modified_instruction_list in
      if loops then terminate (pos + 1) instruction_list else acc

  let naloga1 vsebina_datoteke =
    vsebina_datoteke |> List.lines |> get_instruction_list []
    |> find_loop PosSet.empty 0 0
    |> fun (loops, acc) ->
    match loops with
    | false -> failwith "no loop found"
    | true -> string_of_int acc

  let naloga2 vsebina_datoteke _part1 =
    vsebina_datoteke |> List.lines |> get_instruction_list [] |> terminate 0
    |> string_of_int
end
