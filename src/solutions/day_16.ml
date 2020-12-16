open Utils.Signature
open Utils.List_utils
include Map

module Solver : Solver = struct
  module Memory = Map.Make (Int)

  let rec get_valid_numbers arr acc = function
    | [] -> (arr, acc)
    | rule :: xs ->
        let left = List.nth (String.split_on_char ':' rule) 0 in
        let right = List.nth (String.split_on_char ':' rule) 1 in
        let ranges =
          List.filter
            (fun s -> String.contains s '-')
            (String.split_on_char ' ' right)
          |> List.map (fun s ->
                 let li = String.split_on_char '-' s in
                 ( left,
                   (int_of_string (List.nth li 0), int_of_string (List.nth li 1))
                 ))
        in
        let rec make_valid arr = function
          | [] -> ()
          | (_, (lower, upper)) :: ys ->
              for i = lower to upper do
                arr.(i) <- true
              done;
              make_valid arr ys
        in
        make_valid arr ranges;
        get_valid_numbers arr (List.rev_append ranges acc) xs

  let parse lines =
    let tickets =
      match
        List.filter
          (fun s -> not (String.length s = 0 || String.contains s ':'))
          lines
      with
      | [] -> failwith "no lines in parse1"
      | xs -> List.map (String.split_on_char ',') xs |> List.map List.int_list
    and valid_numbers =
      let numbers = Array.make 1001 false
      and posibble = List.filter (fun s -> String.contains s ':') lines in
      let num_rules = List.length posibble - 2 in
      let rules = List.filteri (fun i _ -> i < num_rules) posibble in
      get_valid_numbers numbers [] rules
    in
    (valid_numbers, tickets)

  let process1 (valid, tickets) =
    let valid_numbers = fst valid in
    let rec aux acc = function
      | [] -> acc
      | ticket :: xs ->
          let new_acc =
            acc + List.sum (List.filter (fun x -> not valid_numbers.(x)) ticket)
          in
          aux new_acc xs
    in
    aux 0 tickets

  let naloga1 vsebina_datoteke =
    vsebina_datoteke |> List.lines |> parse |> process1 |> string_of_int

  let clean_ranges n ranges =
    let valid_range_names =
      List.map fst
        (List.filter
           (fun (_, (lower, upper)) -> lower <= n && n <= upper)
           ranges)
    in
    List.filter (fun (name, _) -> List.mem name valid_range_names) ranges

  let rec cleanup arr = function
    | [] -> ()
    | ticket :: xs ->
        for i = 0 to Array.length arr - 1 do
          arr.(i) <- clean_ranges (List.nth ticket i) arr.(i)
        done;
        cleanup arr xs

  let rec reduce acc names =
    if Array.for_all (fun x -> x <> "") acc then ()
    else
      let changed = ref false in
      for i = 0 to Array.length names - 1 do
        if List.length names.(i) = 1 then acc.(i) <- List.nth names.(i) 0
      done;
      for i = 0 to Array.length names - 1 do
        names.(i) <-
          List.filter
            (fun x ->
              if not (Array.mem x acc) then true
              else
                let _ = changed := true in
                false)
            names.(i)
      done;
      if !changed then reduce acc names else ()

  let process2 ((valid_numbers, ranges), tickets) =
    let valid_tickets =
      List.filter
        (fun ticket -> List.for_all (fun x -> valid_numbers.(x)) ticket)
        tickets
    in
    let my_ticket = List.nth valid_tickets 0 in
    let possible_ranges = Array.make (List.length my_ticket) ranges in
    cleanup possible_ranges valid_tickets;

    let possible_names = Array.make (Array.length possible_ranges) [] in
    for i = 0 to Array.length possible_ranges - 1 do
      possible_names.(i) <- List.map (fun (name, _) -> name) possible_ranges.(i);
      possible_names.(i) <-
        List.filteri (fun i _ -> i mod 2 = 0) possible_names.(i)
    done;

    let sol = Array.make (List.length my_ticket) "" in
    reduce sol possible_names;

    let out = ref 1 in
    for i = 0 to Array.length sol - 1 do
      if List.nth (String.split_on_char ' ' sol.(i)) 0 = "departure" then
        out := !out * List.nth my_ticket i
    done;
    !out

  let naloga2 vsebina_datoteke _part1 =
    vsebina_datoteke |> List.lines |> parse |> process2 |> string_of_int
end
