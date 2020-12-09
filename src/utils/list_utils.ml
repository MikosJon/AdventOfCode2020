module List = struct
  include List

  let int_list l = List.map int_of_string l

  let max l =
    let rec max' acc = function [] -> acc | x :: xs -> max' (max x acc) xs in
    match l with [] -> failwith "Max of empty list!" | x :: _ -> max' x l

  let min l =
    let rec min' acc = function [] -> acc | x :: xs -> min' (min x acc) xs in
    match l with [] -> failwith "Min of empty list!" | x :: _ -> min' x l

  let sum l =
    let rec sum' a = function [] -> a | x :: xs -> sum' (a + x) xs in
    sum' 0 l

  let prod l =
    let rec prod' a = function [] -> a | x :: xs -> prod' (a * x) xs in
    prod' 1 l

  let apply arg l =
    let rec apply' acc = function
      | [] -> acc
      | f :: fs -> apply' (f arg :: acc) fs
    in
    List.rev (apply' [] l)

  let lines = String.split_on_char '\n'

  let with_index l = List.mapi (fun i x -> (i, x)) l
end
