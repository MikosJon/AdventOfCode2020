module List = struct
  include List

  let int_list l = List.map int_of_string l

  let sum l =
    let rec sum' a = function [] -> a | x :: xs -> sum' (a + x) xs in
    sum' 0 l

  let lines = String.split_on_char '\n'

  let non_empty_lines l = List.filter (fun x -> String.length x > 0) (lines l)

  let with_index l = List.mapi (fun i x -> (i, x)) l
end
