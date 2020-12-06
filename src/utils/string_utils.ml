module String = struct
  include String

  let tail = function
    | "" -> failwith "Tail of empty string!"
    | s -> String.sub s 1 (String.length s - 1)

  let explode =
    let rec explode' acc = function
      | "" -> List.rev acc
      | s -> explode' (s.[0] :: acc) (tail s)
    in
    explode' []

  let implode =
    let rec implode' acc = function
      | [] -> acc
      | x :: xs -> implode' (acc ^ Char.escaped x) xs
    in
    implode' ""
end
