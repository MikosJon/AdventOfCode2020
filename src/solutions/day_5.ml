open Utils.Signature
open Utils.List_utils

module Solver : Solver = struct
  let rec to_bin power = function
    | "" -> 0
    | x ->
        (if Char.equal x.[0] 'F' || Char.equal x.[0] 'L' then 0 else 1)
        * int_of_float (2. ** power)
        + to_bin (power -. 1.) (String.sub x 1 (String.length x - 1))

  let rec find_seat = function
    | x :: y :: xs -> if y - x = 2 then y - 1 else find_seat (y :: xs)
    | _ -> failwith "No seat found"

  let naloga1 vsebina_datoteke =
    vsebina_datoteke |> List.lines
    |> List.map (to_bin 9.)
    |> List.max |> string_of_int

  let naloga2 vsebina_datoteke _part1 =
    vsebina_datoteke |> List.lines
    |> List.map (to_bin 9.)
    |> List.sort Stdlib.compare |> find_seat |> string_of_int
end
