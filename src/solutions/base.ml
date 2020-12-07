open Utils.Signature
open Utils.List_utils

module Solver : Solver = struct
  let naloga1 vsebina_datoteke = Char.escaped vsebina_datoteke.[0]

  let naloga2 vsebina_datoteke _part1 = Char.escaped vsebina_datoteke.[0]
end
