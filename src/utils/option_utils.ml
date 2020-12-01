module Option = struct
  include Option

  type 'a t = 'a Option.t

  let with_default def = function Some x -> x | None -> def

  let force_val = function Some x -> x | None -> failwith "forced failed"

  let map f = function Some x -> Some (f x) | None -> None
end
