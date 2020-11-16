(*
  Replace ** with the current day.
*)

let naloga1 vsebina_datoteke =
  "10"

let naloga2 vsebina_datoteke =
  string_of_int (String.length vsebina_datoteke)

let _ =
  let preberi_datoteko ime_datoteke =
      let chan = open_in ime_datoteke in
      let vsebina = really_input_string chan (in_channel_length chan) in
      close_in chan;
      vsebina
  and izpisi_datoteko ime_datoteke vsebina =
      let chan = open_out ime_datoteke in
      output_string chan vsebina;
      close_out chan
  in
  let vsebina_datoteke = preberi_datoteko "day_**.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "day_**_1.out" odgovor1;
  izpisi_datoteko "day_**_2.out" odgovor2