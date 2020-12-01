let get_int_list vsebina_datoteke =
  let lines =
    List.filter (fun x -> String.length x > 0) (String.split_on_char '\n' vsebina_datoteke)
  in
  List.map int_of_string lines

let rec knapsack (sum_left, num_left) acc = function
  | [] -> if sum_left = 0 && num_left = 0 then acc else 1
  | x :: xs ->
      let take =
        if x <= sum_left && num_left > 0 then
          knapsack (sum_left - x, num_left - 1) (acc * x) xs
        else 1
      and not_take = knapsack (sum_left, num_left) acc xs in
      max take not_take

let naloga1 vsebina_datoteke =
  let int_list = get_int_list vsebina_datoteke in
  string_of_int (knapsack (2020, 2) 1 int_list)

let naloga2 vsebina_datoteke =
  let int_list = get_int_list vsebina_datoteke in
  string_of_int (knapsack (2020, 3) 1 int_list)

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
  let vsebina_datoteke = preberi_datoteko "day_1.in" in
  let odgovor1 = naloga1 vsebina_datoteke
  and odgovor2 = naloga2 vsebina_datoteke
  in
  izpisi_datoteko "day_1_1.out" odgovor1;
  izpisi_datoteko "day_1_2.out" odgovor2