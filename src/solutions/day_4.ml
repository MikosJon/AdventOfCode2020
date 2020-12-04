open Utils.Signature
open Utils.List_utils

module Solver : Solver = struct
  let rec update_acc acc = function
    | [] -> acc
    | [ field; value ] :: xs -> update_acc ((field, value) :: acc) xs
    | _ -> failwith "Bad update_acc data!"

  let process str =
    str |> String.split_on_char ' '
    |> List.filter (fun x -> String.length x > 0)
    |> List.map (fun x -> String.split_on_char ':' x)

  let group =
    let rec group' pport_acc pport_list = function
      | [] -> pport_acc :: pport_list
      | x :: xs -> (
          match x with
          | "" -> group' [] (pport_acc :: pport_list) xs
          | _ ->
              let new_acc = update_acc pport_acc (process x) in
              group' new_acc pport_list xs)
    in
    group' [] []

  let is_valid1 passport =
    match List.length passport with
    | 8 -> true
    | 7 when List.for_all (fun x -> fst x <> "cid") passport -> true
    | _ -> false

  let check_pid value =
    String.length value = 9
    && List.for_all
         (fun x ->
           List.mem value.[x]
             [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ])
         [ 0; 1; 2; 3; 4; 5; 6; 7; 8 ]

  let check_hgt value =
    let l = String.length value in
    if l = 5 then
      let units = Char.escaped value.[3] ^ Char.escaped value.[4] in
      if units = "cm" then
        let v =
          Char.escaped value.[0]
          ^ Char.escaped value.[1]
          ^ Char.escaped value.[2]
        in
        if
          List.for_all
            (fun x ->
              List.mem v.[x]
                [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ])
            [ 0; 1; 2 ]
        then
          let hgt = int_of_string v in
          hgt >= 150 && hgt <= 193
        else false
      else false
    else if l = 4 then
      let units = Char.escaped value.[2] ^ Char.escaped value.[3] in
      if units = "in" then
        let v = Char.escaped value.[0] ^ Char.escaped value.[1] in
        if
          List.for_all
            (fun x ->
              List.mem v.[x]
                [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ])
            [ 0; 1 ]
        then
          let hgt = int_of_string v in
          hgt >= 59 && hgt <= 76
        else false
      else false
    else false

  let check_hcl value =
    let l = String.length value in
    l = 7
    && value.[0] = '#'
    &&
    let v =
      Char.escaped value.[1]
      ^ Char.escaped value.[2]
      ^ Char.escaped value.[3]
      ^ Char.escaped value.[4]
      ^ Char.escaped value.[5]
      ^ Char.escaped value.[6]
    in
    List.for_all
      (fun x ->
        List.mem v.[x]
          [
            '0';
            '1';
            '2';
            '3';
            '4';
            '5';
            '6';
            '7';
            '8';
            '9';
            'a';
            'b';
            'c';
            'd';
            'e';
            'f';
          ])
      [ 0; 1; 2; 3; 4; 5 ]

  let rec check_fields = function
    | [] -> true
    | (field, value) :: xs -> (
        match field with
        | "byr" ->
            let year = int_of_string value in
            year >= 1920 && year <= 2002 && check_fields xs
        | "iyr" ->
            let year = int_of_string value in
            year >= 2010 && year <= 2020 && check_fields xs
        | "eyr" ->
            let year = int_of_string value in
            year >= 2020 && year <= 2030 && check_fields xs
        | "hgt" -> check_hgt value && check_fields xs
        | "hcl" -> check_hcl value && check_fields xs
        | "ecl" ->
            List.mem value [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ]
            && check_fields xs
        | "pid" -> check_pid value && check_fields xs
        | "cid" -> check_fields xs
        | _ -> failwith "invalid field")

  (* let rec print_pport x =
     match x with
     | [] -> ()
     | (f, v) :: xs ->
         print_string (f ^ " : " ^ v ^ ", ");
         print_pport xs *)

  (*
  let rec print_pports passports =
    match passports with
    | [] -> ()
    | p :: ps ->
        print_pport p;
        print_newline ();
        print_pports ps *)

  let is_valid2 passport =
    match List.length passport with
    | 8 -> check_fields passport
    | 7 when List.for_all (fun x -> fst x <> "cid") passport ->
        check_fields passport
    | _ -> false

  let rec count_valid validation_fun = function
    | [] -> 0
    | pport :: pports ->
        (if validation_fun pport then 1 else 0)
        + count_valid validation_fun pports

  let naloga1 vsebina_datoteke =
    let lines = List.lines vsebina_datoteke in
    let passports = group lines in
    string_of_int (count_valid is_valid1 passports)

  let naloga2 vsebina_datoteke _part1 =
    let lines = List.lines vsebina_datoteke in
    let passports = group lines in
    string_of_int (count_valid is_valid2 passports)
end
