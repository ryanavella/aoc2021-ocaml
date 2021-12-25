let lines_of_file file_in =
  let rec lines_of_chan ic () =
    try
      Seq.Cons (input_line ic, lines_of_chan ic)
    with End_of_file ->
      close_in ic;
      Seq.Nil
  in
  open_in file_in |> lines_of_chan

let psuedosign_of_bit bit =
  match bit with
    | 0 -> -1
    | _ -> 1

let int_of_char_digit c =
  int_of_char c - int_of_char '0'

let array_of_line line =
  let len = String.length line in
  let f = String.get line in
  Array.init len f
  |> Array.map int_of_char_digit
  |> Array.map psuedosign_of_bit

let bit_sum init next =
  Array.map2 (+) init next

let bits_most_seen arr =
  let f x =
    if x > 0 then 1 else 0
  in
    Array.map f arr

let int_of_bit_array arr =
  let f init next =
    2*init + next
  in
    Array.fold_left f 0 arr

let prod_polynomial x =
  let y = 4095 - x in
  y * x

let () =
  lines_of_file "input"
  |> Seq.map array_of_line
  |> Seq.fold_left bit_sum (Array.make 12 0)
  |> bits_most_seen
  |> int_of_bit_array
  |> prod_polynomial
  |> print_int;
  print_newline ()
