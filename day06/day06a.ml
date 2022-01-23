let string_of_input file_in =
  let chan = open_in file_in in
  let len = in_channel_length chan in
  let text = really_input_string chan len in
  close_in chan;
  text

let tick x =
  (match x with
  | 0 -> [6; 8]
  | x -> [x - 1])
  |> List.to_seq

let tick_many lst =
  Seq.flat_map tick lst

let rec tick_many_n n lst =
  match n with
  | 0 -> lst
  | x -> tick_many lst |> tick_many_n (n-1)

let () =
  string_of_input "input"
  |> String.split_on_char ','
  |> List.to_seq
  |> Seq.map int_of_string
  |> tick_many_n 80
  |> List.of_seq
  |> List.length
  |> print_int;
  print_newline ()
