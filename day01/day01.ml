let string_of_input file_in =
  let chan = open_in file_in in
  let text = really_input_string chan (in_channel_length chan) in
  close_in chan;
  text

let unreachable () = exit 1

let count_increases list =
  let rec aux list_prev list_next sum =
    match (list_prev, list_next) with
    | prev :: tail_prev, next :: tail_next ->
        let sum_new = if next > prev then sum + 1 else sum in
        aux tail_prev tail_next sum_new
    | _ -> sum
  in
  aux list (List.tl list) 0

let window_sums list =
  let rec aux list_l list_m list_r list_out =
    match (list_l, list_m, list_r) with
    | _, _, [] -> list_out
    | head_l :: tail_l, head_m :: tail_m, head_r :: tail_r ->
      aux tail_l tail_m tail_r (head_l + head_m + head_r :: list_out)
    | _ -> unreachable ()
  in
  let list_m = List.tl list in
  let list_r = List.tl list_m in
  aux list list_m list_r []
  |> List.rev

let () =
  string_of_input "input"
  |> String.split_on_char '\n'
  |> List.map int_of_string
  |> window_sums
  |> count_increases
  |> print_int;
  print_newline ()
