let lines_of_file file_in =
  let rec lines_of_chan ic () =
    try
      Seq.Cons (input_line ic, lines_of_chan ic)
    with End_of_file ->
      close_in ic;
      Seq.Nil
  in
  open_in file_in |> lines_of_chan

type action = Forward of int | Up of int | Down of int

type state = { horizontal : int; depth : int; aim : int }

let action_of_line line =
  match String.split_on_char ' ' line with
  | [ s; x ] -> (
      let x = int_of_string x in
      match s with
      | "forward" -> Some (Forward x)
      | "down" -> Some (Down x)
      | "up" -> Some (Up x)
      | s -> None)
  | _ -> None

let next_state state action =
  match action with
  | Forward x ->
      {
        horizontal = state.horizontal + x;
        depth = state.depth + (state.aim * x);
        aim = state.aim;
      }
  | Up x ->
      {
        horizontal = state.horizontal;
        depth = state.depth;
        aim = state.aim - x;
      }
  | Down x ->
      {
        horizontal = state.horizontal;
        depth = state.depth;
        aim = state.aim + x;
      }

let dist_prod x = x.horizontal * x.depth

let () =
  lines_of_file "input"
  |> Seq.map action_of_line
  |> Seq.map Option.get
  |> Seq.fold_left next_state { horizontal = 0; depth = 0; aim = 0 }
  |> dist_prod
  |> print_int;
  print_newline ()
