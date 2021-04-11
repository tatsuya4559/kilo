let get_char () =
  let termio = Unix.tcgetattr Unix.stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN { termio with c_icanon = false };
  Fun.protect (fun () ->
    try
      Some (input_char stdin)
    with End_of_file -> None
  ) ~finally:(fun () -> Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio)

let () =
  let rec loop () =
    match get_char () with
    | Some _ -> loop ()
    | None -> ()
  in
  loop ()
