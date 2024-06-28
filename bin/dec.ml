let () =
  match Icfp.parse_expr (In_channel.input_all Stdlib.stdin) with
  | Icfp.Strn msg -> Stdlib.print_string msg
  | _ -> failwith "not a string response"
