let () =
  let parsed = Icfp.parse_expr (In_channel.input_all Stdlib.stdin) in
  Stdlib.print_string (Icfp.encode_expr (Icfp.eval_expr parsed));;
