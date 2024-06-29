let () =
  let result = Icfp.parse_expr (In_channel.input_all Stdlib.stdin) in
  Stdlib.print_string @@ Icfp.show_expr result;
  Stdlib.print_newline ();
