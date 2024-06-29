let () =
  Stdlib.print_string (Icfp.encode_expr (Icfp.run (In_channel.input_all Stdlib.stdin)));;
