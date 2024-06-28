let () =
  Stdlib.print_string (Icfp.encode_expr (Icfp.Strn (In_channel.input_all Stdlib.stdin)));;
