let () =
  In_channel.input_all Stdlib.stdin |> Icfp.parse_expr |> Icfp.to_sexp |> Stdlib.print_string;
  Stdlib.print_newline ();
