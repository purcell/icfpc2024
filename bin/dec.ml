let () =
  let result = Icfp.parse_expr (In_channel.input_all Stdlib.stdin) in
         match result with
           | Icfp.Strn s -> Stdlib.print_string s
         | _ -> Stdlib.print_string @@ Icfp.show_expr result;
  Stdlib.print_newline ();
