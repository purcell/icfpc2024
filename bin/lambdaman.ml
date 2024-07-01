let () =
  let maze = Maze.read @@ In_channel.input_all Stdlib.stdin in
  let steps = Maze.walk maze in
  let uncompressed = Icfp.encode_expr (Icfp.Strn steps) in
  let compressed = Maze.compress steps in
  (* ( match Icfp.run compressed with *)
  (* | Icfp.Strn s -> if s != steps then failwith "compressed version differs" *)
  (* | _ -> failwith "bad expansion result"); *)

  if String.length compressed < String.length uncompressed then (
    Printf.fprintf stderr "Compressed version is shorter\n";
    Stdlib.print_string compressed;)
  else (
    Printf.fprintf stderr "Compression made things worse\n";
    Stdlib.print_string uncompressed)
;;
