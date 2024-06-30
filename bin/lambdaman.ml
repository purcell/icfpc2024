let () =
  let maze = Maze.read @@ In_channel.input_all Stdlib.stdin in
  Stdlib.print_string @@ Maze.walk maze;
;;
