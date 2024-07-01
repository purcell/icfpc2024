let read s =
  let g = String.trim s |> String.split_on_char '\n' |> Array.of_list |> Array.map (fun l -> String.to_seq l |> Array.of_seq) in
    let rowlens = Array.map Array.length g in
  if Array.fold_left max 0 rowlens != Array.fold_left min max_int rowlens then
    failwith "Unequal row lengths"
  else g;;

let at (x, y) maze = let row = Array.get maze y in Array.get row x;;

let height maze = Array.length maze;;

let width maze = Array.length (Array.get maze 0);;

let is_open pos maze =
  let c = at pos maze in (c = '.' || c = 'L');;

let valid_pos (x, y) maze =
  x >= 0 && y >= 0 && x < width maze && y < height maze;;

let find_first char maze =
  Array.find_mapi (fun y row -> Option.map (fun x -> (x, y)) (Array.find_index (fun c -> c = char) row)) maze;;

let startpos maze = Option.get @@ find_first 'L' maze;;

let iter_positions f maze =
  let (w, h) = width maze, height maze in
  for x = 0 to w - 1 do
    for y = 0 to h - 1 do
      if is_open (x, y) maze then f (x, y)
      else ()
    done
  done
;;

module Node = struct
    type t = (int * int)
    let compare (ax, ay) (bx, by) = (match Stdlib.compare ax bx with
        | 0 -> Stdlib.compare ay by
        | v -> v)
      let hash = Hashtbl.hash
      let equal = (=)
    end

module Edge = struct
    type t = int * int * char
    let compare (ax, ay, ad) (bx, by, bd) =
      match Stdlib.compare ax bx with
      | 0 -> (match Stdlib.compare ay by with 0 -> Stdlib.compare ad bd | v -> v)
      | v -> v
    let default = (-1, -1, '?')
  end

module Move = struct
    type t = (Node.t * Node.t)
    let compare (ap1, ap2) (bp1, bp2) = match Node.compare ap1 bp1 with
      | 0 -> Node.compare ap2 bp2
      | v -> v
  end

module G = Graph.Imperative.Graph.ConcreteLabeled(Node)(Edge)
module Walk = Graph.Traverse.Bfs(G)
module MST = Graph.Kruskal.Make(G)(Edge)

let make_graph maze =
  let (w, h) = width maze, height maze in
  let g = G.create ~size:(w * h) () in
  iter_positions (fun (x, y) ->
          if x < w - 1 && is_open (x + 1, y) maze then G.add_edge_e g (G.E.create (x, y) (x, y, 'R') (x + 1, y)) else ();
          if x > 0     && is_open (x - 1, y) maze then G.add_edge_e g (G.E.create (x, y) (x, y, 'L') (x - 1, y)) else ();
          if y < h - 1 && is_open (x, y + 1) maze then G.add_edge_e g (G.E.create (x, y) (x, y, 'D') (x, y + 1)) else ();
          if y > 0     && is_open (x, y - 1) maze then G.add_edge_e g (G.E.create (x, y) (x, y, 'U') (x, y - 1)) else ()
      ) maze;
  g
;;

module NodeSet = Set.Make(Node)
module MoveSet = Set.Make(Move)
module NodeMap = Map.Make(Node)

let add_move (a, b) m =
  NodeMap.add a (match NodeMap.find_opt a m with
      | Some dests -> NodeSet.add b dests
      | None -> NodeSet.singleton b
    ) m;;

let del_move (a, b) m =
  let newdests = NodeMap.find a m |> NodeSet.remove b in
  if NodeSet.is_empty newdests then
    NodeMap.remove a m
  else NodeMap.add a newdests m
;;

let get_nexts a m =
  match NodeMap.find_opt a m with
  | Some dests -> NodeSet.to_list dests
  | _ -> [];;

let print_move ((ax, ay), (bx, by)) = Printf.printf "(%d, %d) -> (%d, %d)\n" ax ay bx by;;

let move_to_dir ((ax, ay), (bx, by)) =
  match bx - ax with
  |  1 -> 'R'
  | -1 -> 'L'
  | 0 ->
      (match by - ay with
        | 1 -> 'D'
        | -1 -> 'U'
        | _ -> failwith "gaaah")
  | _ -> failwith "gah"
;;


let walk maze =
  let mst = make_graph maze |> MST.spanningtree |> List.map (fun (p1, _e, p2) -> p1, p2) in
  let moves_from = ref @@ List.fold_left (fun m (p1, p2) -> add_move (p1, p2) m |> add_move (p2, p1)) NodeMap.empty mst in
  let pos_todo = ref NodeSet.empty in
  iter_positions (fun pos -> pos_todo := NodeSet.add pos !pos_todo) maze;
  let pos = ref (startpos maze) in
  pos_todo := NodeSet.remove !pos !pos_todo;
  let moves_taken = ref [] in
  while not (NodeSet.is_empty !pos_todo) do
    let nexts = get_nexts !pos !moves_from in
    let (backtracking, next) = (match List.find_opt (fun n -> NodeSet.mem n !pos_todo) nexts with
                  | Some n -> (false, n)
                  | None -> (true, List.hd nexts)) in
    let this_move = (!pos, next) in
    (* don't go that way again *)
    if backtracking then moves_from := del_move (next, !pos) !moves_from else ();
    moves_taken := this_move :: !moves_taken;
    pos := next;
    pos_todo := NodeSet.remove next !pos_todo;
  done;
  List.rev !moves_taken |> List.map move_to_dir |> List.to_seq |> String.of_seq
;;

let chunks s =
  String.to_seq s
    |> Seq.group (=)
    |> Seq.map (fun seq -> ((let (c, _) = Option.get (Seq.uncons seq) in c), Seq.length seq)) |> List.of_seq
;;

module IntSet = Set.Make(Int)

(* Run-length encoding in the lambda calculus sounds hard, so instead I
bind some hopefully-reused strings to variables and substitute them *)
let compress str =
  let expansions = Array.of_list [
          (String.make 640 'U');
          (String.make 640 'D');
          (String.make 640 'L');
          (String.make 640 'R');
          (String.make 320 'U');
          (String.make 320 'D');
          (String.make 320 'L');
          (String.make 320 'R');
          (String.make 160 'U');
          (String.make 160 'D');
          (String.make 160 'L');
          (String.make 160 'R');
          (String.make 80 'U');
          (String.make 80 'D');
          (String.make 80 'L');
          (String.make 80 'R');
          (String.make 40 'U');
          (String.make 40 'D');
          (String.make 40 'L');
          (String.make 40 'R');
          (String.make 20 'U');
          (String.make 20 'D');
          (String.make 20 'L');
          (String.make 20 'R');
          (String.make 10 'U');
          (String.make 10 'D');
          (String.make 10 'L');
          (String.make 10 'R');
          "ULULULULULULULUL";
          "URURURURULULULUL";
          "ULULULULULULULUL";
          "URURURURULULULUL";
          "ULULULUL";
          "URURURUR";
          "ULULULUL";
          "URURURUR";
    ] in
  Icfp.(
    let rec to_expr (s : string) (sofar : string) (seen : IntSet.t) =
      match Array.find_index (fun e -> String.starts_with ~prefix:e s) expansions with
      | Some i -> let expansion_len = (String.length (Array.get expansions i)) in
                  let rest = (String.sub s expansion_len (String.length s - expansion_len)) in
                  let (rest_exp, seen) = to_expr rest "" (IntSet.add i seen) in
                  let ex = BinOp { op = '.'; arg1 = Var { varnum = Z.of_int i }; arg2 = rest_exp } in
                  if sofar = "" then (ex, seen)
                  else (BinOp { op = '.'; arg1 = Strn sofar; arg2 = ex }, seen)
      | None -> if s = "" then (Strn sofar, seen)
                else to_expr (String.sub s 1 (String.length s - 1)) (sofar ^ String.sub s 0 1) seen
    in
    let (expr, seen) = to_expr str "" IntSet.empty in
    let expander = IntSet.fold (fun i e -> Icfp.( BinOp { op = '$'; arg1 = Lambda { varnum = Z.of_int i; arg = e }; arg2 = Strn (Array.get expansions i)})) seen expr in
      Icfp.encode_expr expander
  )
;;
