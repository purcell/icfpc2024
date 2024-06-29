type expr =
| Integer of int64
| Strn of string
| BinOp of { op : char;  arg1 : expr; arg2 : expr }
| UnOp of { op : char; arg : expr }
| If of { cond : expr; when_true : expr; when_false: expr; }
| Bool of bool
| Lambda of { varnum: int64; arg : expr }
| Var of { varnum: int64; }
[@@deriving show]
;;


(* ---------------------------------------------------------------------- *)
(* BASE-94 ENCODING *)

let string_chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n";;
let encode_char c = String.index string_chars c + 33 |> Char.chr;;
let encode_string s = String.map encode_char s;;

let encode_int i =
  let rec go x = let div = Int64.div x 94L in
                 let rem = Int64.rem x 94L in
                 (if div = 0L then [] else go div) @ [Int64.to_int rem];
  in
  go i |> List.map (fun x -> Char.chr (x + Char.code '!')) |> List.to_seq |> String.of_seq;;

let rec encode_expr e =
                  match e with
                  | Bool b -> "B" ^ if b then "T" else "F"
                  | Strn s -> "S" ^ encode_string s
                  | Integer i -> "I" ^ encode_int i
                  | If x -> "? " ^ encode_expr x.cond ^ " " ^ encode_expr x.when_true ^ " " ^ encode_expr x.when_false
                  | _ -> failwith "unsupported expr";;

assert (encode_expr (Strn "Hello World!") = "SB%,,/}Q/2,$_");;
assert (encode_expr (Integer 1337L) = "I/6");;
assert (encode_expr (Integer 15818151L) = "I4%34");;


(* ---------------------------------------------------------------------- *)
(* BASE-94 ENCODING *)

let decode_char c = string_chars.[Char.code c - 33];;
let decode_string s = String.map decode_char s;;

let decode_int s = String.to_seq s
                     |> Seq.mapi (fun i c -> Z.mul (Z.pow (Z.of_int 94) (String.length s - i - 1))
                                               (Z.of_int (Char.code c - Char.code '!')))
                     |> Seq.fold_left Z.add Z.zero |> Z.to_int64;;

assert (decode_int "/6" = 1337L);;
assert (decode_string "B%,,/}Q/2,$_" = "Hello World!");;


(* ---------------------------------------------------------------------- *)
(* PARSING *)

let parse_expr s =
  let rec go toks = match toks with
      | tok :: rest ->
          let body = String.sub tok 1 (String.length tok - 1) in
          (match tok.[0] with
            | 'B' -> assert (String.length body = 1);
                     let arg1, rest = go rest in
                     let arg2, rest = go rest in
                     (BinOp { op = body.[0]; arg1 = arg1; arg2 = arg2; }, rest)
            | 'U' -> assert (String.length body = 1);
                     let arg, rest = go rest in
                     (UnOp { op = body.[0]; arg = arg }, rest)
            | 'L' -> let arg, rest = go rest in
                     (Lambda { varnum = (decode_int body); arg = arg }, rest)
            | 'v' -> (Var { varnum = (decode_int body) }, rest)
            | 'T' -> (Bool true, rest)
            | 'F' -> (Bool false, rest)
            | 'S' -> (Strn (decode_string body), rest)
            | 'I' -> (Integer (decode_int body), rest)
            | '?' -> let cond, rest = go rest in
                     let when_true, rest = go rest in
                     let when_false, rest = go rest in
                     (If { cond = cond; when_true = when_true; when_false = when_false; }, rest)
            | _ -> failwith ("unsupported indicator in token " ^ tok))
      | [] -> failwith "no more tokens"
    in
  let e, rem = go (String.split_on_char ' ' s) in
  if not (List.is_empty rem) then failwith ("dangling tokens: " ^ String.concat " " rem);
  e
;;


(* ---------------------------------------------------------------------- *)
(* EVALUATION *)

let integer_val e = match e with Integer v -> v | _ -> failwith "not an integer";;
let bool_val e = match e with Bool v -> v | _ -> failwith "not a bool";;
let string_val e = match e with Strn v -> v | _ -> failwith "not a string";;

module IntMap = Map.Make(Int64);;
type scope = expr IntMap.t;;


let rec beta_reduce e scope =
  let recur x = beta_reduce x scope in
  match e with
  | Strn _ | Integer _ | Bool _ -> e
  | If i -> If { cond = recur i.cond; when_true = recur i.when_true; when_false = recur i.when_false; }
  | UnOp o ->  UnOp { op = o.op; arg = recur o.arg }
  | BinOp o -> (match o.op with
                 | '$' -> (match o.arg1 with
                       | Lambda l ->
                           Stdlib.print_string ("Applying " ^ (Int64.to_string l.varnum) ^ "\n");
                           beta_reduce o.arg1 (IntMap.add l.varnum (recur o.arg2) scope)
                       | _ -> recur o.arg1
                     )
                 | _ -> BinOp { op = o.op; arg1 = recur o.arg1; arg2 = recur o.arg2; }
               )
  | Lambda _l -> recur _l.arg
  | Var v -> IntMap.find v.varnum scope
;;

let rec eval e scope =
  Stdlib.print_string "-------------------------------\n";
  Stdlib.print_string ("Scope: " ^ [%show: (int64 * expr) list] (IntMap.to_list scope));
  Stdlib.print_newline ();
  Stdlib.print_string (show_expr e);
  Stdlib.print_newline ();
  match e with
  | Strn _ | Integer _ | Bool _ -> e
  | If i -> if bool_val (eval i.cond scope)
            then eval i.when_true scope else eval i.when_false scope
  | UnOp o -> let argval = eval o.arg scope in
      (match o.op with
        | '-' -> Integer (Int64.mul (-1L) (integer_val argval))
        | '!' -> Bool (not (bool_val argval))
        | '#' -> Integer (decode_int (encode_string (string_val argval)))
        | '$' -> Strn (decode_string (encode_int (integer_val argval)))
        | _ -> failwith "unsupported unop"
      )
  | BinOp o -> let intop f = Integer (f (integer_val (eval o.arg1 scope)) (integer_val (eval o.arg2 scope))) in
               let cmpop f = Bool (f (integer_val (eval o.arg1 scope)) (integer_val (eval o.arg2 scope))) in
               (match o.op with
                 | '+' -> intop Int64.add
                 | '-' -> intop Int64.sub
                 | '*' -> intop Int64.mul
                 | '/' -> intop Int64.div
                 | '%' -> intop Int64.(rem)
                 | '<' -> cmpop (<)
                 | '>' -> cmpop (>)
                 | '=' -> Bool (match (eval o.arg1 scope) with
                         | Integer v -> v = integer_val (eval o.arg2 scope)
                         | Bool v -> v = bool_val (eval o.arg2 scope)
                         | Strn v -> v = string_val (eval o.arg2 scope)
                         | _ -> failwith "unsupported comparison"
                       )
                 | '|' -> Bool (bool_val (eval o.arg1 scope) || bool_val (eval o.arg2 scope))
                 | '&' -> Bool (bool_val (eval o.arg1 scope) && bool_val (eval o.arg2 scope))
                 | '.' -> Strn (string_val (eval o.arg1 scope) ^ string_val (eval o.arg2 scope))
                 | 'T' -> Strn (String.sub (string_val (eval o.arg2 scope)) 0 (Int64.to_int (integer_val (eval o.arg1 scope))))
                 | 'D' -> let s = string_val (eval o.arg2 scope) in
                          let n = Int64.to_int (integer_val (eval o.arg1 scope)) in
                          Strn (String.sub s n (String.length s - n))
                 | '$' -> (match o.arg1 with
                       | Lambda l ->
                           Stdlib.print_string ("Applying " ^ (Int64.to_string l.varnum) ^ "\n");
                           eval l.arg (IntMap.add l.varnum o.arg2 scope)
                       | _ -> eval o.arg1 scope
                     )
                 | _ -> failwith "unsupported binop"
               )
  | Lambda _l -> eval _l.arg scope
  | Var v ->
      (match IntMap.find_opt v.varnum scope with
        | Some expr' -> eval expr' scope
        | None -> failwith ("Missing value: " ^ Int64.to_string v.varnum)
      )
;;


let run s = eval (parse_expr s) IntMap.empty;;

(* assert (run "? B> I# I$ S9%3 S./" = Strn "no");; *)
(* assert (run "U- I$" = Integer (-3L));; *)
(* assert (run "U! T" = Bool false);; *)
(* assert (run "U# S4%34" = Integer 15818151L);; *)
(* assert (run "U$ I4%34" = Strn "test");; *)
(* assert (integer_val (run "B+ I# I$") = 5L);; *)
(* assert (integer_val (run "B- I$ I#") = 1L);; *)
(* assert (integer_val (run "B* I$ I#") = 6L);; *)
(* assert (integer_val (run "B/ U- I( I#") = (-3L));; *)
(* assert (integer_val (run "B% U- I( I#") = (-1L));; *)
(* assert (bool_val (run  "B= I$ I#") = false);; *)
(* assert (bool_val (run  "B< I$ I#") = false);; *)
(* assert (bool_val (run  "B> I$ I#") = true);; *)
(* assert (bool_val (run "B| T F") = true);; *)
(* assert (bool_val (run "B& T F") = false);; *)
(* assert (string_val (run "B. S4% S34") = "test");; *)
(* assert (string_val (run "BT I$ S4%34") = "tes");; *)
(* assert (string_val (run "BD I$ S4%34") = "t");; *)

Stdlib.print_string @@ "\n\nREDUCED: " ^ (show_expr (beta_reduce (parse_expr "B$ B$ L# L$ v# B. SB%,,/ S}Q/2,$_ IK") IntMap.empty));
(* Stdlib.print_string @@ "\n\nRESULT: " ^ (show_expr (run "B$ B$ L# L$ v# B. SB%,,/ S}Q/2,$_ IK")); *)
(* assert (run "B$ B$ L# L$ v# B. SB%,,/ S}Q/2,$_ IK" = Strn "Hello World!");; *)

(* assert (run "B$ L\" B+ v\" v\" B* I$ I#" = parse_expr "I-");; *)
(* assert (run "B$ L# B$ L\" B+ v\" v\" B* I$ I# v8" = parse_expr "I-");; *)
