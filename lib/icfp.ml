module I = struct
   include Z
   let pp = Z.pp_print
end;;

type expr =
| Integer of I.t
| Strn of string
| BinOp of { op : char;  arg1 : expr; arg2 : expr }
| UnOp of { op : char; arg : expr }
| If of { cond : expr; when_true : expr; when_false: expr; }
| Bool of bool
| Lambda of { varnum: I.t; arg : expr }
| Var of { varnum: I.t; }
[@@deriving show]
;;


(* ---------------------------------------------------------------------- *)
(* BASE-94 ENCODING *)

let string_chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n";;
let encode_char c = String.index string_chars c + 33 |> Char.chr;;
let encode_string s = String.map encode_char s;;

let encode_int i =
  let rec go x = let div, rem = I.div_rem x (I.of_int 94) in
                 (if div = I.zero then [] else go div) @ [I.to_int rem];
  in
  go i |> List.map (fun x -> Char.chr (x + Char.code '!')) |> List.to_seq |> String.of_seq;;

let rec encode_expr e =
                  match e with
                  | Bool b -> "B" ^ if b then "T" else "F"
                  | Strn s -> "S" ^ encode_string s
                  | Integer i -> "I" ^ encode_int i
                  | Var v -> "v" ^ encode_int v.varnum
                  | If x -> "? " ^ encode_expr x.cond ^ " " ^ encode_expr x.when_true ^ " " ^ encode_expr x.when_false
                  | _ -> failwith "unsupported expr";;

assert (encode_expr (Strn "Hello World!") = "SB%,,/}Q/2,$_");;
assert (encode_expr (Integer (I.of_int 1337)) = "I/6");;
assert (encode_expr (Integer (I.of_int 15818151)) = "I4%34");;


(* ---------------------------------------------------------------------- *)
(* BASE-94 ENCODING *)

let decode_char c = string_chars.[Char.code c - 33];;
let decode_string s = String.map decode_char s;;

let decode_int s = String.to_seq s
                     |> Seq.mapi (fun i c -> I.mul (I.pow (I.of_int 94) (String.length s - i - 1))
                                               (I.of_int (Char.code c - Char.code '!')))
                     |> Seq.fold_left I.add I.zero;;

assert (decode_int "/6" = (I.of_int 1337));;
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

let rec reduce e var value =
  let recurse ex = reduce ex var value in
  match e with
  | Var v -> if v.varnum = var then value else e
  | Strn _ | Integer _ | Bool _ -> e
  | UnOp o -> UnOp { op = o.op; arg = recurse o.arg }
  | BinOp o -> BinOp { op = o.op; arg1 = recurse o.arg1; arg2 = recurse o.arg2 }
  | If i -> If { cond = recurse i.cond; when_true = recurse i.when_true; when_false = recurse i.when_false; }
  | Lambda l -> if l.varnum = var then e else Lambda { varnum = l.varnum; arg = recurse l.arg }
;;

(* ---------------------------------------------------------------------- *)
(* EVALUATION *)

let integer_val e = match e with Integer v -> v | _ -> failwith "not an integer";;
let bool_val e = match e with Bool v -> v | _ -> failwith "not a bool";;
let string_val e = match e with Strn v -> v | _ -> failwith "not a string";;

module IntMap = Map.Make(Int64);;
let show_intmap m = [%show: (int64 * expr) list] (IntMap.to_list m);;

let rec eval e =
  match e with
  | Strn _ | Integer _ | Bool _ -> e
  | If i -> eval (if bool_val (eval i.cond) then i.when_true else i.when_false)
  | UnOp o -> let argval = eval o.arg in
      (match o.op with
        | '-' -> Integer (I.mul (I.of_int (-1)) (integer_val argval))
        | '!' -> Bool (not (bool_val argval))
        | '#' -> Integer (decode_int (encode_string (string_val argval)))
        | '$' -> Strn (decode_string (encode_int (integer_val argval)))
        | _ -> failwith "unsupported unop"
      )
  | BinOp o -> let intop f = Integer (f (integer_val (eval o.arg1)) (integer_val (eval o.arg2))) in
               let cmpop f = Bool (f (integer_val (eval o.arg1)) (integer_val (eval o.arg2))) in
               (match o.op with
                 | '+' -> intop I.add
                 | '-' -> intop I.sub
                 | '*' -> intop I.mul
                 | '/' -> intop I.div
                 | '%' -> intop I.rem
                 | '<' -> cmpop Z.lt
                 | '>' -> cmpop Z.gt
                 | '=' -> Bool (match eval o.arg1 with
                         | Integer v -> v = integer_val (eval o.arg2)
                         | Bool v -> v = bool_val (eval o.arg2)
                         | Strn v -> v = string_val (eval o.arg2)
                         | _ -> failwith "unsupported comparison"
                       )
                 | '|' -> Bool (bool_val (eval o.arg1) || bool_val (eval o.arg2))
                 | '&' -> Bool (bool_val (eval o.arg1) && bool_val (eval o.arg2))
                 | '.' -> Strn (string_val (eval o.arg1) ^ string_val (eval o.arg2))
                 | 'T' -> Strn (String.sub (string_val (eval o.arg2)) 0 (I.to_int (integer_val (eval o.arg1))))
                 | 'D' -> let s = string_val (eval o.arg2) in
                          let n = I.to_int (integer_val (eval o.arg1)) in
                          Strn (String.sub s n (String.length s - n))
                | '$' -> (match o.arg1 with
                      | Lambda l -> eval (reduce l.arg l.varnum o.arg2)
                      | _ -> eval (BinOp { op = '$'; arg1 = eval o.arg1; arg2 = o.arg2 })   (* Force head *)
                      )
                 | _ -> failwith "unsupported binop"
               )
  | Lambda _ -> e
  | Var v -> failwith @@ Printf.sprintf "unbound var: %s (%s)" (Z.to_string v.varnum) (encode_int v.varnum)
;;


let run s = eval (parse_expr s);;

assert (run "? B> I# I$ S9%3 S./" = Strn "no");;
assert (run "U- I$" = Integer (I.of_int (-3)));;
assert (run "U! T" = Bool false);;
assert (run "U# S4%34" = Integer (I.of_int 15818151));;
assert (run "U$ I4%34" = Strn "test");;
assert (integer_val (run "B+ I# I$") = I.of_int 5);;
assert (integer_val (run "B- I$ I#") = I.of_int 1);;
assert (integer_val (run "B* I$ I#") = I.of_int 6);;
assert (integer_val (run "B/ U- I( I#") = I.of_int (-3));;
assert (integer_val (run "B% U- I( I#") = I.of_int (-1));;
assert (bool_val (run  "B= I$ I#") = false);;
assert (bool_val (run  "B< I$ I#") = false);;
assert (bool_val (run  "B> I$ I#") = true);;
assert (bool_val (run "B| T F") = true);;
assert (bool_val (run "B& T F") = false);;
assert (string_val (run "B. S4% S34") = "test");;
assert (string_val (run "BT I$ S4%34") = "tes");;
assert (string_val (run "BD I$ S4%34") = "t");;
assert (run "B$ B$ L# L$ v# B. SB%,,/ S}Q/2,$_ IK" = Strn "Hello World!");;
assert (run "B$ L\" B+ v\" v\" B* I$ I#" = parse_expr "I-");;
assert (run "B$ L# B$ L\" B+ v\" v\" B* I$ I# v8" = parse_expr "I-");;

(* Example that "uses 109 beta reductions" *)
assert (run "B$ B$ L\" B$ L# B$ v\" B$ v# v# L# B$ v\" B$ v# v# L\" L# ? B= v# I! I\" B$ L$ B+ B$ v\" v$ B$ v\" v$ B- v# I\" I%" = Integer (Z.of_int 16));;

let example raw =
  (* Stdlib.print_string @@ "\n============================================================\nRAW:\n" ^ (show_expr (parse_expr raw)); *)
  (* Stdlib.print_string @@ "\n============================================================\nREDUCED:\n" ^ (show_expr (beta_reduce (parse_expr raw))); *)
  Stdlib.print_string @@ "\n============================================================\nRESULT:\n" ^ (show_expr (run raw));;
;;

let language_test =
  "? B= B$ B$ B$ B$ L$ L$ L$ L# v$ I\" I# I$ I% I$ ? B= B$ L$ v$ I+ I+ ? B= BD I$ S4%34 S4 ? B= BT I$ S4%34 S4%3 ? B= B. S4% S34 S4%34 ? U! B& T F ? B& T T ? U! B| F F ? B| F T ? B< U- I$ U- I# ? B> I$ I# ? B= U- I\" B% U- I$ I# ? B= I\" B% I( I$ ? B= U- I\" B/ U- I$ I# ? B= I# B/ I( I$ ? B= I' B* I# I$ ? B= I$ B+ I\" I# ? B= U$ I4%34 S4%34 ? B= U# S4%34 I4%34 ? U! F ? B= U- I$ B- I# I& ? B= I$ B- I& I# ? B= S4%34 S4%34 ? B= F F ? B= I$ I$ ? T B. B. SM%,&k#(%#+}IEj}3%.$}z3/,6%},!.'5!'%y4%34} U$ B+ I# B* I$> I1~s:U@ Sz}4/}#,!)-}0/).43}&/2})4 S)&})3}./4}#/22%#4 S\").!29}q})3}./4}#/22%#4 S\").!29}q})3}./4}#/22%#4 S\").!29}q})3}./4}#/22%#4 S\").!29}k})3}./4}#/22%#4 S5.!29}k})3}./4}#/22%#4 S5.!29}_})3}./4}#/22%#4 S5.!29}a})3}./4}#/22%#4 S5.!29}b})3}./4}#/22%#4 S\").!29}i})3}./4}#/22%#4 S\").!29}h})3}./4}#/22%#4 S\").!29}m})3}./4}#/22%#4 S\").!29}m})3}./4}#/22%#4 S\").!29}c})3}./4}#/22%#4 S\").!29}c})3}./4}#/22%#4 S\").!29}r})3}./4}#/22%#4 S\").!29}p})3}./4}#/22%#4 S\").!29}{})3}./4}#/22%#4 S\").!29}{})3}./4}#/22%#4 S\").!29}d})3}./4}#/22%#4 S\").!29}d})3}./4}#/22%#4 S\").!29}l})3}./4}#/22%#4 S\").!29}N})3}./4}#/22%#4 S\").!29}>})3}./4}#/22%#4 S!00,)#!4)/.})3}./4}#/22%#4 S!00,)#!4)/.})3}./4}#/22%#4";;

assert ((run language_test) = Strn "Self-check OK, send `solve language_test 4w3s0m3` to claim points for it");;
