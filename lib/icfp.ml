type expr =
| Integer of Z.t
| Strn of string
| BinOp of { op : char;  arg1 : expr; arg2 : expr }
| UnOp of { op : char; arg : expr }
| If of { cond : expr; when_true : expr; when_false: expr; }
| Bool of bool
| Lambda of { varnum: Z.t; arg : expr }
| Var of { varnum: Z.t; }
;;



(* ENCODING -------------------------------------------------- *)

let string_chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n";;
let encode_char c = String.index string_chars c + 33 |> Char.chr;;
let encode_string s = String.map encode_char s;;

let encode_int i =
  let rec go x = let (div, rem) = Z.div_rem x (Z.of_int 94) in
                 (if div = Z.zero then [] else go div) @ [Z.to_int rem];
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
assert (encode_expr (Integer (Z.of_int 1337)) = "I/6");;
assert (encode_expr (Integer (Z.of_int64 15818151L)) = "I4%34");;

(* PARSING -------------------------------------------------- *)

let decode_char c = string_chars.[Char.code c - 33];;
let decode_string s = String.map decode_char s;;

let decode_int s = String.to_seq s
                     |> Seq.mapi (fun i c -> Z.mul (Z.pow (Z.of_int 94) (String.length s - i - 1))
                                               (Z.of_int (Char.code c - Char.code '!')))
                     |> Seq.fold_left Z.add Z.zero;;

assert (decode_int "/6" = (Z.of_int 1337));;

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
            | 'L' -> 
                     let arg, rest = go rest in
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

let integer_val e = match e with Integer v -> v | _ -> failwith "not an integer";;
let bool_val e = match e with Bool v -> v | _ -> failwith "not a bool";;
let string_val e = match e with Strn v -> v | _ -> failwith "not a string";;

let rec eval_expr e = match e with
  | Strn _ | Integer _ | Bool _ -> e
  | If i -> if bool_val (eval_expr i.cond)
            then eval_expr i.when_true else eval_expr i.when_false
  | UnOp o -> let argval = eval_expr o.arg in
      (match o.op with
        | '-' -> Integer (Z.mul (Z.of_int (-1)) (integer_val argval))
        | '!' -> Bool (not (bool_val argval))
        | '#' -> Integer (decode_int (encode_string (string_val argval)))
        | '$' -> Strn (decode_string (encode_int (integer_val argval)))
        | _ -> failwith "unsupported unop"
      )
  | BinOp o -> let arg1val = eval_expr o.arg1 in
               let arg2val = eval_expr o.arg2 in
               let intop f = Integer (f (integer_val arg1val) (integer_val arg2val)) in
               let cmpop f = Bool (f (integer_val arg1val) (integer_val arg2val)) in
               (match o.op with
                 | '+' -> intop Z.add
                 | '-' -> intop Z.sub
                 | '*' -> intop Z.mul
                 | '/' -> intop Z.div
                 | '%' -> intop Z.(mod)
                 | '<' -> cmpop Z.(lt)
                 | '>' -> cmpop Z.(gt)
                 | '=' -> Bool (match arg1val with
                         | Integer v -> v = integer_val arg2val
                         | Bool v -> v = bool_val arg2val
                         | Strn v -> v = string_val arg2val
                         | _ -> failwith "unsupported comparison"
                       )
                 | '|' -> Bool (bool_val arg1val || bool_val arg2val)
                 | '&' -> Bool (bool_val arg1val && bool_val arg2val)
                 | '.' -> Strn (string_val arg1val ^ string_val arg2val)
                 | 'T' -> Strn (String.sub (string_val arg2val) 0 (Z.to_int (integer_val arg1val)))
                 | 'D' -> let s = string_val arg2val in
                          let n = Z.to_int (integer_val arg1val) in
                          Strn (String.sub s n (String.length s - n))
                 | _ -> failwith "unsupported binop"
               )
  | Lambda _ | Var _ -> failwith "unsupported expr"
;;


let run s = eval_expr (parse_expr s);;


assert (eval_expr (parse_expr "? B> I# I$ S9%3 S./") = Strn "no");;
assert (run "U- I$" = Integer (Z.of_int (-3)));;
assert (run "U! T" = Bool false);;
assert (run "U# S4%34" = Integer (Z.of_int 15818151));;
assert (run "U$ I4%34" = Strn "test");;
assert (integer_val (run "B+ I# I$") = Z.of_int 5);;
assert (integer_val (run "B- I$ I#") = Z.of_int 1);;
assert (integer_val (run "B* I$ I#") = Z.of_int 6);;
assert (integer_val (run "B/ U- I( I#") = Z.of_int (-3));;
assert (integer_val (run "B% U- I( I#") = Z.of_int (-1));;
assert (bool_val (run  "B= I$ I#") = false);;
assert (bool_val (run  "B< I$ I#") = false);;
assert (bool_val (run  "B> I$ I#") = true);;
assert (bool_val (run "B| T F") = true);;
assert (bool_val (run "B& T F") = false);;
assert (string_val (run "B. S4% S34") = "test");;
assert (string_val (run "BT I$ S4%34") = "tes");;
assert (string_val (run "BD I$ S4%34") = "t");;
