
#use "pc.ml";;
open PC;;

exception X_not_yet_implemented;;
exception X_this_should_not_happen;;
  
type number =
  | Fraction of int * int
  | Float of float;;
  
type sexpr =
  | Bool of bool
  | Nil
  | Number of number
  | Char of char
  | String of string
  | Symbol of string
  | Pair of sexpr * sexpr;;

let rec sexpr_eq s1 s2 =
  match s1, s2 with
  | Bool(b1), Bool(b2) -> b1 = b2
  | Nil, Nil -> true
  | Number(Float f1), Number(Float f2) -> abs_float(f1 -. f2) < 0.001
  | Number(Fraction (n1, d1)), Number(Fraction (n2, d2)) -> n1 = n2 && d1 = d2
  | Char(c1), Char(c2) -> c1 = c2
  | String(s1), String(s2) -> s1 = s2
  | Symbol(s1), Symbol(s2) -> s1 = s2
  | Pair(car1, cdr1), Pair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2)
  | _ -> false;;

module Reader
: sig
  val read_sexprs : string -> sexpr list 
end
= struct
let normalize_scheme_symbol str =
  let s = string_to_list str in
  if (andmap
	(fun ch -> (ch = (lowercase_ascii ch)))
	s) then str
  else Printf.sprintf "|%s|" str;;


(* **********************Our code start here**********************  *)

(*val make_paired : ('a -> 'b * 'c) -> ('d -> 'e * 'f) -> ('c -> 'g * 'd) -> 'a -> 'g * 'f = <fun>  *)
let make_paired nt_left nt_right nt =
  let nt = caten nt_left nt in
  let nt = pack nt (fun (_,d) -> d) in
  let nt = caten nt nt_right in
  pack nt (fun (d,_) -> d);;

(*3.2.1*)
(*val nt_whitespaces : char list -> char list * char list = <fun> *)
let nt_whitespaces = star(nt_whitespace);;

(*val make_spaced : (char list -> 'a * char list) -> char list -> 'a * char list = <fun> *)
(*The parser will skips whitespaces from left and right*)
let make_spaced nt = make_paired nt_whitespaces nt_whitespaces nt;;

(*3.2.2*)
(*char list -> sexpr * char list = <fun> *)
let nt_line_comment =
  let comment_start = (char ';') in
  let end_of_line = char (char_of_int 10) in
  let comment_end = disj end_of_line (pack nt_end_of_input (fun (d) -> 'd')) in
  let comment = star(diff nt_any comment_end) in
  let nt = caten comment_start comment in
  let nt = pack nt (fun (_,d)->d) in
  let nt = caten nt comment_end in
  let nt = pack nt (fun (_)->[]) in
  nt;;

let d_c_a_ws = disj nt_whitespaces nt_line_comment;;
let make_comment_and_whitespaced nt = make_paired d_c_a_ws d_c_a_ws nt;;

(*3.3.1*)
(*char list -> sexpr * char list = <fun> *)
let nt_boolean = 
  let hashtag = (char '#') in
  let bool_true = (char_ci 't') in
  let bool_false = (char_ci 'f') in
  let hashtag_true = caten hashtag bool_true in
  let hashtag_false = caten hashtag bool_false in
  let bool_true = pack hashtag_true (fun (t) -> true) in
  let bool_false = pack hashtag_false (fun (f) -> false) in
  let nt = disj bool_true bool_false in
  let nt = pack nt (fun (x) -> (Bool x)) in
  make_spaced nt;;

(* 3.3.3 *)
(* let lowercase_letters = (range 'a' 'z');; *)
let digit = (range '0' '9');;
let dot = char '.';;

let uppercase_letters = 
    let nt = (range_ci 'A' 'Z') in
    let nt = pack nt lowercase_ascii in
    nt;;

let punctuation = disj_list [(char '!');(char '$');(char '^');(char '*');(char '-');(char '_');
                            (char '=');(char '+');(char '<');(char '>');(char '/');(char '?');(char ':')];;

(* char list -> char list * char list = <fun> *)
let nt_symbol = 
  let symbol_char_not_dot = disj_list [uppercase_letters; punctuation; digit] in
  let symbol_char = (disj symbol_char_not_dot dot) in
  let psc = (plus symbol_char) in
  let scpsc = pack (caten symbol_char psc) (fun (e, es) -> (e :: es))  in
  let nt = pack (caten symbol_char_not_dot nt_epsilon) (fun (e, es) -> (e :: es)) in
  let nt = (disj scpsc nt) in 
  let nt = pack nt (fun (e) -> Symbol (list_to_string e)) in
   nt;; 

(*3.3.2*)
let rec gcd x y =  if y==0 then x else gcd y (x mod y);;

let nt_natural = 
  let nt = plus digit in
  let nt = pack nt (fun x -> int_of_string(list_to_string x)) in
  nt;;

let nt_sign = 
  let nt = maybe (disj (char '+') (char '-')) in
  let nt = pack nt (fun(sign) -> 
  match sign with
  | Some('-') -> -1
  | _ -> +1) in
  nt;;

 (* char list -> int * char list = <fun>  *)
let nt_int = 
  let nt = caten nt_sign nt_natural in
  let nt = pack nt (fun (a,b) -> a*b) in
  nt;;

(* char list -> sexpr * char list = <fun>  *)
let nt_fraction = 
  let slash = char '/' in 
  let nt = (caten nt_int (caten slash nt_natural)) in
  let nt = pack nt (fun (n,(s,d)) -> Number (Fraction(n/(gcd n d),d/(gcd n d)))) in
  nt;;

 (* char list -> sexpr * char list = <fun>  *)
let nt_integer = 
  let nt = not_followed_by nt_int nt_symbol in 
  let nt = pack nt (fun (int) -> Number (Fraction (int,1))) in
  nt;;

let nt_float_unpacked = 
  let nt = (caten nt_int (caten dot nt_natural)) in
  let nt = pack nt (fun (int,(b,natural)) -> float_of_string((string_of_int int) ^ "." ^ (string_of_int natural))) in
  nt;;

let nt_float = 
  let nt = pack nt_float_unpacked (fun (num) -> Number (Float (num))) in
  nt;;

(* 4.1 *)
let nt_scientific_notation = 
  let e = char_ci 'e' in
  let i_t_f = pack nt_int (fun num -> float_of_int num) in
  let nt = disj nt_float_unpacked i_t_f in
  let nt = caten nt (caten e i_t_f) in
  let nt = pack nt (fun (n,(e,exp))-> let num = n *. (10. ** exp) in Number(Float(num))) in
  nt;;

let nt_number = disj_list [nt_scientific_notation ; nt_float; nt_fraction; nt_integer];;

(*3.3.4*)
(* char list -> sexpr * char list = <fun> *)
let nt_string = 
  let quote = char (char_of_int 34) in
  let string_char = diff nt_any (disj (char (char_of_int 92)) (char (char_of_int 34))) in
  let meta_chars = disj_list [
    pack (word "\\r") (fun(_) -> (char_of_int 13));
    pack (word "\\n") (fun(_) -> (char_of_int 10));
    pack (word "\\t") (fun(_) -> (char_of_int 9));
    pack (word "\\f") (fun(_) -> (char_of_int 12));
    pack (word "\\\\") (fun(_) -> (char_of_int 92));
    pack (word "\\\"") (fun(_) -> (char_of_int 34));
  ] in
  let string = disj string_char meta_chars in
  let nt = caten quote (star(string)) in
  let nt = pack nt (fun (_,s) -> s) in
  let nt = caten nt quote in 
  let nt = pack nt (fun (s,_)-> (String(list_to_string s))) in 
  make_spaced nt;;

(*3.3.5*)
(*char list -> sexpr * char list = <fun> *)
  let nt_char = 
    let char_start = caten (char '#') (char '\\') in
    let visible_char = const (fun ch -> (char_of_int 32) < ch) in
    let named_char = disj_list [
      pack (word_ci "nul") (fun(_) -> (char_of_int 0));
      pack (word_ci "newline") (fun(_) -> (char_of_int 10));
      pack (word_ci "return") (fun(_) -> (char_of_int 13));
      pack (word_ci "tab") (fun(_) -> (char_of_int 9));
      pack (word_ci "page") (fun(_) -> (char_of_int 12));
      pack (word_ci "space") (fun(_) -> (char_of_int 32));
    ] in
    let nt = caten char_start (disj named_char visible_char) in  
    let nt = pack nt (fun (_,ch) -> Char ch) in
    make_spaced nt;;


(*3.3.6*)
let left_bracket = make_paired nt_whitespaces nt_whitespaces (char '(');;
let right_bracket = make_paired nt_whitespaces nt_whitespaces (char ')');;

let nt_nil =
  let nt = caten left_bracket (star(nt_line_comment)) in
  let nt = pack nt (fun (_,x) -> x) in
  let nt = caten nt right_bracket in
  let nt = pack nt (fun (_) -> Nil) in
  nt;;

let nt_dottedList = pack (const (fun s -> true)) (fun x -> Nil);;

let nt_quoted = pack (const (fun s -> true)) (fun x -> Nil);;

let nt_quasiQuoted = pack (const (fun s -> true)) (fun x -> Nil);;

let nt_unquoted = pack (const (fun s -> true)) (fun x -> Nil);;

let nt_unquoteAndSpliced = pack (const (fun s -> true)) (fun x -> Nil);;

let make_brackets nt = make_paired (char '(') (char ')') nt;;

let rec nt_sexpr s = 
  let sexpr_blocks = disj_list [
                          nt_boolean;
                          nt_char;
                          nt_number;
                          nt_string;
                          nt_symbol; 
                          nt_list;
                          nt_dottedList; 
                          nt_quoted;
                          nt_quasiQuoted;
                          nt_unquoted;
                          nt_unquoteAndSpliced
                          ] in 
  (make_comment_and_whitespaced sexpr_blocks) s
  and nt_list s =
    let nt = make_brackets (star nt_sexpr) in
    let nt = pack nt (fun (expr) -> match expr with
    |[] -> Nil
    |list -> List.fold_right (fun s1 s2 -> Pair(s1,s2)) list Nil) in
    nt s 
  and nt_dottedList s =
    let nt = caten (plus nt_sexpr)(caten dot nt_sexpr) in
    let nt = make_brackets nt in
    let nt = pack nt (fun (expr) -> match expr with
    |(f_sexpr,(_,s_sexpr)) -> List.fold_right (fun s1 s2 -> Pair(s1,s2)) f_sexpr s_sexpr) in
    nt s
  and nt_quoted s = 
    let quote = char '\'' in
    let nt = caten quote nt_sexpr in
    let nt = pack nt (fun (_,s) -> Pair(Symbol("quote"),Pair(s,Nil))) in
    nt s
  and nt_quasiQuoted s =
    let tik = char '`' in
    let nt = caten tik nt_sexpr in
    let nt = pack nt (fun (_,s) -> Pair(Symbol("quasiquote"), Pair(s,Nil))) in
    nt s
  and nt_unquoted s = 
    let comma = char ',' in
    let nt = caten comma nt_sexpr in
    let nt = pack nt (fun (_,s) -> Pair(Symbol("unquote"), Pair(s,Nil))) in
    nt s
  and  nt_unquoteAndSpliced s = 
    let comma = char ',' in
    let shtrudel = char '@' in
    let prefix = caten comma shtrudel  in
    (* let prefix = pack prefix () *)
    let nt = caten prefix nt_sexpr in
    let nt = pack nt (fun (_,s) -> Pair(Symbol("unquote-splicing"), Pair(s,Nil))) in
    nt s;;

let read_sexprs string =
  let (ast,s) = ((star nt_sexpr) (string_to_list string)) in
  ast;;

end;;