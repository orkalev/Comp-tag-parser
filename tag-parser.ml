#use "reader.ml";;

type constant =
  | Sexpr of sexpr
  | Void

type expr =
  | Const of constant
  | Var of string
  | If of expr * expr * expr
  | Seq of expr list
  | Set of expr * expr
  | Def of expr * expr
  | Or of expr list
  | LambdaSimple of string list * expr
  | LambdaOpt of string list * string * expr
  | Applic of expr * (expr list);;

let rec expr_eq e1 e2 =
  match e1, e2 with
  | Const Void, Const Void -> true
  | Const(Sexpr s1), Const(Sexpr s2) -> sexpr_eq s1 s2
  | Var(v1), Var(v2) -> String.equal v1 v2
  | If(t1, th1, el1), If(t2, th2, el2) -> (expr_eq t1 t2) &&
                                            (expr_eq th1 th2) &&
                                              (expr_eq el1 el2)
  | (Seq(l1), Seq(l2)
    | Or(l1), Or(l2)) -> List.for_all2 expr_eq l1 l2
  | (Set(var1, val1), Set(var2, val2)
    | Def(var1, val1), Def(var2, val2)) -> (expr_eq var1 var2) &&
                                             (expr_eq val1 val2)
  | LambdaSimple(vars1, body1), LambdaSimple(vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) &&
       (expr_eq body1 body2)
  | LambdaOpt(vars1, var1, body1), LambdaOpt(vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) &&
         (expr_eq body1 body2)
  | Applic(e1, args1), Applic(e2, args2) ->
     (expr_eq e1 e2) &&
       (List.for_all2 expr_eq args1 args2)
  | _ -> false;;
	
                       
exception X_syntax_error;;

exception X_sexpr of expr;;
exception X_first_problem;;

module type TAG_PARSER = sig
  val tag_parse_expressions : sexpr list -> expr list
end;; (* signature TAG_PARSER *)

module Tag_Parser : TAG_PARSER = struct

let reserved_word_list =
  ["and"; "begin"; "cond"; "define"; "else";
   "if"; "lambda"; "let"; "let*"; "letrec"; "or";
   "quasiquote"; "quote"; "set!"; "pset!"; "unquote";
   "unquote-splicing"];;  


let rec tag_parse_exp s = match s with
  (*constants*)
  (*need to add the quoted and unquoted*)
  |Nil -> Const(Void)
  |Bool(expr) -> Const(Sexpr(Bool(expr)))
  |Char(expr) -> Const(Sexpr(Char(expr)))
  |Number(expr) -> Const(Sexpr(Number(expr)))
  |String(expr) -> Const(Sexpr(String(expr)))
  |Pair(Symbol("quote"), Pair(expr, Nil)) -> Const(Sexpr(expr))
  (* |Pair(Symbol "quasiquote", Pair(Symbol expr, Nil)) ->  *)

  (*veriables*)
  |Symbol(expr) -> veriables_not_reserved_word expr

  (*conditionals*)
  |Pair(Symbol("if"), Pair(test, Pair(dit, Nil))) -> If((tag_parse_exp test),(tag_parse_exp dit), Const(Void))
  |Pair(Symbol("if"), Pair(test, Pair(dit, Pair(dif, Nil)))) -> If((tag_parse_exp test),(tag_parse_exp dit),(tag_parse_exp dif))
  
(* lambda *)
  |Pair(Symbol("lambda"),Pair(_,Nil)) -> raise X_syntax_error
  |Pair(Symbol("lambda"),Pair(Nil,body)) -> LambdaSimple([],tag_parse_seq body)
  |Pair(Symbol("lambda"),Pair(Symbol(s),body)) -> LambdaOpt([],s,tag_parse_seq body)
  |Pair(Symbol("lambda"),lambda_expr) -> handle_lambda lambda_expr

(* +  and handle_let_rec sexpr =
+    match sexpr with
+    | Pair(args_list,body) -> (tag_parse (Pair(Symbol("let"), Pair((handle_args_letrec args_list), (handle_body_letrec args_list body)))))
+    | _ -> raise X_syntax_error
+   and handle_args_letrec args_list =
+    match args_list with
+    | Pair(Pair(Symbol(x),a),b) -> Pair(Pair(Symbol(x),Pair(Pair(Symbol("quote") , Pair(Symbol("whatever") , Nil)),Nil)),(handle_args_letrec b))
+    | _ -> Nil
+   and handle_body_letrec args_list body =
+    match args_list with
+    | Pair(Pair(Symbol(x),a),b) -> Pair(Pair(Symbol("set!"), Pair(Symbol(x),a)),(handle_body_letrec b body))
+    | _ -> body *)


  |Pair(Symbol("let"),Pair(parms,body)) -> Applic(LambdaSimple(List.map string_of_symbol (get_args parms),tag_parse_seq body),List.map tag_parse_exp (get_vals parms))
  |Pair(Symbol("let*"),Pair(Nil,body)) -> (tag_parse_exp (Pair(Symbol("let"), Pair(Nil,body))))
  |Pair(Symbol("let*"),Pair(Pair(args,Nil),body)) -> (tag_parse_exp (Pair(Symbol("let"), Pair(Pair(args,Nil),body))))
  |Pair(Symbol("let*"),Pair(Pair(car,cdr),body)) -> (tag_parse_exp (Pair(Symbol("let"), Pair(Pair(car,Nil), Pair(Pair(Symbol("let*"),Pair(cdr, body)),Nil)))))
  (* |Pair(Symbol("lstrec"),Pair(args,body)) -> (tag_parse_exp (Pair(Symbol("let"), Pair((get_args args))))) *)
  (* |Pair(Symbol("letrec"),Pair(car,cdr)) -> tag_parse_exp *)

  (*disjunction*)
  |Pair(Symbol("or"),Nil) -> Const(Sexpr (Bool false))
  |Pair(Symbol("or"),Pair(car_expr,Nil)) -> Const(Sexpr(car_expr))
  |Pair(Symbol("or"),or_expr) -> Or(List.map tag_parse_exp (list_of_sexpr or_expr))
  
  |Pair(Symbol("and"),Nil) -> Const(Sexpr(Bool true))
  |Pair(Symbol("and"),Pair(car_expr,Nil)) -> tag_parse_exp car_expr
  |Pair(Symbol("and"),Pair(car_expr,cdr_expr)) -> If((tag_parse_exp car_expr), (tag_parse_exp (Pair(Symbol("and"),cdr_expr))), Const(Sexpr(Bool false)))

  (*definitions*)
  |Pair(Symbol("define"),Pair(Pair(car,cdr),body)) -> (tag_parse_exp (Pair(Symbol("define"),(Pair(car,(Pair(Pair(Symbol("lambda"), Pair(cdr,body)),Nil)))))))
  |Pair(Symbol("define"),Pair(car_expr,Nil)) -> Def((tag_parse_exp car_expr), Const(Void))
  |Pair(Symbol("define"),Pair(car_expr,Pair(cdar,Nil))) -> Def((tag_parse_exp car_expr),(tag_parse_exp cdar))

  (*assignments*)
  |Pair(Symbol("set!"), Pair(var, Pair(set_expr,Nil))) -> Set((tag_parse_exp var), (tag_parse_exp set_expr))
  |Pair(Symbol("pset!"), Pair(var, Pair(set_expr,Nil))) -> Set((tag_parse_exp var), (tag_parse_exp set_expr))


  (*sequences*)
  |Pair(Symbol("begin"),Nil) -> Const(Void)
  |Pair(Symbol("begin"),Pair(car_expr,Nil)) -> tag_parse_exp car_expr
  |Pair(Symbol("begin"),Pair(car_expr,cdr_expr)) ->Seq(List.map tag_parse_exp (list_of_sexpr (Pair(car_expr,cdr_expr))))


  (*applications*)
  |Pair(car_expr,cdr_expr) -> Applic(tag_parse_exp car_expr, List.map tag_parse_exp (list_of_sexpr cdr_expr))

  (*TODO:   |_ -> raise X_syntax_error*)
  | _ -> raise X_first_problem

  and veriables_not_reserved_word e = 
        let tester = List.mem e reserved_word_list in
          if tester
            then raise X_syntax_error 
            else Var(e)

  and list_of_sexpr s = match s with
    | Nil -> []
    | Pair(car,cdr) -> List.append [car] (list_of_sexpr cdr)
    | _ -> raise X_syntax_error

  and handle_lambda lambda_expr = 
   match lambda_expr with
    |Pair(Pair(car,cdr),body) -> 
      if (is_proper_list (Pair(car,cdr)))
      then let c_args = List.map string_of_symbol (list_of_sexpr (Pair(car,cdr))) in
          if (dupplicate_tester c_args) 
          then raise X_syntax_error
          else LambdaSimple(c_args,(tag_parse_seq body))
      else  
      let without_last = get_without_last (Pair(car,cdr)) in
      let last = get_last (Pair(car,cdr)) in
      if (dupplicate_tester (List.append without_last [last]))
        then raise X_syntax_error
        else LambdaOpt(without_last,last,(tag_parse_seq body))
      |_ -> raise X_syntax_error 

   
  and tag_parse_seq body = match body with
    |Pair(car,Nil) -> tag_parse_exp car
    |Pair(car,cdr) -> Seq(List.map tag_parse_exp (list_of_sexpr body))
    |_ -> raise X_syntax_error

  and is_proper_list list = match list with
    |Nil -> true
    |Pair(car,cdr) -> is_proper_list cdr
    |_ -> false 

  and string_of_symbol s = match s with
    |Symbol(e) -> e
    |_ -> raise X_syntax_error 

  and dupplicate_tester c_args = match c_args with
  |[] -> false
  |car :: cdr -> if (cdr_contains_car car cdr) 
    then true
    else dupplicate_tester cdr
  |_ -> raise X_syntax_error 

    
  and cdr_contains_car car cdr = match cdr with
  |[] -> false
  |x :: y -> 
    if x = car 
    then true 
    else cdr_contains_car car y
  |_ -> raise X_syntax_error 


  and get_last args = match args with
  |Pair(Symbol(x),Symbol(y)) -> y
  |Pair(Symbol(x), Pair(car,cdr)) -> get_last(Pair(car,cdr))
  |_ -> raise X_syntax_error

  and get_without_last args = match args with
  |Pair(Symbol(x),Symbol(y)) -> [x]
  |Pair(Symbol(x), Pair(car,cdr)) -> List.append [x] (get_without_last (Pair(car,cdr)))
  |_ -> raise X_syntax_error

  and get_args parms = match parms with
  |Nil -> []
  |Pair(Pair(var,_),rest) -> List.append [var] (get_args rest)
  |_ -> raise X_syntax_error 

  and get_vals parms = match parms with
  |Nil -> []
  |Pair(Pair(_,Pair(vals,Nil)),rest) -> List.append [vals] (get_vals rest)
  |_ -> raise X_syntax_error 

  ;;


let tag_parse_expressions sexpr = List.map tag_parse_exp sexpr;;

  
end;; (* struct Tag_Parser *)

