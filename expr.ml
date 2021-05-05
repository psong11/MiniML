(* 
                         CS 51 Final Project
                        MiniML -- Expressions
*)

(*......................................................................
  Abstract syntax of MiniML expressions 
 *)

open CS51Utils ;;
open Absbook ;;

type unop =
  | Negate
;;
    
type binop =
  | Plus
  | Minus
  | Times
  | Equals
  | LessThan
;;

type varid = string ;;
  
type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of unop * expr                  (* unary operators *)
  | Binop of binop * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
;;
  
(*......................................................................
  Manipulation of variable names (varids) and sets of them
 *)

(* varidset -- Sets of varids *)
module SS = Set.Make (struct
                       type t = varid
                       let compare = String.compare
                     end ) ;;

type varidset = SS.t ;;

(* same_vars varids1 varids2 -- Tests to see if two `varid` sets have
   the same elements (for testing purposes) *)
let same_vars : varidset -> varidset -> bool =
  SS.equal;;

(* vars_of_list varids -- Generates a set of variable names from a
   list of `varid`s (for testing purposes) *)
let vars_of_list : string list -> varidset =
  SS.of_list ;;
  
(* free_vars exp -- Returns the set of `varid`s corresponding to free
   variables in `exp` *)

let free_vars (exp : expr) : varidset =
  let rec find_free_vars (expression : expr) : SS.t =
    match expression with
    | Var x -> SS.singleton x
    | Num _ -> SS.empty
    | Bool _ -> SS.empty
    | Unop (_, expr) -> find_free_vars expr
    | Binop (_, expr1, expr2) ->
      SS.union (find_free_vars expr1) (find_free_vars expr2)
    | Conditional (condition, expr1, expr2) -> SS.union (find_free_vars condition) (SS.union (find_free_vars expr1) (find_free_vars expr2))
    | Fun (varid, expr) -> SS.remove varid (find_free_vars expr)
    | Let (varid, expr1, expr2) -> 
      SS.union (find_free_vars expr1) (SS.remove varid (find_free_vars expr2))
    | Letrec (varid, expr1, expr2) -> 
      SS.remove varid (SS.union (find_free_vars expr1) (find_free_vars expr2))
    | App (expr1, expr2) -> SS.union (find_free_vars expr1) (find_free_vars expr2) 
    | Raise | Unassigned -> SS.empty in            
  find_free_vars exp
   ;;

  
(* new_varname () -- Returns a freshly minted `varid` constructed with
   a running counter a la `gensym`. Assumes no variable names use the
   prefix "var". (Otherwise, they might accidentally be the same as a
   generated variable name.) *)
let new_varname : unit -> varid =
  let suffix = ref 0 in
    fun () -> let symbol = "var" ^ string_of_int !suffix in
              suffix := !suffix + 1;
              symbol ;;

(*......................................................................
  Substitution 

  Substitution of expressions for free occurrences of variables is the
  cornerstone of the substitution model for functional programming
  semantics.
 *)

(* subst var_name repl exp -- Return the expression `exp` with `repl`
   substituted for free occurrences of `var_name`, avoiding variable
   capture *)
let rec subst (var_name : varid) (repl : expr) (exp : expr) : expr =
  (* perform this particular substitution of variable and replacement *)
  let rec sub_this (expression : expr) : expr =
    match expression with
    | Var x -> if x = var_name then repl else expression
    | Num _ -> expression
    | Unop (op, expr) -> Unop(op, sub_this expr)
    | Binop (op, expr1, expr2) -> Binop(op, sub_this expr1, sub_this expr2)
    | Conditional (condition, expr1, expr2) -> 
       Conditional (sub_this condition, sub_this expr1, sub_this expr2)
    | Fun (x, expr) -> 
      if x = var_name then Fun (x, expr)
      else 
        if SS.mem x (free_vars repl) 
        then let z = new_varname () in 
            Fun (z, sub_this((subst x (Var(z)) expr)))
        else Fun (x, sub_this expr)
    | Let (x, def, body) ->
      if x = var_name then Let(x, sub_this def, body)
      else 
        if SS.mem x (free_vars repl)
        then let z = new_varname () in Let (z, sub_this def, sub_this(subst x (Var(z)) body))
        else Let(x, sub_this def, sub_this body) 
    | Letrec (x, def, body) ->
      if x = var_name then expression
      else 
        if SS.mem x (free_vars repl)
        then let z = new_varname () in Letrec (z, sub_this def, sub_this(subst x (Var(z)) body))
        else Letrec(x, sub_this def, sub_this body) 
    | App (funexpr, expr) -> App(sub_this funexpr, sub_this expr)
    | _ -> exp in
  sub_this exp ;;
     
(*......................................................................
  String representations of expressions
 *)
   
(* exp_to_concrete_string exp -- Returns a string representation of
   the concrete syntax of the expression `exp` *)
let rec exp_to_concrete_string (exp : expr) : string =
  match exp with
  | Var x -> x
  | Num i -> string_of_int i
  | Bool b -> string_of_bool b
  | Unop (unop, expr) -> 
    let helperunop : string =
      match unop with
      | Negate -> " ~- " in
      helperunop ^ exp_to_concrete_string expr
  | Binop (binop, expr1, expr2) -> 
      let helperbinopp : string = 
        match binop with
        | Plus     -> " + "
        | Minus    -> " - "
        | Times    -> " * "
        | Equals   -> " = "
        | LessThan -> " < " in
      exp_to_concrete_string expr1 ^ helperbinopp ^ exp_to_concrete_string expr2
  | Conditional (condition, expr1, expr2) -> "if " ^ exp_to_concrete_string condition ^ 
                                             " then " ^ exp_to_concrete_string expr1 ^ 
                                             " else " ^ exp_to_concrete_string expr2
  | Fun (x, expr) -> "fun " ^ x ^ " -> " ^ exp_to_concrete_string expr
  | Let (x, def, body) -> "let " ^ x ^ " = " ^ exp_to_concrete_string def ^ " in " ^ exp_to_concrete_string body
  | Letrec (x, def, body) -> "let rec " ^ x ^ " = " ^ exp_to_concrete_string def ^ " in " ^ exp_to_concrete_string body
  | Raise -> "raise "
  | Unassigned -> "unassigned"
  | App (funexpr, expr) -> exp_to_concrete_string funexpr ^ " " ^ exp_to_concrete_string expr ;;
     
(* exp_to_abstract_string exp -- Return a string representation of the
   abstract syntax of the expression `exp` *)
let rec exp_to_abstract_string (exp : expr) : string =
  match exp with
  | Var x -> "Var(" ^ x ^ ")"
  | Num i -> "Num(" ^ string_of_int i ^ ")"
  | Bool b -> "Bool(" ^ string_of_bool b ^ ")"
  | Unop (unop, expr) -> 
    let helperunop : string =
      match unop with
      | Negate -> "Negate, " in
      "Unop(" ^ helperunop ^ exp_to_abstract_string expr ^ ")"
  | Binop (binop, expr1, expr2) -> 
      let helperbinopp : string = 
        match binop with
        | Plus     -> "Plus, "
        | Minus    -> "Minus, "
        | Times    -> "Times, "
        | Equals   -> "Equals, "
        | LessThan -> "LessThan, " in
      "Binop(" ^ helperbinopp ^ exp_to_abstract_string expr1 ^ ", " ^ exp_to_abstract_string expr2 ^ ")"
  | Conditional (condition, expr1, expr2) -> "Conditional(" ^ exp_to_abstract_string condition ^ 
                                             ", " ^ exp_to_abstract_string expr1 ^ 
                                             ", " ^ exp_to_abstract_string expr2 ^ ")"
  | Fun (x, expr) -> "Fun(" ^ x ^ ", " ^ exp_to_abstract_string expr ^ ")"
  | Let (x, def, body) -> "Let(" ^ x ^ ", " ^ exp_to_abstract_string def ^ ", " ^ exp_to_abstract_string body ^ ")"
  | Letrec (x, def, body) -> "Letrec(" ^ x ^ ", " ^ exp_to_abstract_string def ^ ", " ^ exp_to_abstract_string body ^ ")"
  | Raise -> "Raise"
  | Unassigned -> "Unassigned"
  | App (funexpr, expr) -> "App(" ^ exp_to_abstract_string funexpr ^ ", " ^ exp_to_abstract_string expr ^ ")"
  
   ;;
