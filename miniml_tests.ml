open Evaluation ;;
open Expr ;;
open Miniml ;;
open CS51Utils ;;
open Absbook ;;

let tests () =
unit_test ((exp_to_concrete_string (Var("x"))) = "x")
        "exp_to_concrete_string var";
unit_test ((exp_to_concrete_string (Num(3))) = "3")
        "exp_to_concrete_string num";
unit_test ((exp_to_concrete_string (Bool(true))) = "true")
        "exp_to_concrete_string bool";
unit_test ((exp_to_concrete_string 
        (Unop(Negate, Num(4)))) = " ~- 4")
        "exp_to_concrete_string unop";
unit_test ((exp_to_concrete_string 
        (Binop(Times, Num(2), Num(5)))) = "2 * 5")
        "exp_to_concrete_string var";
unit_test ((exp_to_concrete_string 
        (Conditional(Bool(true), Num(1), Num(2)))) 
        = "if true then 1 else 2")
        "exp_to_concrete_string conditional";
unit_test ((exp_to_concrete_string 
        (Fun("x", Binop(Times, Var("x"), Num(2))))) 
        = "fun x -> x * 2")
        "exp_to_concrete_string fun";
unit_test ((exp_to_concrete_string 
        (Let("x", Num(3), Binop(Plus, Var("x"), Num 1)))) 
        = "let x = 3 in x + 1")
        "exp_to_concrete_string let";
unit_test ((exp_to_concrete_string 
        (Letrec("f", Fun("x", Num(2)), App(Var("f"), Num(3))))) 
        = "let rec f = fun x -> 2 in f 3")
        "exp_to_concrete_string letrec";
unit_test ((exp_to_concrete_string (Raise)) = "raise ")
        "exp_to_concrete_string raise";
unit_test ((exp_to_concrete_string (Unassigned)) = "unassigned")
        "exp_to_concrete_string unassigned";
unit_test ((exp_to_concrete_string (App(Var("f"), Num(3)))) = "f 3")
        "exp_to_concrete_string app";

unit_test ((exp_to_abstract_string (Var("x"))) = "Var(x)")
        "exp_to_abstract_string var";
unit_test ((exp_to_abstract_string (Num(3))) = "Num(3)")
        "exp_to_abstract_string num";
unit_test ((exp_to_abstract_string (Bool(true))) = "Bool(true)")
        "exp_to_abstract_string bool";
unit_test ((exp_to_abstract_string (Unop(Negate, Num(4)))) 
        = "Unop(Negate, Num(4))")
        "exp_to_abstract_string unop";
unit_test ((exp_to_abstract_string (Binop(Times, Num(2), Num(5)))) 
        = "Binop(Times, Num(2), Num(5))")
        "exp_to_abstract_string binop";
unit_test ((exp_to_abstract_string 
        (Conditional(Bool(true), Num(1), Num(2)))) 
        = "Conditional(Bool(true), Num(1), Num(2))")
        "exp_to_abstract_string conditional";
unit_test ((exp_to_abstract_string 
        (Fun("x", Binop(Times, Var("x"), Num(2))))) 
        = "Fun(x, Binop(Times, Var(x), Num(2)))")
        "exp_to_abstract_string fun";
unit_test ((exp_to_abstract_string 
        (Let("x", Num(3), Binop(Plus, Var("x"), Num 1)))) 
        = "Let(x, Num(3), Binop(Plus, Var(x), Num(1)))")
        "exp_to_abstract_string let";
unit_test ((exp_to_abstract_string 
        (Letrec("f", Fun("x", Num(2)), App(Var("f"), Num(3))))) 
        = "Letrec(f, Fun(x, Num(2)), App(Var(f), Num(3)))")
        "exp_to_abstract_string letrec";
unit_test ((exp_to_abstract_string (Raise)) = "Raise")
        "exp_to_abstract_string raise";
unit_test ((exp_to_abstract_string (Unassigned)) = "Unassigned")
        "exp_to_abstract_string unassigned";
unit_test ((exp_to_abstract_string (App(Var("f"), Num(3)))) 
        = "App(Var(f), Num(3))")
        "exp_to_abstract_string app";

unit_test (same_vars (free_vars (Var("x"))) (vars_of_list ["x"]) = true)
        "free_vars var";
unit_test (same_vars (free_vars (Num(3))) (vars_of_list []) = true)
        "free_vars num";
unit_test (same_vars (free_vars (Bool(true))) (vars_of_list []) = true)
        "free_vars bool";
unit_test (same_vars (free_vars (Unop(Negate, Var("y")))) (vars_of_list ["y"]) = true)
        "free_vars unop";
unit_test (same_vars (free_vars 
        (Binop(Times, Var("f"), Var("g")))) (vars_of_list ["f"; "g"]) = true)
        "free_vars binop";
unit_test (same_vars (free_vars 
        (Conditional(Binop(Equals, Var("x"), Num(2)), Num(3), Num(5)))) (vars_of_list ["x"]) = true)
        "free_vars conditional";
unit_test (same_vars (free_vars 
        (Fun("x", Var("x")))) (vars_of_list []) = true)
        "free_vars fun";
unit_test (same_vars (free_vars 
        (Let("x", Num(2), Binop(Times, Var("x"), Num(4))))) (vars_of_list []) = true)
        "free_vars let";
unit_test (same_vars (free_vars 
        (Letrec("x", Num(2), Binop(Times, Var("x"), Var("y"))))) (vars_of_list ["y"]) = true)
        "free_vars letrec";
unit_test (same_vars (free_vars 
        (App(Fun("x", Var("x")), Num(3)))) (vars_of_list []) = true)
        "free_vars app no free";
unit_test (same_vars (free_vars 
        (App(Fun("x", Var("x")), Var("b")))) (vars_of_list ["b"]) = true)
        "free_vars app yes free";

unit_test ((subst ("x") (Num(2)) (Binop(Times, Var("x"), Num(3)))) 
        = Binop(Times, Num(2), Num(3)))
        "subst into binop";
unit_test ((subst ("x") (Num(2)) (Unop(Negate, Var("x")))) 
        = Unop(Negate, Num(2)))
        "subst into unop";
unit_test ((subst ("x") (Num(2)) (Let("y", Var("x"), Var("y")))) 
        = Let("y", Num(2), Var("y")))
        "subst into let statement";
unit_test ((subst ("x") (Num(2)) (Letrec("f", Fun("x", App(Var("f"), Var("x"))), App(Var("f"),Var("x"))))) 
        = Letrec ("f", Fun ("x", App (Var "f", Var "x")), App (Var "f", Num 2)))
        "subst into letrec statement that has a free and bound instance of x";
unit_test ((subst ("x") (Num(2)) (Num(4))) 
        = Num(4))
        "subst into num expression";
unit_test ((subst ("x") (Num(2)) (Var("x"))) 
        = Num(2))
        "subst into let statement";
unit_test ((subst ("x") (Num(2)) (Let("x", (Let("x", Num(3), Var("x"))), Var("x")))) 
        = Let("x", (Let("x", Num(3), Var("x"))), Var("x")))
        "subst into nested let statement";

unit_test (eval_s (Num(3)) (Env.empty())
        = Env.Val(Num(3)))
        "eval_s number";
unit_test (eval_s (Bool(false)) (Env.empty())
        = Env.Val(Bool(false)))
        "eval_s bool";
unit_test (eval_s (Binop(Times, Num(2), Num(3))) (Env.empty())
        = Env.Val(Num(6)))
        "eval_s binop";
unit_test (eval_s (Let("x", Num(1), Binop(LessThan, Var("x"), Num(2)))) (Env.empty())
        = Env.Val(Bool(true)))
        "eval_s let and binop and conditional";
unit_test (eval_s (Let("x", Num(1), Let("f", Fun("y", Binop(Plus, Var("x"), 
        Var("y"))), Let("x", Num(2), App(Var("f"), Num(3)))))) (Env.empty())
        = Env.Val(Num(4)))
        "eval_s function evaluation at definition or application";
unit_test (eval_s (Letrec("f", Fun("x", Conditional(Binop(Equals, Var("x"), Num(0)), 
        Var("x"), App(Var("f"), Binop(Minus, Var("x"), Num(1))))), App(Var("f"), 
        Num(2)))) (Env.empty())
        = Env.Val(Num(0)))
        "eval_s recursive case";

unit_test (Env.close (Fun("x", Binop(Plus,Var("x"),Num(3)))) (Env.empty()) 
        = Closure(Fun("x", Binop(Plus,Var("x"),Num(3))), Env.empty()))
        "close function";

unit_test (Env.env_to_string (Env.extend (Env.empty()) "x" (ref (Env.Val(Num(3)))))
        = "{x: 3}")
        "extend a variable into an env";
unit_test (Env.env_to_string (Env.extend (Env.empty()) "f" (ref (Env.Val(Fun("x", Binop(Times,Var("x"), Num(3)))))))
        = "{f: fun x -> x * 3}")
        "extend a function into an env";

unit_test (Env.lookup (Env.extend (Env.empty()) "x" (ref (Env.Val(Num(100))))) "x"
        = Env.Val(Num(100)))
        "lookup numerical value";
unit_test (Env.lookup (Env.extend (Env.empty()) "f" (ref (Env.Val(Fun("x", Binop(Times,Var("x"), Num(3))))))) "f"
        = Env.Val(Fun("x", Binop(Times,Var("x"), Num(3)))))
        "lookup function definition";

unit_test (eval_d (Num(3)) (Env.empty())
        = Env.Val(Num(3)))
        "eval_d number";
unit_test (eval_d (Bool(false)) (Env.empty())
        = Env.Val(Bool(false)))
        "eval_d bool";
unit_test (eval_d (Binop(Times, Num(2), Num(3))) (Env.empty())
        = Env.Val(Num(6)))
        "eval_d binop";
unit_test (eval_d (Let("x", Num(1), Binop(LessThan, Var("x"), Num(2)))) (Env.empty())
        = Env.Val(Bool(true)))
        "eval_d let and binop and conditional";
unit_test (eval_d (Let("x", Num(1), Let("f", Fun("y", Binop(Plus, Var("x"), 
        Var("y"))), Let("x", Num(2), App(Var("f"), Num(3)))))) (Env.empty())
        = Env.Val(Num(5)))
        "eval_d function evaluation at definition or application";
unit_test (eval_d (Letrec("f", Fun("x", Conditional(Binop(Equals, Var("x"), Num(0)), 
        Var("x"), App(Var("f"), Binop(Minus, Var("x"), Num(1))))), App(Var("f"), 
        Num(2)))) (Env.empty())
        = Env.Val(Num(0)))
        "eval_d recursive case";

unit_test (eval_l (Num(3)) (Env.empty())
        = Env.Val(Num(3)))
        "eval_l number";
unit_test (eval_l (Bool(false)) (Env.empty())
        = Env.Val(Bool(false)))
        "eval_l bool";
unit_test (eval_l (Binop(Times, Num(2), Num(3))) (Env.empty())
        = Env.Val(Num(6)))
        "eval_l binop";
unit_test (eval_l (Let("x", Num(1), Binop(LessThan, Var("x"), Num(2)))) (Env.empty())
        = Env.Val(Bool(true)))
        "eval_l let and binop and conditional";
unit_test (eval_l (Let("x", Num(1), Let("f", Fun("y", Binop(Plus, Var("x"), 
        Var("y"))), Let("x", Num(2), App(Var("f"), Num(3)))))) (Env.empty())
        = Env.Val(Num(4)))
        "eval_l function evaluation at definition or application";
unit_test (eval_l (Letrec("f", Fun("x", Conditional(Binop(Equals, Var("x"), Num(0)), 
        Var("x"), App(Var("f"), Binop(Minus, Var("x"), Num(1))))), App(Var("f"), 
        Num(2)))) (Env.empty())
        = Env.Val(Num(0)))
        "eval_l recursive case";
;;

let _ = tests ();;