(* enumerations, including carrying other data *)
datatype suit = Club | Diamond | Heart | Spade;
datatype rank = Jack | Queen | King | Ace | Num of int;

(* way of identifying real-world things *)
datatype id = StudentNum of int
	    | Name of string
		      * (string option)
		      * string;
(* expression trees *)
datatype exp = Constant of int
	     | Negate of exp
	     | Add of exp * exp
	     | Multiply of exp * exp;

Add (Constant (10 + 9), Negate (Constant 4));

fun eval e =
    case e of
	Constant i => i
      | Negate e2  => ~ (eval e2)
      | Add (e1, e2)  => (eval e1) + (eval e2)
      | Multiply (e1, e2)  => (eval e1) * (eval e2);

fun number_of_adds e = (* exp  -> int *)
    case e of
	Constant 1 => 0
      | Negate e2  => number_of_adds e2
      | Add (e1, e2)  => 1 + number_of_adds e1 + number_of_adds e2
      | Multiply (e1, e2)  => number_of_adds e1 + number_of_adds e2;

val example_exp = Add (Constant 19, Negate (Constant 4));

val example_ans = eval example_exp;

val example_addcount = number_of_adds (Multiply(example_exp, example_exp));
