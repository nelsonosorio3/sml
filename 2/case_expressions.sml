datatype mytype = TwoInts of int * int
		| Str of string
		| Pizza
(* mytype -> int *)
		      
fun f (x : mytype)  = (* f has type mytpe -> int *)
    case x of
	Pizza => 3
      | TwoInts (i1, i2)  => i1 + i2
      | Str s  => String.size s;

(* | Pizza => 4; (* redundant case: error *) *)
(* fun g x = case x of Pizza => 3 (* missing cases: warning *) *)
