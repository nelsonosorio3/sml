(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer;

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern;

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu;

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end;

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string;

(**** you can put all your code here ****)
		
fun only_capitals list =
    List.filter (fn word => Char.isUpper (String.sub (word, 0))) list;

val longest_string1 =
    List.foldl (fn (word, longest) => if String.size word > String.size longest then word else longest) "";

val longest_string2 =
    List.foldl (fn (word, longest) => if String.size word >= String.size longest then word else longest) "";

fun longest_string_helper f =
    List.foldl (fn (word, longest) => if f(String.size word, String.size longest) then word else longest) "";

val longest_string3 = longest_string_helper (fn (x, y) => x > y);

val longest_string4 = longest_string_helper (fn (x, y) => x >= y);

val longest_capitalized = longest_string1 o only_capitals;
    
val rev_string = fn str =>
    String.implode (List.rev (String.explode str));



fun first_answer f  list =
    case list of
	[] => raise NoAnswer
      | head :: tail => case f (head) of
			    NONE => first_answer f tail
			  | SOME v => v;

fun all_answers f list =
    let fun helper (list, acc) =
	case list of
	    [] => SOME acc
	  | head :: tail => case f (head) of
				NONE => NONE
			      | SOME v => helper(tail, v @ acc)
    in
	helper (list, [])
    end;


val count_wildcards = g (fn () => 1) (fn (x)=> 0);

val count_wild_and_variable_lengths = g (fn () => 1) (fn (x) => String.size x);

fun count_some_var (str, pattern) =
    g (fn () => 0) (fn (x) => if str = x then 1 else 0) pattern

				
fun check_pat pattern =
    let fun helper1 p =
	    case p of
		TupleP ps => List.foldl (fn (i, acc) => (case i of
							     Variable s => [s] @ acc 
							  | _ => acc )) [] ps
	      | ConstructorP (s, p) => helper1 p
	      | _ => []
			 
	fun helper2 (l, acc) =
	    case l of
		[] => acc
	      | head :: tail => helper2 (tail, List.exists (fn x => head = x) tail andalso acc)
    in
	helper2 ((helper1 pattern), true)
    end;
	
		       
fun match (valu, pattern) =
    case pattern of
	Wildcard => SOME []
     |  Variable str => (case valu of
			    Constructor (s, v) => SOME [(s, v)]
			  | _ => NONE)
     |  UnitP => (case valu of
		      Unit => SOME []
			   | _ => NONE)
     | ConstP i => (case valu of
			Const j => if i = j
				   then SOME []
				   else NONE					    
				| _ => NONE)
     | TupleP ps => (case valu of
			 Tuple vs => SOME []
				    | _ => NONE)
     | ConstructorP (s1, p)=> (case valu of
				   Constructor (s2, v) => if s1 = s2
							  then match(v, p)
							  else NONE

				 | _ => NONE)

fun first_match valu list =
    SOME (first_answer (fn x => match (valu, x))  list)
    handle NoAnswer => NONE;

fun typecheck_patterns (list :(string * string * typ) list, plist : pattern list) =
    NONE;
    
