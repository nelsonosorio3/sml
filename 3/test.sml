(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw3.sml";

val test1a = only_capitals ["A","B","C"] = ["A","B","C"]
val test1b = only_capitals ["Aasfdds","B","Cda"] = ["Aasfdds","B","Cda"]
val test1c = only_capitals ["jasfdds","B","Cda"] = ["B","Cda"]

val test2a = longest_string1 ["A","bc","C"] = "bc"
val test2b = longest_string1 ["A","bc","Casdds"] = "Casdds"
val test2c = longest_string1 ["Ac","bc","C"] = "Ac"
val test2d = longest_string1 [""] = ""

val test3a = longest_string2 ["A","bc","C"] = "bc"
val test3b = longest_string2 ["A","bc","Casdds"] = "Casdds"
val test3c = longest_string2 ["Ac","bc","C"] = "bc"
val test3d = longest_string2 [""] = ""
val test3e = longest_string2 ["A","b","C"] = "C"

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c1 = count_some_var ("x", Variable("x")) = 1
val test9c2 = count_some_var ("wild",ConstructorP ("wild",Wildcard)) = 0
val test9c3 = count_some_var ("x",TupleP[Wildcard,ConstP 17,Variable "x",UnitP,TupleP[UnitP,UnitP,UnitP],ConstructorP ("",UnitP),TupleP[],ConstructorP ("wild",Wildcard),TupleP[Wildcard,ConstP 17,Variable "x",UnitP,TupleP[UnitP,UnitP,UnitP],ConstructorP ("",UnitP),TupleP[],ConstructorP ("wild",Wildcard)]]) = 2
val test9c4 = count_some_var ("y",TupleP[Wildcard,ConstP 17,Variable "x",UnitP,TupleP[UnitP,UnitP,UnitP],ConstructorP ("",UnitP),TupleP[],ConstructorP ("wild",Wildcard),TupleP[Wildcard,ConstP 17,Variable "x",UnitP,TupleP[UnitP,UnitP,UnitP],ConstructorP ("",UnitP),TupleP[],ConstructorP ("wild",Wildcard)]]) = 0
val test9c5 = count_some_var ("x",TupleP[TupleP[TupleP[Variable "x",ConstructorP ("wild",Wildcard)],Wildcard],Variable "x"]) = 2
val test9c6 = count_some_var ("y",TupleP[TupleP[TupleP[Variable "x",ConstructorP ("wild",Wildcard)],Wildcard],Variable "x"]) = 0
val test9c7 = count_some_var ("wild",TupleP[TupleP[TupleP[Variable "x",ConstructorP ("wild",Wildcard)],Wildcard],Variable "x"]) = 0

val test10a = check_pat (Variable("x")) = true
val test10b = check_pat (TupleP[Variable "x",Variable "x"]) = false
val test10c = check_pat (TupleP[TupleP[Variable "x",ConstructorP ("wild",Wildcard)],Variable "x"]) = false
val test10d = check_pat (TupleP[TupleP[TupleP[Variable "x",ConstructorP ("wild",Wildcard)],Wildcard],Variable "x"]) = false
val test10e = check_pat (ConstructorP ("hi",TupleP[Variable "x",Variable "x"])) = false
val test10f = check_pat (ConstructorP ("hi",TupleP[Variable "x",ConstructorP ("yo",TupleP[Variable "x",UnitP])])) = false

val test11a = match (Const(1), UnitP) = NONE
val test11b = match (Const 17,ConstP 4) = NONE
val test11c = match (Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4))],TupleP[Wildcard,Wildcard]) = NONE

val test12 = first_match Unit [UnitP] = SOME []

