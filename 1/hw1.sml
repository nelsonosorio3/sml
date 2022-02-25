(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "homework1.sml";

val test1a = is_older ((1,2,3),(2,3,4)) = true
val test1b = is_older ((4,2,3),(2,3,4)) = false
val test1c = is_older ((1,2,3),(1,3,4)) = true
val test1d = is_older ((1,3,3),(1,2,4)) = false
val test1e = is_older ((1,2,3),(1,2,4)) = true
val test1f = is_older ((1,2,4),(1,2,3)) = false
					     

val test2a = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
val test2b = number_in_month ([(2012,2,28),(2013,2,1)],2) = 2
val test2c = number_in_month ([(2012,12,28),(2013,12,1)],2) = 0
val test2d = number_in_month ([(2012,2,28),(2013,12,1),,(2013,2,1)],2) = 2
val test2e = number_in_month ([(2012,2,28)],2) = 1
val test2f = number_in_month ([(2012,12,28)],2) = 0
val test2g = number_in_month ([],2) = 0

val test3a = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test3b = number_in_months ([(2012,2,28),(2013,4,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 4
val test3c = number_in_months ([(2012,12,28),(2013,12,1),(2011,13,31),(2011,14,28)],[2,3,4]) = 0
val test3d = number_in_months ([(2012,2,28)],[2,3,4]) = 1
val test3e = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test3f = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test3g = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

val test9 = what_month 70 = 3

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
