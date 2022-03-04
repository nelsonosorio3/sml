(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw2.sml";

val test1a = all_except_option ("string", ["string"]) = SOME []
val test1b = all_except_option ("string", ["string", "other"]) = SOME ["other"]
val test1c = all_except_option ("string", ["test","string", "other"]) = SOME ["test", "other"]
val test1d = all_except_option ("strdsafing", ["string"]) = NONE
val test1e = all_except_option ("str", ["stafdafdaing"]) = NONE
val test1f = all_except_option ("str", []) = NONE
val test1g = all_except_option ("string", ["test","straing", "other"]) = NONE


val test2a = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test2b = get_substitutions1 ([["foo", "fo", "f", "oo"],["there"]], "foo") = ["fo", "f", "oo"]
val test2c = get_substitutions1 ([["foo", "oo"],["there", "th"], ["f", "foo", "fo"]], "foo") = ["oo", "f", "fo"]


val test3a = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test3b = get_substitutions2 ([["foo", "fo", "f", "oo"],["there"]], "foo") = ["fo", "f", "oo"]
val test3c = get_substitutions2 ([["foo", "oo"],["there", "th"], ["f", "foo", "fo"]], "foo") = ["oo", "f", "fo"]

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5a = card_color (Clubs, Num 2) = Black
val test5b = card_color (Clubs, Num 5) = Black
val test5c = card_color (Spades, Num 2) = Black
val test5d = card_color (Diamonds, Ace) = Red
val test5e = card_color (Hearts, King) = Red

val test6a = card_value (Clubs, Num 2) = 2
val test6b = card_value (Spades, Num 7) = 7
val test6c = card_value (Diamonds, Ace) = 11
val test6d = card_value (Hearts, King) = 10
val test6e = card_value (Clubs, Queen) = 10
val test6f = card_value (Clubs, Jack) = 10


val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test8a = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test8b = all_same_color [(Hearts, Ace), (Hearts, Num 2)] = true
val test8c = all_same_color [(Diamonds, Ace), (Hearts, Num 2)] = true
val test8d = all_same_color [(Hearts, Ace), (Spades, Ace)] = false
val test8f = all_same_color [(Spades, Ace), (Clubs, Num 2)] = true
val test8g = all_same_color [(Diamonds, Ace), (Hearts, Num 2), (Clubs, Jack)] = false
val test8h = all_same_color [(Hearts, Ace)] = true


val test9a = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test9b = sum_cards [(Clubs, Num 2)] = 2
val test9c = sum_cards [(Clubs, Num 5),(Hearts, Num 7)] = 12
val test9d = sum_cards [(Diamonds, Jack),(Clubs, Num 3), (Spades, Ace), (Hearts, King), (Clubs, Queen)] = 44
val test9e = sum_cards [] = 0

val test10a = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test10b = score ([(Hearts, Num 2),(Diamonds, Num 4)],11) = 3
val test10c = score ([(Spades, Num 2),(Hearts, Num 4)],3) = 9
val test10d = score ([(Diamonds, Ace),(Hearts, Num 4)],30) = 0

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)

             
