(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2;

(* put your solutions for problem 1 here *)

fun all_except_option (word, word_list) =
    let
	  fun all_except list =
	      case list of
		  [] => []
		| head :: tail => if same_string(word, head)
				  then all_except(tail)
				  else head :: all_except(tail)
	  val list = all_except(word_list)
      in
	  if word_list = list
	  then NONE
	  else SOME (list)
    end;


fun get_substitutions1 (word_list_list, word) =
    
    case word_list_list of
	[] => []
     | word_list :: tail => let val list = all_except_option (word, word_list)
			    in
				case list of
				    NONE => get_substitutions1 (tail, word)
				  | SOME words => words @ get_substitutions1 (tail, word)
			    end;

fun get_substitutions2 (word_list_list, word) =
    let fun aux (word_list_list, acc) =
	    case word_list_list of
		[] => acc
	      | word_list :: tail => let val list = all_except_option (word, word_list)
				      in
					  case list of
					      NONE => aux(tail, acc)
					    | SOME words => words @ aux(tail, acc)
				      end;
    in
	aux (word_list_list, [])
    end;

fun similar_names (word_list_list, full_name) =
    let val {first = first, middle = middle, last = last} = full_name
	val nicknames = first :: get_substitutions1 (word_list_list, first)
	fun name (nickname, full_name) =
	    {first = nickname, middle = middle, last = last}
	fun names (nicknames) =
	    case nicknames of
		[] => []
	      | nickname :: tail => name (nickname, full_name) :: names(tail)
								       
    in
	names (nicknames)
    end;
						   
    
					 	     
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades;
datatype rank = Jack | Queen | King | Ace | Num of int ;
type card = suit * rank;

datatype color = Red | Black;
datatype move = Discard of card | Draw ;

exception IllegalMove;

(* put your solutions for problem 2 here *)

fun card_color card =
    case card of
	(Clubs,_) => Black
      | (Spades,_) => Black
      | _ => Red;

fun card_value card =
    case card of
	(_, Num x) => x
      | (_, rank) => if rank = Ace
		     then 11
		     else 10;

fun remove_card (cs, c, e) =
    case cs of
	[] => []
      | hd :: [] => if hd = c
		    then []
		    else raise e
      | hd :: tail => if hd = c						  
		      then tail				   
		      else hd :: remove_card (tail, c ,e);

fun all_same_color ([]) = true
  | all_same_color (c::[]) = true
  | all_same_color (c :: c' :: []) = card_color c = card_color c'
  | all_same_color (c :: tail) = all_same_color(tail);

fun sum_cards cards =
    let fun aux(cards, acc) =
	    case cards of
		[] => acc
	      | card :: cards' => card_value (card) + aux(cards', acc)
    in
	aux(cards, 0)
    end;

fun score (cards, goal) =
    let val sum = sum_cards cards
	val score = if sum > goal then 3 * (sum - goal) else goal - sum
    in
	if all_same_color (cards)
	then  score div 2
	else score
    end;

fun officiate (cards_list, moves, goal) =
    let
	fun state (hand, moves, cards_list) =
	    if sum_cards (hand) > goal
	    then score (hand, goal)
	    else
		case (hand, moves, cards_list) of
		    (_,[], _) => score (hand, goal)
		  | (_, _, []) => score (hand, goal)
		  | (_, Draw :: tail, card :: cards) => state (card :: hand, tail, cards)
		  | (_, Discard card :: tail, _) => state (remove_card (hand, card, IllegalMove), tail, cards_list)
    in
	state ([], moves, cards_list)
    end;

fun score_challenge (cards, goal) =
    let
	fun scores (hand, acc, goal) =
	    let val goal' = goal + 10
	    in
		
	    case hand of
		[] => [acc]
	      | card :: tail => if card_value (card) = 11
				then [acc] @ scores (tail, score(cards, goal'), goal')
				else scores (tail, acc, goal)
	    end;
	fun min (s, m) =
	    case s of
		[] => m
		   | head :: tail => if m > head
				     then min (tail, head)
				     else min (tail, m);
		
    in
	min (scores (cards, score(cards, goal), goal), 9999)
    end;

fun officiate_challenge (cards_list, moves, goal) =
    let
	fun aces (hand) =
	    case hand of
		[] => 0
	      | card :: tail => if card_value (card) = 11
				then 1 + aces (tail)
				else aces (tail)
					  
	fun state (hand, moves, cards_list) =
	    
	    if sum_cards (hand) - (aces (hand) * 10) > goal
	    then score (hand, goal)
	    else
		case (hand, moves, cards_list) of
		    (_, [], _) => score_challenge (hand, goal)
		  | (_, _, []) => score_challenge (hand, goal)
		  | (_, Draw :: tail, card :: cards) => state (card :: hand, tail, cards)
		  | (_, Discard card :: tail, _) => state (remove_card (hand, card, IllegalMove), tail, cards_list)
    in
	state ([], moves, cards_list)
    end;


	
	    

			       
	

					   

