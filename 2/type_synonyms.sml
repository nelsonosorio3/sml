datatype suit = Club | Diamond | Heart | Spade;

datatype rank = Jack | Queen | King | Ace | Num of int;

type card = suit * rank;

type name_record = { student_num : int option,
		     first : string,
		     middle : string options,
		     last : string };

fun is_queen_of_spades (c : card) =
    #1 c = Spade andalso #2 c = Queen;

val c1 : card = (Diamond, Ace);
val c2 : suit * rank = (Heart, Ace);
val c3 : (Space, Ace);

