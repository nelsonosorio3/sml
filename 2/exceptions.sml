fun hd xs =
    case xs of
	[] => raise List.Empty
      | x :: _ => x;

exception MyUndesirableCondition;

exception MyOtherException of int * int;

fun mydiv (x, y) =
    if y = 0
    then raise MyUndesirableCondition
    else x div y;

fun maxlist (xs, ex) = (* int list * exn -> int *)
    case xs of
	[] => raise ex
      | x :: []  => Int.max (x, maxList(xs', ex));

val w = maxlist ([3,4,5], MyUndesirableCondition);

val x = maxlist ([3,4,5], MyUndesirableCondition)
	handle MyUndesirableCondition => 42;

val z = maxlist ([], MyUndesirableContion)
	handle MyUndesirableCondition => 42;

						  
