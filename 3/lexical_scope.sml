(* 1 *) val x = 1;
   (* x maps to 1 *)
(* 2 *) fun f y = x + y;
   (* f maps to a function that adds 1 to its argument *)
(* 3 *) val x = 2;
   (* x maps to 2 *)
(* 4 *) val y = 3;
   (* y maps to 3 *)
(* 5 *) val z = f (x + y); (* call the function defined on line 2 with 5 *)
   (* z maps to 6 *)


val x = 1; (* irrelevant *)

fun f y =
    let
	val x = y + 1
    in
	fn z => x + y + z (* take z return 2y +1 + z *)
    end;

val x = 3; (* irrelevant *)

val g = f 4; (* return a function that adds 9 to its argument *)

val y = 5; (* irrelevant *)

val z = g 6; (* get 15 *)

fun f g =
    let
	val x = 3 (* irrelevant *)
    in
	g 2
    end;

val x = 4;

fun h y = x + y;  (* add 4 to its argument *)

val z = f h; (* 6 *)


fun filter (f, xs) =
    case xs of
	[] => []

      | x :: xs' =>  if f x then x :: (filter(f, xs')) else filter (f, xs');

fun greaterThan x = fn y => y > x;

fun noNegatives xs = filter (greaterThanX ~1, xs);

fun allGreater (xs, n) = filter (fn x => x > n, xs);
