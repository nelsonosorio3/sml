(* old way *)
fun sorted3_tupled (x, y, z) = z >= y andalso y >= x;

val t1 = sorted3_tupled (7, 9, 11);

(* new way: curryng *)

val sorted3 = fn x => fn y => fn z = z >= andalso y >=x;

val t2 = ((sorted3 7) 9) 11;

val t3 = sorted 7 9 11;

fun sorted3_nicer x y z = z >= y andalso y >= x;

val t4 = sorted3_nicer 7 9 11;
val t5 = ((sorted3_nicer 7) 9) 11;

fun fold f acc xs =
    case xs of
	[] => acc
      | x :: xs' => fold f (f(acc, x)) xs';

fun sum xs = fold (fn (x, y) => x + y) 0 xs;

val is_nonnegative = sorted3 0 0;

val sum = fold (fn (x, y) => x + y) 0;


fun range i j = if i > j then [] else i :: range (i + 1) j;

val count up  = range 1;

fun exists predicate xs =
    case xs of
	[] => false
      | x :: xs' => predicate x orelse exists preidcate xs';

val no = exists (fn x => x = 7) [4, 11, 23];

val hasZero = exists (fn x => x = 0);

val incrementAll = List.map (fn x => x + 1);

val removeZeros = List.filter (fn x => x <> 0);

fun pairWithOne xs = List.map (fn x => (x, 1)) xs;

val pairWithOne : string list -> (string * int) list = List.map (f n x => (x, 1));


fun curry f = fn x => fn y => f (x, y);

fun curry f x y = f (x, y);

fun uncurry f (x, y) = f x y;

fun other_curry f = fn x => fn y => f y x;

fun other_curry f x y = f y x;


fun range (i, j) = if i > j then [] else i :: range (i + 1, j);

val countup = (curry range) 1;

val xs = countup 7;
