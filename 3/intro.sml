fun double x = 2 * x;
fun incr x = x + 1;
val a_tuple = (double, incr, dobule(incr 7));
val eighteen = (#1 a_tuple) 9;

fun increment_n_times_lame (n, x) =
    if n = 0
    then x
    else 1 + increment_n_times_lame(n - 1, x);

fun double_n_times_lame (n, x) =
    if n = 0
    then x
    else 2 * double_n_times_lame(n - 1, x);

fun nth_tail_lame (n, xs) =
    if n = 0
    then xs
    else tl (nth_tail_lame(n - 1, xs));

fun n_times (f, n, x) =
    if n = 0
    then x
    else f (n_times(f, n - 1, x));

fun incremente x = x + 1;
fun dobule x = x + x;

val x1 = n_times (double, 4, 7);
val x2 = n_times (increment, 4, 7);
val x3 = n_times (tl, 2, [4, 8, 12, 16]);

fun addition (n, x) = n_times (increment, n x);
fun double_n_times (n, x) = n_times (dobule, n, x);
fun nth_tail (n, x) = n_tiemes (tl, n, x);

fun triple x = 3 * x;
fun triple_n_times (n, x) = n_times (triple, n, x);


fun times_until_zero (f, x) =
    if x = 0 then 0 else i + times_until_zero(f, f x);

fun len xs =
    case xs of
	[] => 0
      | _ :: xs' => 1 + len xs';




		     
