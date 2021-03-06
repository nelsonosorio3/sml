fun countdown (x : int, y : int) =
    if x = y
    then []
    else x :: countdown (x - 1, y);

fun countup (x: int, y : int) =
    if x = y
    then []
    else x :: countdown (x+1, y);

fun skip1 (xs : int list) =
    if null xs
    then []
    else if null(tl xs)
    then [hd xs]
    else
	hd xs :: skip1(tl (tl xs));

fun skip2 (xs : int list) =
    if null xs
    then []
    else if null (tl xs)
    then []
    else
	hd (tl xs) :: skip2(tl (tl xs));

fun sum (xs : int list) =
    if null xs
    then 0
    else
	hd xs + sum(tl xs);

fun sub1 (xs : int list) =
    if null xs
    then 0
    else
	~ (hd xs) + sub1(tl xs);

fun sub2 (xs : int list) =
    ~(sum xs);

fun alternate (xs : int list) =
    sum (skip1 (xs)) - sum (skip2 (xs));

fun min (xs : int list) =
    if null xs
    then 0
    else if null (tl xs)
    then hd xs
    else
	let val tail = min(tl xs)
	in
	    if hd xs < tail
	    then hd xs
	    else tail
	end;

fun max (xs : int list) =
    if null xs
    then 0
    else if null (tl xs)
    then hd xs
    else
	let val tail = min(tl xs)
	in
	    if hd xs > tail
	    then hd xs
	    else tail
	end;

fun min_max (xs : int list) =
    if null (tl xs)
    then (hd xs, hd xs)
    else
	(min(xs), max(xs));

