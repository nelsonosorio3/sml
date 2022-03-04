fun fact1 n = if n = 0 then 1 else n * fact1 (n - 1);

val x = fact1 3;

fun fact2 n =
    let fun aux (n, acc) =
	    if n = 0
	    then acc
	    else aux (n - 1, acc * n)
    in
	aux(n, 1)
    end;

val x = fact2 3;

fun sum xs =
    case xs of
	[] => 0
      | x :: xs' => x + sum xs';

fun sum xs =
    let fun aux (xs, acc) =
	    case xs of
		[] => 0
	      | x :: xs' => aux(xs', x + acc)
    in
	aux(xs, 0)
    end;

fun rev xs =
    case xs of
	[] => []
      | x :: xs' => (rev xs) @ [x];

fun rev xs =
    let fun aux(xs, acc) =
	    case xs of
		[] => acc
	      | x :: xs' => aux(xs', x :: acc)
    in
	aux(xs, [])
    end;


			 
