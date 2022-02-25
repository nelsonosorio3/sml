[];
[3,4,5];
[4,3];
[3,4,5,6];
[(1+2),3+4,7];
[true,false,true];

val x = [7,8,9];

5::x;
6::5::x;
6::x;

[6]::[[7,5],[5,2]];

null x;

null [];

val y = [];

null y;

hd x;

tl x;

hd (tl x);

tl (tl x);

tl (tl (tl x));

(* tl (tl (tl (tl x))); *)

val z = [5,1,6,8];

val el = hd (tl (tl z));
