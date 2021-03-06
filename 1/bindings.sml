(* This is a comment. This is our first program. *)


val x = 34; (* int *)
(* static environment: x : int *)
(* dynamic environment: x --> 34 *)

val y = 17;
(* static environment: x: int, y: int *)
(* dynamic environment: x --> 34, y --> 17 *)

val z = (x + y) + (y + 2);
(* static environment : x : int, y : int, z : int *)
(* dynamic environment: x --> 34, y --> 17, z --> 70 *)

val q = z + 1;
(* static enfironment: x : int, y : int, z : int, q : int *)
(* dynaic enfironment: x --> 34, y --> 17, z --> 70, w --> 71 *)

val abs_of_z = if z < 0 then 0 - z else z; (* bool *) (* int *)
(* static enfironment: ..., abs_of_z : int *)
(* dynamic environment: ..., abs_of_z --> 70 *)

val test = 12 + (if 28 > 56 then 18 else 11);
(* static environment: ..., test : int *)
(* dynamic envornment: ..., test --> 23 *)
		    
val abs_of_z_simpler = abs z;

val less = 23 < 59;
