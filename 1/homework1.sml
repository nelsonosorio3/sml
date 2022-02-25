fun is_older (date1 : (int * int * int), date2: (int * int * int)) =
    if (#1 date1) < (#1 date2)		
    then true	     
    else if (#1 date1)  > (#1 date2)			      
    then false	     
    else	
	if (#2 date1) < (#2 date2)		    
	then true	 
	else if (#2 date1) > (#2 date2)			 
	then false	 
	else 
	    (#3 date1) < (#3 date2);

fun number_in_month (list_dates : (int * int * int) list, month: int) =
    if null list_dates
    then 0
    else
	if (#2 (hd list_dates)) = month
	then 1 + number_in_month(tl list_dates, month)
	else 0 + number_in_month(tl list_dates, month)

fun number_in_months (list_dates : (int * int * int) list, months: int list) =
    if null months
    then 0
    else
	number_in_month(list_dates, hd months) + number_in_months(list_dates, tl months);

fun dates_in_month (list_dates : (int * int * int) list, month: int) =
    if null list_dates
    then []
    else
	if (#2 (hd list_dates)) = month
	then (hd list_dates)::dates_in_month(tl list_dates, month)
	else dates_in_month(tl list_dates, month);

fun dates_in_months (list_dates: (int * int * int) list, months: int list) =
    if null months
    then []
    else
	dates_in_month(list_dates, hd months) @ dates_in_months(list_dates, tl months);

fun get_nth (string : string list, n: int) =
    if n = 1
    then hd string
    else
	get_nth(tl string, n - 1);

fun date_to_string (date : (int * int * int)) =
    let val dates = ["January", "February", "March", "April",
		     "May", "June", "July", "August", "September",
		     "October", "November", "December"]
    in
	get_nth(dates, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end;

fun number_before_reaching_sum (sum : int, nums : int list) =
    if sum <= hd nums
    then 0
    else
	1 + number_before_reaching_sum(sum - hd nums, tl nums);

fun what_month (day : int) =
    let val months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	1 + number_before_reaching_sum(day, months)
    end;

fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else
	what_month(day1)::month_range(day1 + 1, day2);

fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
    else
	let val tale_dates = oldest(tl dates)
	in if isSome tale_dates andalso is_older(valOf tale_dates, hd dates)
	   then tale_dates
	   else SOME (hd dates)
	end;

fun is_in_list (xs : int list, x : int) =
    if null xs
    then false
    else if hd xs = x
    then true
    else
	is_in_list(tl xs, x);

fun remove_repeated (xs : int list) =
    if null xs
    then []
    else
	if is_in_list(tl xs, hd xs)
	then remove_repeated(tl xs)
	else
	    hd xs::remove_repeated(tl xs);
		  
fun number_in_months_challenge (list_dates : (int * int * int) list, months : int list) =
    let val unique_months = remove_repeated(months)
    in
	number_in_months(list_dates, unique_months)
    end;

fun dates_in_months_challenge (list_dates : (int * int * int) list, months : int list) =
    let val unique_months = remove_repeated(months)
    in
	dates_in_months(list_dates, unique_months)
    end;

fun reasonable_date (date: (int * int * int)) =
    let val months = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	val day = #3 date
	val month = #2 date
	val year = #1 date
	fun get_nth(n : int) =
	    if n = 1
	    then hd months
	    else
		get_nth(n-1)
	fun is_leap(n : int) =
	    n mod 4 = 0 andalso (n mod 400 = 0 orelse n mod 100 <> 0)						
    in
	if year < 1
	then false
	else
	    if month < 1 orelse month > 12
	    then false
	    else
		if day < 1 orelse day > get_nth(month)
		then false
		else
		    if day < 29
		    then true
		    else is_leap(year)
    end;
			
		
		     

	       

	
		 
		       
    
			     

				 
	    
				   
						      
