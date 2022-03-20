(* hello world *)
fun hello () = "Hello, World!";

(* leap year *)
fun isLeapYear year =
  year mod 4 = 0 andalso (year mod 400 = 0 orelse year mod 100 <> 0);

(* two fer *)
fun name (input: string option) =
  case input of
      NONE => "One for you, one for me."
    | SOME name => "One for " ^ name ^ ", one for me.";

(* space age *)
datatype planet = Mercury | Venus | Earth | Mars
                | Jupiter | Saturn | Neptune | Uranus
fun age_on planet seconds =
    let val year = (real seconds) / (31557600.0)
        in
          case planet of
              Mercury => year / 0.2408467
            | Venus => year / 0.61519726
            | Earth => year
            | Mars => year / 1.8808158
            | Jupiter => year / 11.862615
            | Saturn => year / 29.447498
            | Uranus => year / 84.016846
            | Neptune => year / 164.79132
        end;

datatype planet = Mercury | Venus | Earth | Mars
                | Jupiter | Saturn | Neptune | Uranus
fun age_on planet seconds =
    let val year = (real seconds) / (31557600.0)
        in
          case planet of
              Mercury => year / 0.2408467
            | Venus => year / 0.61519726
            | Earth => year
            | Mars => year / 1.8808158
            | Jupiter => year / 11.862615
            | Saturn => year / 29.447498
            | Uranus => year / 84.016846
            | Neptune => year / 164.79132
        end;


fun isPangram s =
  let 
        val alphabet = ["q", "j", "z", "x", "v", "k", "w", "y", "f", "b", "g", "h", 
                    "m","p", "d", "u", "c", "l", "s", "n", "t", "o", "i", "r", "a", "e"]
        val s = String.map Char.toLower s
        fun helper alphabet =
            case alphabet of
                [] => true
                | head :: tail => if String.isSubstring head s 
                                    then helper tail
                                    else false
    in    
        helper alphabet
    end;


fun collatz n = 
    let 
        fun helper (n, acc) =
            case n of
                1 => acc
                | n => if n mod 2 = 0
                        then helper (n div 2, acc + 1)
                        else helper ((3 * n) + 1, acc + 1)
    in
        if n < 1
        then NONE
        else SOME (helper (n, 0))
    end;


fun isBalanced s =
  let
    fun char_to_str list =
        case list of
            [] => []
            |head :: tail => Char.toString head :: char_to_str(tail)
    val list = char_to_str (explode s)
    fun helper (list, acc) =
        case list of
            [] => if null acc then true else false
            | "]" :: tail => if null acc orelse hd acc = "{" orelse hd acc = "("
                                then false        
                                else helper (tail, tl acc)
            | "}" :: tail => if null acc orelse hd acc = "[" orelse hd acc = "("
                                then false        
                                else helper (tail, tl acc)
            | ")" :: tail => if null acc orelse hd acc = "[" orelse hd acc = "{"
                                then false        
                                else helper (tail, tl acc)
            | "[" :: tail => helper (tail, "[" :: acc)
            | "{" :: tail => helper (tail, "{" :: acc)
            | "(" :: tail => helper (tail, "(" :: acc)
            | _ :: tail => helper(tail, acc)
    in    
        helper (list, [])
    end;



datatype allergen = Eggs
                  | Peanuts
                  | Shellfish
                  | Strawberries
                  | Tomatoes
                  | Chocolate
                  | Pollen
                  | Cats
fun allergies (score: int): allergen list =
  let
        val list_allergen = [Eggs, Peanuts, Shellfish, Strawberries, Tomatoes, Chocolate, Pollen, Cats]   
        fun helper (list, score) =
            case list of
            [] => []
            | head :: tail => if score mod 2 = 1
                                then head :: helper (tail, score div 2)    
                                else helper (tail, score div 2)
    in    
        helper (list_allergen, score)
    end;
fun allergicTo (score: int) (a: allergen): bool =
  let 
    val list_allergen = allergies score
    fun helper list =
        case list of
            [] => false
            | head :: tail => if head = a
                                then true
                                else helper tail
    in    
        helper list_allergen
    end;


fun accumulate (f, xs) =
  case xs of
    [] => []
    | head :: tail => f head :: accumulate (f, tail)


fun rebase (inBase: int, outBase: int, digits: int list): int list option =
    let 
        fun power (num, exp) =
            if exp = 0
            then 1
            else num * power(num, exp - 1)
        fun invalid_digits digits = 
            case digits of
            [] => true
            | head :: [] => head < 0 orelse head >= inBase
            | head :: tail => head < 0 orelse head >= inBase orelse invalid_digits tail
        fun to_decimal (list, exponent) =
            case list of
                [] => []
                | head :: tail => (head * power (inBase, exponent)) :: to_decimal (tail, exponent - 1)
        val decimal = foldl (fn (n, acc) => n + acc) 0 (to_decimal (digits, List.length digits - 1))
        fun from_decimal num =
            if num < outBase 
            then [num]
            else num mod outBase :: from_decimal (num div outBase)
    in
        if inBase < 2 orelse outBase < 2 orelse invalid_digits digits orelse decimal = 0
        then NONE
        else SOME (List.rev (from_decimal decimal))
    end;



val cipher = [("a","z"),("b","y"),("c","x"),("d","w"),("e","v"),("f","u"),("g","t"),
		 ("h","s"),("i","r"),("j","q"),("k","p"),("l","o"),("m","n"),("1","1"),("2","2"),("3","3"),
            ("4","4"),("5","5"),("6","6"),("7","7"),("8","8"),("9","9")]
fun to_string_list char_list =
    case char_list of 
        [] => []
      | head :: tail => (Char.toString (Char.toLower head)) :: to_string_list (tail);
fun get_counterpart (letter, cipher) =
	case cipher of
	    [] => ""		      
	  | (letter1, letter2) :: tail => if letter1 = letter							   
					                    then letter2						   
                                        else if letter2 = letter						
                                        then letter1	
					                    else get_counterpart (letter, tail);
fun get_counterpart_decode (letters, cipher) =
    case letters of
        [] => ""
        | head :: tail => get_counterpart (head, cipher) ^ get_counterpart_decode (tail, cipher)
fun decode (phrase: string): string =
    let 
        val string_list = to_string_list (explode phrase)
    in
        get_counterpart_decode (string_list, cipher)
    end;
fun encode (phrase: string): string =
    let 
        val string_list = to_string_list (explode phrase)
        val encode_no_space = decode phrase
        val list = to_string_list (explode encode_no_space)
        fun add_space (list, count) =
            case list of
                [] => []
                |head :: [] => [head]
                | head :: tail => if count = 5 then head :: " " :: add_space(tail, 1) else head :: add_space (tail, count + 1)
        val list = add_space (list, 1)
    in
        List.foldl (fn (letter, acc) => acc ^ letter) "" list 
    end;


fun square n =
    n * n
fun squareOfSum n =
  square(((n * (n + 1)) div 2))
fun sumOfSquares n =
  (n * (n + 1) * ((2 * n) +1)) div 6
fun differenceOfSquares n =
  (squareOfSum n) - (sumOfSquares n)
