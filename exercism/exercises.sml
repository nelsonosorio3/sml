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