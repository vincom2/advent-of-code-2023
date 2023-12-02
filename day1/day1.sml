val read_file = TextIO.inputAll o TextIO.openIn

fun c2d c = Char.ord c - Char.ord #"0"

fun successful_attempt (n, (pref, suff)) = if Substring.size pref = 0 then SOME (0, n)
  else if Substring.size suff = 0 then NONE
  else let
    val pref' = Substring.string pref
    val pref'' = String.explode pref'
  in
    if List.exists Char.isDigit pref'' then NONE else SOME (Substring.size pref, n)
  end

fun digits_of_line line = let
  val conv = [(1, "one"), (2, "two"), (3, "three"), (4, "four"), (5, "five"), (6, "six"), (7, "seven"), (8, "eight"), (9, "nine")]
  val conv_backwards = List.map (fn (n, s) => (n, String.rev s)) conv
  val ss = Substring.full line
  val ss_backwards = Substring.full (String.rev line)
  val attempts = List.map (fn (n, c) => (n, Substring.position c ss)) conv
  val attempts_backwards = List.map (fn (n, c) => (n, Substring.position c ss_backwards)) conv_backwards
  val attempts' = List.map successful_attempt attempts
  val attempts_backwards' = List.map successful_attempt attempts_backwards
  val attempts'' = List.foldl (fn (attempt, acc) => case acc of
      NONE => attempt
    | SOME (al, _) => (case attempt of
        NONE => acc
      | SOME (l, _) => if l < al then attempt else acc)) NONE attempts'
  val attempts_backwards'' = List.foldl (fn (attempt, acc) => case acc of
      NONE => attempt
    | SOME (al, _) => (case attempt of
        NONE => acc
      | SOME (l, _) => if l < al then attempt else acc)) NONE attempts_backwards'
  val chars = String.explode line
  val digits = List.filter Char.isDigit chars
in
  case (attempts'', attempts_backwards'') of
    (NONE, NONE) => (case List.length digits of
      0 => raise Fail "wtf"
    | 1 => let
      val digit :: _ = digits
      val digit' = c2d digit
    in (digit', digit') end
    | _ => let
      val first :: _ = digits
      val first' = c2d first
      val last :: _ = List.rev digits
      val last' = c2d last
    in (first', last') end)
  | (NONE, _) => let val (SOME (_, last)) = attempts_backwards'' in case List.length digits of
      0 => (last, last)
    | _ => let
      val digit :: _ = digits
      val digit' = c2d digit
    in (digit', last) end
  end
  | (_, NONE) => let val (SOME (_, first)) = attempts'' in case List.length digits of
      0 => (first, first)
    | _ => let
      val digit :: _ = List.rev digits
      val digit' = c2d digit
    in (first, digit') end
  end
  | (_, _) => let
    val (SOME (_, first)) = attempts''
    val (SOME (_, last)) = attempts_backwards''
  in (first, last) end
end

fun process filename = let
  val text = read_file filename
  val strings = String.fields (fn c => c = #"\n") text
  val strings' = List.take (strings, List.length strings - 1)
  val digits_of_lines = List.map digits_of_line strings'
in
  List.foldl (fn ((first, last), acc) => acc + first*10 + last) 0 digits_of_lines
end

(* try one two three four five six seven eight nine 
 * against the string. if you get an empty prefix or an entirely nonnumeric prefix, take it as first.
 * then reverse the string and what you're checking for and apply the same logic to get the last. *)