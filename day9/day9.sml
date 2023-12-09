val read_file = TextIO.inputAll o TextIO.openIn

fun next_sequence_rev [] = []
fun next_sequence_rev (first :: rest) = #2 (List.foldl (fn (b, (a, acc)) => (b, (b-a) :: acc)) (first, []) rest)

val are_we_done = List.all (fn n => n = 0)

fun collect_intermediaries_rev _ [] = []
  | collect_intermediaries_rev intermediaries sequence = let
  val sequence' as (intermediary :: _) = next_sequence_rev sequence
  val intermediaries' = intermediary :: intermediaries
in
  if are_we_done sequence' then intermediaries' else
    collect_intermediaries_rev intermediaries' (List.rev sequence')
end

(* lmaooooooooo op- isn't associative *)
fun extrapolate (intermediaries, first) = List.foldl op+ first intermediaries

fun process filename = let
  val input = read_file filename
  val lines = String.tokens (fn c => c = #"\n") input
  val lines' = List.map (String.tokens (fn c => c = #" ")) lines
  val number_lines = List.map (List.map (Option.valOf o Int.fromString)) lines'
  val number_line_sequences = List.map (collect_intermediaries_rev []) number_lines
  val last_of_first = List.map (List.hd o List.rev) number_lines
  val everything = ListPair.zip (number_line_sequences, last_of_first)
  val next_numbers = List.map extrapolate everything
in
  List.foldl op+ 0 next_numbers
end

fun collect_intermediaries_rev2 _ [] = []
  | collect_intermediaries_rev2 intermediaries sequence = let
  val sequence' = next_sequence_rev sequence
  val sequence'' as (intermediary :: _) = List.rev sequence'
  val intermediaries' = intermediary :: intermediaries
in
  if are_we_done sequence' then intermediaries' else
    collect_intermediaries_rev2 intermediaries' sequence''
end

(* you will never believe this but basically all the time i took between parts 1 and 2 was spent
 * misunderstanding op- X.x *)
fun extrapolate_backwards (intermediaries, first) = let
  val (first' :: intermediaries') = intermediaries
  val intermediaries'' = intermediaries' @ [first]
in
  List.foldl op- first' intermediaries''
end

fun process2 filename = let
  val input = read_file filename
  val lines = String.tokens (fn c => c = #"\n") input
  val lines' = List.map (String.tokens (fn c => c = #" ")) lines
  val number_lines = List.map (List.map (Option.valOf o Int.fromString)) lines'
  val number_line_sequences = List.map (collect_intermediaries_rev2 []) number_lines
  val first_of_first = List.map List.hd number_lines
  val everything = ListPair.zip (number_line_sequences, first_of_first)
  val next_numbers = List.map extrapolate_backwards everything
in
  List.foldl op+ 0 next_numbers
end