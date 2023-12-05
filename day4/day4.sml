val read_file = TextIO.inputAll o TextIO.openIn

structure StringOrd : ORD_KEY = struct
  type ord_key = string
  val compare = String.compare
end
structure I = RedBlackSetFn(StringOrd)

fun evaluate_card card = let
  val [_, useful] = String.tokens (fn c => c = #":") card
  val [winning_text, have_text] = String.tokens (fn c => c = #"|") useful
  val winning_numbers = String.tokens (fn c => c = #" ") winning_text
  val winning_set = List.foldl (fn (i, acc) => I.add (acc, i)) I.empty winning_numbers
  val have_numbers = String.tokens (fn c => c = #" ") have_text
in
  List.foldl
    (fn (i, points) => if I.member (winning_set, i) then if points = 0 then 1 else points * 2 else points) 0 have_numbers
end

fun process filename = let
  val input = read_file filename
  val cards = String.tokens (fn c => c = #"\n") input
in
  List.foldl (fn (card, total) => total + evaluate_card card) 0 cards
end

structure M = IntRedBlackMap

val get_prizes = let
  val memo = ref (Array.array (200, NONE))
  fun get_prizes' cards card = case Array.sub (!memo, card) of
    SOME count => count
  | NONE => let
    val n = M.lookup (cards, card)
    (* this is so bad *)
    val prizes = List.tabulate (n, fn i => i+1+card)
    val total = List.foldl (fn (c, acc) => acc + get_prizes' cards c) n prizes
    val () = Array.update (!memo, card, SOME total)
  in
    total
  end
in
  get_prizes'
end

fun evaluate_card2 card = let
  val [number_text, card'] = String.tokens (fn c => c = #":") card
  val [_, number_text'] = String.tokens (fn c => c = #" ") number_text
  val SOME card_number = Int.fromString number_text'
  val [winning_text, have_text] = String.tokens (fn c => c = #"|") card'
  val winning_numbers = String.tokens (fn c => c = #" ") winning_text
  val winning_set = List.foldl (fn (i, acc) => I.add (acc, i)) I.empty winning_numbers
  val have_numbers = String.tokens (fn c => c = #" ") have_text
  val count = List.foldl
    (fn (i, points) => if I.member (winning_set, i) then points + 1 else points) 0 have_numbers
in
  (card_number, count)
end

fun process2 filename = let
  val input = read_file filename
  val cards = String.tokens (fn c => c = #"\n") input
  val card_map = List.foldl (fn (card, acc) => let
    val (card_number, count) = evaluate_card2 card
  in
    M.insert (acc, card_number, count)
  end) M.empty cards
in
  List.foldl (fn (card, total) => total + get_prizes card_map card) 0 (M.listKeys card_map) + M.numItems card_map
end