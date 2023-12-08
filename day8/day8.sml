val read_file = TextIO.inputAll o TextIO.openIn

structure StringOrd : ORD_KEY = struct
  type ord_key = string
  val compare = String.compare
end
structure M = RedBlackMapFn(StringOrd)

datatype steps = Done of int | NotYet of int * string
fun follow _ node [] steps = NotYet (steps, node)
  | follow node_map node (instruction :: instructions) steps =
      if node = "ZZZ" then Done steps else let
        val (left, right) = M.lookup (node_map, node)
        val next = (case instruction of
          #"L" => left
        | _ => right)
      in
        follow node_map next instructions (steps + 1)
      end

fun follow_until_done node_map node instructions steps =
  case follow node_map node instructions steps of
    Done steps => steps
  | NotYet (steps', node') => follow_until_done node_map node' instructions steps'

fun process filename = let
  val input = read_file filename
  val instructions :: nodes = String.tokens (fn c => c = #"\n") input
  fun process_node_line nl = let
    val [src, rest] = String.tokens (fn c => c = #"=") nl
    val src' = String.extract (src, 0, SOME 3)
    val [left, right] = String.tokens (fn c => c = #",") rest
    val left' = String.extract (left, 2, SOME 3)
    val right' = String.extract (right, 1, SOME 3)
  in
    (src', left', right')
  end
  val node_map = List.foldl
    (fn (line, node_map) => let
      val (src, left, right) = process_node_line line
    in
      M.insert (node_map, src, (left, right))
    end) M.empty nodes
in
  follow_until_done node_map "AAA" (String.explode instructions) 0
end