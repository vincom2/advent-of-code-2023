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

datatype steps2 = Done2 of int | NotYet2 of int * string list
local
  val are_we_done = List.all (fn node => String.sub (node, 2) = #"Z")
  fun step node_map instruction node = let
    val (left, right) = M.lookup (node_map, node)
  in
    case instruction of
      #"L" => left
    | _ => right
  end
in
  fun follow2 _ nodes [] steps = NotYet2 (steps, nodes)
    | follow2 node_map nodes (instruction :: instructions) steps =
        if are_we_done nodes then Done2 steps else
          follow2 node_map (List.map (step node_map instruction) nodes) instructions (steps + 1)
end

fun follow_until_done2 node_map nodes instructions steps =
  case follow2 node_map nodes instructions steps of
    Done2 steps => steps
  | NotYet2 (steps', nodes') => follow_until_done2 node_map nodes' instructions steps'

fun process2 filename = let
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
  val (node_map, starting_nodes) = List.foldl
    (fn (line, (node_map, starting_nodes)) => let
      val (src, left, right) = process_node_line line
      val node_map' = M.insert (node_map, src, (left, right))
    in
      if String.sub (src, 2) = #"A" then (node_map', src :: starting_nodes) else (node_map', starting_nodes)
    end) (M.empty, []) nodes
in
  follow_until_done2 node_map starting_nodes (String.explode instructions) 0
end

(* figure out how many steps it takes to reach **Z from each of the **A
 * then calculate the LCM *)

local
  (* open IntInf *)
  datatype steps3 = Done3 of int | NotYet3 of int * string
  fun are_we_done node = String.sub (node, 2) = #"Z"
in
  fun real_follow2 _ node [] steps = NotYet3 (steps, node)
    | real_follow2 node_map node (instruction :: instructions) steps =
        if are_we_done node then Done3 steps else let
          val (left, right) = M.lookup (node_map, node)
          val next = (case instruction of
            #"L" => left
          | _ => right)
        in
          real_follow2 node_map next instructions (steps + 1)
        end

  fun real_follow_until_done2 node_map node instructions steps =
    case real_follow2 node_map node instructions steps of
      Done3 steps => steps
    | NotYet3 (steps', node') =>real_follow_until_done2 node_map node' instructions steps'

  fun real_process2 filename = let
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
    val (node_map, starting_nodes) = List.foldl
      (fn (line, (node_map, starting_nodes)) => let
        val (src, left, right) = process_node_line line
        val node_map' = M.insert (node_map, src, (left, right))
      in
        if String.sub (src, 2) = #"A" then (node_map', src :: starting_nodes) else (node_map', starting_nodes)
      end) (M.empty, []) nodes
    val instructions' = String.explode instructions
    val steps = List.map (fn node => real_follow_until_done2 node_map node instructions' 0) starting_nodes
  in
    (* List.foldl op* 1 steps *)
    steps
  end
end