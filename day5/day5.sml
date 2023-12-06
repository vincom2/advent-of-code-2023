val read_file = TextIO.inputAll o TextIO.openIn

exception Mapped of int

(* give it something that starts with a "map:" line. it'll process until a newline, and strip that last newline and return *)
(* lines -> translator list -> translator list * lines *)
fun parse_map [] translators = (translators, [])
  | parse_map (line :: lines) translators = if String.size line = 0 then (translators, lines)
    else if String.sub (line, String.size line - 1) = #":" then parse_map lines translators
    else let
      val [dest, src, len] = List.map (Option.valOf o Int.fromString) (String.tokens (fn c => c = #" ") line)
      fun f n = if n >= src andalso n < src+len then raise Mapped (dest + n - src) else n
    in
      parse_map lines (f :: translators)
    end

fun parse_maps lines maps = case parse_map lines [] of
    (translators, []) => List.rev (translators :: maps)
  | (translators, lines') => parse_maps lines' (translators :: maps)

fun apply_map map seed = List.foldl (fn (f, loc) => f loc) seed map
  handle Mapped n => n
(* oops, you have a (int -> int) list list.
 * every list is functions doing a particular mapping job.
 * so you have to try every function from a list to get an output,
 * and _then_ move on to the next list. *)
fun where_to_plant_seed maps seed =
  List.foldl (fn (map, loc) => apply_map map loc) seed maps

fun process filename = let
  val input = read_file filename
  (* parse seeds from 1st line *)
  val seed_line :: _ :: lines = String.fields (fn c => c = #"\n") input
  val [_, seed_line'] = String.tokens (fn c => c = #":") seed_line
  val seeds = List.map (Option.valOf o Int.fromString) (String.tokens (fn c => c = #" ") seed_line')
  (* parse maps from rest of file *)
  val maps = parse_maps lines []
  val planting_locations = List.map (where_to_plant_seed maps) seeds
in
  List.foldl Int.min (List.hd planting_locations) (List.tl planting_locations)
end

(* start, length *)
type seed = int * int

exception Mapped2 of (int * int) * (int * int) list

fun parse_map2 [] translators = (translators, [])
  | parse_map2 (line :: lines) translators = if String.size line = 0 then (translators, lines)
    else if String.sub (line, String.size line - 1) = #":" then parse_map2 lines translators
    else let
      val [dest, src, len] = List.map (Option.valOf o Int.fromString) (String.tokens (fn c => c = #" ") line)
      fun f (s, l) = let
        val e = s + l - 1
        val ms = src
        val me = src + len - 1
      in
        if s >= ms andalso e <= me (* seed range is fully contained within mapping range *)
          then raise Mapped2 ((dest + s - src, l), [])
        else if s < ms andalso e > ms andalso e <= me (* right side of seed range overlaps with mapping range *)
          then raise Mapped2 ((dest, e - ms + 1), [(s, ms - s + 1)])
        else if s > ms andalso s < me  andalso e >= me (* left side of seed range overlaps with mapping range *)
          then raise Mapped2 ((dest + s - src, me - s + 1), [(me+1, e - me)])
        else if ms >= s andalso me <= e (* seed range fully contains mapping range *)
          then raise Mapped2 ((dest, len), [(s, ms - s), (me + 1, e - me)])
        else (* no overlap *)
          ((s, l), [])
      end
    in
      parse_map2 lines (f :: translators)
    end

fun parse_maps2 lines maps = case parse_map2 lines [] of
    (translators, []) => List.rev (translators :: maps)
  | (translators, lines') => parse_maps2 lines' (translators :: maps)

(* each "map" is a (int * int -> (int * int) * (int * int) list) list
 * take it, apply the mapping, take the new seed range as "processed",
 * keep the new ranges in an unprocessed list for later *)
fun apply_map2 map seed = List.foldl (fn (f, (seed, _)) => f seed) (seed, []) map
  handle Mapped2 output => output

fun where_to_plant_seed2 maps seed = let
  val (loc, new_ranges) = List.foldl
    (fn (map, (loc, new_ranges)) => let
      val (loc', more_new_ranges) = apply_map2 map loc
    in
      (loc', more_new_ranges :: new_ranges)
    end) (seed, []) maps
in
  (* oh shit you can't just concat all the new ranges together; they correspond to different "stages" *)
  (* ok so now the new_ranges is a list list where each list corresponds to a map *)
  (loc, List.rev new_ranges)
end

fun transpose [] = []
  | transpose ([] :: _) = []
  | transpose l = (List.map List.hd l) :: (transpose (List.map List.tl l))

fun plant_until_done maps [] = []
  | plant_until_done maps seeds = let
    val one_round = List.map (where_to_plant_seed2 maps) seeds
    val done = List.map #1 one_round
    val new_ranges = List.map #2 one_round
    val new_ranges' = transpose new_ranges
    val new_ranges'' = List.map List.concat new_ranges'
    (* now you want to apply plant_until_done to these with different "depths" of maps *)
    (* assume matching lengths *)
    fun helper [] [] = []
      | helper (maps as (map :: rest)) (s :: ss) = let
        val curr = plant_until_done maps s
        val others = helper rest ss
      in
        curr @ others
      end
    val rest = helper maps new_ranges''
  in
    done @ rest
  end

fun process2 filename = let
  val input = read_file filename
  (* parse seed ranges from 1st line *)
  val seed_line :: _ :: lines = String.fields (fn c => c = #"\n") input
  val [_, seed_line'] = String.tokens (fn c => c = #":") seed_line
  val seed_line_numbers = List.map (Option.valOf o Int.fromString) (String.tokens (fn c => c = #" ") seed_line')
  val (_, seeds) = List.foldl (fn
    (n, (SOME first, acc)) => (NONE, (first, n) :: acc)
  | (n, (NONE, acc)) => (SOME n, acc)) (NONE, []) seed_line_numbers
  (* parse maps from rest of file *)
  val maps = parse_maps2 lines []
  val final_ranges = plant_until_done maps seeds
  val final_ranges' = List.map #1 final_ranges
in
  List.foldl Int.min (List.hd final_ranges') (List.tl final_ranges')
end