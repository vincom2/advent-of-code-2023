fun main () = let
  val [filename, part] = CommandLine.arguments ()
  val output = if part = "part1" then process filename else process2 filename
in
  print (Int.toString output)
end

val () = main ()