fun charToInt str =
  case str of
    nil => 0
  | c :: cs =>
    let
      val code = Char.ord c
    in
      code
    end

val example = charToInt "G"
