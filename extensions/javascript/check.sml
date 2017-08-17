datatype State = Closed | Opened | Suspect
local
fun eval(text, state, start) = case state of Closed => closed(text, start)
                                           | Opened => opened(text, start)
                                           | Suspect => suspect(text)

and closed(#"#"::text, true) = hash(Closed, text)
  | closed(x::text, _) = if delimiter(x)
                            then closed(text, false)
                            else (case x of
                      #"\n" => closed(text, true)
                    | #"/"  => (case text of #"*"::rest => comment(Closed, rest)
                                           | _ => opened(text, false))
                    | #"%" => linecomment(Closed, text)
                    | #"`" => backquote(text)
                    | _ => opened(text, false))
  | closed([], _) = exit(Closed)

and opened(#"#"::text, true) = hash(Opened, text)
  | opened(x::text, _) = (case x of
                      #";" => suspect(text)
                    | #"/"  => (case text of #"*"::rest => comment(Opened, rest)
                                           | _ => opened(text, false))
                    | #"%" => linecomment(Opened, text)
                    | #"\n" => opened(text, true)
                    | _ => opened(text, false))
  | opened([], _) = exit(Opened)

and suspect(x::text) = if delimiter(x)
                          then suspect(text)
                          else (case x of
                      #"\n" => closed(text, true)
                    | #"/"  => (case text of #"*"::rest => comment(Suspect, rest)
                                           | _ => opened(text, false))
                    | #"%" => linecomment(Closed, text)
                    | _ => opened(text, false))
  | suspect([]) = exit(Suspect)

and delimiter(#" " | #"\t" | #"\r") = true
  | delimiter _ = false

and comment(state, #"*"::(#"/"::text)) = eval(text, state, false)
  | comment(state, _::text) = comment(state, text)
  | comment(state, []) = exit(state)

and linecomment(state, #"\n"::text) = eval(text, state, true)
  | linecomment(state, _::text) = linecomment(state, text)
  | linecomment(state, []) = exit(state)

and hash(state, #"\n"::text) = eval(text, state, true)
  | hash(state, _::text) = hash(state, text)
  | hash(state, []) = exit(state)

and backquote(#"`"::text) = closed(text, false)
  | backquote(_::text) = backquote(text)
  | backquote([]) = exit(Closed)

and exit(Opened) = 8
  | exit(_) = 0
in
fun stringOfFile (filename) = let val f = TextIO.openIn filename
                                  val ans = TextIO.inputAll f
                                  fun iter _ = ans
                              in iter(TextIO.closeIn f) end

fun check(text) = closed(explode(text), true)

fun doit file = check(stringOfFile file)
end

