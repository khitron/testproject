(* compiler.sml
 *
 * Assignment 4.
 * Khitron Igal, ID 308808070.
 * Gido Naama,   ID 021528799.
 *
 * A compiler for Scheme, written in SML/NJ,
 * as part of the compiler construction course.
 * There is a full compiler here.
 * 
 * This assignment is the compiler third part:
 * Code generator.
 *)

(* Scanner tokens to hold scanning results *)
datatype SchemeToken = LparenToken             (* "(" *)
                     | RparenToken             (* ")" *)
                     | QuoteToken              (* "'" *)
                     | DotToken                (* "." *)
                     | VectorToken             (* "#(" *)
                     | IntToken of int         (* integer *)
                     | CharToken of char       (* character *)
                     | StringToken of string   (* string *)
                     | SymbolToken of string   (* identifier *)
                     | BoolToken of bool       (* boolean *)

(* Symbol expression kinds to hold reader results *)
datatype Sexpr = Void                          (* "unit" type *)
               | Nil                           (* empty list *)
               | Pair of Sexpr * Sexpr         (* cons of two Sexpressions *)
               | Vector of Sexpr list          (* vector *)
               | Symbol of string              (* identifier *)
               | String of string              (* string *)
               | Number of int                 (* integer *)
               | Bool of bool                  (* boolean *)
               | Char of char                  (* character *)

(* Tagged expressions kinds to hold tag parser and semantic analyzis results *)
datatype Expr = Const of Sexpr                          (* constant *)
              | Var of string                           (* variable name *)
              | VarFree of string                       (* free variable *)
              | VarParam of string * int                (* parameter variable *)
              | VarBound of string * int * int          (* bound variable *)
              | If of Expr * Expr * Expr                (* if *)
              | Abs of (string list) * Expr             (* regular lambda *)
              | AbsOpt of (string list) * string * Expr (* optional lambda *)
              | AbsVar of string * Expr                 (* variadic lambda *)
              | App of Expr * (Expr list)               (* application *)
              | AppTP of Expr * (Expr list)             (* in tail position *)
              | Seq of Expr list                        (* sequence *)
              | Or of Expr list                         (* or *)
              | Def of Expr * Expr                      (* define *)

(* Auxiliary functions *)
signature AUX =                        (* Contract signature *)
sig
    val gensym        : string                 -> string
    val pairs         : (Sexpr list) * Sexpr   -> Sexpr
    val isSet         : string list            -> bool
    val findinlist    : string * (string list) -> int option
    val printcode     : string * string        -> string
    val stringOfFile  : string                 -> string
    val stringToFile  : string * string        -> unit
    val run           : unit                   -> unit
    val sexprToString : Sexpr                  -> string
    val exprToString  : Expr                   -> string
end

signature SCANNER =                    (* Contract signature *)
sig
    val stringToTokens : string -> SchemeToken list
end

signature READER =                     (* Contract signature *)
sig
    val stringToSexpr  : string -> Sexpr
    val stringToSexprs : string -> Sexpr list
end

signature TAG_PARSER =                 (* Contract signature *)
sig
    val stringToPE  : string -> Expr
    val stringToPEs : string -> Expr list
end

signature SEMANTIC_ANALYSIS =          (* Contract signature *)
sig
    val annotateTC        : Expr -> Expr
    val lexicalAddressing : Expr -> Expr
end

signature CODE_GEN =                   (* Contract signature *)
sig
    val cg                : Expr list       -> string
    val compileSchemeFile : string * string -> unit
end

structure Aux : AUX =
struct

(* Function creates unique string for unused yet symbol name *)
  val a = ref 0                                              (* counter *)
  local fun iter(x, _) = "G_" ^ x ^ "_" ^ Int.toString(!a)   (* create string *)
  in
     fun gensym x = iter(x, a := (!a) + 1)                   (* increment *)
  end (* 'gensym' uses 'iter' *)

(* Function makes "Sexpr list by pairs" from Sexpr list.
 * using tail argument as last element.
 * Meaning if tail is Nil - proper list, else improper list.
 * pairs([], Nil) = Nil.
 *)
  fun pairs(x::y, tail) = Pair(x, pairs(y, tail))
    | pairs(_,    tail) = tail

(* Function checks if string list is a set *)
  fun isSet(x::y) = (not (List.exists (fn a:string => x=a) y)) andalso isSet y
    | isSet _     = true

(* Function finds first appearance in the list *)
  local fun iter(_, [])       = NONE
          | iter(c, true::x)  = SOME c
          | iter(c, false::x) = iter(c+1, x)
  in
      fun findinlist(elem, alist) = iter(0, map (fn x:string => x=elem) alist)
  end (* 'findinlist' uses 'iter' *)

(* Function for debugging - prints the first argument in debugging mode if the
 * second argument is true *)
  local
  fun checkchar [] = []
    | checkchar (#"\n"::rest) = #" "::(checkchar rest)
    | checkchar (#"\\"::(#"\""::rest)) = #"\\"::(#"\""::(checkchar rest)) 
    | checkchar (#"\""::rest) = #"\\"::(#"\""::(checkchar rest))
    | checkchar (x::rest) = x::(checkchar rest)
  in
      fun printcode (text, cond) =
    "#if DO_SHOW==1"                                               ^ "\n" ^
    "if (" ^ (if cond = "" then "1" else cond) ^ ")"               ^ "\n" ^
    "printf(\"" ^ implode(checkchar(explode text)) ^ "\\n\");"     ^ "\n" ^
    "#endif"                                                       ^ "\n"
  end (* 'printcode' uses 'checkchar' *)
  
(* Function to read whole file into single string *)
  fun stringOfFile (filename) =
      let val f = TextIO.openIn filename
          val ans:string = TextIO.inputAll f
          fun iter _ = ans
      in 
          iter(TextIO.closeIn f)
      end (* 'stringOfFile' *)

(* Function to write input string to file, not by appending *)
  fun stringToFile (filename, inp) =
      let val f = TextIO.openOut filename
          fun iter _ = TextIO.closeOut f
      in
          iter(TextIO.output(f, inp))
      end (* 'stringToFile' *)

(* Function to execute any sml command line and exit immediately,
 * for 'make' command usage *)
  local
        fun exit _ = ()
  in
     fun run _ = exit(OS.Process.exit(OS.Process.success))
  end (* 'run' uses 'exit' *)

(* sexprToString and exprToString functions.
 * Converts Sexpr and Expr to scheme language.
 * Programmer: Mayer Goldberg, 2011
 *)
  
  fun sexprToString Nil = "()"
    | sexprToString Void = "(if #f #f)"
    | sexprToString (Bool true) = "#t"
    | sexprToString (Bool false) = "#f"
    | sexprToString (Char ch) = "#" ^ (if ord ch > 31
                                          then "\\" ^ str ch
                                          else case ch
      of (#"\n"| #"\t"| #"\r"| #"\f") => Char.toString ch
       | _ => "\\0" ^ str(chr(48 + ord ch div 8)) ^ str(chr(48 + ord ch mod 8)))
    | sexprToString (Symbol sym) = sym
    | sexprToString (String str) = "\"" ^ str ^ "\""
    | sexprToString (Number n) = if n<0 then "-" ^ Int.toString(~ n)
                                        else Int.toString n
    | sexprToString (Vector []) = "#()"
    | sexprToString (Vector (x::y)) = "#(" ^ sexprToString x ^
             (foldr (fn (a, b) => " " ^ a ^ b) "" (map sexprToString y)) ^ ")"
    | sexprToString (Pair (x, y)) = "(" ^ sexprToString x ^ cdrToString y ^ ")"
  
  and cdrToString Nil = ""
    | cdrToString (Pair (x, y)) = " " ^ sexprToString x ^ cdrToString y
    | cdrToString y = " . " ^ sexprToString y

  fun exprToString (Const Nil) = "'()"
    | exprToString (Const (e as (Symbol _|Pair _|Vector _))) =
                                                          "'" ^ sexprToString e
    | exprToString (Const sexpr) = sexprToString sexpr
    | exprToString (Var v) = v
    | exprToString (VarFree v) = v
    | exprToString (VarParam v) = #1 v
    | exprToString (VarBound v) = #1 v
    | exprToString (If (test, dit, Const Void)) = "(if " ^ (exprToString test)
                                               ^ " " ^ (exprToString dit) ^ ")"
    | exprToString (If (test, dit, dif)) = "(if " ^ (exprToString test) ^ " " ^
          (exprToString dit) ^ " " ^ (exprToString dif) ^ ")"
    | exprToString (Abs (vars, expr)) = "(lambda (" ^ (foldr (fn (a, b) => a ^
          " " ^ b) "" vars) ^ ") " ^ (exprToString expr) ^ ")"
    | exprToString (AbsOpt (vars, opt, expr)) = "(lambda (" ^ (foldr (fn (a, b)
          => a ^ " " ^ b) "" vars) ^ ". " ^ opt ^ ") " ^ (exprToString expr) ^
          ")"
    | exprToString (AbsVar (var, expr)) = "(lambda " ^ var ^ " " ^
          (exprToString expr) ^ ")"    
    | exprToString (App (proc, argl)) = "(" ^ (exprToString proc) ^ (foldr (fn
          (a, b) => " " ^ a ^ b) "" (map exprToString argl)) ^ ")"
    | exprToString (AppTP (proc, argl)) = "(" ^ (exprToString proc) ^ (foldr (fn
          (a, b) => " " ^ a ^ b) "" (map exprToString argl)) ^ ")"
    | exprToString (Seq []) = ""
    | exprToString (Seq [e]) = exprToString e
    | exprToString (Seq (a :: s)) = "(begin " ^ (exprToString a) ^ (foldr (fn
          (a, b) => " " ^ a ^ b) "" (map exprToString s)) ^ ")"
    | exprToString (Or []) = "#f"
    | exprToString (Or [e]) = exprToString e
    | exprToString (Or (a :: s)) = "(or " ^ (exprToString a) ^ (foldr (fn (a, b)
          => " " ^ a ^ b) "" (map exprToString s)) ^ ")"
    | exprToString (Def (var, expr)) = "(define " ^ (exprToString var) ^ " " ^
         (exprToString expr) ^ ")" 
end     (* of structure Aux *)

structure Scanner : SCANNER = 
struct
(* Scanner takes string input and converts it to tokens list *)
 exception scanner_unclosedString
 exception scanner_wrongChar
 exception scanner_wrongHash
 exception scanner_wrongSymbol

 local (* Many local functions in state machine *) 
(* Initial state - make a move in accordance with input list prefix *)
 fun evaluate []                           = [] (* No rest, single exit state *)
   | evaluate (#"("              ::rest)   = [LparenToken]       @ evaluate rest
   | evaluate (#")"              ::rest)   = [RparenToken]       @ evaluate rest
   | evaluate (#"'"              ::rest)   = [QuoteToken]        @ evaluate rest
(* Macro expansion arguments ... in scheme token, as you asked *)
   | evaluate (#"."::(#"."::(#"."::rest))) = [SymbolToken "..."] @ evaluate rest
   | evaluate (#"."              ::rest)   = [DotToken]          @ evaluate rest
   | evaluate (#"#"              ::rest)   = evaluate_hash rest   (* # state *)
(* '"' begins a string *)
   | evaluate (#"\""             ::rest)   = evaluate_string("", rest)
   | evaluate ((#"\t"| #"\f"| #" "| #"\r"| #"\n")::rest) = evaluate rest
   | evaluate (x::rest)                    = if (ord x = 59)
              (* Comments *)                    then skip rest  
              (* Integer&Symbol state *)        else evaluate_else([], x::rest)
 
(* Comments state - skip until end of line *)
 and skip (#"\n"::rest) = evaluate rest
   | skip (a::rest)     = skip rest
   | skip _             = evaluate []  (* end of input - to single exit state *)

(* '#' state for boolean and characters *)
 and evaluate_hash (x::rest) = (case x of
        (#"t"| #"T") => [BoolToken true]  @ evaluate rest
      | (#"f"| #"F") => [BoolToken false] @ evaluate rest
      |  #"("        => [VectorToken]     @ evaluate rest
      |  #"\\"       => let val prefix = implode(map Char.toLower (List.take
                                         (rest, Int.min(List.length rest, 7))))
                        in        (* lower cased input top string to compare *)
                            case (List.filter
                                 (fn (aword, _) => String.isPrefix aword prefix)
                              [("tab", #"\t"), ("page", #"\f"), ("space", #" "),
                                      ("return", #"\r"), ("newline", #"\n")]) of
                             [(aword, c)] => [CharToken c]
                                        @ evaluate(List.drop(rest, size aword))
                                | _       => eval_char rest
                        end
      | _            =>        raise scanner_wrongHash)
   | evaluate_hash _         = raise scanner_wrongHash

(* One character state, octal or regular.
 * 3504 is 48*64+48*8+48, 48 is "0" ASCII. *)
 and eval_char (c1::(c2::(c3::rest))) = if      (c1 >= #"0") andalso (c1 < #"4")
                                        andalso (c2 >= #"0") andalso (c2 < #"8")
                                        andalso (c3 >= #"0") andalso (c3 < #"8")
                                 then [CharToken(chr((ord c1) * 64 + (ord c2) *
                                            8 + ord c3 - 3504))] @ evaluate rest
                                 else [CharToken c1] @ evaluate(c2::(c3::rest))
   | eval_char (c::rest)              = [CharToken c] @ evaluate rest
   | eval_char _                      = raise scanner_wrongChar 

(* String evaluating state, stops on '"'
 * First argument is string accumulator. *)
 and evaluate_string (x, #"\""::rest) = [StringToken x] @ evaluate rest
   | evaluate_string (x, #"\\"::rest) = eval_backslash(x, rest)
   | evaluate_string (x, y    ::rest) = evaluate_string(x ^ str y, rest)
   | evaluate_string _                = raise scanner_unclosedString

(* Backslash in string state.
 * If special character - put it.
 * First argument is string accumulator. *)
 and eval_backslash (x, c::rest) = (case(Char.fromString(implode [#"\\", c])) of
                                 SOME ans => evaluate_string (x ^ str ans, rest)
                               | _        => raise scanner_wrongChar)
   | eval_backslash _            = raise scanner_unclosedString

(* Integer and symbol state
 * If delimiter - to choose state,
 * else accumulate character and continue this state.
 * First argument is char list accumulator. *)
 and evaluate_else (x, rest as (#"\t"| #"\f"| #" "| #"\r"| #"\n"| #"'"| #"\""|
                           #"("| #")"| #"."| #"#")::_) = choose(x, rest)
   | evaluate_else (x, a::rest) = if (ord a = 59)
                                  then choose(x, a::rest)
                                  else evaluate_else(x @ [Char.toLower a], rest)
   | evaluate_else x            =       choose x

(* Choosing between integer and symbol state.
 * If just digits with maybe first "-" - integer.
 * Else if not begins with digit - symbol.
 * Else error.
 * Special case for choose "-".
 *)
 and choose (x, rest) = (case (List.filter (fn c => (not (Char.isDigit c)))
                                 (if (hd x) = #"-" then (tl x) else x)) of
                        [] => (case (Int.fromString (implode x)) of
                                  SOME ans => [IntToken ans]    @ evaluate rest
                                | NONE     => [SymbolToken "-"] @ evaluate rest)
                      | _  => if (Char.isDigit (hd x))
                                 then raise scanner_wrongSymbol
                                 else [SymbolToken(implode x)] @ evaluate rest)
 in
(* Main signature function - uses char list converted from string *)
     fun stringToTokens a = evaluate(explode a)
 end (* 'stringToTokens' uses many functions *)
end (* of structure Scanner *)

structure Reader : READER = 
struct
(* Reader takes Scanner output tokens list
 * and converts it to Symbol expression, one or more,
 * in accordance of user need.
 *)
 exception reader_wrongList
 exception reader_unclosedVector
 exception reader_wrongDot
 exception reader_unbalancedParentheses
 exception reader_wrongQuote
 exception ErrorMoreThanOneSexpr

(* Two local mutual recursive functions.
 * The first function evaluates one Sexpr and returns 2-element tuple option
 * of Sexpr and tokens list rest or NONE if end of tokens list.
 * We could make this without option as just tuple.
 * The program could be a little smaller, but much less readable,
 * require to take care for many situations,
 * and it will be harder to add new Sexpr kinds.
 * The function fails if there are ")" or "." at input top.
 * Quote: find next Sexpr and quote it.
 * Vector: find all Sexprs and care for ")" at the end.
 * Proper list: find all Sexprs till ")".
 * Improper list: find all Sexprs till ".".
 * Take care for at least one before ".",
 * and exactly one and ")" after.
 *)
local fun eval_one ((IntToken    x)::rest) = SOME(Number x, rest)
        | eval_one ((CharToken   x)::rest) = SOME(Char   x, rest)
        | eval_one ((StringToken x)::rest) = SOME(String x, rest)
        | eval_one ((SymbolToken x)::rest) = SOME(Symbol x, rest)
        | eval_one ((BoolToken   x)::rest) = SOME(Bool   x, rest)
        | eval_one (RparenToken    :: _  ) = raise reader_unbalancedParentheses
        | eval_one (DotToken       :: _  ) = raise reader_wrongDot
        | eval_one (QuoteToken     ::rest) = (case (eval_one rest) of
             SOME(ans, rest1) => SOME(Aux.pairs([Symbol "quote", ans], Nil),
                                                                          rest1)
           | NONE             => raise reader_wrongQuote)
        | eval_one (VectorToken    ::rest) = (case eval_all([], rest) of
             (ans, RparenToken::rest1) => SOME(Vector ans, rest1)
           | _                         => raise reader_unclosedVector)
        | eval_one (LparenToken    ::rest) = (case eval_all([], rest) of
             (ans, RparenToken::rest1) => SOME(Aux.pairs(ans, Nil), rest1)
           | (a::s, DotToken  ::rest1) => (case (eval_one rest1) of
                SOME(ans1, RparenToken::rest2) => SOME(Aux.pairs(a::s, ans1),
                                                                          rest2)
              | _                              => raise reader_wrongList)
           | _                                 => raise reader_wrongList)
        | eval_one _                       = NONE
 
(* The second function evaluates all Sexprs at input top
 * until ")", "." or end of input.
 * It returns 2-element tuple of
 * evaluated Sexpr list and the rest of the input.
 * So first argument is just Sexpr list accumulator.
 *)
       and eval_all (x, RparenToken::rest) = (x, RparenToken::rest)
         | eval_all (x, DotToken::rest)    = (x, DotToken::rest)
         | eval_all (x, rest)              = (case (eval_one rest) of
             NONE             =>             (x, [])
           | SOME(ans, rest1) =>             eval_all(x @ [ans], rest1))
 in
(* Two main functions of reader.
 * The first one evaluates one Sexpr and errors if there are more
 * in case when second result option answer element isn't empty.
 *)
    fun stringToSexpr e  = (case eval_one(Scanner.stringToTokens e) of
        NONE          => Void
      | SOME(ans, []) => ans
      | _             => raise ErrorMoreThanOneSexpr)

(* The second one evaluates all Sexprs from the input
 * and errors if there are unused ")" or "." at the end.
 * Because eval_all stops properly at these two tokens or end of input,
 * the last means proper input.
 *)
    and stringToSexprs e = (case eval_all([], Scanner.stringToTokens e) of
        (_, RparenToken::_) => raise reader_unbalancedParentheses
      | (_, DotToken   ::_) => raise reader_wrongDot
      | (ans, _)            => ans)
 end (* 'stringToSexpr' and 'stringToSexprs' uses 'eval_one' and 'eval_all' *)
end (* of structure Reader *)

structure TagParser : TAG_PARSER = 
struct
(* Tag Parser takes Sexpression and creates tagged expression *)
exception parser_wrongIf
exception parser_wrongBegin
exception parser_wrongLambda
exception parser_wrongOr
exception parser_wrongDefine
exception parser_wrongNil
exception parser_wrongLet
exception parser_wrongAnd
exception parser_wrongSequence
exception parser_wrongQuote
exception parser_wrongApp
exception parser_wrongCond
exception parser_redefineParam

local
(* Main TagParser local function *)
(* "Preprocessor" *)
fun eval_one(Pair(Symbol("let"|"let*"|"letrec"), d as Pair(Nil, x))) =
                      eval_one(Pair(Symbol "lambda", d))     (* No parameters *)

  | eval_one(Pair(Symbol "let", Pair(x, Pair y))) =             (* Let expand *)
                                       let val (vars, vals) = takeforlet x in
             eval_one(Pair(Pair(Symbol "lambda", Pair(vars, Pair y)), vals)) end
  
  | eval_one(Pair(Symbol "let*", Pair(Pair(x, Pair y), Pair z))) = 
     (* let* iter *)    eval_one(Pair(Symbol "let", Pair(Pair(x, Nil), Pair(Pair
                                  (Symbol "let*", Pair(Pair y, Pair z)), Nil))))
  | eval_one(Pair(Symbol "let*", x)) = eval_one(Pair(Symbol "let", x))
  
  | eval_one(Pair(Symbol "letrec", Pair(x, body))) =        (* Letrec expand *) 
                                         let val (vars, vals) = takeforlet x
                                             val symb = Aux.gensym "letrec"  in
     eval_one(Pair(Pair(Symbol "lambda", Pair(Symbol "fs", Pair(Pair(Symbol
     "let", Pair(Pair(Pair(Symbol "ms", Pair(Pair(Symbol "map", Pair(Pair(Symbol
     "lambda", Pair(Pair(Symbol "fi", Nil), Pair(Pair(Symbol "lambda", Pair
     (Symbol "ms", Pair(Pair(Symbol "apply", Pair(Symbol "fi", Pair(Pair (Symbol
     "map", Pair(Pair(Symbol "lambda", Pair(Pair(Symbol "mi", Nil), Pair(Pair
     (Symbol "lambda", Pair(Symbol "args", Pair(Pair(Symbol "apply", Pair(Pair
     (Symbol "apply", Pair(Symbol "mi", Pair(Symbol "ms", Nil))), Pair(Symbol
     "args", Nil))), Nil))), Nil))), Pair(Symbol "ms", Nil))), Nil))), Nil))),
     Nil))), Pair(Symbol "fs", Nil))), Nil)), Nil), Pair(Pair(Symbol "apply",
     Pair(Pair(Symbol "car", Pair(Symbol "ms", Nil)), Pair(Symbol "ms", Nil))),
     Nil))), Nil))), Aux.pairs(map (fn x => Pair(Symbol "lambda", Pair(Pair
     (Symbol symb, vars), Pair(x, Nil)))) ((Pair(Symbol "begin", body))::
     (openSexpr(vals))), Nil)))                                             end
  
  | eval_one(Symbol("let"|"let*"|"letrec")|Pair(Symbol("let"|"letrec"), _)) =
                                                           raise parser_wrongLet

  | eval_one(Pair(Symbol "lambda", Pair(x, (d as            (* Nested define *)
                             Pair(Pair(Symbol("define"|"begin"), _), _))))) =
                           (case (redefine(Nil, Aux.pairs(seqOpen([], d), Nil)))
         of (Nil, a) => eval_one(Pair(Symbol "lambda", Pair(x, Pair(Void, d))))
          | a        => eval_one(Pair(Symbol "lambda", Pair(x,
                                    Pair(Pair(Symbol "letrec", Pair a), Nil)))))
  
  | eval_one(Pair(Symbol "and", x)) = expandand(prepare x)
  | eval_one(Symbol "and") = raise parser_wrongAnd

  | eval_one(Pair(Symbol "cond", Nil)) = eval_one Void        (* Cond expand *)
  | eval_one(Pair(Symbol "cond", Pair(Pair(Symbol "else", Pair y), z))) = #1
      (eval_one(Pair(Symbol "begin", Pair y)), eval_one(Pair(Symbol "cond", z)))
  | eval_one(Pair(Symbol "cond", Pair(Pair(x, Pair y), z))) =
                    eval_one(Pair(Symbol "if", Pair(x, Pair(Pair(Symbol "begin",
                           Pair y), Pair(Pair(Symbol "cond", z), Nil)))))
  | eval_one(Symbol("cond"|"else")|Pair(Symbol("cond"|"else"), _)) =
                                                          raise parser_wrongCond

  | eval_one(Pair(Symbol "if", Pair(x, Pair(y, Nil)))) =  (* If-then *)
                  eval_one(Pair(Symbol "if", Pair(x, Pair(y, Pair(Void, Nil)))))

(* "Processor" *)
  | eval_one(Pair(Symbol "if", Pair(x, Pair(y, Pair(z, Nil))))) = 
                    let val triple = (eval_one x, eval_one y, eval_one z) in
              case (#1 triple) of Const(Bool false) => #3 triple
                                | Const _           => #2 triple
                                |  _                => If triple
                                end                      (* Constant folding *)
  | eval_one(Symbol "if"|Pair(Symbol "if", _)) = raise parser_wrongIf

  | eval_one(Pair(Symbol "begin", x)) =                  (* Nested sequences *)
                        beginify(drop(map eval_one (seqOpen ([], x))))
  | eval_one(Symbol "begin") = raise parser_wrongBegin

  | eval_one(Pair(Symbol "lambda", Pair(Nil, Pair y))) =    (* Lambdas kinds *)
                           Abs([], eval_one(Pair(Symbol "begin", Pair y)))
  | eval_one(Pair(Symbol "lambda", Pair(Symbol x, Pair y))) = 
                           AbsVar(x, eval_one(Pair(Symbol "begin", Pair y)))
  | eval_one(Pair(Symbol "lambda", Pair(Pair x, Pair y))) =
          (case prepstr([], Pair x)
         of (A, NONE)   => Abs(A, eval_one(Pair(Symbol "begin", Pair y)))
          | (A, SOME B) => AbsOpt(A, B, eval_one(Pair(Symbol "begin", Pair y))))
  | eval_one(Symbol "lambda"|Pair(Symbol "lambda", _)) =
                                                        raise parser_wrongLambda

  | eval_one(Pair(Symbol "or", x)) = (case (makeor(prepare x)) of
                              []  => Const (Bool false)  (* Constant folding *)
                            | [z] => z
                            |  z  => Or z)
  | eval_one(Symbol "or") = raise parser_wrongOr
  
  | eval_one(Pair(Symbol "quote", Pair(x, Nil))) = Const x       (* Constant *)
  | eval_one(Symbol "quote"|Pair(Symbol "quote", _)) = raise parser_wrongQuote

  | eval_one(Symbol("define"|"set!")|Pair(Symbol("define"|"set!"), _))
                                                      = raise parser_wrongDefine
  
  | eval_one(Pair(x, y)) = (case eval_one x of
                      Const _ => raise parser_wrongApp
                    | func    => App(func, prepare y))         (* Application *)

  | eval_one(Symbol x) = Var x                   (* Variable *)
  | eval_one Nil = raise parser_wrongNil         (* Empty list *)
  | eval_one x = Const x                         (* Constant *)

(* Separate variables and their values in the let forms *)
and takeforlet(Pair(Pair(Symbol x, Pair(y, Nil)), z)) =
                                  let val (vars, vals) = takeforlet z in
                                      (Pair(Symbol x, vars), Pair(y, vals)) end
  | takeforlet Nil = (Nil, Nil)
  | takeforlet _ = raise parser_wrongLet

(* MIT - define *)
and expanddef(Pair(Pair(Symbol x, y), z)) =
               (Symbol x, Pair(Pair(Symbol "lambda", Pair(y, z)), Nil))
  | expanddef(Pair(p as(Symbol _, Pair(_, Nil)))) = p
  | expanddef _ = raise parser_wrongDefine

(* Nested defines grouping *)
and redefine(acc, Pair(Pair(Symbol "define", y), z)) =
                       let val (newacc, newz) = redefine(acc, z) in
                  (Pair(Pair(expanddef y), newacc), newz) end
  | redefine x = x

(* Or constant folding *)
and makeor [] = []
  | makeor((Const(Bool false))::y) = makeor y
  | makeor((Const x)::_) = [Const x]
  | makeor(x::y) = x::(makeor y)

(* Sequence constant folding *)
and drop [] = []
  | drop [x] = [x]
  | drop((Const _)::y) = drop y
  | drop(x::y) = x::(drop y)

(* Simple vice optional lambda *)
and prepstr(acc, Nil)                  = if (Aux.isSet acc)
                                            then (acc, NONE)
                                            else raise parser_redefineParam
  | prepstr(acc, Symbol x)             = if (Aux.isSet(x::acc))
                                            then (acc, SOME x)
                                            else raise parser_redefineParam
  | prepstr(acc, Pair(Symbol x, rest)) = prepstr(acc@[x], rest)
  | prepstr _                          = raise parser_wrongLambda

(* And constant folding *)
and expandand [] = Const(Bool true)
  | expandand [x] = x
  | expandand((Const(Bool false))::rest) = Const (Bool false)
  | expandand((Const _)::y)= expandand y
  | expandand(x::y) = If(x, expandand y, Const (Bool false))

(* Convert Sexpr list to ml one *)
and openSexpr(Pair(x, Pair y)) = x::(openSexpr(Pair y))
  | openSexpr(Pair(x, Nil)) = [x]
  | openSexpr Nil = []
  | openSexpr _ = raise parser_wrongSequence

(* Nested begin *)
and seqOpen(acc, Pair(Pair(Symbol "begin", x), y)) =
                                                seqOpen(acc@(seqOpen([], x)), y)
  | seqOpen(acc, Pair(x, y)) = seqOpen(acc@[x], y)
  | seqOpen(acc, Nil) = acc
  | seqOpen _ = raise parser_wrongSequence

(* Convert little "begin"s *)
and beginify []  = Const Void
  | beginify [x] = x
  | beginify  x  = Seq x

(* Sexpr list parsing *)
and prepare x = map eval_one (openSexpr x)

(* Entrance to the parser for inner define disabling *)
and open_eval(Pair(Symbol "define", a)) = let val (Symbol x, y) = expanddef a
                            in Def(Var x, eval_one(Pair(Symbol "begin", y))) end
  | open_eval(Pair(Symbol "begin", x)) =
                                  beginify(drop(map open_eval (seqOpen([], x))))
  | open_eval x = eval_one x

in
(* Signature binded parser functions, for Sexpr and Sexpr list *)
   fun stringToPE  e = open_eval(Reader.stringToSexpr e)
   fun stringToPEs e = map open_eval (Reader.stringToSexprs e)
end (* 'stringToPE' and 'stringToPEs' uses many functions *)
end (* of structure TagParser *)

structure SemanticAnalysis : SEMANTIC_ANALYSIS =
struct
(* Semantic analysis finds tail position application
 * and variable lexical addressing *)

(* Tail position application converting.
 * Recursive check of expressions.
 * The second argument is flag which declares if there could be tail position *)
local fun eval(If(a, b, c), flag) =
                                If(eval(a, false), eval(b, flag), eval(c, flag))
        (* New environment always opens an option for tail position *)
        | eval(Abs(a, b), _) = Abs(a, eval(b, true))
        | eval(AbsOpt(a, b, c), _) = AbsOpt(a, b, eval(c, true))
        | eval(AbsVar(a, b), _) = AbsVar(a, eval(b, true))
        (* Found *)
        | eval(App(a, b), true) = AppTP(eval(a, false), eval_all(b, false))
        | eval(App(a, b), false) = App(eval(a, false), eval_all(b, false))
        | eval(Seq a, flag) = Seq(eval_seq(a, flag))
        | eval(Or a, flag) = Or(eval_seq(a, flag))
        | eval(Def(a, b), _) = Def(a, eval(b, false))
        | eval(a, _) = a

      and eval_all(x, flag) = map (fn a => eval(a, flag)) x           (* list *)

      (* In "sequence" and "or" all but last must not be in tail position *)
      and eval_seq([], _)      = []
        | eval_seq([x], true)  = [eval(x, true)]
        | eval_seq(x::y, flag) = (eval(x, false))::(eval_seq(y, flag))

in
    (* Main expression could have tail positions *)
    fun annotateTC e = eval(e, true)  
end (* 'annotateTC' uses 'eval', 'eval_all' and 'eval_seq' *)

(* Lexical addressing evaluating.
 * Recursive check of expressions. *)
local fun eval(Var x, env) = (case (getvar(x, env, ~1)) of         (* convert *)
                  NONE        => VarFree x
                | SOME(~1, a) => VarParam(x, a)
                | SOME(l, a)  => VarBound(x, l, a))
        | eval(If(a, b, c), env) = If(eval(a, env), eval(b, env), eval(c, env))
                               (* In lambda we need to extend the environment *)
        | eval(Abs(params, body), env) = Abs(params, eval(body, params::env))
        | eval(AbsOpt(params, param, body), env) =
                        AbsOpt(params, param, eval(body, (params@[param])::env))
        | eval(AbsVar(param, body), env) =
                                         AbsVar(param, eval(body, [param]::env))
        | eval(App(func, body), env) = App(eval(func, env), eval_all(body, env))
        | eval(AppTP(func, body), env) =
                                     AppTP(eval(func, env), eval_all(body, env))
        | eval(Or x, env) = Or(eval_all(x, env))
        | eval(Def(Var x, y), env) = Def(VarFree x, eval(y, env))
        | eval(Seq a, env) = Seq(eval_all(a, env))
        | eval(x, _) = x
      
      and eval_all(exprs, env) = map (fn x => eval(x, env)) exprs     (* list *)
      
      (* Find variable in string list list *)
      and getvar(x, lay::env, c) = (case (Aux.findinlist(x, lay)) of
                    NONE     => getvar(x, env, c+1)
                  | SOME num => SOME(c, num))
        | getvar _ = NONE
      in
      (* The environment is empty for now *)
          fun lexicalAddressing e = eval(e, [])
      end (* 'lexicalAddressing' uses 'eval', 'eval_all' and 'getvar' *)
end (* of structure SemanticAnalysis *)

structure CodeGen : CODE_GEN =
struct
(* Compiler code generator.
 * Creates executable file from scheme text file.
 *)
exception codeGen_Nonsens

(* The list of all primitive scheme functions -
 * those which were implfemented in assembly.
 * This list exists because we want to constrain all these primitives
 * to be always in the same place in the symbol table,
 * for assembly procedures proper loading.
 * integer? and number? will get the same procedure.
 *)
    val primitives = ["+", "-", "*", "/", "=", "<", ">", "apply", "boolean?",
         "car", "cdr", "char->integer", "char?", "cons", "assembly_eq?",
         "integer?", "integer->char", "make-string", "make-vector", "null?",
         "number?", "pair?", "procedure?", "remainder", "set-car!", "set-cdr!",
         "string->symbol", "string-length", "string-ref", "string-set!",
         "string?", "symbol?", "symbol->string", "vector-length", "vector-ref",
         "vector-set!", "vector?", "void?", "write", "zero?"]

(* Code generator main local function for translating Expr parsing tree
 * to assembly code.
 * 
 * Void, Nil, false, true - just take accordant singleton scheme object.
 * Number - create new scheme object with the number
 * (or with opposite of negative number,
 * because of sml '~' negative numbers printing).
 * Char - create new scheme object with the char in it.
 *)

                              local fun evaluate (Const Void, st) = ("\n" ^ 
    "MOV(R0, SOB_VOID);"                                           ^ "\n", st)

                                       | evaluate (Const Nil, st) = ("\n" ^  
    "MOV(R0, SOB_NIL);"                                            ^ "\n", st)

                               | evaluate (Const(Bool false), st) = ("\n" ^ 
    "MOV(R0, SOB_FALSE);"                                          ^ "\n", st)

                                | evaluate (Const(Bool true), st) = ("\n" ^ 
    "MOV(R0, SOB_TRUE);"                                           ^ "\n", st)

                                            | evaluate (Const(Number x), st) =
                                 let val numb = if x < 0
                                                   then "-" ^  Int.toString(~x)
                                                   else Int.toString x 
                                                                 in ("\n" ^ 
    "PUSH(IMM(" ^ numb ^ "));"                                     ^ "\n" ^
    "CALL(MAKE_SOB_INTEGER);"                                      ^ "\n" ^
    "DROP(1);"                                                     ^ "\n", st)
                                                                             end

                                   | evaluate (Const(Char x), st) = ("\n" ^
    "PUSH(IMM(" ^ printchar x ^ "));"                              ^ "\n" ^
    "CALL(MAKE_SOB_CHAR);"                                         ^ "\n" ^
    "DROP(1);"                                                     ^ "\n", st)

(* Pair, Vector and String constants should be evaluated in the initial part of
 * the program. So we allocate special place at top part of the memory and
 * use for each constant like that as one cell with pointer to it.
 * We always know in advance which cell is associated with each constant,
 * because the place in this table is allocated for the constants
 * by the order of their appearance in the scheme program.
 * The memory allocation at the memory top works just as regular 'malloc',
 * in opposite direction.
 *)
                                       | evaluate (Const(Pair(x, y)), st) = 
             let val (sx, stx) = evaluate(Const x, st)
                 val (sy, (stsymb, stconst, constnum)) = evaluate(Const y, stx)
                 val init =                                          "\n" ^
    "PUSH(R1);"                                                    ^ "\n" ^
    sy                                                             ^ "\n" ^
    "PUSH(R0);"                                                    ^ "\n" ^
    sx                                                             ^ "\n" ^
    "PUSH(R0);"                                                    ^ "\n" ^
    "CALL(MAKE_SOB_PAIR);"                                         ^ "\n" ^
    "DROP(2);"                                                     ^ "\n" ^
    "MOV(R1, IMM(TOP));"                                           ^ "\n" ^
    "SUB(R1, IMM(" ^ Int.toString constnum ^ "));"                 ^ "\n" ^
    "MOV(IND(R1), R0);"                                            ^ "\n" ^
    "POP(R1);"                                                     ^ "\n"
                                                                 in ("\n" ^
    "MOV(R0, IMM(TOP));"                                           ^ "\n" ^
    "SUB(R0, IMM(" ^ Int.toString constnum ^ "));"                 ^ "\n" ^
    "MOV(R0, IND(R0));"                                            ^ "\n",
                                    (stsymb, stconst ^ init, constnum + 1)) end

                                  | evaluate (Const(Vector x), st) = let 
       val (sx, (stsymb, stconst, constnum)) = evalparams(map Const (rev x), st)
       val init =                                                   "\n" ^
    "PUSH(R1);"                                                    ^ "\n" ^
    sx                                                             ^ "\n" ^
    "PUSH(IMM(" ^ Int.toString(length x) ^ "));"                   ^ "\n" ^
    "CALL(MAKE_SOB_VECTOR);"                                       ^ "\n" ^
    "DROP(" ^ Int.toString(1 + length x) ^ ");"                    ^ "\n" ^
    "MOV(R1, IMM(TOP));"                                           ^ "\n" ^
    "SUB(R1, IMM(" ^ Int.toString constnum ^ "));"                 ^ "\n" ^
    "MOV(IND(R1), R0);"                                            ^ "\n" ^
    "POP(R1);"                                                     ^ "\n"
                                                                 in ("\n" ^
    "MOV(R0, IMM(TOP));"                                           ^ "\n" ^
    "SUB(R0, IMM(" ^ Int.toString constnum ^ "));"                 ^ "\n" ^
    "MOV(R0, IND(R0));"                                            ^ "\n",
                                    (stsymb, stconst ^ init, constnum + 1)) end

                    | evaluate (Const(String x), (stsymb, stconst, constnum)) =
                 let val init =                                      "\n" ^
    "PUSH(R1);"                                                    ^ "\n" ^
    makestring(explode x)                                          ^ "\n" ^
    "PUSH(IMM(" ^ Int.toString(size x) ^ "));"                     ^ "\n" ^
    "CALL(MAKE_SOB_STRING);"                                       ^ "\n" ^
    "DROP(" ^ Int.toString(1 + size x) ^ ");"                      ^ "\n" ^
    "MOV(R1, IMM(TOP));"                                           ^ "\n" ^
    "SUB(R1, IMM(" ^ Int.toString constnum ^ "));"                 ^ "\n" ^
    "MOV(IND(R1), R0);"                                            ^ "\n" ^
    "POP(R1);"                                                     ^ "\n"
                                                                 in ("\n" ^
    "MOV(R0, IMM(TOP));"                                           ^ "\n" ^
    "SUB(R0, IMM(" ^ Int.toString constnum ^ "));"                 ^ "\n" ^
    "MOV(R0, IND(R0));"                                            ^ "\n",
                                    (stsymb, stconst ^ init, constnum + 1)) end

(* We use symbol table for symbols and free variables.
 * The table was placed at the top of the memory, just after the constant table.
 * So it can grow downward as needed.
 * The exact place of symbol table beginning is calculated using the length
 * of the constant table - the CONSTNUM macro is created after the single code
 * generator pass and glued at the top of the file.
 * VarParams are taken from the stack.
 * VarBounds are taken using the environments model, by two jumps.
 *)
                                            | evaluate (Const(Symbol x), st) =
                                              let val (stt, numb) = exp(st, x)
                                                                 in ("\n" ^ 
    "MOV(R0, IMM(TOP));"                                           ^ "\n" ^
    "SUB(R0, CONSTNUM);"                                           ^ "\n" ^
    "SUB(R0, IMM(" ^ Int.toString numb ^ "));"                     ^ "\n" ^
    "PUSH(IND(R0));"                                               ^ "\n" ^
    "CALL(MAKE_SOB_SYMBOL);"                                       ^ "\n" ^
    "DROP(1);"                                                     ^ "\n", stt)
                                                                            end

                                         | evaluate (Def(VarFree x, y), st) =
                                           let val (ey, sty) = evaluate (y, st)
                                               val (stt, numb) = exp(sty, x)
                                                                 in ("\n" ^ 
    "PUSH(R1);"                                                    ^ "\n" ^
    ey                                                             ^ "\n" ^
    "MOV(R1, IMM(TOP));"                                           ^ "\n" ^
    "SUB(R1, IMM(" ^ Int.toString numb ^ "));"                     ^ "\n" ^
    "SUB(R1, CONSTNUM);"                                           ^ "\n" ^
    "MOV(R1, IND(R1));"                                            ^ "\n" ^
    "MOV(VAL(R1), R0);"                                            ^ "\n" ^
    "MOV(R0, SOB_VOID);"                                           ^ "\n" ^
    "POP(R1);"                                                     ^ "\n", stt)
                                                                            end
   
                                          | evaluate ((Def _|Var _), _) =
                                            raise codeGen_Nonsens
                                          
                                          | evaluate (VarFree x, st) =
                                              let val (stt, numb) = exp(st, x)
                                                                 in ("\n" ^ 
    "MOV(R0, IMM(TOP));"                                           ^ "\n" ^
    "SUB(R0, CONSTNUM);"                                           ^ "\n" ^
    "SUB(R0, IMM(" ^ Int.toString numb ^ "));"                     ^ "\n" ^
    "MOV(R0, IND(R0));"                                            ^ "\n" ^
    "CMP(VAL(R0), IMM(0));"                                        ^ "\n" ^
    "JUMP_EQ(VAR_UNDEF);"                                          ^ "\n" ^
    "MOV(R0, VAL(R0));"                                            ^ "\n", stt)
                                                                            end

                                 | evaluate (VarParam(_, mi), st) = ("\n" ^ 
    "MOV(R0, FPARG(" ^ Int.toString(mi + 2) ^ "));"                ^ "\n", st)

                             | evaluate (VarBound(_, ma, mi), st) = ("\n" ^ 
    "MOV(R0, INDD(FPARG(0), " ^ Int.toString(ma + 1) ^ "));"       ^ "\n" ^
    "MOV(R0, INDD(R0, " ^ Int.toString mi ^ "));"                  ^ "\n", st)

(* 'If' form is evaluated by checking the condition result
 * and navigating according to it.
 * 'Or' form is evaluated in another ml procedure.
 * 'Begin' form elements are evaluated one after another,
 * the result of the last one stays as whole form result.
 *)
                                          | evaluate (If(a, b, c), st) =
                                         let val label = Aux.gensym "IF"
                                             val (ea, sta) = evaluate(a, st)
                                             val (eb, stb) = evaluate(b, sta)
                                             val (ec, stc) = evaluate(c, stb)
                                                                in  ("\n" ^ 
    ea                                                             ^ "\n" ^
    "CMP(R0, SOB_FALSE);"                                          ^ "\n" ^
    "JUMP_EQ(NO_" ^ label ^ ");"                                   ^ "\n" ^
    eb                                                             ^ "\n" ^
    "JUMP(END_" ^ label ^ ");"                                     ^ "\n" ^
    "NO_" ^ label ^ ":"                                            ^ "\n" ^
    ec                                                             ^ "\n" ^
    "END_" ^ label ^ ":"                                           ^ "\n", stc)
                                                                            end

                    | evaluate (Or x, st) = codeexpandor(x, Aux.gensym "OR", st)

                    | evaluate (Seq [], st) = ("", st)

                    | evaluate (Seq(x::y), st) =
                                     let val (ex, stx) = evaluate(x, st)
                                         val (ey, sty) = evaluate(Seq y, stx)
                                                                 in ("\n" ^ 
    ex ^ ey,                                                               sty)
                                                                            end
(* Applications - regular and tail call. *)
                    | evaluate (App(x, y), st) =
                                         let val label = Aux.gensym "APP"
                                             val (ey, sty) = evalparams(y, st)
                                             val (ex, stx) = evaluate(x, sty)
                                                                 in ("\n" ^ 
    ey                                                             ^ "\n" ^
    "PUSH(IMM(" ^ Int.toString(length y) ^ "));"                   ^ "\n" ^
    ex                                                             ^ "\n" ^
    "CMP(IND(R0), T_CLOSURE);"                                     ^ "\n" ^
    "JUMP_NE(NOT_CLOSURE);"                                        ^ "\n" ^
    "PUSH(VAL(R0));"                                               ^ "\n" ^
    "PUSH(LABEL(" ^ label ^ "));"                                  ^ "\n" ^
    "JUMPA(INDD(R0, 2));"                                          ^ "\n" ^
    label ^ ":"                                                    ^ "\n" ^
    "DROP(2);"                                                     ^ "\n" ^
    "DROP(STACK(SP));"                                             ^ "\n", stx)
                                                                             end

                                          | evaluate (AppTP(x, y), st) =
                                        let val (ey, sty) = evalparams(y, st)
                                            val (ex, stx) = evaluate(x, sty)
                                            val ly = length y
                                                                 in ("\n" ^ 
    ey                                                             ^ "\n" ^
    "PUSH(IMM(" ^ Int.toString ly ^ "));"                          ^ "\n" ^
    ex                                                             ^ "\n" ^
    "CMP(IND(R0), T_CLOSURE);"                                     ^ "\n" ^
    "JUMP_NE(NOT_CLOSURE);"                                        ^ "\n" ^
    "PUSH(VAL(R0));"                                               ^ "\n" ^
    "PUSH(FPARG(-1));"                                             ^ "\n" ^
    "PUSH(FPARG(-2));"                                             ^ "\n" ^
    "PUSH(FP);"                                                    ^ "\n" ^
    "SUB(STARG(-1), FPARG(1));"                                    ^ "\n" ^
    "ADD(STARG(-1), IMM(" ^ Int.toString ly ^ "));"                ^ "\n" ^
    "PUSH(IMM(1));"                                                ^ "\n" ^
    "PUSH(FP);"                                                    ^ "\n" ^
    "SUB(STARG(-1), IMM(4));"                                      ^ "\n" ^
    "SUB(STARG(-1), FPARG(1));"                                    ^ "\n" ^
    "PUSH(FP);"                                                    ^ "\n" ^
    "PUSH(IMM(" ^ Int.toString(4 + ly) ^ "));"                     ^ "\n" ^
    "CALL(MEMMOVE);"                                               ^ "\n" ^
    "DROP(4);"                                                     ^ "\n" ^
    "POP(SP);"                                                     ^ "\n" ^
    "POP(FP);"                                                     ^ "\n" ^
    "JUMPA(INDD(R0, 2));"                                          ^ "\n", stx)
                                                                             end
    
(* Lambdas - regular, optional and variadic.
 * The two last ones have special treatment for null arguments list case.
 *)
                                          | evaluate (Abs(x, b), st) =
                                         let val label = Aux.gensym "ABS"
                                             val (eb, stb) = evaluate(b, st)
                                                                 in ("\n" ^ 
    "JUMP(" ^ label ^ ");"                                         ^ "\n" ^
    "BODY_" ^ label ^ ":"                                          ^ "\n" ^
    "PUSH(FP);"                                                    ^ "\n" ^
    "MOV(FP, SP);"                                                 ^ "\n" ^
    "CMP(FPARG(1), IMM(" ^ Int.toString(length x) ^ "));"          ^ "\n" ^
    "JUMP_NE(WRONG_PARAMS);"                                       ^ "\n" ^
    eb                                                             ^ "\n" ^
    "MOV(SP, FP);"                                                 ^ "\n" ^
    "POP(FP);"                                                     ^ "\n" ^
    "RETURN;"                                                      ^ "\n" ^
    label ^ ":"                                                    ^ "\n" ^
    "PUSH(LABEL(BODY_" ^ label ^ "));"                             ^ "\n" ^
    "CALL(CREATE_CLOSURE);"                                        ^ "\n" ^
    "DROP(1);"                                                     ^ "\n", stb)
                                                                            end

                                          | evaluate (AbsOpt(x, _, b), st) =
                                         let val label = Aux.gensym "ABSOPT"
                                             val lx = length x
                                             val slx = Int.toString lx
                                             val (eb, stb) = evaluate(b, st)
                                                                 in ("\n" ^ 
    "JUMP(" ^ label ^ ");"                                         ^ "\n" ^
    "BODY_" ^ label ^ ":"                                          ^ "\n" ^
    "PUSH(FP);"                                                    ^ "\n" ^
    "MOV(FP, SP);"                                                 ^ "\n" ^
    "CMP(FPARG(1), IMM(" ^ slx ^ "));"                             ^ "\n" ^
    "JUMP_LT(WRONG_PARAMS);"                                       ^ "\n" ^
    "JUMP_EQ(EMPTY_" ^ label ^ ");"                                ^ "\n" ^
    "MOV(R0, FP);"                                                 ^ "\n" ^
    "SUB(R0, IMM(4));"                                             ^ "\n" ^
    "SUB(R0, FPARG(1));"                                           ^ "\n" ^
    "PUSH(R0);"                                                    ^ "\n" ^
    "MOV(R0, FPARG(1));"                                           ^ "\n" ^
    "SUB(R0, IMM(" ^ slx ^ "));"                                   ^ "\n" ^
    "PUSH(R0);"                                                    ^ "\n" ^
    "CALL(MAKE_PARAMS);"                                           ^ "\n" ^
    "DROP(2);"                                                     ^ "\n" ^
    "JUMP(CONT_" ^ label ^ ");"                                    ^ "\n" ^
    "EMPTY_" ^ label ^ ":"                                         ^ "\n" ^
    "DROP(-1);"                                                    ^ "\n" ^   
    "PUSH(IMM(-1));"                                               ^ "\n" ^
    "PUSH(FP);"                                                    ^ "\n" ^
    "PUSH(FP);"                                                    ^ "\n" ^
    "DECR(STARG(-1));"                                             ^ "\n" ^
    "PUSH(FPARG(1));"                                              ^ "\n" ^
    "ADD(STARG(-1), IMM(4));"                                      ^ "\n" ^
    "CALL(MEMMOVE);"                                               ^ "\n" ^
    "DROP(4);"                                                     ^ "\n" ^
    "INCR(FP);"                                                    ^ "\n" ^
    "INCR(FPARG(1));"                                              ^ "\n" ^
    "MOV(R0, SOB_NIL);"                                            ^ "\n" ^
    "CONT_" ^ label ^ ":"                                          ^ "\n" ^
    "MOV(FPARG(" ^ Int.toString(lx + 2) ^ "), R0);"                ^ "\n" ^
    eb                                                             ^ "\n" ^
    "MOV(SP, FP);"                                                 ^ "\n" ^
    "POP(FP);"                                                     ^ "\n" ^
    "RETURN;"                                                      ^ "\n" ^
    label ^ ":"                                                    ^ "\n" ^
    "PUSH(LABEL(BODY_" ^ label ^ "));"                             ^ "\n" ^
    "CALL(CREATE_CLOSURE);"                                        ^ "\n" ^
    "DROP(1);"                                                     ^ "\n", stb)
                                                                            end

                                          | evaluate (AbsVar(_, b), st) =
                                          let val label = Aux.gensym "ABSVAR"
                                              val (eb, stb) = evaluate(b, st)
                                                                 in ("\n" ^ 
    "JUMP(" ^ label ^ ");"                                         ^ "\n" ^
    "BODY_" ^ label ^ ":"                                          ^ "\n" ^
    "PUSH(FP);"                                                    ^ "\n" ^
    "MOV(FP, SP);"                                                 ^ "\n" ^
    "CMP(FPARG(1), IMM(0));"                                       ^ "\n" ^
    "JUMP_EQ(EMPTY_" ^ label ^ ");"                                ^ "\n" ^
    "MOV(R0, FP);"                                                 ^ "\n" ^
    "SUB(R0, IMM(4));"                                             ^ "\n" ^
    "SUB(R0, FPARG(1));"                                           ^ "\n" ^
    "PUSH(R0);"                                                    ^ "\n" ^
    "PUSH(FPARG(1));"                                              ^ "\n" ^
    "CALL(MAKE_PARAMS);"                                           ^ "\n" ^
    "DROP(2);"                                                     ^ "\n" ^
    "JUMP(CONT_" ^ label ^ ");"                                    ^ "\n" ^
    "EMPTY_" ^ label ^ ":"                                         ^ "\n" ^
    "DROP(-1);"                                                    ^ "\n" ^
    "MOV(FPARG(-3), FPARG(-2));"                                   ^ "\n" ^
    "MOV(FPARG(-2), FPARG(-1));"                                   ^ "\n" ^
    "MOV(FPARG(-1), FPARG(0));"                                    ^ "\n" ^
    "MOV(FPARG(0), IMM(1));"                                       ^ "\n" ^
    "INCR(FP);"                                                    ^ "\n" ^
    "MOV(R0, SOB_NIL);"                                            ^ "\n" ^
    "CONT_" ^ label ^ ":"                                          ^ "\n" ^
    "MOV(FPARG(2), R0);"                                           ^ "\n" ^
    eb                                                             ^ "\n" ^
    "MOV(SP, FP);"                                                 ^ "\n" ^
    "POP(FP);"                                                     ^ "\n" ^
    "RETURN;"                                                      ^ "\n" ^
    label ^ ":"                                                    ^ "\n" ^
    "PUSH(LABEL(BODY_" ^ label ^ "));"                             ^ "\n" ^
    "CALL(CREATE_CLOSURE);"                                        ^ "\n" ^
    "DROP(1);"                                                     ^ "\n", stb)
                                                                            end
                                          
(* This function expands compile time data base with new symbol.
 * The data base 'st' is transferred at all parsing tree evaluating.
 * It consists from three parts:
 * 1: symbols list - initialized with assembly primitives, as explained above.
 * At the evaluating finishing there are all static symbols set in the list,
 * so we can create the symbol table in init part of the program, before the
 * usage of the symbols, in code and even in the complicated constants creation
 * part.
 * If there exists the symbol with the same name in the list, we return the
 * same list and symbol's serial number in it, from one.
 * Otherwise we return the list with new element in the end of it, for uniform
 * list order, and the length of the input symbol list, as serial number of the
 * new element.
 * 2: constants initialization string - each complicated constant concatenates
 * its creation code at the end of this string.
 * It is just empty string at the pass start, and in its end there is the
 * constant creation code.
 * 3; constants serial number (from one). It is just 1 at the pass start,
 * increments at every complicated constant, and saved as CONSTNUM macro at the
 * end.
 *)
and exp (st as (stsymb, stconst, constnum), x) = case Aux.findinlist(x, stsymb)
         of SOME place => (st, 1 + place)
          | _          => ((stsymb @ [x], stconst, constnum), 1 + length stsymb)

(* Function for Expr list evaluating and saving in the stack,
 * as function arguments.
 *)
and evalparams ([], st) = ("", st)
  | evalparams (x::y, st) = let val (ey, sty) = evalparams(y, st)
                                val (ex, stx) = evaluate(x, sty) in
   (ey ^ ex                                                               ^
    "PUSH(R0);"                                                    ^ "\n", stx)
                                                                             end

(* Function for visual characters in the code -
 * use ascii number for special symbols and just a symbol otherwise.
 *)
and printchar x = if (ord x > 31)
                     then "'" ^ str x ^ "'"
                     else Int.toString(ord x)

(* Function for saving a string in the stack, char by char *)
and makestring [] = ""
  | makestring (x::y) =
    "PUSH(IMM(" ^ printchar x ^ "));"                              ^ "\n" ^
    makestring y
      
(* 'Or' form expanding: in all choices but the last, check if it's a false,
 * then continue to the next, else go out.
 * The last just should be evaluated.
 *)
and codeexpandor ([x], label, st) = let val (ex, stx) = evaluate(x, st) in
   (ex                                                                    ^
    label ^ ":"                                                    ^ "\n", stx)
                                                                             end
  | codeexpandor (x::y, label, st) = let val (ex, stx) = evaluate(x, st)
                                  val (ey, sty) = codeexpandor(y, label, stx) in
   (ex                                                                    ^
    "CMP(R0, SOB_FALSE);"                                          ^ "\n" ^
    "JUMP_NE(" ^ label ^ ");"                                      ^ "\n" ^
    ey,                                                                  sty)
                                                                             end
  | codeexpandor _ = raise codeGen_Nonsens

(* Function for symbol table creation - take every symbol in the list,
 * and create basket for it. At the end save primitive procedures labels.
 *)
and baskets [] =
    "CALL(LABELS);"                                                ^ "\n"
  | baskets (x::y) =                                                 "\n" ^
    makestring(rev(explode x))                                     ^ "\n" ^
    "PUSH(IMM(" ^ Int.toString(size x) ^ "));"                     ^ "\n" ^
    "CALL(MAKE_NEW_BASKET);"                                       ^ "\n" ^
    "DROP(" ^ Int.toString(1 + size x) ^ ");"                      ^ "\n" ^
    baskets y
                          
(* Function for creating code for one parsing tree in the input forest.
 * Prepares the stack, evaluates the expression, and prints the result.
 *)
and makecg ([], st) =                                               ("\n" ^
   "  STOP_MACHINE;"                                               ^ "\n" ^
   " }"                                                            ^ "\n", st)
  | makecg (x::y, st) = let val label = Aux.gensym "BACK"
                            val (sx, stx) = evaluate(x, st)
                            val (my, sty) = makecg(y, stx)
                                                                 in ("\n" ^
    "PUSH(IMM(0));"                                                ^ "\n" ^
    "PUSH(GLOBAL);"                                                ^ "\n" ^
    "PUSH(LABEL(" ^ label ^ "));"                                  ^ "\n" ^
    "PUSH(IMM(0));"                                                ^ "\n" ^
    "MOV(FP, SP);"                                                 ^ "\n" ^
    sx                                                             ^ "\n" ^
    "DROP(2);"                                                     ^ "\n" ^
    label ^ ":"                                                    ^ "\n" ^
    "PUSH(R0);"                                                    ^ "\n" ^
    "CALL(MY_WRITE_SOB);"                                          ^ "\n" ^
    "DROP(3);"                                                     ^ "\n" ^
    "DROP(STACK(SP));"                                             ^ "\n" ^
    "MOV(FP, IMM(0));"                                             ^ "\n" ^     
    my,                                                                sty) end

(* Structure public function, creates code for the input scheme text string. *)
in fun cg x = let
     val (text, (symbols, consts, constnum)) = makecg(x, (primitives, "", 1)) in
   "#define CONSTNUM IMM(" ^ Int.toString(constnum - 1) ^ ")"      ^ "\n" ^
   Aux.stringOfFile "lib/prefix.c"                                        ^
   baskets symbols                                                 ^ "\n" ^
   consts                                                          ^ "\n" ^
   text                                                            ^ "\n" end
end

(* Structure public function, the whole compiler. *)
fun compileSchemeFile (inputname, outputname) = Aux.stringToFile(outputname,
           cg (map SemanticAnalysis.lexicalAddressing
              (map SemanticAnalysis.annotateTC
              (TagParser.stringToPEs
     (Aux.stringOfFile "lib/support-code.scm" ^ Aux.stringOfFile inputname)))))
end
