type t =
  | DuplicateKey  of string * Loc.t
  | UnexCharInBin of char   * Lexing.lexbuf
  | UnexString    of string * Lexing.lexbuf

exception Error of t

let raise_duplicate_key key loc =
  raise (Error (DuplicateKey (key, loc)))

let raise_unex_char_bv c lexbuf =
  raise (Error (UnexCharInBin (c, lexbuf)))

let raise_unex_string str lexbuf =
  raise (Error (UnexString (str, lexbuf)))

module Pretty = struct
  let pp ppf err =
    match err with
    | DuplicateKey (key, loc) ->
      Fmt.pf ppf "@[<v>%a@,The field name %s is used more than once.@,@]"
        Fmt.string (Loc.range loc)
        key

    | UnexCharInBin (c, lexbuf) ->
      Fmt.pf ppf "@[<v>%a@,Unexpected character %c.@,@]"
        Fmt.string (Loc.location lexbuf)
        c

    | UnexString (str, lexbuf) ->
      Fmt.pf ppf "@[<v>%a@,Syntax error: %s@,@]"
        Fmt.string (Loc.location lexbuf)
        str
end
