open Hdlutils

type kind =
  | Lexing
  | Parsing
  | Dependency
  | TypeError

let lexing = Lexing
let parsing = Parsing
let dependency = Dependency
let type_error = TypeError

let pp_kind ppf kind =
  match kind with
  | Lexing -> Fmt.string ppf "Lexing Error"
  | Parsing -> Fmt.string ppf "Parsing Error"
  | Dependency -> Fmt.string ppf "Cyclic Dependency Error"
  | TypeError -> Fmt.string ppf "Type Error"
;;

type severity =
  | Error
  | Warning
  | Bug

let error = Error
let warning = Warning
let bug = Bug

let pp_severity ppf severity =
  Fmt.pf
    ppf
    (match severity with
     | Error -> "Error"
     | Warning -> "Warning"
     | Bug -> "Buf")
;;

type t =
  { kind : kind
  ; severity : severity
  ; code : int
  ; source : Loc.t
  ; summary : string
  ; message : string
  }

let create ~kind ~severity ~code ~source ~summary ~message =
  { kind; severity; code; source; summary; message }
;;

let pp_header ppf error = Fmt.pf ppf "[%d] %a" error.code pp_kind error.kind
let pp_line width ppf (idx, line) = Fmt.pf ppf " %-*d | %s" width idx line
let pp_lines width = Fmt.(vbox (list ~sep:cut (pp_line width)))

(* TODO Add caret highlighting *)
let pp_source ppf error =
  let source = error.source in
  let fname = source.loc_start.pos_fname in
  let fst = source.loc_start.pos_lnum in
  let lst = source.loc_end.pos_lnum in
  let width = String.length (string_of_int lst) in
  let lines = In_channel.with_open_text fname (Io.file_range fst lst) in
  Fmt.pf ppf "@[<v 0>%a@,%a@,@]" Loc.error_fmt source (pp_lines width) lines
;;

let pp_summary ppf error = Fmt.string ppf error.summary
let pp_message ppf error = Fmt.string ppf error.message
let pp_detail = Fmt.(vbox (concat [ pp_header; pp_source; pp_message ]))
let pp_short = Fmt.(vbox (concat [ pp_header; pp_source; pp_summary ]))
