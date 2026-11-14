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
  | Bug

let error = Error
let bug = Bug

let pp_severity ppf severity =
  Fmt.pf
    ppf
    (match severity with
     | Error -> "Error"
     | Bug -> "Bug")
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

let pp_carets width ppf (pos1, pos2) =
  let char1 = max 0 (pos1.Lexing.pos_cnum - pos1.Lexing.pos_bol) in
  let char2 = max (char1 + 1) (pos2.Lexing.pos_cnum - pos1.Lexing.pos_bol) in
  let len = char2 - char1 in
  let spaces = String.make char1 ' ' in
  let carets = String.make len '^' in
  Fmt.pf ppf " %*s | %s%s" width "" spaces carets
;;

let pp_source_with (resolver : Source.resolver) ppf error =
  let source = error.source in
  let fname = source.loc_start.pos_fname in
  let fst = source.loc_start.pos_lnum in
  let lst = source.loc_end.pos_lnum in
  let width = String.length (string_of_int lst) in
  let lines =
    match resolver fname with
    | Some src -> Source.get_lines src ~first:fst ~last:lst
    | None ->
      if Sys.file_exists fname then
        Source.get_lines (Source.from_file fname) ~first:fst ~last:lst
      else
        []
  in
  let pp_content ppf =
    match lines with
    | [ (idx, line) ] when fst = lst ->
      Fmt.pf ppf "%a@,%a" (pp_line width) (idx, line) (pp_carets width) (source.loc_start, source.loc_end)
    | _ ->
      pp_lines width ppf lines
  in
  Fmt.pf ppf "@[<v 0>%a@,%t@,@]" Loc.error_fmt source pp_content
;;

let pp_summary ppf error = Fmt.string ppf error.summary
let pp_message ppf error = Fmt.string ppf error.message
let pp_detail_with resolver = Fmt.(vbox (concat [ pp_header; pp_source_with resolver; pp_message ]))
let pp_short_with resolver = Fmt.(vbox (concat [ pp_header; pp_source_with resolver; pp_summary ]))
let pp_detail = pp_detail_with Source.default_resolver
let pp_short = pp_short_with Source.default_resolver
