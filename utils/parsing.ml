(*
    parsing.ml

    Utility functions related to parsing, lexing, etc.
*)


(* Get the ln'th line from source code string src. ln is 1-indexed. *)
let get_line src ln =
    let lines = (String.split_on_char '\n' src) in
    (*List.iter print_endline lines ;*)
    (*"meow"*)
    List.nth lines (ln - 1)


(* carat_string n is (n - 1) spaces followed by a carat character (^). *)
let carat_string n =
    String.make (n - 1) ' ' ^ "^"

(* Expand all tabes in s to width-many spaces. *)
let expand_tabs width s =
    let spaces = String.make width ' ' in
    String.concat spaces (String.split_on_char '\t' s)

