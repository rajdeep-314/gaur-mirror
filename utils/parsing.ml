(*
    parsing.ml

    Utility functions related to parsing, lexing, etc.
*)


(* Get the ln'th line from source code string src. ln is 1-indexed.
   NOTE: It is assumed that the line number is valid. *)
let get_line src ln =
    let lines = (String.split_on_char '\n' src) in
    List.nth lines (ln - 1)


(* carat_string n is (n - 1) spaces followed by a carat character. *)
let carat_string ?(char='^') n =
    String.make (n - 1) ' ' ^ String.make 1 char

(* carats_string n l is (n - 1) spaces followed by l carat characters. *)
let carats_string ?(char='^') n l =
    String.make (n - 1) ' ' ^ String.make l char


(* Expand all tabes in s to width-many spaces. *)
let expand_tabs width s =
    let spaces = String.make width ' ' in
    String.concat spaces (String.split_on_char '\t' s)


(* Get lines from line numbers l1 to l2 in src, both inclusive.
   Both line numbers are 1-indexed.
   NOTE: It is assumed that the line numbers are both valid. *)
let get_lines src l1 l2 =
    let lines = String.split_on_char '\n' src in
    (* Sub-list. *)
    let sub_list l i j =
        List.take (j - i + 1) (List.drop i l) in
    sub_list lines (l1 - 1) (l2 - 1)


(* Split string s into two, the first one being of length n. *)
let split_string s n =
    String.sub s 0 n, String.sub s n (String.length s - n)

(* Insert into string s, the string x, starting at character number i. *)
let insert_into_string s x i =
    let pre, post = split_string s (i - 1) in
    pre ^ x ^ post

(* Get a string corresponding to lines from l1 to l2 in src, with the region
   from column c1 of l1 to column c2 of l2 enclosed in open_m and close_m. *)
let get_lines_with_markers open_m close_m src l1 l2 c1 c2 =
    if l1 = l2 then
        let l = get_line src l1 in
        l ^ "\n" ^ carats_string c1 (c2 - c1)
    else
        let ls = get_lines src l1 l2 in
        let ls_str = String.concat "\n" ls in
        let c1 = carat_string ~char:'*' c1 in
        let c2 = carat_string ~char:'*' c2 in
        String.concat "\n" [c1; ls_str; c2]

