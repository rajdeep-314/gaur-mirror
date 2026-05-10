open Hdlutils

exception DuplicateKey of string * Loc.t

exception SyntaxError of string
