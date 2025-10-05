open PHDL
(* exception VMError of string *)

(** [parse s] parses [s] into an AST *)
let _parse (s : string) : Ast.program = 
    let lexbuf = Lexing.from_string s in
    let ast = Parser.main Lexer.read lexbuf in
    ast

let _not = "# nand is a builtin chip, which can be called by nand {a = x, b = y}, where x and y are 
# of type bits 1, and returns value of type bits 1.

# CHIP is a keyword to begin a chip declaration.
CHIP not {
    INPUT in: bits 1
    # INPUT is a keyword to declare that the following 
    # is an input of type bitvector n or some record.

    OUTPUT out: bits 1
    # similarly for OUTPUT.

    out := nand {a = in, b = in}
    # in is a record which has field in, so you acess it as you access ocaml records.
    # as nand returns a type bits 1 and out is also of type bits 1, this works.
}"

let _and = "CHIP and {
    INPUT in: {a : bits 1, b : bits 1 }
    OUTPUT out: bits 1

    not_a := not {in = in.a}
    not_b := not {in = in.b}
    out := nand {a = not_a, b = not_b}
}"

let _or = "CHIP or {
    INPUT in: {a : bits 1, b : bits 1}
    OUTPUT out: bits 1

    not_a := not {in = a}
    not_b := not {in = b}
    not_a_or_b := and {a = not_a, b = not_b}
    out := not {in = not_a_or_b}
}"

let _mux = "CHIP mux {
    INPUT in: {a: bits 1, b : bits 1, sel : bits 1}
    OUTPUT out: bits 1

    not_sel := not {in = in.sel}
    a_and_not_sel := and {a = not_sel, b = in.a}
    a_and_sel := and {a = in.b, b = in.sel}
    out := or {a = a_and_not_sel, b = a_and_sel}
}"

let _bit = "CHIP bit {
    INPUT in: {in : bits 1, load : bits 1 }
    OUTPUT out : bits 1
    STATES dffo : bits 1, dffooo : {bar: bits 1, bex: bits 10}
    # states are declared using the STATES keyword, it is a comma separated list
    # with elements being bitvector or a record type.

    dffoʹ := mux {a = dffo, b = in.in, sel = in.load}
    out := dffo
}"

let _reg = "CHIP register {
    INPUT in: {in: bits 4, load: bits 1}
    OUTPUT out: bits 4

    out[0:0] := bit {in = in[0:0], load = load}
    out[1:1] := bit {in = in[1:1], load = load}
    out[2:2] := bit {in = in[2:2], load = load}
    out[3:3] := bit {in = in[3:3], load = load}
}"

(* let _notp = _parse _not *)
(* let _andp = _parse _not *)
(* let _orp = _parse _or *)
(* let _muxp = _parse _mux *)
let _bitp = _parse _bit
(* let _regp = _parse _reg *)

(* 
(** [read_file ic] reads contents of in-channel [ic] as a string and closes it. *)
let read_file (ic : in_channel) : string =
    let buffer = Buffer.create 4096 in
    try
        while true do
        Buffer.add_channel buffer ic 4096
        done;
        ""
    with End_of_file ->
        close_in_noerr ic;
        Buffer.contents buffer


(** [write_line oc s] writes the string [s] onto out-channel [oc]. *)
let write_line (oc : out_channel) (s : string) : unit = 
    Printf.fprintf oc "%s\n" s


(** [write_lines oc sl] writes [sl] string list onto out-channel [oc]. *)
let write_lines (oc : out_channel) (sl : string list) : unit = 
    List.iter (write_line oc) sl;
    flush oc


(** [basename f] returns name of file [f] without the file extension. *)
let basename (filename : string) : string = 
    let dot_index = String.index filename '.' in
    String.sub filename 0 dot_index


(** [change_extension f e] replaces extension of file [f] with [e]. *)
let change_extension (filename : string) (extension : string) : string = 
    basename filename ^ "." ^ extension


(* this function checks if all arguments are valid file paths. *)
let () = 
    for i = 1 to Array.length Sys.argv - 1 do
        let input_file = Sys.argv.(i) in
        if not( Sys.file_exists input_file) then
            raise (VMError input_file)
    done

(** [file_list ()] returns a string which contains all the input arguments. *)
let rec file_list ?(argi=Array.length Sys.argv - 1) () : string list =
    if argi = 0 then [] else
    let input_file = Sys.argv.(argi) in
    input_file :: file_list ~argi:(argi - 1) ()


(** [get_vmast files] returns a list of tuples, where second element is a function, and first element
is the file in which the function is declared. *)
let rec get_vmast (files : string list) : (string * (string, string) Vmast.Function.t) list = 
    match files with
    | [] -> []
    | head :: tail ->
        let in_ch = open_in head in 
        let functions = in_ch |> read_file |> parse in
        List.map ((fun s f -> (s, f)) head) functions @ get_vmast tail


(** [fun_encode f] takes a list from [get_vmast] and returns the ASM instructions by combining the
output of the individual ASM instructions from each function. *)
let rec func_encode (s : (string * (string, string) Vmast.Function.t) list) : string Ast.instruction list =
    match s with
    | [] -> []
    | head :: tail -> Encode.Function.encode (fst head) (snd head) @ func_encode tail


(** [main_encode s] calls the [func_encode f] and adds Bootstrap code in the beginning. *)
let main_encode (s : (string * (string, string) Vmast.Function.t) list) : string Ast.instruction list =
    Encode.Bootstrap.v @ 
    func_encode s


(* main program *)
let () = 
    let output_file = change_extension Sys.argv.(1) "hack" in
    let out_ch = open_out output_file in
    file_list () |> get_vmast |> main_encode |> Machine.Program.v |> write_lines out_ch *)