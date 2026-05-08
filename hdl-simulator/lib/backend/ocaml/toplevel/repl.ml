open Hdlutils

module type CHIP = sig
  type t
  type tin
  type tout

  val out : tin -> t -> tout
  val clk : tin -> t -> t
  (* val pp_tin : tin Fmt.t *)
  val pp_tout : tout Fmt.t
  val typeof_tin : Parse.Types.typ
  val totype_tin : Parse.Ast.expr -> tin
  (* val totype_tout : Loc.t Parse.Ast.expr -> tout *)
  val def_tin : tin
  val def_t : t
end

let repl outx (module C : CHIP) =
  let chip = ref C.def_t in
  let input = ref C.def_tin in
  let rec go () =
    let cmd = Prompt.read outx () in
    match cmd with
    | Input e ->
      let open Typecheck in
      (try Typechecker.tc_input C.typeof_tin e with
       | Errors.Error (err, loc) ->
         Io.output_stringln outx (Fmt.to_to_string Errors.Pretty.pp (err, loc)));
       input := C.totype_tin e.expr;
      go ()

    | Output ->
      Io.output_stringln outx (Fmt.to_to_string C.pp_tout (C.out !input !chip));
      go ()

    | Clock ->
      chip := C.clk !input !chip;
      go ()

    | Quit -> ()
  in
  go ()
;;
