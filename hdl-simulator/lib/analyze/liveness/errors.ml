open Var
open Hdlutils

module VarSet = Set.Make(Var)

type t =
  | Use_before_def  of VarSet.t
  | Multiple_def    of VarSet.t
  | Use_with_def    of VarSet.t
  | Unused          of VarSet.t
  | Undefined       of VarSet.t

module Pretty = struct
  let rec pp ppf warn =
    match warn with
    | Use_before_def vars ->
      Fmt.pf ppf "@[<v 2>These variables are used before they are defined:@,%a@,@]"
        pp_vars vars

    | Multiple_def vars ->
      Fmt.pf ppf "@[<v 2>These variables are defined more than once:@,%a@,@]"
        pp_vars vars

    | Use_with_def vars ->
      Fmt.pf ppf "@[<v 2>These variables are used and defined simultaneously:@,%a@,@]"
        pp_vars vars

    | Unused vars ->
      Fmt.pf ppf "@[<v 2>These variables are never used:@,%a@,@]"
        pp_vars vars

    | Undefined vars ->
      Fmt.pf ppf "@[<v 2>These variables are never defined:@,%a@,@]"
        pp_vars vars

  and pp_vars ppf vars =
    Fmt.(vbox (list ~sep:cut Var.Pretty.pp_var)) ppf (VarSet.to_list vars)
end

let raise_use_before_def vars loc =
  let warn = Use_before_def vars in
  Loc.error_fmt Fmt.stderr loc;
  Pretty.pp Fmt.stderr warn;
  Format.pp_print_flush Fmt.stderr ()

let raise_multiple_def vars loc =
  let warn = Multiple_def vars in
  Loc.error_fmt Fmt.stderr loc;
  Pretty.pp Fmt.stderr warn;
  Format.pp_print_flush Fmt.stderr ()

let raise_use_with_def vars loc =
  let warn = Use_with_def vars in
  Loc.error_fmt Fmt.stderr loc;
  Pretty.pp Fmt.stderr warn;
  Format.pp_print_flush Fmt.stderr ()

let raise_unused vars loc =
  let warn = Unused vars in
  Loc.error_fmt Fmt.stderr loc;
  Pretty.pp Fmt.stderr warn;
  Format.pp_print_flush Fmt.stderr ()

let raise_undefined vars loc =
  let warn = Undefined vars in
  Loc.error_fmt Fmt.stderr loc;
  Pretty.pp Fmt.stderr warn;
  Format.pp_print_flush Fmt.stderr ()

