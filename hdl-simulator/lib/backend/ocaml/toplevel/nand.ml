open Parse.Ast
open Parse.Helpers
open Parse.Types

module Chip_Nand = struct
  type state = unit
  type input = < var_a : bv ; var_b : bv >
  type output = bv

  let output (ip : input) (_ : state) : output = [| nand ip#var_a.(0) ip#var_b.(0) |]
  let clock (_ : input) (_ : state) = ()

  let default_state = ()

  let default_input =
    object
      method var_a = [| false |]
      method var_b = [| false |]
    end
  ;;

  let typ_of_input = trecord_of_list [ "var_a", tbits 1; "var_b", tbits 1 ]

  let input_of_expr e : input =
    object
      method var_a = e |>? "var_a" |> get_bv
      method var_b = e |>? "var_b" |> get_bv
    end
  ;;

  let expr_of_output (op : output) =
    ebits op

  let expr_of_input (ip : input) =
    erecord_of_list ["a", ebits ip#var_a; "b", ebits ip#var_b]
end
