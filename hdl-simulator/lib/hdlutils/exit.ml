let err = ref 0
let next_err () =
  let e = !err in
  incr err;
  e

let usage = next_err ()
let lexing_error = next_err ()
let parsing_error = next_err ()
let typeerror = next_err ()
let cycleerror = next_err ()
let liveness_error = next_err ()
