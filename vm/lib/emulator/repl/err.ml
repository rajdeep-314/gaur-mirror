type t =
    | EmptyCallStack
    | ExecuteTillReturnFromMain

let to_string = function
    | EmptyCallStack ->
        "Call stack is empty."
    | ExecuteTillReturnFromMain ->
        "Can't perform execute till return from main."


type exn +=
    | ReplError of t


module Helper = struct
    let empty_call_stack = ReplError EmptyCallStack
    let e_till_return_from_main = ReplError ExecuteTillReturnFromMain
end

