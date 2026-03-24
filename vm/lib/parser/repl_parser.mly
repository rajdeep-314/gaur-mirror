%{
    open Syntax.Repl
%}


%token <int> NUM
%token SHOW CALLS STACK FUNCTION
%token EXECUTE STEPS STEP TILL BEFORE RETURN TERMINATION
%token EXIT

%type <Syntax.Repl.t> cmd

%start cmd


%%


cmd:
    | SHOW; CALLS
        { ShowCalls}
    | SHOW; STACK
        { ShowStack }
    | SHOW; FUNCTION
        { ShowFunction }
    | EXECUTE; STEP
        { ExecuteStep }
    | EXECUTE; STEPS; n = NUM
        { ExecuteSteps n }
    | EXECUTE; TILL; RETURN
        { ExecuteTillReturn }
    | EXECUTE; TILL; BEFORE; RETURN
        { ExecuteTillBeforeReturn }
    | EXECUTE; TILL; TERMINATION
        { ExecuteTillTermination }
    | EXIT
        { Exit }


