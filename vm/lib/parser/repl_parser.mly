%{
    open Syntax.Repl
%}


%token <int> NUM
%token SHOW CALLS STACK FUNCTION
%token EXECUTE STEPS STEP TILL BEFORE RETURN TERMINATION
%token EXIT
%token EOF

%type <Syntax.Repl.t> stmt
%type <Syntax.Repl.t> cmd

%start cmd


%%


stmt:
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

cmd:
    | s = stmt; EOF      { s }

