%{
    open Syntax.Repl
%}


%token <int> NUM
%token SHOW CALLS STACK TOP FUNCTION SCREEN
%token EXECUTE STEPS STEP TILL BEFORE RETURN TERMINATION
%token RESET EXIT
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
    | SHOW; STACK; TOP; n = NUM
        { ShowStackTop n }
    | SHOW; FUNCTION
        { ShowFunction }
    | SHOW; SCREEN
        { ShowScreen }
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
    | RESET
        { Reset }
    | EXIT
        { Exit }

cmd:
    | s = stmt; EOF      { s }

