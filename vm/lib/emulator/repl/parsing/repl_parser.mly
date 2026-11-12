%{
    open Ast
%}


%token <int> NUM
%token <string> ID
%token <char> CHAR
%token HELP
%token TEMP STATIC LOCAL ARGUMENT
%token SHOW CALLS STACK TOP FUNCTION SCREEN SEGMENT FRAME FRAMES STATE KEYBOARD
%token EXECUTE STEPS STEP TILL BEFORE RETURN TERMINATION
%token NEXT BREAKPOINT
%token SET UNSET
%token RESET EXIT
%token EOF

%type <Ast.t> stmt
%type <Ast.t> cmd

%start cmd


%%

segment:
    | TEMP      { Temp }
    | STATIC    { Static }
    | LOCAL     { Local }
    | ARGUMENT  { Argument }

stmt:
    | HELP
        { Help }
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
    | SHOW; KEYBOARD
        { ShowKeyboard }
    | SHOW; SEGMENT; s = segment
        { ShowSegment s }
    | SHOW; FRAME
        { ShowFrame }
    | SHOW; FRAMES
        { ShowFrames }
    | SHOW; STATE
        { ShowState }
    | EXECUTE; STEP
        { ExecuteStep }
    | EXECUTE; STEPS; n = NUM
        { ExecuteSteps n }
    | EXECUTE; TILL; BREAKPOINT; s = ID
        { ExecuteTillNamedBpt (s) }
    | EXECUTE; TILL; NEXT; BREAKPOINT
        { ExecuteTillNextBpt }
    | EXECUTE; TILL; RETURN
        { ExecuteTillReturn }
    | EXECUTE; TILL; BEFORE; RETURN
        { ExecuteTillBeforeReturn }
    | EXECUTE; TILL; TERMINATION
        { ExecuteTillTermination }
    | SET; KEYBOARD; c = CHAR
        { SetKeyboard c }
    | UNSET; KEYBOARD
        { UnsetKeyboard }
    | RESET
        { Reset }
    | EXIT
        { Exit }

cmd:
    | EOF                { Empty }
    | s = stmt; EOF      { s }

