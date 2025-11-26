(*
    main.ml

    Make a basic top-level here.
    Just take input file name as cmd line input and an optional output file name.
*)


(*
Parameters:
    - Output file, by default the file name without an extension (remove the last
      dot and the characters after that)
    - Output format (ascii or inter or bin, default bin)
        - ascii: each line is a 16-character ASCII string with 0s and 1s
        - inter: the assembly, with labels resolved
        - bin:   the binary format

Example invocations:
    as foo.pasm
        => makes foo.pmac - the assembled binary
    as bar.pasm --format=inter --out=meow
        => makes meow - the intermediate assembly
    as baz.meow.purr --format=ascii
        => makes baz.meow.pasc - the ASCII representation
*)

