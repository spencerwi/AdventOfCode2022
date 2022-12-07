open System
open Lib 

[<EntryPoint>]
let main args =
    let input_filename = 
        if args.Length > 0 then
            args.[0]
        else
            "input.txt"
    in
    let input = System.IO.File.ReadAllText input_filename in
    printfn "Part 1: %s" (Puzzle.part1 input);
    printfn "Part 2: %s" (Puzzle.part2 input);
    0
