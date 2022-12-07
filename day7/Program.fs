open System
open Lib

[<EntryPoint>]
let main args =
    let input_filename = 
        if args.Length = 0 then
            "input.txt"
        else
            args.[0]
    in
    let raw_lines = System.IO.File.ReadAllText input_filename in
    let filesystem = Filesystem.parse raw_lines in
    printfn "Filesystem:\n %s" (Filesystem.to_string 0 (Filesystem.Dir_node filesystem))
    printfn "Part 1: %d" (Puzzle.part1 filesystem)
    printfn "Part 2: %d" (Puzzle.part2 filesystem)
    0
