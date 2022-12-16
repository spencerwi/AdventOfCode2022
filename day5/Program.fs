open System
open System.Collections.Generic
open System.Text.RegularExpressions

module Stacks = begin
    type t = Stack<char> array

    type Movement = {
        count: int
        src: int
        dest: int
    } with 
        static member parse (line : string) : Movement =
            let pattern = "move (?<count>\d+) from (?<src>\d) to (?<dest>\d)" in
            let matched = Regex.Match(line, pattern) in
            let src = (int matched.Groups["src"].Value) - 1 in
            let dest = (int matched.Groups["dest"].Value) - 1 in
            {
                count = (int matched.Groups["count"].Value)
                src = src
                dest = dest
            }

    /// <description>
    /// WARNING: modifies the stacks in-place!
    /// Moves some crates from one stack to another one at a time.
    /// </description>
    let move (stacks : t) (movement : Movement) =
        for i in 1..movement.count do
            stacks.[movement.dest].Push(
                stacks.[movement.src].Pop()
            )

    /// <description>
    /// WARNING: modifies the stacks in-place!
    /// Moves some crates from one stack to another as a group, preserving their original order.
    /// </description>
    let move_retaining_order (stacks : t) (movement : Movement) =
        // To retain order rather than reversing the elements we want, we'll use a temporary "playback"
        //  intermediate stack variable.
        let mutable unreverser = new Stack<char>() in
        for i in 1..movement.count do
            unreverser.Push(
                stacks.[movement.src].Pop()
            )
        // Now every crate is on our reverser stack in reversed order. Popping them all off that should un-reverse it.
        for i in 1..movement.count do
            stacks.[movement.dest].Push(
                unreverser.Pop()
            )

    let parse_stacks (lines : string array) : t =
        let number_of_stacks = (lines.[0].Length + 1) / 4 in // "[A] [B]" is 7 chars for 2 stacks, for example
        let mutable stacks = Array.init number_of_stacks (fun _ -> new Stack<char>()) in
        for line in (lines |> Seq.rev |> Seq.skip 1) do // work from the bottom up, skipping the labels
            let columns = Seq.chunkBySize 4 line in // Each crate name is one char, with 2 bracket chars, and the space that comes before the next stack.
            for (column_number, contents) in Seq.indexed columns do
                let crateName = contents.[1] // given "[A] ", get "A"
                if crateName <> ' ' then
                    stacks.[column_number].Push(crateName)
        stacks
end

let run_instructions (stacks : Stacks.t) (instructions: Stacks.Movement seq) mover_fn =
    for movement in instructions do
        mover_fn stacks movement;
    stacks
    |> Array.map (fun stack -> stack.Peek())
    |> Seq.map string
    |> String.concat ""

let part1 (stacks_str : string array) (instructions : Stacks.Movement seq) =
    // We defer parsing of the stacks until here because we want to ensure
    // that part1's "modify-in-place" operations don't affect part2.
    let stacks = Stacks.parse_stacks stacks_str in
    run_instructions stacks instructions Stacks.move

let part2 (stacks_str : string array) (instructions : Stacks.Movement seq) =
    // We defer parsing of the stacks until here because we want to ensure
    // that part1's "modify-in-place" operations don't affect part2.
    let stacks = Stacks.parse_stacks stacks_str in
    run_instructions stacks instructions Stacks.move_retaining_order

let () =
    let raw_lines = 
        System.IO.File.ReadAllLines "input.txt"
    in
    let blank_line_index = Array.IndexOf(raw_lines, "") in
    let stacks_str = raw_lines.[0..(blank_line_index - 1)] in
    let instructions = 
        raw_lines 
        |> Array.skip (blank_line_index + 1)
        |> Seq.map Stacks.Movement.parse
    in 
    printfn "Part 1: %s" (part1 stacks_str instructions)
    printfn "Part 2: %s" (part2 stacks_str instructions)
