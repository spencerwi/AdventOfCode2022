open System

module Rucksack = begin
    type t = string * string

    let parse (input : string) : t = 
        let length = input.Length in
        let midpoint = length / 2 in
        (
            input.Substring(0, midpoint),
            input.Substring(midpoint)
        )

    let common_item (rucksack : t) : char =
        let (compartment1, compartment2) = rucksack in
        let common_elements = Set.intersect (Set.ofSeq compartment1) (Set.ofSeq compartment2) in
        Seq.exactlyOne common_elements

    let priority (item : char) =
        let lower_alpha = [| 'a' .. 'z' |] in
        let upper_alpha = [| 'A' .. 'Z' |] in
        let all_chars = Array.append lower_alpha upper_alpha in
        Array.IndexOf(all_chars, item) + 1
        
end

let part1 (lines : string seq) = 
    lines
    |> Seq.map Rucksack.parse
    |> Seq.map Rucksack.common_item
    |> Seq.map Rucksack.priority
    |> Seq.sum

let part2 (lines : string seq) =
    query {
        for group in Seq.chunkBySize 3 lines do
            let badge = 
                group 
                |> Seq.map Set.ofSeq
                |> Set.intersectMany
                |> Seq.exactlyOne
            in 
            sumBy (Rucksack.priority badge)
    } 

let () =
    let input = 
        System.IO.File.ReadAllLines "input.txt"
        |> Seq.ofArray 
    in
    printfn "Part 1: %d" (part1 input)
    printfn "Part 2: %d" (part2 input)
    ()
