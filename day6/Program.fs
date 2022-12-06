let find_marker (marker_length: int) (input : string) = 
    let first_possible_index = marker_length - 1 in
    let possible_indexes = seq { first_possible_index .. (input.Length - 1) } in
    let zero_offset_answer = 
        possible_indexes
        |> Seq.find (fun idx ->
            let substring = input.[(idx - first_possible_index)..idx] in
            let unique_letters = Set.ofSeq substring in
            unique_letters.Count = marker_length 
        )
    in zero_offset_answer + 1

let part1 (input : string) =
    find_marker 4 input
    
let part2 (input : string) =
    find_marker 14 input

let () = 
    let input = System.IO.File.ReadAllText "input.txt" in
    printfn "Part 1: %d" (part1 input)
    printfn "Part 2: %d" (part2 input)
