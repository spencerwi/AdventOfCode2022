let find_marker (marker_length: int) (input : string) = 
    input
    |> Seq.windowed marker_length
    |> Seq.indexed
    |> Seq.map (fun (idx, substring) -> 
        (idx + marker_length, Set.ofArray substring)
    )
    |> Seq.find (fun (idx, unique_letters) -> 
        unique_letters.Count = marker_length
    )
    |> fst

let part1 (input : string) =
    find_marker 4 input
    
let part2 (input : string) =
    find_marker 14 input

let () = 
    let input = System.IO.File.ReadAllText "input.txt" in
    printfn "Part 1: %d" (part1 input)
    printfn "Part 2: %d" (part2 input)
