type Assignment = {
    start : int
    endInclusive : int
}

let parse_assignment (assignment_str : string) : Assignment =
    let parts = assignment_str.Split [|'-'|] in
    {
        start = (int parts.[0])
        endInclusive = (int parts.[1])
    }

let fully_contains a1 a2 =
    (
        a1.start <= a2.start 
        && a1.endInclusive >= a2.endInclusive
    )

let overlaps a1 a2 =
    (
        //  |--- a1 ---|
        //        |--- a2 ---|
        (a1.start <= a2.start && a1.endInclusive >= a2.start) 
        || 
        //        |--- a1 ---|
        //  |--- a2 ---|
        (a2.start <= a1.start && a2.endInclusive >= a1.start)
    )

let parse_elfpair (line : string) =
    let elfPair = 
        line.Split [|','|] 
        |> Array.map parse_assignment
    in
    (elfPair.[0], elfPair.[1])

let part1 (input : seq<Assignment * Assignment>) =
    query {
        for (elf1, elf2) in input do
        where (
            (elf1 |> fully_contains <| elf2)
            ||
            (elf2 |> fully_contains <| elf1)
        )
        count
    }

let part2 (input : seq<Assignment * Assignment>) =
    query {
        for (elf1, elf2) in input do
        where (elf1 |> overlaps <| elf2)
        count
    }

let () = 
    let input = 
        System.IO.File.ReadAllLines "input.txt"
        |> Seq.ofArray
        |> Seq.map parse_elfpair
    in
    printfn "Part 1: %d" (part1 input)
    printfn "Part 2: %d" (part2 input)
