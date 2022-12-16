module Assignment = begin
    type t = {
        start : int
        endInclusive : int
    } with 
        static member parse (assignment_str : string) : t =
            let parts = assignment_str.Split [|'-'|] in
            {
                start = (int parts.[0])
                endInclusive = (int parts.[1])
            }

        member this.fully_contains other =
            (
                this.start <= other.start
                && this.endInclusive >= other.endInclusive
            )

        member this.overlaps other =
            (
                // "this-leading" cases:
                //  |--- this ---|
                //        |--- other ---|
                // or 
                //  |--- this ---------|
                //    |--- other ---|
                // or
                //  |--- this ---|
                //  |--- other ---|
                // or 
                //  |--- this ---|
                //  |----- other ------|
                (this.start <= other.start && this.endInclusive >= other.start) 
                || 
                // "other-leading" cases, which are the same but flipped this/other
                (other.start <= this.start && other.endInclusive >= this.start)
            )

    let parse_elfpair (line : string) =
        let elfPair = 
            line.Split [|','|] 
            |> Array.map t.parse
        in
        (elfPair.[0], elfPair.[1])


end

let part1 (input : seq<Assignment.t * Assignment.t>) =
    query {
        for (elf1, elf2) in input do
        where (
            (elf1.fully_contains elf2)
            ||
            (elf2.fully_contains elf1)
        )
        count
    }

let part2 (input : seq<Assignment.t * Assignment.t>) =
    query {
        for (elf1, elf2) in input do
        where (elf1.overlaps elf2)
        count
    }

let () = 
    let input = 
        System.IO.File.ReadAllLines "input.txt"
        |> Seq.ofArray
        |> Seq.map Assignment.parse_elfpair
    in
    printfn "Part 1: %d" (part1 input)
    printfn "Part 2: %d" (part2 input)
