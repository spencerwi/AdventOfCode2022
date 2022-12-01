let split_seq_when (test: 'a -> bool) (inputs: 'a seq) : 'a seq seq =
    seq {
        let mutable current_seq = Seq.empty in
        for current_value in inputs do
            if (test current_value) then
                yield current_seq;
                current_seq <- Seq.empty
            else
                current_seq <- Seq.append current_seq (seq { current_value })
    }

let () =
    let input_lines = 
        System.IO.File.ReadAllLines "input.txt"
        |> Seq.ofArray
    in
    let elves : int seq seq =
        input_lines
        |> split_seq_when ((=) "")
        |> Seq.map (Seq.map int)
    in
    let top_elf_weights =
        elves
        |> Seq.map Seq.sum
        |> Seq.sortDescending
    let heaviest_elf_weight = Seq.head top_elf_weights in
    let top_three_sum = 
        top_elf_weights
        |> Seq.take 3 
        |> Seq.sum
    printfn "Part 1: %d" heaviest_elf_weight
    printfn "Part 2: %d" top_three_sum

