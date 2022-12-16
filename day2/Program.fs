module RPS = begin
    type Play =
        | Rock
        | Paper
        | Scissors
    with 
        static member parse = function
            | 'A' | 'X' -> Rock
            | 'B' | 'Y' -> Paper
            | 'C' | 'Z' -> Scissors
            | other -> failwith ("Unrecognized play: " + (string other))

    let loser_to = function
        | Rock -> Scissors
        | Paper -> Rock
        | Scissors -> Paper

    let winner_over = function 
        | Rock -> Paper
        | Paper -> Scissors
        | Scissors -> Rock

    type Winner =
        | You
        | Opponent
        | Tie

    type Round = {
        you: Play
        opponent: Play
    } with
        member this.winner =
            match (this.you, this.opponent) with
            | you, opponent when you = opponent -> Tie
            | you, opponent when you = (loser_to opponent) -> Opponent
            | _, _ -> You
                
        member this.score =
            let shape_score = 
                match this.you with
                | Rock -> 1
                | Paper -> 2
                | Scissors -> 3
            in
            let win_or_loss_score = 
                match this.winner with
                | You -> 6
                | Tie -> 3
                | Opponent -> 0
            in
            shape_score + win_or_loss_score

        static member parse_part1 (input : string) =
            {
                opponent = Play.parse input.[0]
                you = Play.parse input.[2]
            }

        static member parse_part2 (input : string) =
            let opponent = Play.parse input.[0]
            let you =
                match input.[2] with
                | 'Y' -> opponent // Tie on purpose by playing the same thing
                | 'X' -> loser_to opponent            
                | 'Z'  -> winner_over opponent
                | other -> failwith ("Unrecognized instruction for you: " + (string other))
            in 
            { opponent = opponent; you = you }

end


let () =
    let input_lines = 
        System.IO.File.ReadAllLines "input.txt"
        |> Seq.ofArray
    in
    let part1_score = 
        input_lines
        |> Seq.map RPS.Round.parse_part1
        |> Seq.fold (fun score round -> score + (round.score)) 0
    in
    let part2_score = 
        input_lines
        |> Seq.map RPS.Round.parse_part2
        |> Seq.fold (fun score round -> score + (round.score)) 0
    in
    printfn "Part 1: %d" part1_score;
    printfn "Part 2: %d" part2_score
