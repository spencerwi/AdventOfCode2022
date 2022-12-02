module RPS = begin
    type Play =
        | Rock
        | Paper
        | Scissors

    type Round = {
        you: Play
        opponent: Play
    }

    type Winner =
        | You
        | Opponent
        | Tie

    let loser_to = function
        | Rock -> Scissors
        | Paper -> Rock
        | Scissors -> Paper

    let winner_over = function 
        | Rock -> Paper
        | Paper -> Scissors
        | Scissors -> Rock

    let winner round =
        match (round.you, round.opponent) with
        | you, opponent when you = opponent -> Tie
        | you, opponent when you = (loser_to opponent) -> Opponent
        | _, _ -> You

    let score round =
        let shape_score = 
            match round.you with
            | Rock -> 1
            | Paper -> 2
            | Scissors -> 3
        in
        let win_or_loss_score = 
            match winner round with
            | You -> 6
            | Tie -> 3
            | Opponent -> 0
        in
        shape_score + win_or_loss_score

    let parse_part1_play = function
        | 'A' | 'X' -> Rock
        | 'B' | 'Y' -> Paper
        | 'C' | 'Z' -> Scissors
        | other -> failwith ("Unrecognized play: " + (string other))

    let parse_round_part1 (input: string) = 
        {
            opponent = parse_part1_play input.[0]
            you = parse_part1_play input.[2]
        }

    let parse_round_part2 (input: string) =
        let opponent = parse_part1_play input.[0]
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
        |> Seq.map RPS.parse_round_part1
        |> Seq.fold (fun score round -> score + (RPS.score round)) 0
    in
    let part2_score = 
        input_lines
        |> Seq.map RPS.parse_round_part2
        |> Seq.fold (fun score round -> score + (RPS.score round)) 0
    in
    printfn "Part 1: %d" part1_score;
    printfn "Part 2: %d" part2_score
