module Lib

open System

module RopeMovement = begin
    type Position = {
        x: int
        y: int
    }
    let origin : Position =
        {
            x = 0
            y = 0
        }

    type Direction =
        | Up
        | Down
        | Left
        | Right

    type Movement = 
        {
            direction: Direction
            count : int
        }

    let up count = { direction = Up; count = count }
    let down count = { direction = Down; count = count }
    let left count = { direction = Left; count = count }
    let right count = { direction = Right; count = count }

    let parse (input : string) : Movement =
        let pieces = input.Split [|' '|] in
        let steps = int pieces.[1] in
        match pieces.[0] with 
        | "U" -> up steps
        | "D" -> down steps
        | "L" -> left steps
        | "R" -> right steps
        | _ -> failwith ("Unrecognized instruction: " + input)

    type State = {
        head : Position
        head_history : Set<Position>
        tail : Position
        tail_history : Set<Position>
    }

    let initial_state = {
        head = origin
        head_history = Set.add origin Set.empty
        tail = origin
        tail_history = Set.add origin Set.empty
    }

    let neighbors_of (point : Position) : Position seq =
        seq {
            for dx = -1 to 1 do
                for dy = -1 to 1 do
                    if not (dx = 0 && dy = 0) then
                        yield {
                            x = point.x + dx
                            y = point.y + dy
                        }
        }

    let is_touching (p1 : Position) (p2: Position) =
        let neighbor_distances = seq {
            for x = -1 to 1 do
                for y = -1 to 1 do
                    yield (x, y)
        }
        let distance = (p1.x - p2.x, p1.y - p2.y) in
        Seq.contains distance neighbor_distances


    let rec move (state : State) (movement : Movement) : State =
        if movement.count = 0 then
            state
        else
            let head_dx, head_dy = 
                match movement.direction with
                | Up -> (0, +1)
                | Down -> (0, -1)
                | Left -> (-1, 0)
                | Right -> (1, 0)
            in
            let new_head_location = {
                x = state.head.x + head_dx
                y = state.head.y + head_dy
            }
            in
            let tail_dx, tail_dy =
                if (state.tail |> is_touching <| new_head_location) then
                    (0, 0)
                else
                    match (new_head_location.x - state.tail.x, new_head_location.y - state.tail.y) with
                    | (0, y) when y >= 2 -> (0, 1)
                    | (0, y) when y <= -2 -> (0, -1)
                    | (x, 0) when x >= 2 -> (1, 0)
                    | (x, 0) when x <= -2 -> (-1, 0)
                    | (x, y) when x > 0 && y > 0 -> (1, 1) // diagonally up-and-right
                    | (x, y) when x > 0 && y < 0 -> (1, -1) // diagonally down-and-right
                    | (x, y) when x < 0 && y > 0 -> (-1, 1) // diagonally up-and-left
                    | (x, y) when x < 0 && y < 0 -> (-1, -1) // diagonally down-and-left
                    | (x, y) -> failwith <| sprintf "Something went wrong; distance between new head and current tail was (%d, %d)" x y
            in 
            let new_tail_location = {
                x = state.tail.x + tail_dx
                y = state.tail.y + tail_dy
            }
            in
            let stepped_state = 
                { 
                    head = new_head_location
                    head_history = Set.add new_head_location state.head_history
                    tail = new_tail_location
                    tail_history = Set.add new_tail_location state.tail_history
                }
            in
            printfn "Head is at %A, tail is at %A" new_head_location new_tail_location;
            let next_step = { movement with count = movement.count - 1} in
            move stepped_state next_step
    
end

module Puzzle = begin
    let part1 (input: string seq) =
        let movements = Seq.map RopeMovement.parse input in
        let final_state = 
            movements
            |> Seq.fold RopeMovement.move RopeMovement.initial_state
        in
        final_state.tail_history.Count


    let part2 (input: string seq) =
        "the right answer"
end
