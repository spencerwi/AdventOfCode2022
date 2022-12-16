module Lib

open System

module Rope = begin
    type Position = {
        x: int
        y: int
    } with 
        member this.neighbors = 
            seq {
                for y = -1 to 1 do
                    for x = -1 to 1 do
                        let candidate = {x = this.x + x; y = this.y + y}
                        if candidate <> this then
                            yield candidate
            }

        member this.is_touching (other: Position) =
            (other = this) ||
            Seq.contains other this.neighbors

    let origin : Position = {
        x = 0
        y = 0
    }

    type Direction =
        | Up
        | Down
        | Left
        | Right

    type Movement = {
        direction: Direction
        count : int
    }

    // Convenient factory functions for parsing and testing purposes
    let up count = { direction = Up; count = count }
    let down count = { direction = Down; count = count }
    let left count = { direction = Left; count = count }
    let right count = { direction = Right; count = count }

    type Movement with
        static member parse (input : string) : Movement =
            let pieces = input.Split [|' '|] in
            let steps = int pieces.[1] in
            match pieces.[0] with 
            | "U" -> up steps
            | "D" -> down steps
            | "L" -> left steps
            | "R" -> right steps
            | _ -> failwith ("Unrecognized instruction: " + input)


    type Segment = {
        position : Position
        history : Set<Position>
    } with 
        static member make () = 
            { 
                position = origin
                history = Set.add origin Set.empty
            }
        member this.moved_to (new_position : Position) =
            {
                position = new_position
                history = this.history.Add new_position
            }

    type State = {
        segments : Segment array
    } with
        member this.head = this.segments.[0]
        member this.tail =
            Array.last this.segments

    let make length = {
        segments = Array.init length (fun _ -> Segment.make())
    }

    let move_follower (leader_position : Position) (follower : Segment) : Segment =
        let follower_dx, follower_dy =
            if (follower.position.is_touching leader_position) then
                (0, 0)
            else
                match (leader_position.x - follower.position.x, leader_position.y - follower.position.y) with
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
        let new_follower_position = {
            x = follower.position.x + follower_dx
            y = follower.position.y + follower_dy
        }
        in follower.moved_to new_follower_position 

    let rec move (state : State) (movement : Movement) : State =
        if movement.count = 0 then
            state
        else
            let head = state.head in
            let head_dx, head_dy = 
                match movement.direction with
                | Up -> (0, +1)
                | Down -> (0, -1)
                | Left -> (-1, 0)
                | Right -> (1, 0)
            in
            let new_head_position = {
                x = head.position.x + head_dx
                y = head.position.y + head_dy
            }
            let new_head = head.moved_to new_head_position in
            let mutable new_segments = Array.copy state.segments in
            new_segments.[0] <- new_head
            for i = 0 to (new_segments.Length - 2) do
                let leader = new_segments.[i] in
                let follower = new_segments.[i + 1] in
                let new_follower = move_follower leader.position follower in
                new_segments.[i + 1] <- new_follower
            done;
            let new_state = { segments = new_segments } in
            let next_movement = { movement with count = movement.count - 1 } in
            move new_state next_movement
    
end

module Puzzle = begin

    let solve_for_rope_length (rope_length : int) (input : string seq) : int =
        let movements = Seq.map Rope.Movement.parse input in
        let final_state = 
            movements
            |> Seq.fold Rope.move (Rope.make rope_length)
        in
        final_state.tail.history
        |> Set.count

    let part1 input = solve_for_rope_length 2 input

    let part2 input = solve_for_rope_length 10 input
end
