module Lib

open System
open System.Collections.Generic

type Coords = {
    row: int
    col: int
} with
    member this.up = {this with col = this.col - 1}
    member this.down = {this with col = this.col + 1}
    member this.left = {this with row = this.row - 1}
    member this.right = {this with row = this.row + 1}

type Mountain = {
    locations: int[,]
} with
    member this.height = Array2D.length1 this.locations
    member this.width = Array2D.length2 this.locations
    member this.size = (this.height, this.width)

    member this.Item 
        with get (coords : Coords) =
            this.locations[coords.row, coords.col]

    member this.is_in_bounds (point : Coords) =
        point.row >= 0 && point.col >= 0 &&
        point.row < this.height &&
        point.col < this.width


    member this.all_low_points =
        seq {
            for row = 0 to this.height - 1 do
            for col = 0 to this.width - 1 do
            if this.locations[row, col] = 0 then
                yield { row = row; col = col }
        }

    member this.is_reachable_from (src : Coords) (dest : Coords) =
        this[dest] <= (this[src] + 1)

    member this.available_next_steps_from (current_position: Coords) =
        seq {
                         current_position.up;
            current_position.left; current_position.right;
                         current_position.down
        }
        |> Seq.filter this.is_in_bounds
        |> Seq.filter (this.is_reachable_from current_position)

    /// <description>
    /// Dijkstra's algorithm, recycled from AoC 2021 day 15, because why figure this out from scratch again?
    /// </description>
    member this.find_shortest_path (goal : Coords) (start : Coords) =
        let height, width = this.size in
        // Let's dijkstra this business, yo
        
        // We'll keep track of how far each point in the grid is from the source by using a separate grid of the same size
        let distancesFromSource = Array2D.create height width Int32.MaxValue in
        distancesFromSource[start.row, start.col] <- 0 // the source *is* the source, my dude

        // Now, we want to walk from the source "outwards" and trace each shortest path. 
        // Sometimes, we'll see a cell again but on a shorter path. No worries, dawg, just 
        // update its distance-from-the-source and go check it out again to see if anything's 
        // changed about it.
        let unsettled = new Queue<Coords>([start]) in
        while unsettled.Count > 0 do
            let current = unsettled.Dequeue() in
            for neighbor in (this.available_next_steps_from current) do
                let existingDistance = distancesFromSource[neighbor.row, neighbor.col] in
                let distanceThroughCurrent = distancesFromSource[current.row, current.col] + 1 in
                if distanceThroughCurrent < existingDistance then
                    distancesFromSource.[neighbor.row, neighbor.col] <- distanceThroughCurrent
                    unsettled.Enqueue neighbor
            done
        done;
        // Finally, we can answer the question: 
        // How long is the shortest path between start and end?
        distancesFromSource[goal.row, goal.col]


type State = {
    current_position : Coords
    goal : Coords
    mountain : Mountain
} with
    static member parse (input : string array) = 
        let mutable startPosition = {
            row = 0; col = 0
        }
        let mutable goalPosition = {
            row = 0; col = 0
        }
        let alphabet = [|'a' .. 'z'|] in
        let mountain = 
            [|
                for row = 0 to (input.Length - 1) do
                    yield [|
                        for (col, cell) in Array.indexed (Array.ofSeq input.[row]) do
                            if cell = 'S' then 
                                startPosition <- {
                                    row = row
                                    col = col
                                }
                                yield 0
                            elif cell = 'E' then
                                goalPosition <- {
                                    row = row
                                    col = col
                                }
                                yield 25
                            else
                                yield Array.IndexOf(alphabet, cell)
                    |]
            |] |> array2D
        in
        { 
            current_position = startPosition
            goal = goalPosition
            mountain = {
                locations = mountain
            }
        }

module Puzzle = begin
    let part1 (input: string array) =
        let state = State.parse input in
        state.mountain.find_shortest_path state.goal state.current_position

    let part2 (input: string array) =
        let state = State.parse input in
        let candidate_start_positions = state.mountain.all_low_points in
        candidate_start_positions
        |> Seq.map (state.mountain.find_shortest_path state.goal)
        |> Seq.min

end
