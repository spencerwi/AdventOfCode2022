module Lib

open System
open System.Collections.Generic

module Mountain = begin
    type t = int[,]

    let size (mountain : t) = 
        (Array2D.length1 mountain, Array2D.length2 mountain)

    type Coords = {
        row: int
        col: int
    }

    let up (coords : Coords) = {coords with col = coords.col - 1}
    let down (coords : Coords) = {coords with col = coords.col + 1}
    let left (coords : Coords) = {coords with row = coords.row - 1}
    let right (coords : Coords) = {coords with row = coords.row + 1}

    let all_low_points (mountain : t) : Coords seq =
        seq {
            for row = 0 to (Array2D.length1 mountain) - 1 do
            for col = 0 to (Array2D.length2 mountain) - 1 do
            if mountain[row, col] = 0 then
                yield { row = row; col = col }
        }


    type State = {
        current_position : Coords
        goal : Coords
        mountain : t
    }

    let is_in_bounds (mountain : t) (point : Coords) =
        point.row >= 0 && point.col >= 0 &&
        point.row < (Array2D.length1 mountain) &&
        point.col < (Array2D.length2 mountain)

    let is_reachable_from (mountain : t) (src : Coords) (dest : Coords) =
        mountain.[dest.row, dest.col] <= (mountain.[src.row, src.col] + 1)

    let parse (input : string array) = 
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
            mountain = mountain
        }

    let available_next_steps_from (mountain : t) (current_position: Coords) =
        seq {
            (up current_position);
            (left current_position); (right current_position);
            (down current_position)
        }
        |> Seq.filter (is_in_bounds mountain)
        |> Seq.filter (is_reachable_from mountain current_position)

    /// <description>
    /// Dijkstra's algorithm, recycled from AoC 2021 day 15, because why figure this out from scratch again?
    /// </description>
    let find_shortest_path (mountain : t) (goal : Coords) (start : Coords) =
        let height, width = size mountain in
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
            for neighbor in (available_next_steps_from mountain current) do
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
        
end

module Puzzle = begin
    let part1 (input: string array) =
        let state = Mountain.parse input in
        Mountain.find_shortest_path state.mountain state.goal state.current_position

    let part2 (input: string array) =
        let state = Mountain.parse input in
        let candidate_start_positions = Mountain.all_low_points state.mountain in
        candidate_start_positions
        |> Seq.map (Mountain.find_shortest_path state.mountain state.goal)
        |> Seq.min

end
