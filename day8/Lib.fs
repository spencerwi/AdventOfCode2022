module Lib

open System

module Forest = begin
    type Coords = {
        row: int
        col: int
    }

    type Direction =
        | Up
        | Down
        | Left
        | Right

    let directions = seq {
        Up; Down; Left; Right
    }

    type t = {
        cells : int[,]
    } with
        member this.height = Array2D.length1 this.cells
        member this.width = Array2D.length2 this.cells

        member this.get (coords : Coords) =
            this.cells[coords.row, coords.col]

        member this.Item 
            with get (coords : Coords) = this.get coords

        member this.sightline_from (coords : Coords) (direction : Direction) =
            match direction with
            | Up ->
                seq {
                    for i = (coords.row - 1) downto 0 do
                        yield { coords with row = i }
                }
            | Down -> 
                seq {
                    for i = (coords.row + 1) to (this.height - 1) do
                        yield { coords with row = i }
                }
            | Left -> 
                seq {
                    for i = (coords.col - 1) downto 0 do
                        yield { coords with col = i }
                }
            | Right -> 
                seq { 
                    for i = (coords.col + 1) to (this.width - 1) do
                        yield { coords with col = i }
                }

        member this.all_coords : Coords seq =
            seq {
                for row = 0 to (this.height - 1) do
                    for col = 0 to (this.width - 1) do
                        yield {row = row; col = col}
            }

        member this.is_visible_from_edges (coords : Coords) : bool =
            let this_tree = this[coords] in
            directions
            |> Seq.exists (fun direction ->
                this.sightline_from coords direction
                |> Seq.map this.get
                |> Seq.forall (fun other_tree -> other_tree < this_tree)
            )

        member this.trees_visible_from_point (coords : Coords) (direction : Direction) : int seq =
            let this_tree = this[coords] in
            let mutable has_hit_boundary = false in
            seq {
                for other_tree_coords in (this.sightline_from coords direction) do
                    let other_tree = this[other_tree_coords] in
                    if not has_hit_boundary then
                        if (other_tree < this_tree) then
                            yield other_tree
                        else 
                            has_hit_boundary <- true
                            yield other_tree
            }

        member this.scenic_score (coords : Coords) : int =
            let this_tree = this[coords] in
            let viewing_distances = seq {
                for direction in directions do
                    let viewing_distance = 
                        this.trees_visible_from_point coords direction
                        |> Seq.length
                    in
                    yield viewing_distance
            }
            in
            Seq.reduce (*) viewing_distances


    let parse (input : string array) : t = 
        let cells = 
            input
            |> Array.map (fun line -> [| 
                for character in line.Trim() do
                    yield character |> string |> int
            |])
            |> array2D
        in { cells = cells }
end

module Puzzle = begin
    let part1 (input: string array) =
        let forest = Forest.parse input in
        forest.all_coords
        |> Seq.filter forest.is_visible_from_edges
        |> Seq.length

    let part2 (input: string array) =
        let forest = Forest.parse input in
        forest.all_coords
        |> Seq.map forest.scenic_score
        |> Seq.max
end
