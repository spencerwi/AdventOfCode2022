module Lib

open System

module Forest = begin
    type t = int[,]
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


    let parse (input : string array) : t =
        input
        |> Array.map (fun line -> [| 
            for character in line.Trim() do
                yield character |> string |> int
        |])
        |> array2D

    let height (forest : t) =
        Array2D.length1 forest

    let width (forest : t) =
        Array2D.length2 forest

    let get (forest : t) (coords : Coords) : int =
        forest.[coords.row, coords.col]

    let all_coords_in (forest : t) : Coords seq =
        seq {
            for row = 0 to ((height forest) - 1) do
                for col = 0 to ((width forest) - 1) do
                    yield { row = row; col = col}
        }

    let is_in_bounds (coords : Coords) (forest : t) =
        coords.row >= 0 && coords.row < (height forest) &&
        coords.col >= 0 && coords.col < (width forest)

    let sightline_cells (forest : t) (coords : Coords) (direction : Direction)  =
        match direction with
        | Up ->
            seq {
                for i = (coords.row - 1) downto 0 do
                    yield { coords with row = i }
            }
        | Down -> 
            seq {
                for i = (coords.row + 1) to ((height forest) - 1) do
                    yield { coords with row = i }
            }
        | Left -> 
            seq {
                for i = (coords.col - 1) downto 0 do
                    yield { coords with col = i }
            }
        | Right -> 
            seq { 
                for i = (coords.col + 1) to ((width forest) - 1) do
                    yield { coords with col = i }
            }

    let is_visible_from_edges (forest : t) (coords : Coords) : bool =
        let this_tree = get forest coords in
        directions
        |> Seq.exists (fun direction ->
            sightline_cells forest coords direction
            |> Seq.map (get forest)
            |> Seq.forall (fun other_tree -> other_tree < this_tree)
        )

    let trees_visible_from_point (forest : t) (coords : Coords) (direction : Direction) =
        let this_tree = get forest coords in
        let mutable has_hit_boundary = false in
        seq {
            for other_tree_coords in (sightline_cells forest coords direction) do
                let other_tree = get forest other_tree_coords in
                if not has_hit_boundary then
                    if (other_tree < this_tree) then
                        yield other_tree
                    else 
                        has_hit_boundary <- true
                        yield other_tree

        }

    let scenic_score (forest : t) (coords : Coords) : int =
        let this_tree = get forest coords in
        let viewing_distances = seq {
            for direction in directions do
                let viewing_distance = 
                    trees_visible_from_point forest coords direction
                    |> Seq.length
                in
                yield viewing_distance
        }
        in
        Seq.reduce (*) viewing_distances

end

module Puzzle = begin
    let part1 (input: string array) =
        let forest = Forest.parse input in
        Forest.all_coords_in forest
        |> Seq.filter (Forest.is_visible_from_edges forest)
        |> Seq.length

    let part2 (input: string array) =
        let forest = Forest.parse input in
        Forest.all_coords_in forest
        |> Seq.map (Forest.scenic_score forest)
        |> Seq.max
end
