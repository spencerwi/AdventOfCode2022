module Lib

open System

module SeqExtras = begin
    /// <description>
    /// Gets min and max of a seq in one pass, without having to do two full iterations on it
    /// </description>
    let min_and_max (s : 'a seq) =
        let mutable min = None in
        let mutable max = None in
        for element in s do
            match min with
            | None -> 
                min <- Some element
            | Some x when (compare element x) = -1 ->
                min <- Some element
            | _ -> ()
            match max with
            | None ->
                max <- Some element
            | Some x when (compare element x) = 1 ->
                max <- Some element
            | _ -> ()
        done;
        (min, max)
end

module Cave = begin
    type Cell = 
        | Air
        | Rock
        | Sand
        with 
            member this.to_string() : String =
                match this with
                | Air -> "."
                | Rock -> "#"
                | Sand -> "o"

    type t = Cell[,]
    let empty (height : int) (width : int) = 
        Array2D.create height width Air

    let to_string (cave : t) : string =
        String.concat "\n" <| seq {
            for row = 0 to (Array2D.length1 cave) - 1 do
                yield 
                    cave[row, *]
                    |> Seq.map (fun cell -> cell.to_string())
                    |> String.concat ""
        }

    type Coords = {
        row: int
        col: int
    } with 
        static member parse (coords_spec : string) : Coords =
            let pieces = coords_spec.Split "," in
            {
                row = int pieces[1]
                col = int pieces[0]
            }
        member this.rebase_x (x_base : int) : Coords =
            { this with col = this.col - x_base }
        member this.down() : Coords = { this with row = this.row + 1 }
        member this.left() : Coords = { this with col = this.col - 1 }
        member this.right() : Coords = { this with col = this.col + 1 }

    type Line = {
        start : Coords
        stop : Coords
    } with
        static member parse (input : string) : Line seq =
            let points = 
                input.Split " -> " 
                |> Seq.map Coords.parse
            in
            points
            |> Seq.pairwise
            |> Seq.map (fun (start, stop) -> {
                start = start
                stop = stop
            })

    let draw_line (cave : t) (line : Line) =
        let start, stop =
            if line.stop > line.start then (line.start, line.stop)
            else (line.stop, line.start)
        in
        for row = start.row to stop.row do
            for col = start.col to stop.col do
                cave[row, col] <- Rock

    let build (lines : Line seq) =
        let all_line_ends =
            lines
            |> Seq.collect (fun line ->
                seq { line.start ; line.stop }
            )
        let (x_min, x_max) = 
            all_line_ends
            |> Seq.map (fun c -> c.col)
            |> SeqExtras.min_and_max
            |> (fun (min_maybe, max_maybe) ->
                (min_maybe.Value, max_maybe.Value)
            )
        in
        let rebased_lines = 
            lines
            |> Seq.map (fun line ->
                {
                    start = (line.start.rebase_x x_min)
                    stop = (line.stop.rebase_x x_min)
                }
            )
        in
        let width = 1 + (x_max - x_min) in
        let height = 1 + (
            all_line_ends
            |> Seq.map (fun c -> c.row)
            |> Seq.max
        ) in
        let cave = empty height width in
        for line in rebased_lines do
            draw_line cave line
        done;
        cave

    let drop_sand (cave : t) =
        // TODO: drop_sand function, which modifies the cave in-place and returns 
        //  Some (new_sand_location) if the grain of sand landed, or None if it dropped into the abyss
        None

end

module Puzzle = begin
    let part1 (input: string array) =
        let lines = 
            input
            |> Seq.collect Cave.Line.parse
        in
        let cave = Cave.build lines in
        let mutable most_recent_sand_landing = Cave.drop_sand cave in
        let mutable sand_counter = 1 in
        while most_recent_sand_landing.IsSome do
            most_recent_sand_landing <- Cave.drop_sand cave
            sand_counter <- sand_counter + 1
        done;
        sand_counter

    let part2 (input: string array) =
        "the right answer"
end
