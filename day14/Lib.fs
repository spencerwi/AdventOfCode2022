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
        | Abyss
        with 
            member this.to_string() : String =
                match this with
                | Air -> "."
                | Rock -> "#"
                | Sand -> "o"
                | Abyss -> " "
            member this.blocks_sand() : bool =
                match this with
                | Sand -> true
                | Rock -> true
                | _ -> false

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

    let state_to_string (cave : t) (sand_position : Coords) :string = 
        String.concat "\n" <| seq {
            for row = 0 to (Array2D.length1 cave) - 1 do
                yield String.concat "" <| seq {
                    for col = 0 to (Array2D.length2 cave - 1) do
                        if {row = row; col = col} = sand_position then
                            yield Sand.to_string()
                        else
                            yield cave[row, col].to_string()
                }
        }

    let is_in_bounds (cave : t) (coords : Coords) =
        coords.row >= 0 && coords.col >= 0 &&
        coords.row < (Array2D.length1 cave) &&
        coords.col < (Array2D.length2 cave)

    let get (cave : t) (coords : Coords) =
        if is_in_bounds cave coords then
            cave[coords.row, coords.col]
        else
            Abyss

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
        (x_min, cave)

    let drop_sand (x_base : int) (cave : t) =
        let mutable sand_position = {
            row = 0
            col = 500 - x_base
        } in
        let mutable is_at_rest = false in
        let mutable has_fallen_off = false in
        while not is_at_rest && not has_fallen_off do
            Console.Clear();
            Console.WriteLine(state_to_string cave sand_position);
            let options = 
                [
                    sand_position.down();
                    sand_position.down().left();
                    sand_position.down().right()
                ] |> List.map (get cave)
            in
            match options with
            | [Abyss; _; _] -> 
                has_fallen_off <- true
            | [x; Abyss; _] when x.blocks_sand() ->
                has_fallen_off <- true
            | [x; y; Abyss] when x.blocks_sand() && y.blocks_sand() ->
                has_fallen_off <- true
            | [x; y; z] when x.blocks_sand() && y.blocks_sand() && z.blocks_sand() ->
                is_at_rest <- true
            | [Air; _; _] ->
                sand_position <- sand_position.down()
            | [x; Air; _] when x.blocks_sand() ->
                sand_position <- sand_position.down().left()
            | [x; y; Air] when x.blocks_sand() && y.blocks_sand() ->
                sand_position <- sand_position.down().right()
        done;
        Console.Clear();
        Console.WriteLine(state_to_string cave sand_position);
        if has_fallen_off then 
            printfn "Sand fell off!"
            None
        else 
            cave[sand_position.row, sand_position.col] <- Sand
            Some sand_position
end

module Puzzle = begin
    let part1 (input: string array) =
        let lines = 
            input
            |> Seq.collect Cave.Line.parse
        in
        let (x_base, cave) = Cave.build lines in
        let mutable most_recent_sand_landing = Cave.drop_sand x_base cave in
        let mutable sand_counter = 1 in
        while most_recent_sand_landing.IsSome do
            most_recent_sand_landing <- Cave.drop_sand x_base cave
            sand_counter <- sand_counter + 1
        done;
        sand_counter

    let part2 (input: string array) =
        "the right answer"
end
