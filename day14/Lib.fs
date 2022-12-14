module Lib

open System

// A global debug switch that I can turn on and off
let debug = false
//let debug = true

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

    type t = {
        cells: Cell[,]
        x_base: int
    } with
        member this.height = Array2D.length1 this.cells
        member this.width = Array2D.length2 this.cells
        member this.to_string : string =
            String.concat "\n" <| seq {
                for row = 0 to this.height - 1 do
                    yield 
                        this.cells[row, *]
                        |> Seq.map (fun cell -> cell.to_string())
                        |> String.concat ""
            }
    let empty (height : int) (width : int) (x_base : int) = 
            { 
                cells = Array2D.create height width Air
                x_base = x_base
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

    type t with
        member this.rebase (coords : Coords) =
            { coords with
                col = coords.col - this.x_base
            }
        member this.sand_origin =
            this.rebase {row = 0; col = 500}

        member this.is_in_bounds (coords : Coords) =
            coords.row >= 0 && coords.col >= 0 &&
            coords.row < this.height &&
            coords.col < this.width

        member this.get (coords : Coords) =
            if this.is_in_bounds coords then
                this.cells[coords.row, coords.col]
            else
                Abyss

        member this.add_floor() = 
            // TODO: I also need to make it wider. Maybe not infinite, but I could try tripling the width by padding out more and see if that does the trick
            // I would need to adjust the x_base, of course
            let floor_height = this.height + 1 in
            let new_cells = Array2D.init floor_height (this.width * 3) (fun row col ->
                if row < this.height && col >= this.width && col < (this.width * 2) then 
                    this.cells[row, col - this.width]
                else
                    Air
            )
            in
            let new_cave = {
                cells = new_cells
                x_base = this.x_base - this.width
            }
            (floor_height, new_cave)

        member this.get_with_floor (floor_height : int) (coords : Coords) =
            if coords.row >= floor_height then
                Rock
            else
                this.get coords


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

    type t with
        member this.draw_line (line : Line) =
            let start, stop =
                if line.stop > line.start then 
                    (this.rebase line.start, this.rebase line.stop)
                else 
                    (this.rebase line.stop, this.rebase line.start)
            in
            for row = start.row to stop.row do
                for col = start.col to stop.col do
                    this.cells[row, col] <- Rock

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
        let width = 1 + (x_max - x_min) in
        let height = 1 + (
            all_line_ends
            |> Seq.map (fun c -> c.row)
            |> Seq.max
        ) in
        let cave = empty height width x_min in
        for line in lines do
            cave.draw_line line
        done;
        cave

    let state_to_string (cave : t) (sand_position : Coords) :string = 
        String.concat "\n" <| seq {
            for row = 0 to cave.height - 1 do
                yield String.concat "" <| seq {
                    for col = 0 to cave.width - 1 do
                        if {row = row; col = col} = sand_position then
                            yield Sand.to_string()
                        else
                            yield cave.get({row = row; col = col}).to_string()
                }
        }

    type t with
        member this.drop_sand () =
            let mutable sand_position = this.sand_origin in
            let mutable is_at_rest = false in
            let mutable has_fallen_off = false in
            while not is_at_rest && not has_fallen_off do
                if debug then
                    Console.Clear();
                    Console.WriteLine(state_to_string this sand_position);
                let options = 
                    [
                        sand_position.down();
                        sand_position.down().left();
                        sand_position.down().right()
                    ] |> List.map this.get
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
            if debug then
                Console.Clear();
                Console.WriteLine(state_to_string this sand_position);
            if has_fallen_off then 
                if debug then printfn "Sand fell off!"
                None
            else 
                this.cells[sand_position.row, sand_position.col] <- Sand
                Some sand_position

        member this.drop_sand_with_floor (floor_height : int) =
            let mutable sand_position = this.sand_origin in
            let mutable is_at_rest = false in
            while not is_at_rest do
                if debug then
                    Console.Clear();
                    Console.WriteLine(state_to_string this sand_position);
                    Console.WriteLine(String.replicate this.width <| Rock.to_string());
                let options = 
                    [
                        sand_position.down();
                        sand_position.down().left();
                        sand_position.down().right();
                    ] |> List.map (this.get_with_floor floor_height)
                in
                match options with
                | [Air; _; _] -> 
                    sand_position <- sand_position.down()
                | [x; Air; _] when x.blocks_sand() ->
                    sand_position <- sand_position.down().left()
                | [x; y; Air] when x.blocks_sand() && y.blocks_sand() ->
                    sand_position <- sand_position.down().right()
                | _ -> // Abyss is no longer possible, so any other case counts as total blockage
                    is_at_rest <- true
            done;
            this.cells[sand_position.row, sand_position.col] <- Sand
            sand_position

end

module Puzzle = begin
    open type Cave.Coords

    let part1 (input: string array) =
        let lines = 
            input
            |> Seq.collect Cave.Line.parse
        in
        let cave = Cave.build lines in
        let mutable sand_counter = 0 in
        let mutable most_recent_sand_landing = Some {row = 0; col = 0}
        while most_recent_sand_landing.IsSome do
            most_recent_sand_landing <- cave.drop_sand()
            sand_counter <- sand_counter + 1
        done;
        (sand_counter - 1) // subtract one because the previous one fell off, or else we wouldn't have stopped

    let part2 (input: string array) =
        let lines = 
            input
            |> Seq.collect Cave.Line.parse
        in
        let original_cave = Cave.build lines in
        let (floor_height, cave) = original_cave.add_floor() in
        let mutable sand_counter = 0 in
        let mutable most_recent_sand_landing = None
        while most_recent_sand_landing <> Some cave.sand_origin do
            most_recent_sand_landing <- Some (cave.drop_sand_with_floor floor_height)
            sand_counter <- sand_counter + 1
        done;
        sand_counter
end
