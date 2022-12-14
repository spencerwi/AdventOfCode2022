module Lib

open System

// A global debug switch that I can turn on and off quickly
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

    let sand_origin = {
        row = 0
        col = 500
    }

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
        member this.ends =
            seq { this.start; this.stop }

    type t = {
        mutable cells: Map<Coords, Cell>
        mutable floor_depth: int option
    } with
        member this.depth =
            match this.floor_depth with
            | Some x -> x
            | None -> 
                this.cells.Keys
                |> Seq.map (fun point -> point.row)
                |> Seq.max

        member this.width = 
            let (leftmost, rightmost) = this.edges in
            (rightmost - leftmost)
            
        member this.edges = 
            let (leftmost, rightmost) =
                this.cells.Keys
                |> Seq.map (fun point -> point.col)
                |> SeqExtras.min_and_max
            in (leftmost.Value, rightmost.Value)

        member this.get (coords : Coords) =
            if this.cells.ContainsKey coords then
                this.cells[coords]
            elif coords.row > this.depth then
                Abyss
            else
                Air
        member this.Item 
            with get (coords : Coords) = this.get coords
            and set (coords: Coords) (value : Cell) =
                this.cells <- this.cells.Add(coords, value)

        member this.get_with_floor (coords : Coords) =
            match this.floor_depth with
            | None -> this[coords]
            | Some depth ->
                if coords.row >= depth then
                    Rock
                else
                    this.get coords

        member this.draw_line (line : Line) =
            let start, stop =
                if line.stop > line.start then 
                    (line.start, line.stop)
                else 
                    (line.stop, line.start)
            in
            for row = start.row to stop.row do
                for col = start.col to stop.col do
                    let point = {row = row; col = col} in
                    this[point] <- Rock

        member this.to_string : string =
            let (left_edge, right_edge) = this.edges in
            String.concat "\n" <| seq {
                for row = 0 to this.depth do
                    yield String.concat "" <| seq {
                        for col = left_edge to right_edge do
                            let cell = this.get_with_floor {row = row; col = col}
                            yield cell.to_string()
                        }
            }

    let build (with_floor : bool) (lines : Line seq) =
        let all_line_ends =
            lines
            |> Seq.collect (fun line -> line.ends)
        let floor_depth =
            if not with_floor then
                None
            else
                let y_max = 
                    all_line_ends
                    |> Seq.map (fun c -> c.row)
                    |> Seq.max
                in
                Some (y_max + 2)
        let cave = {
            cells = Map.empty
            floor_depth = floor_depth
        } in
        for line in lines do
            cave.draw_line line
        done;
        cave

    let state_to_string (cave : t) (sand_position : Coords) :string = 
        String.concat "\n" <| seq {
            for row = 0 to cave.depth - 1 do
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
            let mutable sand_position = sand_origin in
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
                this[sand_position] <- Sand
                Some sand_position

        member this.drop_sand_with_floor () =
            let mutable sand_position = sand_origin in
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
                    ] |> List.map this.get_with_floor
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
            this[sand_position] <- Sand;
            sand_position

end

module Puzzle = begin
    open type Cave.Coords

    let part1 (input: string array) =
        let lines = 
            input
            |> Seq.collect Cave.Line.parse
        in
        let cave = Cave.build false lines in
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
        let cave = Cave.build true lines in
        let mutable sand_counter = 0 in
        let mutable most_recent_sand_landing = None
        while most_recent_sand_landing <> Some Cave.sand_origin do
            most_recent_sand_landing <- Some (cave.drop_sand_with_floor())
            sand_counter <- sand_counter + 1
        done;
        sand_counter
end
