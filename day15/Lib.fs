module Lib

open System
open System.Collections.Generic
open System.Text.RegularExpressions

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

module Space = begin
    type Point = {
        x: int
        y: int
    } with
        member this.distance_to (other : Point) =
            abs (other.x - this.x) +
            abs (other.y - this.y)

    type SpaceObject =
        | Sensor of closest_beacon: Point
        | Beacon
        | EmptySpace
    with 
        static member isSensor = function
            | Sensor _-> true
            | _ -> false

        member this.closest_beacon() = 
            match this with
            | Sensor b -> Some b
            | _ -> None
        
        member this.to_string() =
            match this with
            | Sensor _ -> "S"
            | Beacon -> "B"
            | EmptySpace -> "."

    type Cell = {
        value : SpaceObject
    }

    type t = {
        mutable objects: Map<Point, SpaceObject>
    } with
        member this.get (p : Point) =
            match this.objects.TryFind p with
            | Some o -> o
            | None -> EmptySpace

        member this.Item 
            with get (p: Point) = this.get p
            and set (p: Point) (value : SpaceObject) =
                this.objects <- this.objects.Add(p, value)

        member this.x_edges =
            this.objects.Keys
            |> Seq.map (fun point -> point.x)
            |> SeqExtras.min_and_max

        member this.y_edges = 
            this.objects.Keys
            |> Seq.map (fun point -> point.y)
            |> SeqExtras.min_and_max

        member this.sensors =
            this.objects.Keys
            |> Seq.map (fun p -> (p, this[p]))
            |> Seq.filter (fun (_, o) -> SpaceObject.isSensor o)

        member this.points_in_range_of (p: Point) (y: int) : Set<Point> =
            match this[p] with
            | Beacon | EmptySpace -> Set.empty
            | Sensor nearest_beacon ->
                let range = p.distance_to nearest_beacon in
                Set.ofSeq <| seq {
                    for x = (p.x - range) to (p.x + range) do
                        let candidate = {x = x; y = y} in
                        if p.distance_to candidate <= range then
                            yield candidate
                }

        member this.to_string() =
            let x_min, x_max = this.x_edges in
            let y_min, y_max = this.y_edges in
            (String.concat "" <| seq {
                for y = y_min.Value to y_max.Value do
                    for x = x_min.Value to x_max.Value do
                        yield this[{x = x; y = y}].to_string()
                    done;
                    yield "\n"
            }).Trim()

        member this.impossible_beacon_locations_on_line (y : int) =
            this.sensors
            |> Seq.map (fun (s, _) -> this.points_in_range_of s y)
            |> Set.unionMany
            |> Set.filter (fun point -> 
                match this.objects.TryFind point with
                | Some Beacon -> false
                | _ -> true
            )

    let empty() = 
        { objects = Map.empty }

    let build (input : string seq) =
        let space = empty() in
        let pattern = new Regex("Sensor at x=(?<sensor_x>-?\d+), y=(?<sensor_y>-?\d+): closest beacon is at x=(?<beacon_x>-?\d+), y=(?<beacon_y>-?\d+)") in
        for line in input do
            let match_groups = pattern.Match(line).Groups in
            let sensor = {
                x = int match_groups["sensor_x"].Value
                y = int match_groups["sensor_y"].Value
            } in
            let beacon = {
                x = int match_groups["beacon_x"].Value
                y = int match_groups["beacon_y"].Value
            } in
            space[sensor] <- Sensor beacon
            space[beacon] <- Beacon
        done;
        space


end

module Puzzle = begin
    let part1 (input: string seq) (line_number : int) =
        let space = Space.build input in
        space.impossible_beacon_locations_on_line line_number
        |> Set.count

    let part2 (input: string seq) =
        "the right answer"
end
