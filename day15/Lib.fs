module Lib

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open FSharp.Collections.ParallelSeq

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

        member this.up() = { this with y = this.y + 1 }
        member this.down() = { this with y = this.y - 1 }
        member this.left() = { this with x = this.x - 1 }
        member this.right() = { this with x = this.x + 1 }
            
        member this.neighbors : Point seq =
            seq {
                this.up(); this.down(); this.left(); this.right();
            }

    type SpaceObject =
        | Sensor of location: Point * range: int
        | Beacon
        | EmptySpace
    with 
        static member isSensor = function
            | Sensor _-> true
            | _ -> false

        static member isBeacon = function
            | Beacon -> true
            | _ -> false
        
        member this.range =
            match this with
            | Sensor (_, range) -> range
            | _ -> 0

        member this.range_boundaries = 
            match this with 
            | Sensor (location, range) ->
                seq {
                    let mutable y = 0 in
                    for x = (location.x - range) to (location.x + range) do
                        yield {x = x; y = location.y + y}
                        if y <> 0 then
                            yield {x = x; y = location.y - y}
                        if x > location.x then
                            y <- y + 1
                        else
                            y <- y - 1
                } |> Set.ofSeq
            | _ -> Set.empty

        member this.to_string() =
            match this with
            | Sensor _ -> "S"
            | Beacon -> "B"
            | EmptySpace -> "."

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

        member this.beacon_locations =
            this.objects
            |> Map.filter (fun location cell -> SpaceObject.isBeacon cell)
            |> Map.keys

        member this.sensors =
            this.objects.Values
            |> Seq.filter (function 
                | Sensor _ -> true
                | _ -> false
            )

        member this.points_in_range_of (p: Point) : Set<Point> =
            match this[p] with
            | Beacon | EmptySpace -> Set.empty
            | Sensor (_, range) ->
                Set.ofSeq <| seq {
                    for x = (p.x - range) to (p.x + range) do
                        for y = (p.x - range) to (p.y + range) do
                            let candidate = {x = x; y = y} in
                            if p.distance_to candidate <= range then
                                yield candidate
                }

        member this.colinear_points_in_range_of (p: Point) (y: int) : Set<Point> =
            match this[p] with
            | Beacon | EmptySpace -> Set.empty
            | Sensor (_, range) ->
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
            |> PSeq.map (fun (Sensor (location, range)) -> this.colinear_points_in_range_of location y)
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
            space[sensor] <- Sensor (sensor, sensor.distance_to beacon)
            space[beacon] <- Beacon
        done;
        space


end

module Puzzle = begin
    open type Space.Point
    open type Space.SpaceObject

    let part1 (input: string seq) (line_number : int) =
        let space = Space.build input in
        space.impossible_beacon_locations_on_line line_number
        |> Set.count

    let part2 (input: string seq) (bounds : int * int) =
        let (min, max) = bounds in
        let is_in_search_area (p: Space.Point) =
            p.x >= min && p.x <= max && p.y >= min && p.y <= max
        in
        let space = Space.build input in
        // Okay, let's think about this. There's _a_ unique solution here: the single distress beacon.
        // That means that in our zone, there's only one square that's not detected by any sensor.
        // Now, why would a square not be detected by a sensor? Because other beacons are "blocking" it.
        // So that means it has to be "boxed in" by other beacons. 
        // But since there's only one square that meets that criteria, that means that the "box" that contains it
        //  is exactly one square...so it has to have the "borders" of our other sensors on all sides of it.
        // So, let's take all our sensors, look at the absolute edges of their ranges, look at all the "outside" neighbors of those border points, and find one that's not in range of any other sensor either.
        let distress_beacon_location =
            space.sensors
            |> PSeq.collect (fun (Sensor (location, range)) -> 
                let sensor_with_larger_range = Sensor (location, range + 1) in
                sensor_with_larger_range.range_boundaries
            )
            |> PSeq.filter is_in_search_area
            |> PSeq.filter (fun candidate -> 
                match space[candidate] with
                | EmptySpace -> true
                | _ -> false
            )
            |> PSeq.find (fun candidate ->
                space.sensors
                |> PSeq.forall (fun (Sensor (sensor_location, range)) ->
                    (sensor_location.distance_to candidate) > range
                )
            )
        in
        let tuning_frequency = 
            ((uint64 distress_beacon_location.x) * 4_000_000UL) + (uint64 distress_beacon_location.y)
        in
        tuning_frequency
end
