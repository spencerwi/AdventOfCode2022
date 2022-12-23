module Lib

open System
open System.Text.RegularExpressions

let debug = false;

module Valves = begin
    type Valve = {
        name : string
        flow_rate : int
        is_open : bool
        connections : string list
    } with
        static member parse (input : string) =
            let match_groups = 
                Regex.Match(
                    input,
                    "Valve (?<name>[A-Z]+) has flow rate=(?<flow_rate>[0-9]+); (tunnels lead to valves|tunnel leads to valve) (?<connections>([A-Z]{2}(, )*)+)"
                ).Groups 
            in
            let connections = 
                let connections_str = match_groups["connections"].Value in
                if connections_str.Contains ","  then
                    connections_str.Split(",", StringSplitOptions.RemoveEmptyEntries)
                    |> Seq.map (fun s -> s.Trim())
                    |> List.ofSeq
                else
                    [ connections_str.Trim() ]
            in
            {
                name = match_groups["name"].Value
                flow_rate = int (match_groups["flow_rate"].Value)
                is_open = false
                connections = connections
            }

        member this.open_valve () =
            { this with is_open = true }


    type ValveSystem = {
        valves : Map<string, Valve>
        mutable travel_times : Map<(string * string), int>
    } with
        member this.Item 
            with get (valve_name : string) = 
                this.valves[valve_name]

        member this.open_valve (valve : Valve) =
            { this with
                    valves = this.valves.Add(valve.name, valve.open_valve())
            }

        member this.open_valves =
            this.valves.Values
            |> Seq.filter (fun valve -> valve.is_open)

        member this.closed_valves =
            this.valves.Values
            |> Seq.filter (fun valve -> not valve.is_open)

        member this.valuable_closed_valves =
            this.closed_valves
            |> Seq.filter (fun valve -> valve.flow_rate > 0)

        member this.total_flow_rate =
            this.open_valves
            |> Seq.map (fun valve -> valve.flow_rate)
            |> Seq.sum

        member this.travel_time (src : string) (dest : string) =
            if src = dest then 0
            else
                match this.travel_times.TryFind (src, dest) with 
                | Some t -> t
                | None ->
                    let rec move (seen_valves : Set<string>) (current : string) = 
                        if current = dest then 
                            Some 0
                        else
                            let current_valve = this[current] in
                            let next_steps = 
                                current_valve.connections
                                |> Seq.filter (not << seen_valves.Contains)
                            in
                            if Seq.isEmpty next_steps then
                                None
                            else
                                next_steps
                                |> Seq.map (fun next ->
                                    move (seen_valves.Add current) next
                                )
                                |> Seq.filter Option.isSome // eliminate results that had no path
                                |> Seq.map Option.get // unwrap those
                                |> Seq.sort // sort ascending, shortest child distance first
                                |> Seq.tryHead // try to take that shortest one if it exists
                                |> Option.map (fun distance -> distance + 1) // and add 1 to it
                    in 
                    let travel_time = move Set.empty src in
                    this.travel_times <- this.travel_times.Add((src, dest), travel_time.Value);
                    travel_time.Value

        member this.highest_flow_rate_closed_valve =
            this.valves.Values
            |> Seq.filter (fun valve -> not valve.is_open)
            |> Seq.maxBy (fun valve -> valve.flow_rate)

        static member parse (input : string seq) =
            let valves =
                input 
                |> Seq.map Valve.parse
                |> Seq.map (fun valve -> (valve.name, valve))
                |> Map.ofSeq
            in
            // Initialize travel times with 1-step connections
            let travel_times =
                valves.Values
                |> Seq.collect (fun src ->
                    seq { 
                        for dest in src.connections do
                            yield ((src.name, dest), 1)
                    }
                )
                |> Map.ofSeq
            in { 
                valves = valves 
                travel_times = travel_times
            }


    type State = {
        valve_system : ValveSystem
        time_left : int
        current_location: string
        total_pressure_released : int
    } with
        static member make (valve_system : ValveSystem) =
            {
                valve_system = valve_system
                time_left = 30
                current_location = "AA"
                total_pressure_released = 0
            }

        member this.tick() =
            if debug then begin
                printfn "== Minute %d ==" (31 - this.time_left);
                if Seq.isEmpty this.valve_system.open_valves then Console.WriteLine "No valves are open."
                elif Seq.length this.valve_system.open_valves = 1 then 
                    let open_valve = Seq.exactlyOne this.valve_system.open_valves in
                    printfn "Valve %s is open, releasing %d pressure." open_valve.name this.valve_system.total_flow_rate
                else
                    let open_valve_names = 
                        this.valve_system.open_valves
                        |> Seq.map (fun v -> v.name)
                        |> String.concat ", "
                    in
                    printfn "Valves %s are open, releasing %d pressure." open_valve_names this.valve_system.total_flow_rate
            end;
            { this with
                    total_pressure_released = this.total_pressure_released + this.valve_system.total_flow_rate
                    time_left = this.time_left - 1
            }

        member this.open_valve (valve : Valve) =
            if debug then printfn "You open valve %s." valve.name
            { this.tick() with
                    valve_system = this.valve_system.open_valve valve
            }

        member this.move_to (dest : Valve) =
            let travel_time = this.valve_system.travel_time this.current_location dest.name in
            if debug then printfn "Moving to %s" dest.name;
            let mutable current_state = this in
            for t in 1 .. travel_time do
                if this.time_left > 0 then
                    current_state <- current_state.tick();
            done
            { current_state with current_location = dest.name }

        member this.drain_clock() =
            let mutable state = this in
            while state.time_left > 0 do
                state <- state.tick()
            done;
            state

        member this.possible_routes() = 
            let rec possible_routes_from ((seen_valves : string list, state : State)) (current_valve: string) : (string list * State) seq =
                if state.time_left <= 0 then [(seen_valves, state)]
                else
                    let remaining_closed_valves_worth_opening = 
                        state.valve_system.valuable_closed_valves 
                    in
                    if Seq.isEmpty remaining_closed_valves_worth_opening then
                        [(seen_valves, state.drain_clock())]
                    else
                        remaining_closed_valves_worth_opening
                        |> Seq.filter (fun closed_valve -> not (List.contains closed_valve.name seen_valves))
                        |> Seq.map (fun next_valve -> 
                            let moved_state = state.move_to next_valve in
                            let updated_state = moved_state.open_valve next_valve in
                            let updated_route_so_far = List.append seen_valves [next_valve.name] in
                            possible_routes_from (updated_route_so_far, updated_state) next_valve.name
                        ) 
                        |> Seq.concat
            in
            possible_routes_from (List.empty, this) this.current_location

end

module Puzzle = begin
    open Valves // lol

    let part1 (input: string seq) =
        let valve_system = ValveSystem.parse input in
        let state = State.make valve_system in
        let possible_routes = state.possible_routes() in
        possible_routes
        |> Seq.map (fun (route, final_state) -> final_state.total_pressure_released)
        |> Seq.max

    let part2 (input: string seq) =
        "the right answer"
end
