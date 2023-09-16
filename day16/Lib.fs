module Lib

open System
open System.Text.RegularExpressions
open FSharp.Collections.ParallelSeq

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


    type ValveSystem(valves : Map<string, Valve>, travel_times : Map<(string * string), int>) =
        new(valves: Map<string, Valve>) = 
            let distance_between (src: string) (dest : string) =
                let rec move (seen_valves : Set<string>) (current : string) = 
                    if current = dest then 
                        Some 0
                    else
                        let current_valve = valves[current] in
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
                in Option.get(move Set.empty src)
            in
            let travel_times = Map.ofSeq (seq {
                for (src, dest) in Seq.allPairs valves.Keys valves.Keys do
                    yield ((src, dest), distance_between src dest)
            }) in
            new ValveSystem(valves, travel_times)


        member this.valves 
            with get() = valves

        member this.travel_times
            with get() = travel_times

        member this.Item 
            with get (valve_name : string) = 
                this.valves[valve_name]

        member this.travel_time (src : string) (dest : string) : int =
            travel_times[(src, dest)]

        member this.open_valve (valve : Valve) =
            new ValveSystem(this.valves.Add(valve.name, valve.open_valve()), travel_times)

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


        static member parse (input : string seq) =
            let valves =
                input 
                |> Seq.map Valve.parse
                |> Seq.map (fun valve -> (valve.name, valve))
                |> Map.ofSeq
            in
            new ValveSystem(valves)


    type State = {
        valve_system : ValveSystem
        time_left : int
        current_location: string
        total_pressure_released : int
        logs : (int * string) list
    } with
        static member make (valve_system : ValveSystem) =
            {
                valve_system = valve_system
                time_left = 30
                current_location = "AA"
                total_pressure_released = 0
                logs = []
            }

        member this.tick() =
            let log_line = 
                if Seq.isEmpty this.valve_system.open_valves then 
                    "No valves are open."
                elif Seq.length this.valve_system.open_valves = 1 then 
                    let open_valve = Seq.exactlyOne this.valve_system.open_valves in
                    sprintf "Valve %s is open, releasing %d pressure." open_valve.name this.valve_system.total_flow_rate
                else
                    let open_valve_names = 
                        this.valve_system.open_valves
                        |> Seq.map (fun v -> v.name)
                        |> String.concat ", "
                    in
                    sprintf "Valves %s are open, releasing %d pressure." open_valve_names this.valve_system.total_flow_rate
            in
            { this with
                    total_pressure_released = this.total_pressure_released + this.valve_system.total_flow_rate
                    time_left = this.time_left - 1
                    logs = (List.append this.logs [(31 - this.time_left, log_line)])
            }

        member this.open_valve (valve : Valve) =
            if this.time_left <= 0 then
                this
            else
                if debug then printfn "You open valve %s." valve.name
                { this.tick() with
                        valve_system = this.valve_system.open_valve valve
                }

        member this.move_to (dest : Valve) =
            if this.time_left <= 0 then
                this
            else
                let travel_time = this.valve_system.travel_time this.current_location dest.name in
                if debug then printfn "Moving to %s" dest.name;
                let mutable current_state = this in
                for t in 1 .. travel_time do
                    if this.time_left > 0 then
                        current_state <- current_state.tick();
                done
                { current_state with current_location = dest.name }

        member this.go_open_valve (dest : Valve) =
            (this.move_to dest).open_valve dest

        member this.drain_clock() =
            let mutable state = this in
            while state.time_left > 0 do
                state <- state.tick()
            done;
            state

        member this.possible_routes() = 
            let rec possible_routes_from ((seen_valves : string list, state : State)) : (string list * State) seq =
                if state.time_left <= 0 then 
                    [(seen_valves, state)]
                else
                    let remaining_closed_valves_worth_opening = 
                        state.valve_system.valuable_closed_valves 
                        // If we can't reach it in time, then there's no point in going there
                        |> Seq.filter (fun valve -> 
                            let time_to_reach = state.valve_system.travel_time state.current_location valve.name in
                            time_to_reach <= state.time_left
                        )
                        // If we're going to revisit some valve, we're wasting time. Let's try avoiding that
                        |> Seq.filter (fun v -> not (List.contains v.name seen_valves))
                    in
                    if Seq.isEmpty remaining_closed_valves_worth_opening then
                        [(seen_valves, state.drain_clock())]
                    else
                        remaining_closed_valves_worth_opening
                        |> Seq.filter (fun closed_valve -> not (List.contains closed_valve.name seen_valves))
                        |> Seq.map (fun next_valve -> 
                            let updated_state = state.go_open_valve next_valve
                            let updated_route_so_far = List.append seen_valves [next_valve.name] in
                            possible_routes_from (updated_route_so_far, updated_state)
                        ) 
                        |> Seq.concat
            in
            possible_routes_from (List.empty, this)

end

module Puzzle = begin
    open Valves // lol

    let part1 (input: string seq) =
        let valve_system = ValveSystem.parse input in
        let state = State.make valve_system in
        let possible_routes = state.possible_routes() in
        possible_routes
        |> PSeq.map (fun (route, final_state) -> final_state.total_pressure_released)
        |> PSeq.max

    let part2 (input: string seq) =
        "the right answer"
end
