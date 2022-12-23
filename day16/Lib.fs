module Lib

open System
open System.Text.RegularExpressions

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
            printfn "== Minute %d ==" (31 - this.time_left);
            let valve_status = 
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
            { this with
                    total_pressure_released = this.total_pressure_released + this.valve_system.total_flow_rate
                    time_left = this.time_left - 1
            }

        member this.open_valve (valve : Valve) =
            printfn "You open valve %s." valve.name
            { this.tick() with
                    valve_system = this.valve_system.open_valve valve
            }

        member this.move_to (dest : Valve) =
            let travel_time = this.valve_system.travel_time this.current_location dest.name in
            printfn "Moving to %s" dest.name;
            let mutable current_state = this in
            for t in 1 .. travel_time do
                if this.time_left > 0 then
                    current_state <- current_state.tick();
            done
            { current_state with current_location = dest.name }

end

module Puzzle = begin
    open Valves // lol

    let part1 (input: string seq) =
        let valve_system = ValveSystem.parse input in
        let mutable state = State.make valve_system in
        while state.time_left > 0 do
            // Let's try a strategy of "always go to the closest one that has the highest yield"
            // TODO: this strategy is wrong. Need to identify a different strategy for picking my next move.
            let closed_valves_worth_opening =
                valve_system.closed_valves
                |> Seq.filter (fun valve -> valve.flow_rate > 0) // 0-pressure valves aren't worth opening.
            if Seq.isEmpty closed_valves_worth_opening then
                state <- state.tick()
            else
                let (travel_time_used, dest) = 
                    valve_system.closed_valves
                    |> Seq.filter (fun valve -> valve.flow_rate > 0) 
                    |> Seq.map (fun valve -> 
                        let travel_time = 
                            valve_system.travel_time state.current_location valve.name
                        in
                        travel_time, valve
                    )
                    |> Seq.sortWith (fun (cost1, valve1) (cost2, valve2) ->
                        if compare cost1 cost2 <> 0 then
                            compare cost1 cost2
                        else
                            compare valve2.flow_rate valve1.flow_rate
                    )
                    |> Seq.head
                in
                state <- state.move_to dest
                if state.time_left > 0 then
                    state <- state.open_valve dest
        done;
        state.total_pressure_released

    let part2 (input: string seq) =
        "the right answer"
end
