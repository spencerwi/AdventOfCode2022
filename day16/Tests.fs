module Tests
open Lib

open NUnit.Framework
open FsUnit

open type Valves.Valve

let sample_input = [|
    "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB";
    "Valve BB has flow rate=13; tunnels lead to valves CC, AA";
    "Valve CC has flow rate=2; tunnels lead to valves DD, BB";
    "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE";
    "Valve EE has flow rate=3; tunnels lead to valves FF, DD";
    "Valve FF has flow rate=0; tunnels lead to valves EE, GG";
    "Valve GG has flow rate=0; tunnels lead to valves FF, HH";
    "Valve HH has flow rate=22; tunnel leads to valve GG";
    "Valve II has flow rate=0; tunnels lead to valves AA, JJ";
    "Valve JJ has flow rate=21; tunnel leads to valve II"
|]

[<TestFixture>]
type ``Tests for Valves`` ()= 

    [<Test>]
    member this.``It should parse an individual valve correctly`` ()=
        Valves.Valve.parse sample_input[1]
        |> should equal {
            name = "BB"
            flow_rate = 13
            is_open = false
            connections = [ "CC"; "AA" ]
        }

    [<Test>]
    member this.``It should parse a ValveSystem correctly`` ()=
        let valve_system = Valves.ValveSystem.parse sample_input in
        valve_system.valves
        |> should equal <| Map.ofSeq (seq {
            ("AA", { name = "AA"; flow_rate = 0; connections = ["DD"; "II"; "BB"]; is_open = false });
            ("BB", { name = "BB"; flow_rate = 13; connections = ["CC"; "AA"]; is_open = false });
            ("CC", { name = "CC"; flow_rate = 2; connections = ["DD"; "BB"]; is_open = false });
            ("DD", { name = "DD"; flow_rate = 20; connections = ["CC"; "AA"; "EE"]; is_open = false });
            ("EE", { name = "EE"; flow_rate = 3; connections = ["FF"; "DD"]; is_open = false });
            ("FF", { name = "FF"; flow_rate = 0; connections = ["EE"; "GG"]; is_open = false });
            ("GG", { name = "GG"; flow_rate = 0; connections = ["FF"; "HH"]; is_open = false });
            ("HH", { name = "HH"; flow_rate = 22; connections = ["GG"]; is_open = false });
            ("II", { name = "II"; flow_rate = 0; connections = ["AA"; "JJ"]; is_open = false });
            ("JJ", { name = "JJ"; flow_rate = 21; connections = ["II"]; is_open = false });
        })

    [<Test>]
    member this.``It should figure out travel times correctly`` ()=
        let valve_system = Valves.ValveSystem.parse sample_input in

        valve_system.travel_time "AA" "DD"
        |> should equal 1

        valve_system.travel_time "AA" "AA"
        |> should equal 0

        valve_system.travel_time "AA" "CC"
        |> should equal 2

        valve_system.travel_time "AA" "GG"
        |> should equal 4

[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member this.``It should solve part 1`` ()=
        Puzzle.part1 sample_input
        |> should equal 1651

    [<Test>]
    member this.``It should solve part 2`` ()=
        Puzzle.part2 sample_input
        |> should equal 1707
