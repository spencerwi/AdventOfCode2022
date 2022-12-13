module Tests
open Lib
open Lib.Mountain

open NUnit.Framework
open FsUnit

let sample_input = [|
    "Sabqponm";
    "abcryxxl";
    "accszExk";
    "acctuvwj";
    "abdefghi"
|]

[<TestFixture>]
type ``Mountain module`` ()=
    [<Test>]
    member this.``It should parse input correctly`` ()=
        Mountain.parse sample_input
        |> should equal {
            current_position = { row = 0; col = 0 }
            goal = { row = 2; col = 5 }
            mountain = array2D [|
                [| 0; 0; 1; 16; 15; 14; 13; 12 |];
                [| 0; 1; 2; 17; 24; 23; 23; 11 |];
                [| 0; 2; 2; 18; 25; 25; 23; 10 |];
                [| 0; 2; 2; 19; 20; 21; 22; 9 |];
                [| 0; 1; 3; 4; 5; 6; 7; 8 |];
            |]
        }

    [<Test>]
    member this.``It should figure out next available steps correctly`` ()=
        let initial_state =
            Mountain.parse sample_input
        in
        Mountain.available_next_steps_from initial_state.mountain initial_state.current_position
        |> Set.ofSeq
        |> should equal (Set.ofSeq <| seq {
            {row = 0; col = 1};
            {row = 1; col = 0};
        })

[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member this.``It should solve part 1`` ()=
        Puzzle.part1 sample_input
        |> should equal 31

    [<Test>]
    member this.``It should solve part 2`` ()=
        Puzzle.part2 sample_input
        |> should equal 29
