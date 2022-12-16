module Tests
open Lib
open Lib.Rope

open NUnit.Framework
open FsUnit

let sample_input = [|
    "R 4";
    "U 4";
    "L 3";
    "D 1";
    "R 4";
    "D 1";
    "L 5";
    "R 2"
|]

[<TestFixture>]
type ``RopeMovement module``()=
    [<Test>]
    member this.``it should parse movements correctly`` ()=
        let movements = Seq.map Rope.Movement.parse sample_input in
        movements
        |> should equal (seq {
            right 4;
            up 4;
            left 3;
            down 1;
            right 4;
            down 1;
            left 5;
            right 2
        })

    [<Test>]
    member this.``it should move correctly`` ()= 
        let movements = Seq.map Rope.Movement.parse sample_input in
        let expected_positions = [|
            // R 4
            ({ x = 4; y = 0 }, {x = 3; y = 0});
            // U 4
            ({ x = 4; y = 4 }, {x = 4; y = 3});
            // L 3
            ({ x = 1; y = 4 }, {x = 2; y = 4});
            // D 1
            ({ x = 1; y = 3 }, {x = 2; y = 4});
            // R 4
            ({ x = 5; y = 3 }, {x = 4; y = 3});
            // D 1
            ({ x = 5; y = 2 }, {x = 4; y = 3});
            // L 5
            ({ x = 0; y = 2 }, {x = 1; y = 2});
            // R 2
            ({ x = 2; y = 2 }, {x = 1; y = 2})
        |]
        let mutable state = Rope.make 2 in
        let mutable i = 0 in
        for movement in movements do
            printfn "Moving %A" movement;
            state <- move state movement
            let current_position = (state.segments.[0].position, state.segments.[1].position) in
            current_position |> should equal (expected_positions.[i]);
            i <- i + 1


[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member this.``It should solve part 1`` ()=
        Puzzle.part1 sample_input
        |> should equal 13

    [<Test>]
    member this.``It should solve part 2`` ()=
        Puzzle.part2 sample_input
        |> should equal 1

        let longer_sample_input = [|
            "R 5";
            "U 8";
            "L 8";
            "D 3";
            "R 17";
            "D 10";
            "L 25";
            "U 20"
        |] in
        Puzzle.part2 longer_sample_input
        |> should equal 36
