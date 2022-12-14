module Tests
open Lib

open NUnit.Framework
open FsUnit

open type Cave.Coords
open type Cave.Cell
open type Cave.Line

let sample_input = [|
    "498,4 -> 498,6 -> 496,6";
    "503,4 -> 502,4 -> 502,9 -> 494,9"
|]

[<TestFixture>]
type ``SeqExtras module`` ()=
    [<Test>]
    member this.``min_and_max works with non-empty sequences`` ()=
        SeqExtras.min_and_max (seq { 1; 2; 3 })
        |> should equal (Some 1, Some 3)

        SeqExtras.min_and_max (seq { 1 })
        |> should equal (Some 1, Some 1)

    [<Test>]
    member this.``min_and_max works with empty sequences`` ()=
        SeqExtras.min_and_max Seq.empty
        |> should equal (None, None)

[<TestFixture>] 
type ``Cave module`` ()=
    [<Test>]
    member this.``It parses coordinates`` ()=
        Cave.Coords.parse "42,999"
        |> should equal { row = 999; col = 42 }

    [<Test>]
    member this.``It parses lines`` ()=
        Cave.Line.parse sample_input[0]
        |> should equal (seq {
            { 
                start = { row = 4; col = 498 }
                stop = { row = 6; col = 498 }
            };
            { 
                start = { row = 6; col = 498 }
                stop = { row = 6; col = 496 }
            }
        })

    [<Test>]
    member this.``It rebases coords correctly along the X axis`` ()=
        let original = Cave.Coords.parse "42,999" in
        original.rebase_x 42
        |> should equal { row = 999; col = 0 }

    [<Test>]
    member this.``It adjusts points in each direction correctly`` ()=
        let original = Cave.Coords.parse "42,999" in
        original.down()
        |> should equal { row = 1000; col = 42 }
        original.left()
        |> should equal { row = 999; col = 41 }
        original.right()
        |> should equal { row = 999; col = 43 }

    [<Test>]
    member this.``It draws lines onto a cave correctly`` ()=
        let cave = Cave.build false Seq.empty in
        let horizontal_line = { 
            start = { row = 2; col = 0 }
            stop = { row = 2; col = 9 }
        } in
        cave.draw_line horizontal_line;
        cave.to_string 
        |> should equal (String.concat "\n" <| [
            "..........";
            "..........";
            "##########";
        ])

        let vertical_line = {
            start = { row = 0; col = 4 }
            stop = { row = 4; col = 4 }
        } in
        cave.draw_line vertical_line;
        cave.to_string 
        |> should equal (String.concat "\n" <| [
            "....#.....";
            "....#.....";
            "##########";
            "....#.....";
            "....#.....";
        ])

[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member this.``It should solve part 1`` ()=
        Puzzle.part1 sample_input
        |> should equal 24

    [<Test>]
    member this.``It should solve part 2`` ()=
        Puzzle.part2 sample_input
        |> should equal 93
