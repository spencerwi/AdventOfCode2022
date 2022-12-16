module Tests
open Lib

open NUnit.Framework
open FsUnit

let short_sample_input = [|
    "noop";
    "addx 3";
    "addx -5"
|]

let long_sample_input = [|
    "addx 15";
    "addx -11";
    "addx 6";
    "addx -3";
    "addx 5";
    "addx -1";
    "addx -8";
    "addx 13";
    "addx 4";
    "noop";
    "addx -1";
    "addx 5";
    "addx -1";
    "addx 5";
    "addx -1";
    "addx 5";
    "addx -1";
    "addx 5";
    "addx -1";
    "addx -35";
    "addx 1";
    "addx 24";
    "addx -19";
    "addx 1";
    "addx 16";
    "addx -11";
    "noop";
    "noop";
    "addx 21";
    "addx -15";
    "noop";
    "noop";
    "addx -3";
    "addx 9";
    "addx 1";
    "addx -3";
    "addx 8";
    "addx 1";
    "addx 5";
    "noop";
    "noop";
    "noop";
    "noop";
    "noop";
    "addx -36";
    "noop";
    "addx 1";
    "addx 7";
    "noop";
    "noop";
    "noop";
    "addx 2";
    "addx 6";
    "noop";
    "noop";
    "noop";
    "noop";
    "noop";
    "addx 1";
    "noop";
    "noop";
    "addx 7";
    "addx 1";
    "noop";
    "addx -13";
    "addx 13";
    "addx 7";
    "noop";
    "addx 1";
    "addx -33";
    "noop";
    "noop";
    "noop";
    "addx 2";
    "noop";
    "noop";
    "noop";
    "addx 8";
    "noop";
    "addx -1";
    "addx 2";
    "addx 1";
    "noop";
    "addx 17";
    "addx -9";
    "addx 1";
    "addx 1";
    "addx -3";
    "addx 11";
    "noop";
    "noop";
    "addx 1";
    "noop";
    "addx 1";
    "noop";
    "noop";
    "addx -13";
    "addx -19";
    "addx 1";
    "addx 3";
    "addx 26";
    "addx -30";
    "addx 12";
    "addx -1";
    "addx 3";
    "addx 1";
    "noop";
    "noop";
    "noop";
    "addx -9";
    "addx 18";
    "addx 1";
    "addx 2";
    "noop";
    "noop";
    "addx 9";
    "noop";
    "noop";
    "noop";
    "addx -1";
    "addx 2";
    "addx -37";
    "addx 1";
    "addx 3";
    "noop";
    "addx 15";
    "addx -21";
    "addx 22";
    "addx -6";
    "addx 1";
    "noop";
    "addx 2";
    "addx 1";
    "noop";
    "addx -10";
    "noop";
    "noop";
    "addx 20";
    "addx 1";
    "addx 2";
    "addx 2";
    "addx -6";
    "addx -11";
    "noop";
    "noop";
    "noop"
|]

[<TestFixture>]
type ``Tests for VideoSystem`` ()=
    [<Test>] 
    member this.``It parses commands correctly`` ()=
        short_sample_input
        |> Seq.map VideoSystem.Command.parse
        |> should equal [| 
            VideoSystem.Command.Noop;
            VideoSystem.Command.Addx 3;
            VideoSystem.Command.Addx -5;
        |]

    [<Test>] 
    member this.``It runs correctly`` ()=
        let expected_states = [|
            1;
            1;
            1;
            4;
            4; 
            -1
        |]
        let commands =
            short_sample_input
            |> Array.map VideoSystem.Command.parse
        let hook (program_counter, x) =
            let expected_register_value = 
                expected_states.[program_counter - 1]
            x |> should equal expected_register_value
        in
        let mutable state = VideoSystem.build hook in
        for command in commands do
            state <- VideoSystem.step state command


[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member this.``It should solve part 1`` ()=
        Puzzle.part1 long_sample_input
        |> should equal 13140

    [<Test>]
    member this.``It should solve part 2`` ()=
        Puzzle.part2 long_sample_input
        |> should equal (String.concat "\n" [
            "##..##..##..##..##..##..##..##..##..##..";
            "###...###...###...###...###...###...###.";
            "####....####....####....####....####....";
            "#####.....#####.....#####.....#####.....";
            "######......######......######......####";
            "#######.......#######.......#######....."
        ])

