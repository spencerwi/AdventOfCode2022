module Tests
open Lib

open NUnit.Framework
open FsUnit

let sample_input = [|
    "Sensor at x=2, y=18: closest beacon is at x=-2, y=15";
    "Sensor at x=9, y=16: closest beacon is at x=10, y=16";
    "Sensor at x=13, y=2: closest beacon is at x=15, y=3";
    "Sensor at x=12, y=14: closest beacon is at x=10, y=16";
    "Sensor at x=10, y=20: closest beacon is at x=10, y=16";
    "Sensor at x=14, y=17: closest beacon is at x=10, y=16";
    "Sensor at x=8, y=7: closest beacon is at x=2, y=10";
    "Sensor at x=2, y=0: closest beacon is at x=2, y=10";
    "Sensor at x=0, y=11: closest beacon is at x=2, y=10";
    "Sensor at x=20, y=14: closest beacon is at x=25, y=17";
    "Sensor at x=17, y=20: closest beacon is at x=21, y=22";
    "Sensor at x=16, y=7: closest beacon is at x=15, y=3";
    "Sensor at x=14, y=3: closest beacon is at x=15, y=3";
    "Sensor at x=20, y=1: closest beacon is at x=15, y=3"
|]

open type Space.t
open type Space.Point
open type Space.SpaceObject

[<TestFixture>]
type ``Space module`` ()=
    [<Test>]
    member this.``It builds a space from puzzle input`` ()=
        let space = (Space.build sample_input) in
        space.to_string()
        |> should equal (String.concat "\n" [
            "....S.......................";
            "......................S.....";
            "...............S............";
            "................SB..........";
            "............................";
            "............................";
            "............................";
            "..........S.......S.........";
            "............................";
            "............................";
            "....B.......................";
            "..S.........................";
            "............................";
            "............................";
            "..............S.......S.....";
            "B...........................";
            "...........SB...............";
            "................S..........B";
            "....S.......................";
            "............................";
            "............S......S........";
            "............................";
            ".......................B...."
        ])

    [<Test>]
    member this.``It preserves closest-beacon information for sensors`` ()=
        let space = (Space.build sample_input) in
        space[{x=8; y=7}]
        |> should equal (Sensor {x=2; y=10})

//    [<Test>]
//    member this.``It finds points in range of a given sensor`` ()=
//        let space = Space.build sample_input in
//        space.points_in_range_of {x=8; y=7}
//        |> should equal (Set.ofSeq <| seq {
//            {x = 
//        })

[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member this.``It should solve part 1`` ()=
        Puzzle.part1 sample_input 10
        |> should equal 26

    [<Test>]
    member this.``It should solve part 2`` ()=
        Puzzle.part2 sample_input
        |> should equal "the right answer"
