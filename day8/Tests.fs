module Tests
open System
open Lib

open NUnit.Framework
open FsUnit

let sample_input = [|
    "30373";
    "25512";
    "65332";
    "33549";
    "35390"
|]

[<TestFixture>]
type ``Forest module`` ()=
    [<Test>]
    member this.``it should parse input correctly`` ()=
        sample_input
        |> Forest.parse
        |> should equal (array2D [|
            [| 3; 0; 3; 7; 3 |];
            [| 2; 5; 5; 1; 2 |];
            [| 6; 5; 3; 3; 2 |];
            [| 3; 3; 5; 4; 9 |];
            [| 3; 5; 3; 9; 0 |]
        |])

    [<Test>]
    member this.``generates sightlines correctly`` ()=
        let forest = 
            sample_input
            |> Forest.parse
        in
        Forest.sightline_cells forest {row = 2; col = 2} Forest.Left
        |> should equal (seq { 
            { Forest.row = 2; Forest.col = 1 }
            { Forest.row = 2; Forest.col = 0 }
        });
        Forest.sightline_cells forest {row = 2; col = 2} Forest.Right
        |> should equal (seq { 
            { Forest.row = 2; Forest.col = 3 }
            { Forest.row = 2; Forest.col = 4 }
        })
        Forest.sightline_cells forest {row = 2; col = 2} Forest.Up
        |> should equal (seq { 
            { Forest.row = 1; Forest.col = 2 }
            { Forest.row = 0; Forest.col = 2 }
        })
        Forest.sightline_cells forest {row = 2; col = 2} Forest.Down
        |> should equal (seq { 
            { Forest.row = 3; Forest.col = 2 }
            { Forest.row = 4; Forest.col = 2 }
        })

    [<Test>]
    member this.``determines edge-visibility correctly`` ()=
        let forest = 
            sample_input
            |> Forest.parse
        in
        Forest.is_visible_from_edges forest {row = 1; col = 1}
        |> should equal true
        Forest.is_visible_from_edges forest {row = 1; col = 2}
        |> should equal true
        Forest.is_visible_from_edges forest {row = 3; col = 2}
        |> should equal true
        Forest.is_visible_from_edges forest {row = 4; col = 3}
        |> should equal true

        Forest.is_visible_from_edges forest {row = 2; col = 2}
        |> should equal false

    [<Test>]
    member this.``determines what trees are visible from a point correctly`` ()=
        let forest =
            sample_input
            |> Forest.parse
        in
        Forest.trees_visible_from_point forest { row = 1; col = 2} Forest.Up
        |> should equal (seq { 3 })
        Forest.trees_visible_from_point forest { row = 1; col = 2} Forest.Left
        |> should equal (seq { 5 })
        Forest.trees_visible_from_point forest { row = 1; col = 2} Forest.Right
        |> should equal (seq { 1; 2 })
        Forest.trees_visible_from_point forest { row = 1; col = 2} Forest.Down
        |> should equal (seq { 3; 5 })

    [<Test>]
    member this.``determines scenic score correctly`` ()=
        let forest = 
            sample_input
            |> Forest.parse
        in
        Forest.scenic_score forest { row = 1; col = 2}
        |> should equal 4;
        Forest.scenic_score forest { row = 3; col = 2}
        |> should equal 8


[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member this.``It should solve part 1`` ()=
        Puzzle.part1 sample_input
        |> should equal 21

    [<Test>]
    member this.``It should solve part 2`` ()=
        Puzzle.part2 sample_input
        |> should equal 8
