module Tests
open Lib

open NUnit.Framework
open FsUnit

let sample_input = """
Fill me in from the problem description!
"""

[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member this.``It should solve part 1`` ()=
        Puzzle.part1 sample_input
        |> should equal "the right answer"

    [<Test>]
    member this.``It should solve part 2`` ()=
        Puzzle.part2 sample_input
        |> should equal "the right answer"
