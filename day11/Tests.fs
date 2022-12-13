module Tests
open System
open System.Collections.Generic
open Lib

open NUnit.Framework
open FsUnit

let sample_input = [|
    "Monkey 0:";
    "  Starting items: 79, 98";
    "  Operation: new = old * 19";
    "  Test: divisible by 23";
    "    If true: throw to monkey 2";
    "    If false: throw to monkey 3";
    "";
    "Monkey 1:";
    "  Starting items: 54, 65, 75, 74";
    "  Operation: new = old + 6";
    "  Test: divisible by 19";
    "    If true: throw to monkey 2";
    "    If false: throw to monkey 0";
    "";
    "Monkey 2:";
    "  Starting items: 79, 60, 97";
    "  Operation: new = old * old";
    "  Test: divisible by 13";
    "    If true: throw to monkey 1";
    "    If false: throw to monkey 3";
    "";
    "Monkey 3:";
    "  Starting items: 74";
    "  Operation: new = old + 3";
    "  Test: divisible by 17";
    "    If true: throw to monkey 0";
    "    If false: throw to monkey 1"
|]

[<TestFixture>]
type ``Tests for Monkeys module`` ()=

    [<Test>]
    member this.``It parses a monkey correctly`` ()=
        Monkeys.parse sample_input.[0..5]
        |> should equal {
            Monkeys.number = 0
            Monkeys.items = new Queue<int>([79; 98])
            Monkeys.operation = Monkeys.Operation.Multiply (Monkeys.Operation.Old, Monkeys.Operation.Constant 19)
            Monkeys.divisibility_test = 23
            Monkeys.true_target = 2
            Monkeys.false_target = 3
            Monkeys.inspection_count = 0UL
        }

    [<Test>]
    member this.``It plays a round with division-by-3 correctly`` ()=
        let monkeys = 
            sample_input
            |> Lib.split_sequence_on ""
            |> Array.ofSeq
            |> Array.map Monkeys.parse
        in
        Monkeys.step (fun x -> x / 3) monkeys
        |> Array.map (fun monkey -> monkey.items.ToArray())
        |> should equal [|
            [| 20; 23; 27; 26 |];
            [| 2080; 25; 167; 207; 401; 1046 |];
            Array.empty;
            Array.empty
        |]

[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member this.``It should solve part 1`` ()=
        Puzzle.part1 sample_input
        |> should equal 10605UL

    [<Test>]
    member this.``It should solve part 2`` ()=
        Puzzle.part2 sample_input
        |> should equal 2713310158UL
