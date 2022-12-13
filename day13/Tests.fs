module Tests
open Lib

open NUnit.Framework
open FsUnit

let sample_input = [|
    "[1,1,3,1,1]";
    "[1,1,5,1,1]";
    "";
    "[[1],[2,3,4]]";
    "[[1],4]";
    "";
    "[9]";
    "[[8,7,6]]";
    "";
    "[[4,4],4,4]";
    "[[4,4],4,4,4]";
    "";
    "[7,7,7,7]";
    "[7,7,7]";
    "";
    "[]";
    "[3]";
    "";
    "[[[]]]";
    "[[]]";
    "";
    "[1,[2,[3,[4,[5,6,7]]]],8,9]";
    "[1,[2,[3,[4,[5,6,0]]]],8,9]"
|]

open type Packet.t
open type Packet.OrderingResult

[<TestFixture>]
type ``Packet module`` ()=
    [<Test>]
    member this.``Parses packets correctly`` ()=
        Packet.Parsing.parse sample_input.[0]
        |> should equal (ListPacket [
            Constant 1;
            Constant 1;
            Constant 3;
            Constant 1;
            Constant 1
        ])

    [<Test>]
    member this.``Parses packet-pairs correctly`` ()=
        Packet.Parsing.parse_pairs sample_input
        |> should equal (seq {
            (
                ListPacket [Constant 1; Constant 1; Constant 3; Constant 1; Constant 1],
                ListPacket [Constant 1; Constant 1; Constant 5; Constant 1; Constant 1]
            );
            (
                ListPacket [ListPacket [Constant 1]; ListPacket [Constant 2; Constant 3; Constant 4]],
                ListPacket [ListPacket [Constant 1]; Constant 4]
            );
            (
                ListPacket [Constant 9],
                ListPacket [ListPacket [Constant 8; Constant 7; Constant 6]]
            );
            (
                ListPacket [ListPacket [Constant 4; Constant 4]; Constant 4; Constant 4],
                ListPacket [ListPacket [Constant 4; Constant 4]; Constant 4; Constant 4; Constant 4]
            );
            (
                ListPacket [Constant 7; Constant 7; Constant 7; Constant 7],
                ListPacket [Constant 7; Constant 7; Constant 7]
            );
            (
                ListPacket [],
                ListPacket [Constant 3]
            );
            (
                ListPacket [ListPacket [ListPacket []]],
                ListPacket [ListPacket []]
            );
            (
                ListPacket [Constant 1; ListPacket [Constant 2; ListPacket [Constant 3; ListPacket [Constant 4; ListPacket [Constant 5; Constant 6; Constant 7]]]]; Constant 8; Constant 9],
                ListPacket [Constant 1; ListPacket [Constant 2; ListPacket [Constant 3; ListPacket [Constant 4; ListPacket [Constant 5; Constant 6; Constant 0]]]]; Constant 8; Constant 9]
            );
        })

    [<Test>] 
    member this.``Compares constant packets correctly`` ()=
        let left = Constant 1 in
        let right = Constant 2 in
        Packet.compare_packets left right
        |> should equal InOrder
        Packet.compare_packets right left
        |> should equal OutOfOrder
        Packet.compare_packets left left
        |> should equal Equal

    [<Test>]
    member this.``Compares different-sized list packets correctly`` ()=
        let left = ListPacket [ ] in
        let right = ListPacket [ Constant 1 ] in
        Packet.compare_packets left right
        |> should equal InOrder
        Packet.compare_packets right left
        |> should equal OutOfOrder

    [<Test>]
    member this.``Compares same-sized list packets correctly`` ()=
        let left = ListPacket [ Constant 1 ] in
        let right = ListPacket [ Constant 2 ] in
        Packet.compare_packets left right
        |> should equal InOrder
        Packet.compare_packets right left
        |> should equal OutOfOrder

    [<Test>]
    member this.``Compares list packets to integer packets correctly`` ()=
        let left = ListPacket [ Constant 0; Constant 0 ; Constant 0 ] in
        let right = Constant 2 in
        Packet.compare_packets left right
        |> should equal InOrder
        Packet.compare_packets right left
        |> should equal OutOfOrder

    [<Test>]
    member this.``Compares packets correctly`` ()=
        let packet_pairs = Packet.Parsing.parse_pairs sample_input in
        let expected_results = [
            InOrder;
            InOrder;
            OutOfOrder;
            InOrder;
            OutOfOrder;
            InOrder;
            OutOfOrder;
            OutOfOrder
        ] in
        let actual_results =
            packet_pairs
            |> Seq.map (fun (packet1, packet2) ->
                Packet.compare_packets packet1 packet2
            )
            |> List.ofSeq
        in
        actual_results
        |> should equal expected_results


[<TestFixture>]
type ``Tests for solution`` ()=
    [<Test>]
    member this.``It should solve part 1`` ()=
        Puzzle.part1 sample_input
        |> should equal 13

    [<Test>]
    member this.``It should solve part 2`` ()=
        Puzzle.part2 sample_input
        |> should equal 140
