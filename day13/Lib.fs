module Lib
open System

module Packet = begin
    type t = 
        | ListPacket of t list
        | Constant of int

    type OrderingResult =
        | InOrder
        | OutOfOrder
        | Equal

    let to_comparator_value = function
        | InOrder -> -1
        | OutOfOrder -> 1
        | Equal -> 0

    let rec compare_packets p1 p2 = 
        let rec compare_lists left right =
            match (left, right) with
            | ([], []) -> Equal // Means we hit the end of the list without finding an inequality, so they're equal
            | ([], _) -> InOrder
            | (_, []) -> OutOfOrder
            | (left_item::left_rest, right_item::right_rest) ->
                match compare_packets left_item right_item with
                | InOrder -> InOrder
                | OutOfOrder -> OutOfOrder
                | Equal -> compare_lists left_rest right_rest
        in
        match (p1, p2) with
        | (Constant left, Constant right) -> 
            if left < right then
                InOrder
            elif left > right then
                OutOfOrder
            else
                Equal
        | (ListPacket left, ListPacket right) -> 
            compare_lists left right
        | (Constant left, ListPacket right) ->
            compare_packets (ListPacket [Constant left]) (ListPacket right)
        | (ListPacket left, Constant right) ->
            compare_packets (ListPacket left) (ListPacket [Constant right])

    let dividers = seq {
        ListPacket [ListPacket [Constant 2]];
        ListPacket [ListPacket [Constant 6]]
    }

    module Parsing = begin
        open FParsec

        let parse (input : string) =
            let pConstant = pint32 |>> Constant in
            let pPacket, pPacketImpl = createParserForwardedToRef() in
            let pListElements = sepBy pPacket (pchar ',') in
            let pListContents = spaces >>. pListElements |>> ListPacket in
            let pList = between (pchar '[') (pchar ']') pListContents in
            pPacketImpl.Value <- choice [ pConstant ; pList ];
            match run pPacket input with
            | Success(result, _, _) -> result
            | Failure(errorMsg, _, _) -> failwith errorMsg

        let parse_pairs (input : string seq) =
            input
            |> Seq.filter (not << String.IsNullOrEmpty)
            |> Seq.map parse
            |> Seq.chunkBySize 2
            |> Seq.map (fun pair ->
                (pair[0], pair[1])
            )
    end
end

module Puzzle = begin
    let part1 (input: string seq) =
        let packet_pairs = Packet.Parsing.parse_pairs input in
        packet_pairs
        |> Seq.mapi (fun idx (left, right) ->
            (idx + 1, Packet.compare_packets left right)
        )
        |> Seq.filter (fun (_, result) -> result = Packet.InOrder)
        |> Seq.map fst
        |> Seq.sum

    let part2 (input: string seq) =
        let all_packets = 
            input
            |> Seq.filter (not << String.IsNullOrEmpty)
            |> Seq.map Packet.Parsing.parse
        in
        let correctly_ordered_packets = 
            (Seq.append all_packets Packet.dividers)
            |> Seq.sortWith (fun a b -> 
                Packet.compare_packets a b
                |> Packet.to_comparator_value
            )
        let decoder_key = 
            correctly_ordered_packets
            |> Seq.mapi (fun idx packet ->
                (idx + 1, Seq.contains packet Packet.dividers)
            )
            |> Seq.filter snd
            |> Seq.map fst
            |> Seq.reduce (*)
        in
        decoder_key
end
