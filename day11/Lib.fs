module Lib

open System
open System.Text.RegularExpressions
open System.Collections.Generic

let split_sequence_on (delimiter : 'a) (sequence : 'a seq) : 'a seq seq =
    seq {
        let mutable current = Seq.empty in
        for item in sequence do
            if item = delimiter then
                yield current
                current <- Seq.empty
            else
                current <- Seq.append current (seq { item })
        yield current
    }

module Monkeys = begin

    module Operation = begin
        type Operand = 
            | Constant of int
            | Old

        type t =
            | Add of Operand * Operand
            | Subtract of Operand * Operand
            | Multiply of Operand * Operand
            | Divide of Operand * Operand

        let parse x operator y =
            let op1 = 
                match x with
                | "old" -> Old
                | _ -> Constant (int x)
            in
            let op2 = 
                match y with
                | "old" -> Old
                | _ -> Constant (int y)
            in
            match operator with
            | "+" -> Add (op1, op2)
            | "-" -> Subtract (op1, op2)
            | "*" -> Multiply (op1, op2)
            | "/" -> Divide (op1, op2)
            | _ -> failwith ("Unrecognized operator: " + operator)

        let eval_operand (old_value : int) = function
            | Constant x -> x
            | Old -> old_value

        let eval (operation: t) (old_value : int) =
            match operation with
            | Add (op1, op2) -> (eval_operand old_value op1) + (eval_operand old_value op2)
            | Subtract (op1, op2) -> (eval_operand old_value op1) - (eval_operand old_value op2)
            | Multiply (op1, op2) -> (eval_operand old_value op1) * (eval_operand old_value op2)
            | Divide (op1, op2) -> (eval_operand old_value op1) / (eval_operand old_value op2)

    end

    type t = {
        number : int
        items : Queue<int>
        operation : Operation.t
        divisibility_test : int
        true_target : int
        false_target : int
        mutable inspection_count : uint64
    }


    let parse (input : string seq) : t =
        let lines = Array.ofSeq input in
        let number = int <| (lines.[0].Split [|' '|]).[1].Replace(":", "") in
        let starting_items = [
            for regex_match in Regex.Matches(lines.[1], "(?<item>\d+)") do
                let item = regex_match.Groups["item"].Value in
                yield int item
        ] in
        let operation_match = Regex.Match(lines.[2], "new = (?<x>old|\d+) (?<operator>\+|-|\*|/) (?<y>old|\d+)") in
        let operation = 
            Operation.parse operation_match.Groups["x"].Value operation_match.Groups["operator"].Value operation_match.Groups["y"].Value in
        let divisibility_test = 
            int <| Regex.Match(lines.[3], "divisible by (?<factor>\d+)").Groups["factor"].Value 
        in
        let true_target = 
            int <| Regex.Match(lines.[4], "throw to monkey (?<monkey>\d+)").Groups["monkey"].Value 
        in
        let false_target = 
            int <| Regex.Match(lines.[5], "throw to monkey (?<monkey>\d+)").Groups["monkey"].Value 
        in
        {
            number = number
            items = new Queue<int>(starting_items)
            operation = operation
            divisibility_test = divisibility_test
            true_target = true_target
            false_target = false_target
            inspection_count = 0UL
        }

    let step (worry_modifier : int -> int) (monkeys : t array) : t array =
        let new_monkeys = Array.copy monkeys in
        let take_turn monkey_number =
            let monkey = new_monkeys.[monkey_number] in
            let this_round_inspection_count = uint64 monkey.items.Count in
            monkey.inspection_count <- monkey.inspection_count + this_round_inspection_count
            while monkey.items.Count > 0 do
                let item = monkey.items.Dequeue() in
                let operation_result = 
                    item
                    |> (Operation.eval monkey.operation)
                in
                let final_worry_level = worry_modifier operation_result in
                let target = 
                    if final_worry_level % (monkey.divisibility_test) = 0 then
                        monkey.true_target
                    else
                        monkey.false_target
                in
                new_monkeys.[target].items.Enqueue final_worry_level 
        in
        for i = 0 to (monkeys.Length - 1) do 
            take_turn i
        done;
        new_monkeys

    let monkey_business_level (monkeys : t array) : uint64 =
        monkeys
        |> Seq.map (fun monkey -> monkey.inspection_count)
        |> Seq.sortDescending 
        |> Seq.take 2
        |> Seq.reduce (*)
end

module Puzzle = begin
    let run (worry_modifier : int -> int) (round_count : int) (input : string seq) : uint64 =
        let mutable monkeys = 
            input
            |> split_sequence_on ""
            |> Array.ofSeq
            |> Array.map Monkeys.parse
        for round = 1 to round_count do
            monkeys <- Monkeys.step (fun x -> x / 3) monkeys
        done;
        Monkeys.monkey_business_level monkeys

    let part1 (input: string seq) =
        run (fun x -> x / 3) 20 input

    let part2 (input: string seq) =
        let some_stupid_obscure_math_trick = (fun x -> x) // TODO in
        run some_stupid_obscure_math_trick 10_000 input
end
