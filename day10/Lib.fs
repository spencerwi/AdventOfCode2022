module Lib

open System

module VideoSystem = begin

    type Hook = (int * int) -> unit

    type State = {
        program_counter : int
        x_register : int
        per_cycle_hook : Hook
        update_history : int list
    }

    let build (per_cycle_hook : Hook) = {
        program_counter = 1
        x_register = 1
        per_cycle_hook = per_cycle_hook
        update_history = []
    }

    module Command = begin
        type t = 
            | Noop
            | Addx of int

        let parse (command_str : string) = 
            if command_str.Trim() = "noop" then 
                Noop
            else
                let segments = command_str.Trim().Split [|' '|] in
                let amount = int segments.[1] in
                Addx amount
    end

    let step (state : State) = function
        | Command.Noop -> 
            state.per_cycle_hook (state.program_counter, state.x_register);
            { state with program_counter = (state.program_counter + 1) }
        | Command.Addx amount ->
            state.per_cycle_hook (state.program_counter, state.x_register);
            state.per_cycle_hook (state.program_counter + 1, state.x_register);
            { state with 
                program_counter = state.program_counter + 2
                x_register = state.x_register + amount
            }

    let run hook commands =
        Seq.fold step (build hook) commands

    type Screen = 
        {
            pixels: string[,]
        } with
            member this.height = Array2D.length1 this.pixels
            member this.width = Array2D.length2 this.pixels
            member this.try_to_draw (program_counter, x) =
                let row = program_counter / this.width in
                let col = program_counter % this.width in
                if List.contains col [x .. (x + 2)] then
                    this.pixels[row, col] <- "#"

            member this.to_string() = 
                String.concat "\n" (seq {
                    for row = 0 to (this.height - 1) do
                        yield String.concat "" this.pixels[row,*]
                })

            static member make height width = 
                {
                    pixels = Array2D.create 6 40 "."
                }
end

module Puzzle = begin
    let part1 (input: string seq) : int =
        let commands = 
            input
            |> Array.ofSeq
            |> Array.map VideoSystem.Command.parse
        in
        let mutable interval_sum = 0 in
        let hook (program_counter, x) = 
            if (program_counter - 20) % 40 = 0 then
                let signal_strength = x * program_counter in
                interval_sum <- interval_sum + signal_strength
        in
        VideoSystem.run hook commands |> ignore;
        interval_sum


    let part2 (input: string seq) =
        let commands =
            input 
            |> Array.ofSeq
            |> Array.map VideoSystem.Command.parse
        in
        let mutable screen = VideoSystem.Screen.make 6 40 in
        let hook = screen.try_to_draw in
        VideoSystem.run hook commands |> ignore;
        screen.to_string()
end
