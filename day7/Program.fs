open System

/// <description>
/// This differs from the one used on day1 because this _includes_ the line where we split
///  as the first entry in each group
/// </dscription>
let split_seq_when (test: 'a -> bool) (inputs: 'a seq) : 'a seq seq =
    seq {
        let mutable current_seq = Seq.empty in
        for current_value in inputs do
            current_seq <- Seq.append current_seq ( seq { current_value })
            if (test current_value) then
                yield current_seq;
                current_seq <- Seq.empty
    }

module Filesystem = begin
    type Node = 
        | Dir_node of Directory 
        | File_node of File
    and Directory = {
        parent: Directory option
        name : string
        mutable subdirs : List<Directory>
        mutable files : List<File>
        mutable size : uint64
    }
    and File = {
        parent: Directory
        name : string
        size: uint64
    } 

    type State = {
        root : Directory
        currentDir : Directory
    }

    let mk_initial_state () =
        let root =
            {
                parent = None
                name = "/"
                subdirs = List.empty
                files = List.empty
                size = 0UL
            }
        in
        {
            root = root
            currentDir = root
        }

    let parse_entry (parent: Directory) (entry : string)  =
        if entry.StartsWith "dir" then
            let name = (entry.Split [|' '|]).[1] in
            Dir_node {
                parent = Some parent
                name = name.Trim()
                subdirs = List.empty
                files = List.empty
                size = 0UL
            }
        else
            let size_and_name = entry.Split [|' '|] in
            let size = uint64 size_and_name.[0] in
            let name = size_and_name.[1] in
            let file = { 
                parent = parent
                name = name.Trim()
                size = size
            } in
            File_node file

    module Commands = begin
        type t =
            | CD of string
            | LS of string seq

        let parse_command (input_lines : string seq) : t =
            match (Seq.head input_lines).[0..1] with
            | "ls" -> 
                let listings = 
                    input_lines
                    |> Seq.skip 1
                in
                LS listings
            | "cd" -> 
                let subdir = (Seq.head input_lines).Substring(2).Trim() in
                CD subdir
            | other -> failwith ("Unrecognized command: " + other)


        let get_dir (current : Directory) (subdir_name : string) : Directory =
            let maybe_subdir = 
                current.subdirs
                |> Seq.tryFind (fun subdir -> subdir.name = subdir_name)
            in
            match maybe_subdir with
            | Some d -> d
            | None -> failwith ("Directory not found: " + subdir_name + " in " + current.name)

        let cd (state : State) (path : string) : State =
            match path with 
            | "/" -> { state with currentDir = state.root }
            | ".." when state.currentDir.parent.IsNone -> state
            | ".." when state.currentDir.parent.IsSome ->  { state with currentDir = state.currentDir.parent.Value }
            | dirname -> 
                { state with currentDir = (get_dir state.currentDir dirname) }

        let ls (state : State) (listings : string seq) : State =
            if (not state.currentDir.subdirs.IsEmpty || not state.currentDir.files.IsEmpty) then
                // if this already has children, we've already listed this directory. Don't duplicate.
                state
            else
                for listing in listings do
                    let parsed_child = parse_entry state.currentDir listing in
                    match parsed_child with 
                    | Dir_node d -> 
                        state.currentDir.subdirs <- List.append state.currentDir.subdirs [d]
                    | File_node f ->
                        state.currentDir.files <- List.append state.currentDir.files [f]
                        state.currentDir.size <- state.currentDir.size + f.size
                        let mutable ancestor = state.currentDir.parent in
                        while (ancestor.IsSome) do
                            ancestor.Value.size <- ancestor.Value.size + f.size
                            ancestor <- ancestor.Value.parent
                state

        let eval (state : State) (command : t) : State =
            match command with
            | LS listings -> ls state listings
            | CD subdir -> cd state subdir
    end

    let rec to_string (indent_level: int) (tree : Node) : string =
        let prefix = (String.replicate indent_level " ") + "- " in
        match tree with
        | File_node f -> 
            prefix + f.name + " " + f.size.ToString()
        | Dir_node d ->
            let files_str = 
                d.files
                |> Seq.map File_node 
                |> Seq.map (to_string (indent_level + 2)) 
                |> String.concat "\n"
            in
            let subdirs_str =
                d.subdirs
                |> Seq.map Dir_node 
                |> Seq.map (to_string (indent_level + 2)) 
                |> String.concat "\n"
            in
            let non_empty_lines = 
                Seq.filter (not << String.IsNullOrWhiteSpace) [
                    prefix + d.name + " (" + d.size.ToString() + ")";
                    files_str;
                    subdirs_str;
                ] 
            in
            String.concat "\n" non_empty_lines
end

let part1 (root : Filesystem.Directory) : uint64 =
    let mutable small_folder_sizes = [] in
    // DFS yo
    let rec check_dir_size (dir : Filesystem.Directory) =
        if dir.size <= 100_000UL then
            small_folder_sizes <- List.append small_folder_sizes [dir.size] 
        for subdir in dir.subdirs do
            check_dir_size subdir
    in check_dir_size root;
    Seq.sum small_folder_sizes

let part2 (root : Filesystem.Directory) : uint64 =
    let total_space = 70_000_000UL in
    let update_needed_available_space = 30_000_000UL in
    let current_available_space = total_space - root.size in
    let space_needed_to_free = update_needed_available_space - current_available_space in
    // DFS yo
    let mutable candidate_size = None in
    let rec check_dir (d : Filesystem.Directory) =
        if (d.size >= space_needed_to_free) then
            candidate_size <- 
                match candidate_size with
                | None -> Some d.size
                | Some existing when d.size < existing -> Some d.size
                | _ -> candidate_size
        for subdir in d.subdirs do
            check_dir subdir
    in check_dir root;
    candidate_size.Value

[<EntryPoint>]
let main args =
    let input_filename = 
        if args.Length = 0 then
            "input.txt"
        else
            args.[0]
    in
    let raw_lines = System.IO.File.ReadAllText input_filename in
    let commands = 
        raw_lines.Split [| '$' |]
        |> Seq.map (fun s -> s.Trim())
        |> Seq.filter (not << String.IsNullOrWhiteSpace)
        |> Seq.map (fun s -> s.Split [| '\n' |])
        |> Seq.map Filesystem.Commands.parse_command
    in
    let initial_state = Filesystem.mk_initial_state() in
    let final_state = 
        commands
        |> Seq.fold Filesystem.Commands.eval initial_state 
    in
    printfn "Filesystem:\n %s" (Filesystem.to_string 0 (Filesystem.Dir_node final_state.root))
    printfn "Part 1: %d" (part1 final_state.root)
    printfn "Part 2: %d" (part2 final_state.root)
    0
