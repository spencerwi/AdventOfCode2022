open System

module Filesystem = begin
    type Node = 
        | Dir_node of Directory 
        | File_node of File
    and Directory = {
        parent: Directory option
        name : string
        mutable subdirs : Directory list
        mutable files : File list
        mutable total_size : uint64
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

    // This accepts unit so that we can ensure we're handing a new instance each time
    let mk_initial_state () =
        let root =
            {
                parent = None
                name = "/"
                subdirs = List.empty
                files = List.empty
                total_size = 0UL
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
                total_size = 0UL
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

    let get_dir (current : Directory) (subdir_name : string) : Directory =
        let maybe_subdir = 
            current.subdirs
            |> Seq.tryFind (fun subdir -> subdir.name = subdir_name)
        in
        match maybe_subdir with
        | Some d -> d
        | None -> failwith ("Directory not found: " + subdir_name + " in " + current.name)

    module Commands = begin
        type t =
            | CD of string
            | LS of string seq

        let parse_command (input_lines : string seq) : t =
            match (Seq.head input_lines).[0..1] with
            | "ls" -> 
                let listings = Seq.tail input_lines in
                LS listings
            | "cd" -> 
                let subdir = (Seq.head input_lines).Substring(2).Trim() in
                CD subdir
            | other -> failwith ("Unrecognized command: " + other)

        let cd (state : State) (path : string) : State =
            match (path, state.currentDir.parent) with 
            | ("/", _) -> { state with currentDir = state.root }
            | ("..", None) -> state // shouldn't happen, but worth handling so the compiler doesn't yell at me about exhaustiveness
            | ("..", Some parent) ->  { state with currentDir = parent }
            | (dirname, _) -> 
                { state with currentDir = (get_dir state.currentDir dirname) }

        let ls (state : State) (listings : string seq) : State =
            if (not state.currentDir.subdirs.IsEmpty || not state.currentDir.files.IsEmpty) then
                // if this already has children, we've already listed this directory. Don't duplicate entries.
                state
            else
                for listing in listings do
                    let parsed_child = parse_entry state.currentDir listing in
                    match parsed_child with 
                    | File_node f ->
                        // add this file to the parent directory
                        state.currentDir.files <- List.append state.currentDir.files [f]
                        // update the "total size" numbers all the way up the tree to the root so we don't have to calculate it later
                        let mutable ancestor = Some state.currentDir in
                        while (ancestor.IsSome) do
                            ancestor.Value.total_size <- ancestor.Value.total_size + f.size
                            ancestor <- ancestor.Value.parent
                    | Dir_node d -> 
                        // add this subdirectory to its parent. It's empty when it's created, so we don't need to do any total_size updates.
                        state.currentDir.subdirs <- List.append state.currentDir.subdirs [d]
                state

        let eval (state : State) (command : t) : State =
            match command with
            | LS listings -> ls state listings
            | CD subdir -> cd state subdir
    end

    /// <description>
    /// Just a convenience method for debugging
    /// </description>
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
                    prefix + d.name + " (" + d.total_size.ToString() + ")";
                    files_str;
                    subdirs_str;
                ] 
            in
            String.concat "\n" non_empty_lines
end

let part1 (root : Filesystem.Directory) : uint64 =
    // depth-first traversal, updating a mutable list as we go
    let mutable small_folder_sizes = [] in
    let rec check_dir_size (dir : Filesystem.Directory) =
        if dir.total_size <= 100_000UL then
            small_folder_sizes <- List.append small_folder_sizes [dir.total_size] 
        for subdir in dir.subdirs do
            check_dir_size subdir
    in check_dir_size root;
    Seq.sum small_folder_sizes

let part2 (root : Filesystem.Directory) : uint64 =
    let total_space = 70_000_000UL in
    let update_needed_available_space = 30_000_000UL in
    let current_available_space = total_space - root.total_size in
    let space_needed_to_free = update_needed_available_space - current_available_space in
    // depth-first traversal, updating a mutable "result" pointer as we go
    let mutable candidate_size = None in
    let rec check_dir (d : Filesystem.Directory) =
        if (d.total_size >= space_needed_to_free) then
            candidate_size <- 
                match candidate_size with
                | None -> Some d.total_size
                | Some existing when d.total_size < existing -> Some d.total_size
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
        raw_lines.Split [| '$' |] // split the whole-file input into command-and-ouput groups
        |> Seq.map (fun s -> s.Trim())
        |> Seq.filter (not << String.IsNullOrWhiteSpace) // eliminate any empty lines, for convenience
        |> Seq.map (fun s -> s.Split [| '\n' |]) // then split within each group into lines, mostly useful for ls
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
