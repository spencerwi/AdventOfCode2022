module Lib

open System

module Filesystem = begin
    type Node = 
        | Dir_node of Directory 
        | File_node of File
    and Directory = {
        parent: Directory option
        name : string
        mutable subdirs : Map<string, Directory>
        mutable files : Map<string, File>
        mutable total_size : uint64
    }
    and File = {
        parent: Directory
        name : string
        size: uint64
    } 

    let root () : Directory =
        {
            parent = None
            name = "/"
            subdirs = Map.empty
            files = Map.empty
            total_size = 0UL
        }

    type State = {
        root : Directory
        currentDir : Directory
    } with
        // This accepts unit so that we can ensure we're handing a new instance each time
        static member make () =
            let root = root() in
            {
                root = root
                currentDir = root
            }

    type Directory with
        member this.subdir (name : string) =
            if this.subdirs.ContainsKey name then
                this.subdirs[name]
            else
                failwith ("Directory not found " + name + " in " + this.name)

        member this.add = function
            | File_node f ->
                // add this file to the parent directory
                this.files <- this.files.Add(f.name, f)
                // update the "total size" numbers all the way up the tree to the root so we don't have to calculate it later
                let mutable ancestor = Some this in
                while (ancestor.IsSome) do
                    ancestor.Value.total_size <- ancestor.Value.total_size + f.size
                    ancestor <- ancestor.Value.parent
            | Dir_node d -> 
                // add this subdirectory to its parent. It's empty when it's created, so we don't need to do any total_size updates.
                this.subdirs <- this.subdirs.Add(d.name, d)

    type Node with
        member this.name =
            match this with
            | File_node f -> f.name
            | Dir_node d -> d.name

        static member parse (parent: Directory) (entry : string) =
            if entry.StartsWith "dir" then
                let name = (entry.Split [|' '|]).[1] in
                Dir_node {
                    parent = Some parent
                    name = name.Trim()
                    subdirs = Map.empty
                    files = Map.empty
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

    module Commands = begin
        type t =
            | CD of string
            | LS of string seq

        let parse (input_lines : string seq) : t =
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
                { state with currentDir = (state.currentDir.subdir dirname) }

        let ls (state : State) (listings : string seq) : State =
            if (not state.currentDir.subdirs.IsEmpty || not state.currentDir.files.IsEmpty) then
                // if this already has children, we've already listed this directory. Don't duplicate entries.
                state
            else
                for listing in listings do
                    let parsed_child = Node.parse state.currentDir listing in
                    state.currentDir.add parsed_child
                state

        let eval (state : State) (command : t) : State =
            match command with
            | LS listings -> ls state listings
            | CD subdir -> cd state subdir
    end

    let parse (input_lines: string) =
        let commands = 
            input_lines.Split [| '$' |] // split the whole-file input into command-and-ouput groups
            |> Seq.map (fun s -> s.Trim())
            |> Seq.filter (not << String.IsNullOrWhiteSpace) // eliminate any empty lines, for convenience
            |> Seq.map (fun s -> s.Split [| '\n' |]) // then split within each group into lines, mostly useful for ls
            |> Seq.map Commands.parse
        in
        let initial_state = State.make() in
        let final_state = 
            commands
            |> Seq.fold Commands.eval initial_state 
        in
        final_state.root

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
                d.files.Values
                |> Seq.map File_node 
                |> Seq.map (to_string (indent_level + 2)) 
                |> String.concat "\n"
            in
            let subdirs_str =
                d.subdirs.Values
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

module Puzzle = begin
    let part1 (root : Filesystem.Directory) : uint64 =
        // depth-first traversal, updating a mutable list as we go
        let mutable small_folder_sizes = [] in
        let rec check_dir_size (dir : Filesystem.Directory) =
            if dir.total_size <= 100_000UL then
                small_folder_sizes <- List.append small_folder_sizes [dir.total_size] 
            for subdir in dir.subdirs.Values do
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
            for subdir in d.subdirs.Values do
                check_dir subdir
        in check_dir root;
        candidate_size.Value
end
