module Tests

open NUnit.Framework
open FsUnit
open Lib

let sample_input = """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"""

[<TestFixture>]
type ``filesystem parsing`` ()=

    let subdir_names (dir : Filesystem.Directory) =
        dir.subdirs.Keys
        |> Set.ofSeq

    let file_names_and_sizes (dir : Filesystem.Directory) =
        dir.files.Values
        |> Seq.map (fun f -> sprintf "%s (%d)" f.name f.size)
        |> Set.ofSeq

    [<Test>]
    member this.``it should parse sample input correctly`` ()=
        let ``/`` = Filesystem.parse sample_input in
        subdir_names ``/``
        |> should equal (Set.ofList [ "a"; "d" ]);

        file_names_and_sizes ``/``
        |> should equal (Set.ofList [ "b.txt (14848514)"; "c.dat (8504156)" ]);

        let ``/a`` = ``/``.subdir "a"in
        subdir_names ``/a``
        |> should equal (Set.ofList ["e"]);

        file_names_and_sizes ``/a``
        |> should equal (Set.ofList ["f (29116)"; "g (2557)"; "h.lst (62596)"]);

        let ``/a/e`` = ``/a``.subdir "e" in
        subdir_names ``/a/e`` 
        |> should be Empty;

        let ``/d`` = ``/``.subdir "d" in
        subdir_names ``/d``
        |> should be Empty;

        file_names_and_sizes ``/d``
        |> should equal (Set.ofList [ "j (4060174)"; "d.log (8033020)"; "d.ext (5626152)"; "k (7214296)"])


[<TestFixture>]
type ``sample solutions`` ()=
    let ``/`` = Filesystem.parse sample_input

    [<Test>]
    member this.``it should solve part 1 for the sample input correctly`` ()=
        Puzzle.part1 ``/``
        |> should equal 95437

    [<Test>]
    member this.``it should solve part 2 for the sample input correctly`` ()=
        Puzzle.part2 ``/``
        |> should equal 24933642

