# My 2022 Advent of Code solutions, in F#

## Adding new solutions

This repo comes with its own nice project template for my solution project structure (using FsUnit+NUnit for tests)

Just run:

```shell
$ cd ./project_template
$ dotnet new install ./
```

...then you can use `./newday.sh DAY_NUMBER` to scaffold out a new day's problem space!

## Running solutions

In every day, you can cd into the directory and run `dotnet run`.

For days on or after day7, you can also run unit tests by cd'ing into the directory and running `dotnet test`
