#!/bin/bash

if [ "$1" = "new" ] ; then
	if [ -z "$2" ] ; then
		echo "USAGE: gen.sh new NUMBER"
		exit 1
	fi
	if [ -d "day$2" ]; then
		echo "day$2 already exists."
		exit 1;
	fi
	mkdir "day$2" && cd "day$2" && dotnet new console --language 'F#'
elif [ "$1" = tests ]; then
	if [ -z "$2" ] ; then
		echo "USAGE gen.sh tests NUMBER"
		exit 1;
	fi
	if [ ! -d "day$2" ]; then
		echo "day$2 doesn't exist. Try gen.sh new $2 first."
		exit 1;
	fi
	cd "day$2" && \
		dotnet add package NUnit && \
		dotnet add package NUnit3TestAdapter && \
		dotnet add package Microsoft.NET.Test.Sdk && \
		dotnet add package FSUnit
	cat <<- 'EOF' > Tests.fs
    module Tests
    open NUnit.Framework
    open FSUnit

    [<TestFixture>]
    type ``Puzzle solution`` ()=
	    [<Test>]
        let ``Something`` =
            1 |> should equal 1
            2 |> should not' equal 1
	EOF
	echo "Tests added to day$2"
	echo "Now move logic to a Lib.fs file, and make sure the CompileGroup is updated in the fsproj with Program.fs at the bottom"
fi

if [ -z "$1" ] ; then
	echo "USAGE: gen.sh [new|tests] NUMBER"
	exit 1
fi


