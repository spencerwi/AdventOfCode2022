#!/bin/bash

if [ -z "$1" ] ; then
	echo "USAGE: newday.sh NUMBER"
	exit 1
fi

if [ -d "day$1" ]; then
	echo "day$1 already exists."
	exit 1;
fi

mkdir "day$1" && cd "day$1" && dotnet new console --language 'F#'
