#!/bin/bash

# take a filename as an argument and call julia -e on it

if [ $# -ne 1 ]; then
	echo "Usage: $0 <filename>"
	exit 1
fi

julia -e "using JuliaFormatter; format_file(\"$1\")"
