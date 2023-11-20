#!/usr/bin/env sh

workdir="unset"

if [ -f "$1" ]; then
    workdir="$(dirname "$1")"
else
    if [ -d "$1" ]; then
        workdir="$1"
    else
        >&2 echo "Could not find working directory for argument '${1}'"
        exit 1
    fi
fi

exo-open --working-directory "$workdir" --launch TerminalEmulator
