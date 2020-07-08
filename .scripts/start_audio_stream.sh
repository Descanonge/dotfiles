#!/bin/bash

# Connect darkice to icecast
# Change output


move () {
    # Move all streams to $1
    target=$1
    while IFS= read -r stream; do
        streamID=$(echo "$stream" | cut -f1)
        old_sink=$(echo "$stream" | cut -f2)
        pactl move-sink-input "$streamID" "$target"
    done < <(pactl list short sink-inputs)
    echo "$old_sink"
}


# Create Null output if not already
if ! pactl list short sinks | grep -q "null_output"; then
    pactl load-module module-null-sink sink_name=null_output
fi
pactl set-sink-volume null_output 100%

old_sink=$(move "null_output")

trap : INT
darkice -c "$HOME/.config/darkice/darkice.cfg"

if [ ! -z "$old_sink" ]; then
    move "$old_sink"
fi
