#!/bin/bash

# Send the audio to a local stream
#
# Create a null audio output if not already created
# Redirect audio to null output
# Send null output to icecast2 using darkice
# (an icecast2 instance must already be running)
#
# Quit streaming audio with C^c (it will cut stream,
# and restore the original audio output)


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
darkice -c "$HOME/.config/audio_stream/darkice.cfg"

if [ ! -z "$old_sink" ]; then
    move "$old_sink"
fi
