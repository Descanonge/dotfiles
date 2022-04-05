#!/usr/bin/env bash
set -euo pipefail

# Reset stereo balance using pactl

getdefaultsinkname() {
    pacmd stat | awk -F": " '/^Default sink name: /{print $2}'
}

getdefaultsinkvolleft() {
    pacmd list-sinks |
        awk '/^\s+name: /{indefault = $2 == "<'"$1"'>"}
            /^\s+volume: / && indefault {print $5; exit}'

}
getdefaultsinkvolright() {
    pacmd list-sinks |
        awk '/^\s+name: /{indefault = $2 == "<'"$1"'>"}
            /^\s+volume: / && indefault {print $12; exit}'

}

sinkname="$(getdefaultsinkname)"

volleft=$(getdefaultsinkvolleft "$sinkname")
volright=$(getdefaultsinkvolright "$sinkname")

volleft=${volleft::-1}
volright=${volright::-1}

if [[ $volleft -ge $volright ]]; then
    volmax=$volleft
else
    volmax=$volright
fi

pactl set-sink-volume "$sinkname" "$volmax%"
