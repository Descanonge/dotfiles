
# Use as "whatever command ; alert"
alert_func () {
    res=$?
    icon="$([ $res = 0 ] && echo terminal || echo error)"
    sum="$(echo $history[$HISTCMD] | sed -e 's/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//')"
    sound="$HOME/.korosensei.ogg"
    notify-send --urgency=low -i "$icon" "$sum" \
	; paplay "$sound" & ; return $res &
}

alias alert="alert_func &!"
