#!/bin/bash

accounts="$(echo "$1" | tr ' ' '\n' | sort -u | tr '\n' ' ')"

for a in $accounts; do
    mbsync -qq "$a"
done

notmuch new --quiet
