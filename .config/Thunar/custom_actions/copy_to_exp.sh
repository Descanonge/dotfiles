#!/usr/bin/env bash

# Copy selected files / folders to an exp. folder.
# Exp is choosen by user with prompt.
# Filetree replicated up to $SUBMESO_COLOR_DATA_DIR (if inside)

if ! cd "$SUBMESO_COLOR_DATA_DIR"; then
    echo "Could not find SUBMESO_COLOR_DATA_DIR"
    exit 1
fi

# Get most experiment folder where there is the most recent file
recent_exp="$(find ./Exp -mindepth 1 -printf '%T/%p\0' \
    | sort -rzn | head -zn1 | cut -d/ -f2-4)"

# Find target directory
target="$(zenity --file-selection --filename="$recent_exp" \
    --directory --title 'Choose experiment directory')"

if [[ ! -d "$target" ]]; then
    echo "Target is not a directory"
    exit 1
fi

files=("$@")

normfiles=("$(realpath "${files[@]}")")
relfiles=("$(realpath --relative-base="$SUBMESO_COLOR_DATA_DIR" "${files[@]}")")


# Outside of DATA_DIR
if [[ "${normfiles[0]}" = "${relfiles[0]}" ]]; then
    echo "${normfiles[@]}" | xargs cp -r -t "$target"
else
    echo "${relfiles[@]}" | xargs cp -r --parents -t "$target"
fi
