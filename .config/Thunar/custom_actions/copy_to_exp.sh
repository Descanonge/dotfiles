#!/usr/bin/env bash

# Copy selected files / folders to an exp. folder.
# Exp is choosen by user with prompt.
# Filetree replicated up to $SUBMESO_COLOR_DATA_DIR (if inside)

files=("$@")
relfiles=("$(realpath --relative-base="$SUBMESO_COLOR_DATA_DIR" "${files[@]}")")
normfiles=("$(realpath -z "${files[@]}" | tr -d '\0')")

if ! cd "$SUBMESO_COLOR_DATA_DIR"; then
    echo "Could not find SUBMESO_COLOR_DATA_DIR"
    exit 1
fi

# Get most experiment folder where there is the most recent file
# 1: get most recently modified file (or experiments directory by using maxdepth)
# 2: get it relative to experiments dir
# 3: get experiment folder (first one)
# 4: remove trailing null character
# 5: get absolute path
recent_exp="$(find "$EXPERIMENTS_DIR" -mindepth 1 -maxdepth 1 -printf '%T@/%p\0' \
    | sort -rzn | head -zn1 | cut -zd/ -f2- \
    | xargs -0 realpath -zs --relative-to="$EXPERIMENTS_DIR" \
    | cut -zd/ -f1 | tr -d '\0')"
recent_exp="$EXPERIMENTS_DIR/$recent_exp"

# Find target directory
target="$(zenity --file-selection --filename="$recent_exp" \
    --directory --title 'Choose experiment directory')"

if [[ ! -d "$target" ]]; then
    echo "Target is not a directory"
    exit 1
fi

# If outside of DATA_DIR
if [[ "${normfiles[0]}" = "${relfiles[0]}" ]]; then
    echo "${normfiles[@]}" | xargs cp -r -t "$target"
else
    echo "${relfiles[@]}" | xargs cp -r --parents -t "$target"
fi
