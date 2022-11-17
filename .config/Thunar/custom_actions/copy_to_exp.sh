#!/usr/bin/env bash

# Copy selected files / folders to an exp. folder.
# Exp is choosen by user with prompt.
# Filetree replicated up to $SMC_DATA_DIR if inside
# We assume all files are all in same directory
# (this is only for custom actions use)

files=("$@")

if [ -z "$SMC_DATA_DIR" ]; then
    echo "SMC_DATA_DIR not defined"
    exit 1
fi
if [ -z "$EXPERIMENTS_DIR" ]; then
    echo "EXPERIMENTS_DIR not defined"
    exit 1
fi
if [ ! -d "$SMC_DATA_DIR" ]; then
    echo "SMC_DATA_DIR not a directory"
    exit 1
fi


# Find newest experiment (based on directory creation) as a default
recent_exp="$(ls "$EXPERIMENTS_DIR" -t | head -n1)"
# recent_exp="$(find "$EXPERIMENTS_DIR" -mindepth 1 -maxdepth 1 -printf '%T@/%p\0' \
#     | sort -rzn | head -zn1 | cut -zd/ -f2- \
#     | xargs -0 realpath -zs --relative-to="$EXPERIMENTS_DIR" \
#     | cut -zd/ -f1 | tr -d '\0')"
recent_exp="$EXPERIMENTS_DIR/$recent_exp"

# Choose experiment to get target directory
# target="$(zenity --file-selection --filename="$recent_exp" \
#     --directory --title 'Choose experiment directory')"
target="$recent_exp"

if [ ! -d "$target" ]; then
    echo "Target is not a directory"
    exit 1
fi


# Check if we are in SMC_DATA_DIR. If yes, we will reproduce the local filetree
# We need to make files relative to SMC_DATA_DIR and move there
# The cp is then simple
if [ "$(realpath --relative-base="$SMC_DATA_DIR" "$(pwd)")" != "$(pwd)" ]; then
    files=("$(realpath --relative-base="$SMC_DATA_DIR" "${files[@]}")")
    cp_opt="--parents"
    if ! cd "$SMC_DATA_DIR"; then
        echo "Could not enter SMC_DATA_DIR"
        exit 1
    fi
fi

echo "${files[@]}" | xargs cp "$cp_opt" -vrt "$target"
    # | zenity --text-info --width=600 --height=800
