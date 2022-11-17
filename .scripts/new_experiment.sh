#!/usr/bin/env bash

[ "$1" == "-f" ] && force="true"

if [ -z "$EXPERIMENTS_DIR" ]; then
    echo "EXPERIMENTS_DIR not defined"
    exit 1
fi

name="$(date +'%y%m')a"

echo "Exp. name [$name]:"
read -r newname
[ -n "$newname" ] && name="$newname"

exp_dir="$EXPERIMENTS_DIR/$name"
exp_file="$HOME/org/todos/Experiments/$name.org"

[ ! -d "$exp_dir" ] && mkdir "$exp_dir"
ln -sf "$exp_file" "$EXPERIMENTS_DIR/$name/$name.org"

if [ -n "$force" ] || [ ! -s "$exp_file" ]; then
    cat <<EOF > "$exp_file"
#+TITLE: $name
#+CREATED:[$(date +'%F %a')]
#+STATUS:OPENED,
EOF
fi
