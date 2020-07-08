
files=(general.zsh
       alert.zsh
       )

for file in ${files[@]}; do
    if [ -f "$ZSH_ALIAS/$file" ]; then
	source "$ZSH_ALIAS/$file"
    fi
done

