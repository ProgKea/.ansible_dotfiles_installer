#!/usr/bin/env bash
selected=`cat ~/.tmux-cht-languages ~/.tmux-cht-command | fzf`
if [[ -z $selected ]]; then
    exit 0
fi

set -o vi
IFS= read -rep "Enter Query: " || {
    exit 0
}

if grep -qs "$selected" ~/.tmux-cht-languages; then
    query=`echo $REPLY | tr ' ' '+'`
    tmux neww bash -c "echo \"curl cht.sh/$selected/$REPLY/\" & curl cht.sh/$selected/$query & while [ : ]; do sleep 1; done" 
else
    query=`echo $REPLY | tr ' ' '+'`
    tmux neww bash -c "echo \"curl cht.sh/$selected~$REPLY\" & curl cht.sh/$selected~$query & while [ : ]; do sleep 1; done" 
fi

