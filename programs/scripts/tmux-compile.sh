#!/bin/bash

directory="$(tmux display-message -p '#{pane_current_path}')"
CMD_PATH="$HOME/.local/share/tmux-compile_last_command"

if [[ $1 == "-r" ]]; then
    tmux-windowizer-dir "$directory" "$(cat $CMD_PATH)"
    exit 0
fi

read -p "Compile command: " compile_command
if [[ -z $compile_command ]]; then
    exit 0
fi

echo "$compile_command" > $CMD_PATH
tmux-windowizer-dir "$directory" "$compile_command"
