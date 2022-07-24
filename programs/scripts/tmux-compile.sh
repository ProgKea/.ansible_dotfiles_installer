#!/bin/bash

directory="$(tmux display-message -p '#{pane_current_path}')"
window_name=$(basename $directory | tr "./" "__")
CMD_PATH="$HOME/.local/share/tmux-compile_last_command"

if [[ $1 == "-r" ]]; then
    tmux-windowizer-dir "$directory" "$(cat $CMD_PATH)"
    tmux select-window -t "$window_name"
    exit 0
fi

set -o vi
IFS= read -rep "Compile command: " || {
    exit 0
}

echo "$REPLY" > $CMD_PATH
tmux-windowizer-dir "$directory" "$REPLY"
tmux select-window -t "$window_name"
