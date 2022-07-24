#!/usr/bin/env bash
set -o emacs
bind '"\C-w": kill-whole-line'
bind '"\e": "\C-w\C-d"'
bind '"\e\e": "\C-w\C-d"'

set -o vi
IFS= read -rep "Enter Query: " || {
    exit 0
}

tmux neww bash -c "so '$REPLY'" 
