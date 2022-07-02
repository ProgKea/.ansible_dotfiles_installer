#!/usr/bin/env bash
read -p "Enter Query: " query
if [[ -z $query ]]; then
    exit 0
fi

tmux neww bash -c "so '$query'" 
