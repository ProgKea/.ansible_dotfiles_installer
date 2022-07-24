#!/bin/sh

file_path="$HOME/Downloads/"
maim --select | tee $file_path$(date +%s).png | xclip -selection clipboard -t image/png
