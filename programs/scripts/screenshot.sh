#!/bin/sh

file_path="/tmp/"
maim --select | tee $file_path$(date +%s).png | xclip -selection clipboard -t image/png
