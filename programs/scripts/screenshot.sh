#!/bin/sh

file_path="$HOME/Downloads/"
file_name=$(: | dmenu -p "File name:")
import $file_path$file_name
