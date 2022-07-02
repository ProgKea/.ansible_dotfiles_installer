#!/bin/sh
xinit_path="$HOME/.ansible_dotfiles_installer/dotfiles/.xinitrc"

if [[ $1 == "" ]]; then
  sudo rm patches.h config.h && sudo make && sudo make install
fi
if [[ $1 == "simple" ]]; then
  if [[ $(head patches.def.h -n 1) == "// fancy" ]]; then
    mv patches.def.h fancy_patches
  elif [[ $(head patches.def.h -n 1) == "// not fancy" ]]; then
    echo "already simple"
    exit 1
  elif [[ $(head patches.def.h -n 1) == "// super" ]]; then
    mv patches.def.h super_fancy_patches
  fi
  mv not_fancy_patches patches.def.h
  sudo rm patches.h config.h && sudo make && sudo make install
  sed -i 's/^xcompmgr \&/#xcompmgr \&/' $xinit_path
  if [[ $(ps aux | grep xcompmgr | grep -o -E "[0-9] xcompmgr$" | wc -l) == "1" ]]; then
    killall xcompmgr
  fi
elif [[ $1 == "fancy" ]]; then
  if [[ $(head patches.def.h -n 1) == "// fancy" ]]; then
    echo "already fancy"
    exit 1
  elif [[ $(head patches.def.h -n 1) == "// not fancy" ]]; then
    mv patches.def.h not_fancy_patches
  elif [[ $(head patches.def.h -n 1) == "// super" ]]; then
    mv patches.def.h super_fancy_patches
  fi
  mv fancy_patches patches.def.h
  sudo rm patches.h config.h && sudo make && sudo make install
  sed -i 's/^xcompmgr \&/#xcompmgr \&/' $xinit_path
  if [[ $(ps aux | grep xcompmgr | grep -o -E "[0-9] xcompmgr$" | wc -l) == "1" ]]; then
    killall xcompmgr
  fi
elif [[ $1 == "super" ]]; then
  if [[ $(head patches.def.h -n 1) == "// fancy" ]]; then
    mv patches.def.h fancy_patches
  elif [[ $(head patches.def.h -n 1) == "// not fancy" ]]; then
    mv patches.def.h not_fancy_patches
  elif [[ $(head patches.def.h -n 1) == "// super" ]]; then
    echo "already super"
    exit 1
  fi
  mv super_fancy_patches patches.def.h
  sudo rm patches.h config.h && sudo make && sudo make install
  sed -i 's/^#xcompmgr \&/xcompmgr \&/' $xinit_path
  if [[ $(ps aux | grep xcompmgr | grep -o -E "[0-9] xcompmgr$" | wc -l) != "1" ]]; then
    xcompmgr &
  fi
fi
