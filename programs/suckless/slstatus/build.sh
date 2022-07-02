#!/bin/sh
killall slstatus ; cp ~/suckless/slstatus/config.h ~/suckless/slstatus/config.def.h 
sudo make -C ~/suckless/slstatus/ clean install 
sudo cp ~/suckless/slstatus/slstatus $HOME/code/scripts/ 
slstatus &
