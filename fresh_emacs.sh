#!/bin/bash

# 1. CAPS-LOCK should be CTRL. ergonomic solution for XFCE4 DE

xtox="xmodmap ~/.Xmodmap"

cp custom/.Xmodmap ~/
echo $xtox >> ~/.xinitrc

# install emacs

sudo pacman -S emacs --needed --noconfirm
emacs --version

# personal emacs config

cp -r emacs ~/.config

# Remove ~/.emacs.d if it exists

rm -r ~/.emacs.d 

# Set Emacs daemon on startup for XFCE DE

cp custom/emacs_daemon.desktop ~/.config/autostart/

# Let me know to logout and log back in to check if it all works.
echo "Logout and log back in"
