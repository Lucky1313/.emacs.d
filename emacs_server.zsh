#!/bin/zsh
source ~/.zshrc
if [ ! -e /tmp/emacs$USERID/server ]; then
    /usr/bin/emacs24 --daemon
fi

# Change emacs menu entrys using cinnamon-menu-editor
# Use this to start up emacs GUI
# /usr/bin/emacsclient --alternate-editor="" --create-frame --no-wait %F
# Use this to start up emacs terminal
# /usr/bin/emacsclient --alternate-editor="" --no-wait -nw %F
# Use this to restart a currently running server
# /usr/bin/emacsclient --eval \"(progn (save-some-buffers t t) (kill-emacs))\"; /usr/bin/emacs24 --daemon
