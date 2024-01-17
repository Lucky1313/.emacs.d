#!/bin/bash

# misc
sudo apt install -y git

# python
sudo apt install -y python3

# vterm deps
sudo apt install -y cmake libtool-bin libvterm-dev

# co-pilot deps
curl -fsSL https://deb.nodesource.com/setup_18.x | sudo -E bash - &&
sudo apt-get install -y nodejs
