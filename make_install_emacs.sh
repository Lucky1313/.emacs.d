#!/bin/bash

set -e
set -x

NPROC=`nproc`

VERSION=26.3

# Dependencies
sudo apt build-dep emacs
sudo apt install libwebkit2gtk-4.0-dev libjpeg-dev libpng-dev libtiff-dev

# Retrieve emacs
EMACS_DOWNLOAD=emacs-${VERSION}.tar.gz
if [ ! -f "${EMACS_DOWNLOAD}" ]; then
    echo "Retrieving emacs version ${VERSION}..."
    wget https://github.com/emacs-mirror/emacs/archive/${EMACS_DOWNLOAD}
fi
if [ ! -d "emacs-emacs-${VERSION}" ]; then
    echo "Unpacking emacs version ${VERSION}..."
    tar -xzf ${EMACS_DOWNLOAD}
fi
cd emacs-emacs-${VERSION}

# Configure
./autogen.sh
./configure --with-xwidgets

# Make
make -j$NPROC

# Install
sudo make install
sudo cp etc/emacs.desktop /usr/share/applications/
