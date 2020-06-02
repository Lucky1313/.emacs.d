#!/bin/bash

set -e
set -x

NPROC=`nproc`

VERSION=27.0.90

# Dependencies
sudo apt install debhelper texinfo libxpm-dev libgif-dev gnutls-dev libtinfo-dev
# Extra dependencies for optional modules
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
# Change desktop file to use emacsclient
sed -i "/^Exec=emacs/ s/emacs/emacsclient -c -a \"emacs\"/" etc/emacs.desktop
# Change service file to always restart server
sed -i "/^Restart=on-failure/ s/on-failure/always/" etc/emacs.service
# Install desktop and systemd files
sudo make install-etc
# Systemd service
systemctl enable --user emacs
systemctl start --user emacs
