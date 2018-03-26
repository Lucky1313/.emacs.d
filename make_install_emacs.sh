apt-get build-deps emacs24
wget https://github.com/emacs-mirror/emacs/archive/emacs-25.3.tar.gz
tar -xzf emacs-25.3.tar.gz
cd emacs-25.3
./autogen.sh
./configure
make
make install
cp etc/emacs.desktop /usr/share/applications/
