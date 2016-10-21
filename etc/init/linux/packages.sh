#!/bin/bash

# Stop script if errors occur
trap 'echo Error: $0:$LINENO stopped; exit 1' ERR INT
set -eu

# Load vital library that is most important and
# constructed with many minimal functions
# For more information, see etc/README.md
. "$DOTPATH"/etc/lib/vital.sh

PACKAGES="zsh tmux
    build-essential diffutils autoconf automake silversearcher-ag
    git rbenv python python-setuptools
    emacs vim aspell ispell nkf lv cmigemo
    curl w3m nkf nmap wakeonlan wireshark
    imagemagick
    wmctrl xsel
    paco"

if has "yum"; then
    log_echo "Install packages with Yellowdog Updater Modified"
    sudo yum -y install $PACKAGES
elif has "apt-get"; then
    log_echo "Install packages with Advanced Packaging Tool"
    sudo apt-get -y install $PACKAGES
else
    log_fail "error: require: YUM or APT"
    exit 1
fi

if ! has "pip"; then
    sudo easy_install pip || log_fail "error: pip: failed to install"
fi

log_pass "packages: installed successfully"