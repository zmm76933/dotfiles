#!/bin/bash

# Stop script if errors occur
trap 'echo Error: $0:$LINENO stopped; exit 1' ERR INT
set -eu

# Load vital library that is most important and
# constructed with many minimal functions
# For more information, see etc/README.md
. "$DOTPATH"/etc/lib/vital.sh

PACKAGES="git zsh tmux
    build-essential diffutils autoconf automake silversearcher-ag
    g++ libxml2-dev libssl-dev libsqlite3-dev libevent-dev
    libsensors4-dev libavahi-common-dev libavahi-client-dev ncurses-dev
    mercurial rbenv python python3 python-pip python3-pip gawk tree paco
    aspell ispell nkf lv cmigemo texinfo etckeeper stress smartmontools
    curl w3m wget nmap wakeonlan testdisk fortunes-ubuntu-server
    openssh-server bind9 isc-dhcp-server"

REPOSITORY="vim emacs25"

if has "yum"; then
    log_echo "Install packages with Yellowdog Updater Modified"
    sudo yum -y install $PACKAGES
elif has "apt"; then
    log_echo "Install packages with Advanced Packaging Tool"
    sudo apt -y install $PACKAGES
    if has "add-apt-repository"; then
        sudo add-apt-repository -y ppa:jonathonf/vim
        sudo add-apt-repository -y ppa:kelleyk/emacs
        sudo apt update
        sudo apt -y install $REPOSITORY
    fi
else
    log_fail "error: require: YUM or APT"
    exit 1
fi

log_pass "packages: installed successfully"
