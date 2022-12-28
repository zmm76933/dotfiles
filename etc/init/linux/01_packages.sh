#!/bin/bash

# Stop script if errors occur
trap 'echo Error: $0:$LINENO stopped; exit 1' ERR INT
set -eu

# Load vital library that is most important and
# constructed with many minimal functions
# For more information, see etc/README.md
. "$DOTPATH"/etc/lib/vital.sh

PACKAGES="
    build-essential 
    procps
    net-tools
    curl
    llvm
    file
    porg
    git
    libbz2-dev
    libffi-dev
    liblzma-dev
    libncursesw5-dev
    libreadline-dev
    libsqlite3-dev
    libssl-dev
    libxml2-dev
    libxmlsec1-dev
    tk-dev
    xz-utils
    zlib1g-dev
    avahi-daemon
    cifs-utils
    etckeeper
    fortunes-ubuntu-server
    isc-dhcp-server
    openssh-server
    samba
    smartmontools
    squid
    wireguard-tools
    "

if has "yum"; then
    log_echo "Install packages with Yellowdog Updater Modified"
    sudo yum -y install $PACKAGES
elif has "apt"; then
    log_echo "Install packages with Advanced Packaging Tool"
    sudo apt -y install $PACKAGES
else
    log_fail "error: require: YUM or APT"
    exit 1
fi

log_pass "packages: installed successfully"
