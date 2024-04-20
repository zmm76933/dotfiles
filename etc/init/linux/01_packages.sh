#!/bin/bash

# Stop script if errors occur
trap 'echo Error: $0:$LINENO stopped; exit 1' ERR INT
set -eu

# Load vital library that is most important and
# constructed with many minimal functions
# For more information, see etc/README.md
. "$DOTPATH"/etc/lib/vital.sh

PACKAGES="
    coreutils
    build-essential
    net-tools
    procps
    sysstat
    iproute2
    iputils-ping
    traceroute
    tcpdump
    netcat-openbsd
    nmap
    bridge-utils
    grep
    lsof
    curl
    wget
    iptables
    porg
    git
    avahi-daemon
    cifs-utils
    samba
    smartmontools
    wireguard-tools
    "

if has "dnf"; then
    log_echo "Install packages with Yellowdog Updater Modified"
    sudo dnf -y install $PACKAGES
elif has "apt"; then
    log_echo "Install packages with Advanced Packaging Tool"
    sudo apt -y install $PACKAGES
else
    log_fail "error: require: DNF or APT"
    exit 1
fi

if grep -vqEi "(Microsoft|WSL)" /proc/version &> /dev/null ; then
    # keyremap for linux
    if [ ! -d "$HOME/.config/evremap" ]; then
        mkdir -p "$HOME/.config/evremap"
    fi
    ln -sf "$DOTPATH/etc/config/linux/evremap/evremap.toml" "$HOME/.config/evremap/evremap.toml"
fi

log_pass "packages: installed successfully"
