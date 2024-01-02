#!/bin/bash

# Stop script if errors occur
trap 'echo Error: $0:$LINENO stopped; exit 1' ERR INT
set -eu

# Load vital library that is most important and
# constructed with many minimal functions
# For more information, see etc/README.md
. "$DOTPATH"/etc/lib/vital.sh

if ! is_desktop; then
    log_fail "error: this script is only supported with X windows system"
    exit 0
fi

PACKAGES="
    wl-clipboard
    tilix
    fcitx5
    fcitx5-skk
    gnome-shell-extension-manager
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

# emacs keybindings
gsettings set org.gnome.desktop.interface gtk-key-theme Emacs

log_pass "private packages: installed successfully"
