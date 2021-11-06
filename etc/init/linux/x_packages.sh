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
    xrdp
    xsel
    rxvt-unicode-256color
    ibus-skk
    skkdic
    wmctrl
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

if has "pip3"; then
    sudo pip3 install xkeysnail || log_fail "error: pip3: failed to install"
    if [ ! -d "$HOME/.config/xkeysnail" ]; then
        mkdir -p "$HOME/.config/xkeysnail"
        ln -sf "$DOTPATH/etc/gui/linux/xkeysnail.py" "$HOME/.config/xkeysnail/config.py"
    fi
fi

log_pass "private packages: installed successfully"
