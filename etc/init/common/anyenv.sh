#!/bin/bash

# Stop script if errors occur
# trap 'echo Error: $0:$LINENO stopped; exit 1' ERR INT
# set -eu

# Load vital library that is most important and
# constructed with many minimal functions
# For more information, see etc/README.md
. "$DOTPATH"/etc/lib/vital.sh

if is_linux; then
    eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
fi

# exit with true if you have anyenv command
if ! has "anyenv"; then
    log_fail "error: this script is only supported with anyenv"
    exit 1
fi

anyenv install --force-init
anyenv install pyenv
anyenv install rbenv
anyenv install nodenv
anyenv install plenv

source $DOTPATH/etc/init/assets/anyenv/install.sh

log_pass "anyenv: installed successfully"
