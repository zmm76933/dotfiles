#!/bin/bash

# Stop script if errors occur
# trap 'echo Error: $0:$LINENO stopped; exit 1' ERR INT
# set -eu

# Load vital library that is most important and
# constructed with many minimal functions
# For more information, see etc/README.md
. "$DOTPATH"/etc/lib/vital.sh

if has "fisher"; then
    log_pass "fisher: already installed"
    exit
fi

if is_linux; then
    eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
fi

if is_macos && is_arm; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

fish -c "curl -sL git.io/fisher | source && fisher update"

log_pass "fisher: installed successfully"
