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

if is_macos; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# exit with true if you have tmux command
if ! has "tmux"; then
    log_fail "error: this script is only supported with tmux"
    exit 1
fi

git clone https://github.com/tmux-plugins/tpm ~/.config/tmux/plugins/tpm
tmux source ~/.config/tmux/tmux.conf
/bin/bash ${HOME}/.config/tmux/plugins/tpm/scripts/install_plugins.sh

log_pass "tmux: tpm installed successfully"
