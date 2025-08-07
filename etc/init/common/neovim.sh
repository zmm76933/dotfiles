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

if ! has "mise"; then
    log_fail "error: this script is only supported with mise"
    exit 1
fi

eval "$(mise activate bash --shims)"

# exit with true if you have nvim command
if ! has "nvim"; then
    log_fail "error: this script is only supported with nvim"
    exit 1
fi

# install Python privider
pip install pynvim > /dev/null

# install Ruby provider
gem install neovim > /dev/null

# install Node.js provider
npm install -g neovim > /dev/null
npm install -g commitizen > /dev/null
npm install -g cz-git > /dev/null

# install Perl provide
cpanm -n App::cpanminus > /dev/null
cpanm -n Neovim::Ext > /dev/null

log_pass "nvim: installed successfully"
