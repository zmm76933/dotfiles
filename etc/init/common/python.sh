#!/bin/bash

# Stop script if errors occur
trap 'echo Error: $0:$LINENO stopped; exit 1' ERR INT
set -eu

# Load vital library that is most important and
# constructed with many minimal functions
# For more information, see etc/README.md
. "$DOTPATH"/etc/lib/vital.sh

if has "pygmentize"; then
    log_pass "pygmentize: already installed"
fi

if has "pipenv"; then
    log_pass "pipenv: already installed"
fi

if has "pyenv"; then
    log_pass "pyenv: already installed"
fi

# The script is dependent on pip3
if ! has "pip3"; then
    log_fail "error: require: pip3"
    exit 1
fi

if ! has "pygmentize"; then
    if sudo pip3 install Pygments; then
        log_pass "pygmentize: installed successfully"
    else
        log_fail "error: pygmentize: failed to install"
    fi

    log_echo "install pygments-style-solarized ..."
    #pip3 install pygments-style-solarized
fi

#if builtin cd "$DOTPATH"/etc/init/assets/pygments/solarized-pygment; then
#    git submodule update
#    sudo ./setup.py install
#fi

if ! has "pipenv"; then
    if sudo pip3 install pipenv; then
        log_pass "pipenv installed successfully"
    else
        log_fail "error: pipenv: failed to install"
    fi
fi

if ! has "pyenv"; then
    if git clone https://github.com/pyenv/pyenv.git ~/.pyenv; then
        log_pass "pyenv installed successfully"
    else
        log_fail "error: pyenv: failed to install"
    fi
fi
