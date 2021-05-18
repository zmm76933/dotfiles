#!/bin/bash

# Stop script if errors occur
trap 'echo Error: $0:$LINENO stopped; exit 1' ERR INT
set -eu

# Load vital library that is most important and
# constructed with many minimal functions
# For more information, see etc/README.md
. "$DOTPATH"/etc/lib/vital.sh

if has "pyenv"; then
    log_pass "pyenv: already installed"
fi

if has "pipenv"; then
    log_pass "pipenv: already installed"
fi

if ! has "pyenv"; then
    if git clone https://github.com/pyenv/pyenv.git ~/.pyenv; then
        log_pass "pyenv installed successfully"
    else
        log_fail "error: pyenv: failed to install"
    fi
fi

# if ! has "pipenv"; then
#     if pip install pipenv; then
#         log_pass "pipenv installed successfully"
#     else
#         log_fail "error: pipenv: failed to install"
#     fi
# fi

