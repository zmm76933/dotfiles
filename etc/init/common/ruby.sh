#!/bin/bash

# Stop script if errors occur
trap 'echo Error: $0:$LINENO stopped; exit 1' ERR INT
set -eu

# Load vital library that is most important and
# constructed with many minimal functions
# For more information, see etc/README.md
. "$DOTPATH"/etc/lib/vital.sh

if has "rbenv"; then
    log_pass "rbenv: already installed"
fi

if ! has "rbenv"; then
    if git clone https://github.com/rbenv/rbenv.git ~/.rbenv; then
        if git clone https://github.com/rbenv/ruby-build.git ~/.rbenv/plugins/ruby-build; then
            log_pass "rbenv installed successfully"
        else
            log_fail "error: rbenv: failed to install"
        fi
    else
        log_fail "error: rbenv: failed to install"
    fi
fi
