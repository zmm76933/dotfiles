#!/bin/bash

# Stop script if errors occur
trap 'echo Error: $0:$LINENO stopped; exit 1' ERR INT
set -eu

# Load vital library that is most important and
# constructed with many minimal functions
# For more information, see etc/README.md
. "$DOTPATH"/etc/lib/vital.sh

if ! has "curl"; then
    if curl -sL https://git.io/fisher | source && fisher install jorgebucaran/fisher; then
        log_pass "fisher installed successfully"
    else
        log_fail "error: fisher: failed to install"
    fi
fi

