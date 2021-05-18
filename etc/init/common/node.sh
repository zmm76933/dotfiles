#!/bin/bash

# Stop script if errors occur
trap 'echo Error: $0:$LINENO stopped; exit 1' ERR INT
set -eu

# Load vital library that is most important and
# constructed with many minimal functions
# For more information, see etc/README.md
. "$DOTPATH"/etc/lib/vital.sh

# exit with true if you have Node command
if has "Node"; then
    log_pass "Node: already installed"
    exit
fi

case "$(get_os)" in
    # Case of OS X
    osx)
        if has "brew"; then
            if brew install nodebrew; then
                if nodebrew setup && nodebrew install-binary latest; then
                    log_pass "Node: installed successfully"
                fi
            else
                log_fail "Node: failed to install Node.js"
                exit 1
            fi
        else
            log_fail "error: require: Homebrew"
            exit 1
        fi
    ;;

    # Case of Linux
    linux)
        if has "curl"; then
            if curl -L git.io/nodebrew | perl - setup; then
                if nodebrew install-binary latest; then
                    log_pass "Node: installed successfully"
                fi
            else
                log_fail "Node: failed to install Node.js"
                exit 1
            fi
        else
            log_fail "error: require: curl"
            exit 1
        fi
    ;;

    # Other platforms such as BSD are supported
    *)
        log_fail "error: this script is only supported osx and linux"
        exit 1
    ;;
esac
