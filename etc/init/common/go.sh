#!/bin/bash

# Stop script if errors occur
trap 'echo Error: $0:$LINENO stopped; exit 1' ERR INT
set -eu

# Load vital library that is most important and
# constructed with many minimal functions
# For more information, see etc/README.md
. "$DOTPATH"/etc/lib/vital.sh

# exit with true if you have go command
if has "go"; then
    log_pass "go: already installed"
    exit
fi

case "$(get_os)" in
    # Case of OS X
    osx)
        if has "brew"; then
            if brew install go; then
                log_pass "go: installed successfully"
            else
                log_fail "go: failed to install golang"
                exit 1
            fi
        else
            log_fail "error: require: Homebrew"
            exit 1
        fi
    ;;

    # Case of Linux
    linux)
        if has "yum"; then
            if sudo yum install epel-release && yum install -y golang; then
                log_pass "go: installed successfully"
            fi
        elif has "apt-get"; then
            if sudo apt-get -y install golang; then
                log_pass "go: installed successfully"
            fi
        else
            log_fail "go: failed to install golang"
            exit 1
        fi
    ;;

    # Other platforms such as BSD are supported
    *)
        log_fail "error: this script is only supported osx and linux"
        exit 1
    ;;
esac
