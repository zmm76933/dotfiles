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

. $(brew --prefix asdf)/libexec/asdf.sh

if ! has "asdf"; then
    log_fail "error: this script is only supported with asdf"
    exit 1
fi

# exit with true if you have nvim command
if ! has "nvim"; then
    log_fail "error: this script is only supported with nvim"
    exit 1
fi

# install Python privider
pip install pynvim

# install Ruby provider
gem install neovim

# install Node.js provider
npm install -g neovim
npm install -g typescript 
npm install -g typescript-language-server
npm install -g diagnostic-languageserver
npm install -g eslint_d 
npm install -g prettier
npm install -g @fsouza/prettierd
npm install -g pyright
npm install -g commitizen
npm install -g cz-conventional-changelog

# install Perl provide
cpanm -n App::cpanminus
cpanm -n Neovim::Ext


if [[ ! -e ~/.local/share/nvim/site/pack/packer/start/packer.nvim ]]; then
  git clone https://github.com/wbthomason/packer.nvim \
    ~/.local/share/nvim/site/pack/packer/start/packer.nvim
fi

nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerInstall'
nvim --headless +TSUpdate +qa

source $DOTPATH/etc/init/assets/neovim/install.sh

log_pass "nvim: packer installed successfully"
