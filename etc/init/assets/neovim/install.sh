#!/bin/bash

. $(brew --prefix asdf)/libexec/asdf.sh

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
