#!/bin/bash

eval "$(anyenv init -)"

# install Python privider
pyenv install 3.9.5
pyenv virtualenv 3.9.5 neovim3
pyenv activate neovim3
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
plenv install-cpanm
cpanm -n Neovim::Ext
