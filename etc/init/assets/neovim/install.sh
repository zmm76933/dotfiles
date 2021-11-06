#!/bin/bash

export PATH="$HOME/.anyenv/bin:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# install Python privider
pyenv install 2.7.16
pyenv virtualenv 2.7.16 neovim2
pyenv activate neovim2
pip install pynvim

pyenv install 3.8.5
pyenv virtualenv 3.8.5 neovim3
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
npm install -g pyright
npm install -g commitizen
