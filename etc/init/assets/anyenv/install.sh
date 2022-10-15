#!/bin/bash

eval "$(anyenv init -)"

# install anyenv update
git clone https://github.com/znz/anyenv-update.git $(anyenv root)/plugins/anyenv-update

# install pyenv modules
git clone https://github.com/pyenv/pyenv-virtualenv.git $(pyenv root)/plugins/pyenv-virtualenv
git clone https://github.com/alefpereira/pyenv-pyright.git $(pyenv root)/plugins/pyenv-pyright

pyenv install 3.10.8
pyenv virtualenv 3.10.8 neovim3
pyenv global neovim3

# install rbenv
rbenv install 3.1.2
rbenv global 3.1.2

# install nodenv
nodenv install 18.5.0
nodenv global 18.5.0

# install plenv
plenv install 5.34.0
plenv global 5.34.0
