#!/bin/bash

eval "$(anyenv init -)"

# install anyenv update
git clone https://github.com/znz/anyenv-update.git $(anyenv root)/plugins/anyenv-update

# install pyenv
git clone https://github.com/pyenv/pyenv-virtualenv.git $(pyenv root)/plugins/pyenv-virtualenv
pyenv install 3.10.0
pyenv global 3.10.0

# install rbenv
rbenv install 3.1.2
rbenv global 3.1.2

# install nodenv
nodenv install 18.5.0
nodenv global 18.5.0

# install plenv
plenv install 5.34.0
plenv global 5.34.0
