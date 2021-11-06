#!/bin/bash

export PATH="$HOME/.anyenv/bin:$PATH"

# install pyenv
anyenv install pyenv
git clone https://github.com/pyenv/pyenv-virtualenv.git $(pyenv root)/plugins/pyenv-virtualenv
pyenv install 3.10.0
pyenv global 3.10.0

# install rbenv
anyenv install rbenv
rbenv install 3.0.2
rbenv global 3.0.2

# install nodenv
anyenv install nodenv
nodenv install 17.0.1
nodenv global 17.0.1
