# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin directories
export PATH=$HOME/.rbenv/bin:$HOME/bin:$PATH
export PATH=$HOME/.pyenv/bin:$HOME/bin:$PATH
eval "$(rbenv init -)"
eval "$(pyenv init -)"
