# Homebrew
eval (/home/linuxbrew/.linuxbrew/bin/brew shellenv)

function brew
    set -xl PATH $PATH # Protect global PATH by local PATH
    if type -q pyenv; and contains (pyenv root)/shims $PATH
        set -e PATH[(contains -i (pyenv root)/shims $PATH)]
    end

    command brew $argv
end

if type -q yum
  alias update "sudo yum -y update && yum clean all"
else if type -q apt
  alias update "brew update && brew upgrade && brew cleanup \
                sudo apt update && sudo apt -y upgrade && sudo apt -y dist-upgrade && sudo apt -y autoremove && sudo apt -y autoclean"
end

# wsl-specific configs
if [ "(uname -r | grep microsoft)" != '' ]
  alias code '"/mnt/c/Users/zmm76933/AppData/Local/Programs/Microsoft VS Code/bin/code"'
end
