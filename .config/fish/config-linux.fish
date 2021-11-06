if type -q yum
  alias update "sudo yum -y update && yum clean all"
end
if type -q apt
  alias update "sudo apt update && sudo apt -y upgrade && sudo apt -y dist-upgrade && sudo apt -y autoremove && sudo apt -y autoclean"
end

# Homebrew for linux
eval (/home/linuxbrew/.linuxbrew/bin/brew shellenv)

# wsl-specific configs
if [ "(uname -r | grep microsoft)" != '' ]
  alias code '"/mnt/c/Users/zmm76933/AppData/Local/Programs/Microsoft VS Code/bin/code"'
end
