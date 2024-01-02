# vagrant
set -gx VAGRANT_WSL_ENABLE_WINDOWS_ACCESS 1
set -gx PATH "/mnt/c/Program Files/Oracle/VirtualBox" $PATH
set -gx PATH /mnt/c/Windows/System32 $PATH
set -gx PATH "/mnt/c/Windows/System32/WindowsPowerShell/v1.0" $PATH

alias code "/mnt/c/Users/zmm76933/AppData/Local/Programs/Microsoft VS Code/bin/code"
alias clip "/mnt/c/Windows/System32/clip.exe"
