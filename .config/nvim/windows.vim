" Description: wsl-specific configs

" Use windows clipboard to copy and to paste
augroup wsl_clip
  autocmd!
  autocmd TextYankPost * :call system('clip.exe', @")
augroup END

" Improve Vim/Neovim experience with input methods
let g:im_select_command = '/mnt/c/ProgramData/zenhan/zenhan.exe'
let g:im_select_default = 0
