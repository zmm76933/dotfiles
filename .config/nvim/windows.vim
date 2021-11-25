" Description: wsl-specific configs

" Use windows clipboard to copy and to paste
augroup wsl_clip
  autocmd!
  autocmd TextYankPost * :call system('clip.exe', @")
augroup END
