" Description: wsl-specific configs
if system("uname -r | grep microsoft") !=  ""
  " Improve Vim/Neovim experience with input methods
  let g:im_select_command = '/mnt/c/ProgramData/zenhan/zenhan.exe'
  let g:im_select_default = 0

  " Use windows clipboard to copy and to paste
  let s:clip = '/mnt/c/Windows/System32/clip.exe'
  if executable(s:clip)
    augroup wsl_clip
      autocmd!
      autocmd TextYankPost * if v:event.operator ==# 'y' | call system(s:clip, @0) | endif
    augroup END
  endif
endif
