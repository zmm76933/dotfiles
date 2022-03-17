" Description: wsl-specific configs
" Use windows clipboard to copy and to paste
if system("uname -r | grep microsoft") !=  ""
  let s:clip = '/mnt/c/Windows/System32/clip.exe'
  if executable(s:clip)
    augroup wsl_clip
      autocmd!
      autocmd TextYankPost * if v:event.operator ==# 'y' | call system(s:clip, @0) | endif
    augroup END
  endif
endif
