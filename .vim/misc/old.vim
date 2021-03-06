"      ___                       ___           ___           ___      
"     /\__\          ___        /\__\         /\  \         /\  \     
"    /:/  /         /\  \      /::|  |       /::\  \       /::\  \    
"   /:/  /          \:\  \    /:|:|  |      /:/\:\  \     /:/\:\  \   
"  /:/__/  ___      /::\__\  /:/|:|__|__   /::\~\:\  \   /:/  \:\  \  
"  |:|  | /\__\  __/:/\/__/ /:/ |::::\__\ /:/\:\ \:\__\ /:/__/ \:\__\ 
"  |:|  |/:/  / /\/:/  /    \/__/~~/:/  / \/_|::\/:/  / \:\  \  \/__/ 
"  |:|__/:/  /  \::/__/           /:/  /     |:|::/  /   \:\  \       
"   \::::/__/    \:\__\          /:/  /      |:|\/__/     \:\  \      
"    ~~~~         \/__/         /:/  /       |:|  |        \:\__\     
"                               \/__/         \|__|         \/__/     

" README:
" Copyright (c) 2013-2015 b4b4r07 (aka BABAROT) All Rights Reserved
"
" BABAROT
" GitHub -> https://github.com/b4b4r07
"

" To write this vimrc, the most important thing is to observe the following
" decencies.
"
" - !! Portability and Multi-platform !!
" - !! Don't rely on plugins too much !!
"
" By integrating many useful functions as much as possible into this vimrc,
" it is possible to construct the highly versatile Vim without Vim plugins
" and Network.
"
" # Description
" I'm a Vim beginner. Therefore, this vimrc is incomplete.
" I appreciate your understanding in this matter.
" Well, in this vimrc, two useful features have been incorporated.
"
" - s:get_buflists()
" - s:MRU (the internal vimrc)
"
" `s:get_buflists()` is a function that manages the buffer in this Vim.
" Thus, you can grasp the status of the buffer unintentionally.
" Next, `s:MRU()` is a function that manage a file that was used recently.
" In recent years, it's features with any editor by default.
" By embedding the vimrc, because it is not affected by the external plugins,
" it is possible to use these features if you have this vimrc.
"
" Otherwise, there is some useful functions that were embedded in this vimrc.
"
" - s:ls()
" - s:smart_bwipeout()
" - s:win_tab_switcher()
" - ...
"
" Thank you.
"==============================================================================

" Startup: {{{1
" Skip initialization for vim-tiny or vim-small
" take account of '-eval'
"if !1 | finish | endif
if 0 | endif

" Use plain vim
" when vim was invoked by 'sudo' command
" or, invoked as 'git difftool'
if exists('$SUDO_USER') || exists('$GIT_DIR')
  finish
endif

if has('vim_starting')
  " Necesary for lots of cool vim things
  "set nocompatible
  " http://rbtnn.hateblo.jp/entry/2014/11/30/174749
  " Define the entire vimrc encoding
  scriptencoding utf-8
  " Initialize runtimepath
  set runtimepath&

  " Vim starting time
  if has('reltime')
    let g:startuptime = reltime()
    augroup vimrc-startuptime
      autocmd! VimEnter * let g:startuptime = reltime(g:startuptime) | redraw
            \ | echomsg 'startuptime: ' . reltimestr(g:startuptime)
    augroup END
  endif
endif

" Script variables {{{2
" boolean
let s:true  = 1
let s:false = 0

" platform
let s:is_windows = has('win16') || has('win32') || has('win64')
let s:is_cygwin = has('win32unix')
let s:is_mac = !s:is_windows && !s:is_cygwin
      \ && (has('mac') || has('macunix') || has('gui_macvim') ||
      \    (!executable('xdg-open') &&
      \    system('uname') =~? '^darwin'))
let s:is_linux = !s:is_mac && has('unix')

let s:vimrc = expand("<sfile>:p")
let $MYVIMRC = s:vimrc

" NeoBundle path
if s:is_windows
  let $DOTVIM = expand('~/vimfiles')
else
  let $DOTVIM = expand('~/.vim')
endif
let $VIMBUNDLE = $DOTVIM . '/bundle'
let $NEOBUNDLEPATH = $VIMBUNDLE . '/neobundle.vim'

" vimrc management variables
let s:vimrc_plugin_on                  = get(g:, 'vimrc_plugin_on',                  s:true)
let s:vimrc_suggest_neobundleinit      = get(g:, 'vimrc_suggest_neobundleinit',      s:true)
let s:vimrc_goback_to_eof2bof          = get(g:, 'vimrc_goback_to_eof2bof',          s:false)
let s:vimrc_save_window_position       = get(g:, 'vimrc_save_window_position',       s:false)
let s:vimrc_restore_cursor_position    = get(g:, 'vimrc_restore_cursor_position',    s:true)
let s:vimrc_statusline_manually        = get(g:, 'vimrc_statusline_manually',        s:true)
let s:vimrc_add_execute_perm           = get(g:, 'vimrc_add_execute_perm',           s:false)
let s:vimrc_colorize_statusline_insert = get(g:, 'vimrc_colorize_statusline_insert', s:true)
let s:vimrc_manage_rtp_manually        = get(g:, 's:vimrc_manage_rtp_manually',      s:false)
let s:vimrc_auto_cd_file_parentdir     = get(g:, 's:vimrc_auto_cd_file_parentdir',   s:true)
let s:vimrc_ignore_all_settings        = get(g:, 's:vimrc_ignore_all_settings',      s:false)

" if s:vimrc_manage_rtp_manually is s:true, s:vimrc_plugin_on is disabled.
let s:vimrc_plugin_on = s:vimrc_manage_rtp_manually == s:true ? s:false : s:vimrc_plugin_on

" nil variables
" Usage:
" if exists('s:vimrc_nil_dummy_variables')
"   execute ...
" This variable is used to disable the feature intentionally.
unlet! s:vimrc_nil_dummy_variables

function! s:vimrc_environment()
  let env = {}

  let env.is_starting = has('vim_starting')
  let env.is_gui = has('gui_running')
  let env.hostname = substitute(hostname(), '[^\w.]', '', '')

  if s:is_windows
    let vimpath = expand('~/vimfiles')
  else
    let vimpath = expand('~/.vim')
  endif
  let vimbundle = vimpath . '/bundle'
  let neobundlepath = vimbundle . '/neobundle.vim'

  let env.path = {
    \ 'vim':       vimpath,
    \ 'bundle':    vimbundle,
    \ 'neobundle': neobundlepath,
  \ }

  let env.support = {
    \ 'ag':        executable('ag'),
    \ 'osascript': executable('osascript'),
  \ }

  let env.is_tabpage = (&showtabline == 1 && tabpagenr('$') >= 2)
              \ || (&showtabline == 2 && tabpagenr('$') >= 1)
  return env
endfunction

" s:env is an environment variable in vimrc
let s:env = s:vimrc_environment()

" Depelopment for Vim plugin {{{2
if len(findfile("Vimpfile", ".;")) > 0
  let s:vimrc_plugin_on = s:false
  set runtimepath&
  execute "set runtimepath+=" . s:cwd
  execute "set runtimepath+=" . s:cwd . "/.vimp"
  for s:plugin in split(glob(s:cwd . "/.vimp/*"), '\n')
    execute 'set runtimepath+=' . s:plugin
  endfor
endif

" NeoBundle: {{{1
" Next generation Vim package manager settings.
" Use the NeoBundle (if installed and found on the default search runtimepath).
" If it is not installed, suggest executing ':NeoBundleInit' command.
" The ':NeoBundleInit' allows us to initialize all about NeoBundle.
" TODO: 
"   - neobundle#tap
"   - TOML
"   - NeoBundleInit TEST
"==============================================================================

" Add neobundle to runtimepath.
if has('vim_starting') && isdirectory($NEOBUNDLEPATH)
  if s:vimrc_plugin_on == s:true
    set runtimepath+=$NEOBUNDLEPATH
  endif
endif

" neobundle {{{
if stridx(&runtimepath, $NEOBUNDLEPATH) != -1
  let g:neobundle#enable_tail_path = 1
  let g:neobundle#default_options = {
        \ 'same' : { 'stay_same' : 1, 'overwrite' : 0 },
        \ '_' : { 'overwrite' : 0 },
        \ }
  "call neobundle#rc($VIMBUNDLE)
  call neobundle#begin($VIMBUNDLE)

  " Taking care of NeoBundle by itself
  NeoBundleFetch 'Shougo/neobundle.vim'

  " NeoBundle List
  "NeoBundle 'Shougo/unite.vim'
  NeoBundle 'Shougo/vimproc'

  if has('lua') && v:version >= 703
    NeoBundleLazy 'Shougo/neocomplete.vim'
  else
    NeoBundleLazy 'Shougo/neocomplcache.vim'
  endif

  NeoBundle 'chase/vim-ansible-yaml'
  "NeoBundle 'Shougo/unite-outline'
  "NeoBundle 'Shougo/unite-help'
  "NeoBundle 'Shougo/neomru.vim'
  "NeoBundle 'Shougo/vimfiler'
  "NeoBundle 'Shougo/vimshell'
  "NeoBundle 'Shougo/neosnippet'
  "NeoBundle 'Shougo/neosnippet-snippets'
  "NeoBundle 'glidenote/memolist.vim'
  "NeoBundle 'severin-lemaignan/vim-minimap'
  "NeoBundle 'thinca/vim-scouter'
  "NeoBundle 'thinca/vim-ref'
  "NeoBundle 'thinca/vim-quickrun'
  "NeoBundle 'thinca/vim-unite-history'
  NeoBundle 'thinca/vim-splash'
  "NeoBundle 'thinca/vim-portal'
  "NeoBundle 'thinca/vim-poslist'
  "NeoBundle 'thinca/vim-tabrecent'
  "NeoBundle 'thinca/vim-qfreplace'
  "NeoBundle 'tyru/nextfile.vim'
  "NeoBundle 'tyru/skk.vim'
  "NeoBundle 'tyru/eskk.vim'
  "NeoBundle 'tyru/open-browser.vim'
  NeoBundle 'tyru/restart.vim'
  "NeoBundle 'sjl/gundo.vim'
  NeoBundle 'ujihisa/neco-look'
  "NeoBundle 'ujihisa/unite-colorscheme'
  "NeoBundle 'b4b4r07/vim-vimp'
  "NeoBundle 'b4b4r07/vim-autocdls'
  NeoBundle 'b4b4r07/mru.vim'
  NeoBundle 'b4b4r07/vim-shellutils'
  "NeoBundle 'b4b4r07/vim-favdir'
  "NeoBundle 'b4b4r07/vim-ezoe'
  "NeoBundle 'b4b4r07/vim-sunset'
  "NeoBundle 'b4b4r07/vim-pt'
  NeoBundle has('gui_running') ? 'itchyny/lightline.vim' : 'b4b4r07/vim-buftabs'
  "NeoBundle 'b4b4r07/vim-buftabs'
  "NeoBundle 'itchyny/calendar.vim'
  "NeoBundle 'nathanaelkane/vim-indent-guides'
  NeoBundle 'scrooloose/syntastic'
  "NeoBundle 'scrooloose/nerdtree'
  NeoBundle 'tpope/vim-surround'
  NeoBundle 'tpope/vim-repeat'
  NeoBundle 'tpope/vim-markdown'
  "NeoBundle 'tpope/vim-fugitive'
  "NeoBundle 'osyo-manga/vim-anzu'
  NeoBundle 'cohama/lexima.vim'
  "NeoBundle 'cohama/vim-insert-linenr'
  "NeoBundle 'cohama/agit.vim'
  "NeoBundle 'LeafCage/yankround.vim'
  "NeoBundle 'LeafCage/foldCC.vim'
  "NeoBundle 'junegunn/vim-easy-align'
  "NeoBundle 'mattn/gist-vim'
  "NeoBundle 'mattn/webapi-vim'
  "NeoBundle 'mattn/benchvimrc-vim'
  "NeoBundle 'vim-scripts/Align'
  "NeoBundle 'vim-scripts/DirDiff.vim'
  "NeoBundle 'mattn/excitetranslate-vim'
  "NeoBundle 'yomi322/unite-tweetvim'
  "NeoBundle 'tsukkee/lingr-vim'
  "NeoBundle 'AndrewRadev/switch.vim'
  "NeoBundle 'Yggdroot/indentLine'
  "NeoBundle 'ervandew/supertab'
  "NeoBundle 'vim-scripts/renamer.vim'
  "NeoBundle 'rking/ag.vim'
  "NeoBundle 'mopp/googlesuggest-source.vim'
  "NeoBundle 'mattn/googlesuggest-complete-vim'
  "NeoBundle 'kana/vim-vspec'
  "NeoBundle 'tell-k/vim-browsereload-mac'
  "NeoBundle 'junegunn/vim-emoji'
  "NeoBundle 'majutsushi/tagbar'
  "NeoBundle 'dgryski/vim-godef'
  "NeoBundle 'vim-jp/vim-go-extra'
  "NeoBundle 'google/vim-ft-go'
  "NeoBundle 'ctrlpvim/ctrlp.vim'
  "NeoBundle 'toyamarinyon/hatenablog-vim'
  NeoBundle 'justinmk/vim-dirvish'
  "NeoBundle 'deris/vim-visualinc'
  "NeoBundle 'hotchpotch/perldoc-vim'
  NeoBundle 'kchmck/vim-coffee-script'
  NeoBundle 'elzr/vim-json'
  NeoBundle 'cespare/vim-toml'
  NeoBundle 'fatih/vim-go'
  NeoBundle 'jnwhiteh/vim-golang'
  NeoBundle 'zaiste/tmux.vim'
  "NeoBundle 'CORDEA/vim-glue'
  "NeoBundle 'dag/vim-fish'
  "NeoBundle 'haya14busa/incsearch.vim'
  "NeoBundle 'rhysd/try-colorscheme.vim'
  "NeoBundle 'rhysd/github-complete.vim'
  NeoBundle 'junegunn/fzf'
  NeoBundle 'junegunn/fzf.vim'

  NeoBundle 'b4b4r07/vim-hcl'
  NeoBundle 'fatih/vim-hclfmt'

  " Japanese help
  NeoBundle 'vim-jp/vimdoc-ja'
  " Vital
  NeoBundle 'vim-jp/vital.vim'

  " Colorschemes
  NeoBundle 'b4b4r07/solarized.vim', { "base" : $HOME."/.vim/colors" }
  NeoBundle 'nanotech/jellybeans.vim', { "base" : $HOME."/.vim/colors" }
  NeoBundle 'tomasr/molokai', { "base" : $HOME."/.vim/colors" }
  NeoBundle 'w0ng/vim-hybrid', { "base" : $HOME."/.vim/colors" }

  " Disable plugins
  if !has('gui_running')
    "NeoBundleDisable lightline.vim
  endif
  "NeoBundleDisable mru.vim
  "NeoBundleDisable vim-buftabs

  " Manually manage rtp
  "NeoBundle 'vim-mru', {'type' : 'nosync', 'base' : '~/.vim/manual'}
  "NeoBundle 'vim-curbuf', {'type' : 'nosync', 'base' : '~/src/github.com/b4b4r07'}

  call neobundle#end()

  " Check.
  NeoBundleCheck
else
  " no neobundle {{{2
  command! NeoBundleInit try | call <SID>neobundle_init()
        \| catch /^neobundleinit:/
        \|   echohl ErrorMsg
        \|   echomsg v:exception
        \|   echohl None
        \| endtry

  function! s:neobundle_init()
    redraw | echo "Installing neobundle.vim..."
    if !isdirectory($VIMBUNDLE)
      call mkdir($VIMBUNDLE, 'p')
      sleep 1 | echo printf("Creating '%s'.", $VIMBUNDLE)
    endif
    cd $VIMBUNDLE

    if executable('git')
      call system('git clone git://github.com/Shougo/neobundle.vim')
      if v:shell_error
        throw 'neobundleinit: Git error.'
      endif
    endif

    set runtimepath& runtimepath+=$NEOBUNDLEPATH
    call neobundle#rc($VIMBUNDLE)
    try
      echo printf("Reloading '%s'", $MYVIMRC)
      source $MYVIMRC
    catch
      echohl ErrorMsg
      echomsg 'neobundleinit: $MYVIMRC: could not source.'
      echohl None
      return 0
    finally
      echomsg 'Installed neobundle.vim'
    endtry

    echomsg 'Finish!'
  endfunction

  if s:vimrc_suggest_neobundleinit == s:true
    autocmd! VimEnter * redraw
          \ | echohl WarningMsg
          \ | echo "You should do ':NeoBundleInit' at first!"
          \ | echohl None
  else
    NeoBundleInit
  endif
endif
"}}}

" nessesary
filetype plugin indent on

if s:vimrc_ignore_all_settings == s:true
  finish
endif

" Library: in vimrc {{{1
" Functions that are described in this section is general functions.
" It is not general, for example, functions used in a dedicated purpose
" has been described in the setting position.
"==============================================================================

" func s:bundle() {{{2
function! s:bundled(bundle)
  if !isdirectory($VIMBUNDLE)
    return s:false
  endif
  if stridx(&runtimepath, $NEOBUNDLEPATH) == -1
    return s:false
  endif

  if a:bundle ==# 'neobundle.vim'
    return s:true
  else
    return neobundle#is_installed(a:bundle)
  endif
endfunction

" func s:has_plugin() {{{2
function! s:has_plugin(name)
  " Check {name} plugin whether there is in the runtime path
  let nosuffix = a:name =~? '\.vim$' ? a:name[:-5] : a:name
  let suffix   = a:name =~? '\.vim$' ? a:name      : a:name . '.vim'
  return &rtp =~# '\c\<' . nosuffix . '\>'
        \   || globpath(&rtp, suffix, 1) != ''
        \   || globpath(&rtp, nosuffix, 1) != ''
        \   || globpath(&rtp, 'autoload/' . suffix, 1) != ''
        \   || globpath(&rtp, 'autoload/' . tolower(suffix), 1) != ''
endfunction

" func s:b4b4r07() {{{2
function! s:b4b4r07()
  hide enew
  setlocal buftype=nofile nowrap nolist nonumber bufhidden=wipe
  setlocal modifiable nocursorline nocursorcolumn

  let b4b4r07 = []
  call add(b4b4r07, 'Copyright (c) 2014                                 b4b4r07''s vimrc.')
  call add(b4b4r07, '.______    _  _    .______    _  _    .______        ___    ______  ')
  call add(b4b4r07, '|   _  \  | || |   |   _  \  | || |   |   _  \      / _ \  |____  | ')
  call add(b4b4r07, '|  |_)  | | || |_  |  |_)  | | || |_  |  |_)  |    | | | |     / /  ')
  call add(b4b4r07, '|   _  <  |__   _| |   _  <  |__   _| |      /     | | | |    / /   ')
  call add(b4b4r07, '|  |_)  |    | |   |  |_)  |    | |   |  |\  \----.| |_| |   / /    ')
  call add(b4b4r07, '|______/     |_|   |______/     |_|   | _| `._____| \___/   /_/     ')
  call add(b4b4r07, 'If it is being displayed, the vim plugins are not set and installed.')
  call add(b4b4r07, 'In this environment, run '':NeoBundleInit'' if you setup vim plugin.')

  silent put =repeat([''], winheight(0)/2 - len(b4b4r07)/2)
  let space = repeat(' ', winwidth(0)/2 - strlen(b4b4r07[0])/2)
  for line in b4b4r07
    put =space . line
  endfor
  silent put =repeat([''], winheight(0)/2 - len(b4b4r07)/2 + 1)
  silent file B4B4R07
  1

  execute 'syntax match Directory display ' . '"'. '^\s\+\U\+$'. '"'
  setlocal nomodifiable
  redraw
  let char = getchar()
  silent enew
  call feedkeys(type(char) == type(0) ? nr2char(char) : char)
endfunction

" func s:escape_filename() {{{2
function! s:escape_filename(fname)
  let esc_filename_chars = ' *?[{`$%#"|!<>();&' . "'\t\n"
  if exists("*fnameescape")
    return fnameescape(a:fname)
  else
    return escape(a:fname, esc_filename_chars)
  endif
endfunction

" func s:has() {{{2
function! s:has(path)
  let save_wildignore = &wildignore
  setlocal wildignore=
  let path = glob(simplify(a:path))
  let &wildignore = save_wildignore
  if exists("*s:escape_filename")
    let path = s:escape_filename(path)
  endif
  return empty(path) ? s:false : s:true
endfunction

" func s:get_dir_separator() {{{2
function! s:get_dir_separator()
  return fnamemodify('.', ':p')[-1 :]
endfunction

" func s:echomsg() {{{2
function! s:echomsg(hl, msg)
  execute 'echohl' a:hl
  try
    echomsg a:msg
  finally
    echohl None
  endtry
endfunction

" func s:error() {{{2
function! s:error(msg)
  echohl ErrorMsg
  echo 'ERROR: ' . a:msg
  echohl None
  return s:false
endfunction

" func s:warning() {{{2
function! s:warning(msg)
  echohl WarningMsg
  echo 'WARNING: ' . a:msg
  echohl None
endfunction

" func s:confirm() {{{2
function! s:confirm(msg)
  return input(printf('%s [y/N]: ', a:msg)) =~? '^y\%[es]$'
endfunction

" func s:mkdir() {{{2
function! s:mkdir(dir)
  if !exists("*mkdir")
    return s:false
  endif

  let dir = expand(a:dir)
  if isdirectory(dir)
    return s:true
  endif

  return mkdir(dir, "p")
endfunction

" func s:auto_mkdir() {{{2
function! s:auto_mkdir(dir, force)
  if !isdirectory(a:dir) && (a:force ||
        \ input(printf('"%s" does not exist. Create? [y/N] ', a:dir)) =~? '^y\%[es]$')
    return mkdir(iconv(a:dir, &encoding, &termencoding), 'p')
  endif
endfunction

" func s:smart_foldcloser() {{{2
function! s:smart_foldcloser()
  if foldlevel('.') == 0
    normal! zM
    return
  endif

  let foldc_lnum = foldclosed('.')
  normal! zc
  if foldc_lnum == -1
    return
  endif

  if foldclosed('.') != foldc_lnum
    return
  endif
  normal! zM
endfunction

" func s:smart_execute() {{{2
function! s:smart_execute(expr)
  let wininfo = winsaveview()
  execute a:expr
  call winrestview(wininfo)
endfunction

" func s:rand() {{{2
function! s:rand(n)
  let match_end = matchend(reltimestr(reltime()), '\d\+\.') + 1
  return reltimestr(reltime())[match_end : ] % (a:n + 1)
endfunction

" func s:random_string() {{{2
function! s:random_string(n)
  let n = a:n ==# '' ? 8 : a:n
  let s = []
  let chars = split('0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ', '\ze')
  let max = len(chars) - 1
  for x in range(n)
    call add(s, (chars[s:rand(max)]))
  endfor
  let @+ = join(s, '')
  return join(s, '')
endfunction

" func s:move_left_center_right() {{{2
function! s:move_left_center_right(...)
  let curr_pos = getpos('.')
  let curr_line_len = len(getline('.'))
  let curr_pos[3] = 0
  let c = curr_pos[2]
  if 0 <= c && c < (curr_line_len / 3 * 1)
    if a:0 > 0
      let curr_pos[2] = curr_line_len
    else
      let curr_pos[2] = curr_line_len / 2
    endif
  elseif (curr_line_len / 3 * 1) <= c && c < (curr_line_len / 3 * 2)
    if a:0 > 0
      let curr_pos[2] = 0
    else
      let curr_pos[2] = curr_line_len
    endif
  else
    if a:0 > 0
      let curr_pos[2] = curr_line_len / 2
    else
      let curr_pos[2] = 0
    endif
  endif
  call setpos('.',curr_pos)
endfunction

" func s:toggle_option() {{{2
function! s:toggle_option(option_name)
  if exists('&' . a:option_name)
    execute 'setlocal' a:option_name . '!'
    execute 'setlocal' a:option_name . '?'
  endif
endfunction

" func s:toggle_variable() {{{2
function! s:toggle_variable(variable_name)
  if eval(a:variable_name)
    execute 'let' a:variable_name . ' = 0'
  else
    execute 'let' a:variable_name . ' = 1'
  endif
  echo printf('%s = %s', a:variable_name, eval(a:variable_name))
endfunction

" func s:rename() {{{2
function! s:rename(new, type)
  if a:type ==# 'file'
    if empty(a:new)
      let new = input('New filename: ', expand('%:p:h') . '/', 'file')
    else
      let new = a:new
    endif
  elseif a:type ==# 'ext'
    if empty(a:new)
      let ext = input('New extention: ', '', 'filetype')
      let new = expand('%:p:t:r')
      if !empty(ext)
        let new .= '.' . ext
      endif
    else
      let new = expand('%:p:t:r') . '.' . a:new
    endif
  endif

  if filereadable(new)
    redraw
    echo printf("overwrite `%s'? ", new)
    if nr2char(getchar()) ==? 'y'
      silent call delete(new)
    else
      return s:false
    endif
  endif

  if new != '' && new !=# 'file'
    let oldpwd = getcwd()
    lcd %:p:h
    execute 'file' new
    execute 'setlocal filetype=' . fnamemodify(new, ':e')
    write
    call delete(expand('#'))
    execute 'lcd' oldpwd
  endif
endfunction

" func s:make_junkfile() {{{2
function! s:make_junkfile()
  let junk_dir = $HOME . '/.vim/junk'. strftime('/%Y/%m/%d')
  if !isdirectory(junk_dir)
    call s:mkdir(junk_dir)
  endif

  let ext = input('Junk Ext: ')
  let filename = junk_dir . tolower(strftime('/%A')) . strftime('_%H%M%S')
  if !empty(ext)
    let filename = filename . '.' . ext
  endif
  execute 'edit ' . filename
endfunction

" func s:copy_current_path() {{{2
function! s:copy_current_path(...)
  let path = a:0 ? expand('%:p:h') : expand('%:p')
  if s:is_windows
    let @* = substitute(path, '\\/', '\\', 'g')
  else
    let @* = path
  endif
  echo path
endfunction

" func s:load_source() {{{2
function! s:load_source(path)
  let path = expand(a:path)
  if filereadable(path)
    execute 'source ' . path
  endif
endfunction

" func s:open() {{{2
function! s:open(file)
  if !executable('open')
    return s:error('open: not supported yet.')
  endif
  let file = empty(a:file) ? expand('%') : fnamemodify(a:file, ':p')
  call system(printf('%s %s &', 'open', shellescape(file)))
  return v:shell_error ? s:false : s:true
endfunction

" func s:rm() {{{2
function! s:rm(...)
  let files = []
  for file in a:0 ? map(copy(a:000), 'expand(v:val)') : split(simplify(expand('%:p')))

    let file = fnamemodify(file, ":p")
    if isdirectory(file)
      let dest = "/tmp/".s:random_string(20)
      if rename(file, dest) == 0
        call add(files, file)
      else
        call s:error("not support a directory")
      endif
    elseif filereadable(file)
      if delete(file) == 0
        call add(files, file)
        let bufname = bufname(fnamemodify(file, ':p'))
        if bufexists(bufname) && buflisted(bufname)
          execute "bwipeout" bufname
        endif
      endif
    else
      echohl WarningMsg | echo "The '" . file . "' does not exist" | echohl NONE
    endif
  endfor

  echo len(files) ? "Removed " . string(files) . "!" : "Removed nothing"
endfunction

" func s:ls() {{{2
function! s:ls(path, bang)
  let path = empty(a:path) ? getcwd() : expand(a:path)
  if filereadable(path)
    if executable("ls")
      echo system("ls -l " . path)
      return v:shell_error ? s:false : s:true
    else
      return s:error('ls: command not found')
    endif
  endif

  if !isdirectory(path)
    return s:error(path.":No such file or directory")
  endif

  let save_ignore = &wildignore
  set wildignore=
  let filelist = glob(path . "/*")
  if !empty(a:bang)
    let filelist .= "\n".glob(path . "/.*[^.]")
  endif
  let &wildignore = save_ignore
  let filelist = substitute(filelist, '', '^M', 'g')

  if empty(filelist)
    return s:error("no file")
  endif

  let lists = []
  for file in split(filelist, "\n")
    if isdirectory(file)
      call add(lists, fnamemodify(file, ":t") . "/")
    else
      if executable(file)
        call add(lists, fnamemodify(file, ":t") . "*")
      elseif getftype(file) == 'link'
        call add(lists, fnamemodify(file, ":t") . "@")
      else
        call add(lists, fnamemodify(file, ":t"))
      endif
    endif
  endfor

  echohl WarningMsg | echon len(lists) . ":\t" | echohl None
  highlight LsDirectory  cterm=bold ctermfg=NONE ctermfg=26        gui=bold guifg=#0096FF   guibg=NONE
  highlight LsExecutable cterm=NONE ctermfg=NONE ctermfg=Green     gui=NONE guifg=Green     guibg=NONE
  highlight LsSymbolick  cterm=NONE ctermfg=NONE ctermfg=LightBlue gui=NONE guifg=LightBlue guibg=NONE

  for item in lists
    if item =~ '/'
      echohl LsDirectory | echon item[:-2] | echohl NONE
      echon item[-1:-1] . " "
    elseif item =~ '*'
      echohl LsExecutable | echon item[:-2] | echohl NONE
      echon item[-1:-1] . " "
    elseif item =~ '@'
      echohl LsSymbolick | echon item[:-2] | echohl NONE
      echon item[-1:-1] . " "
    else
      echon item . " "
    endif
  endfor

  return s:true
endfunction

" func s:buf_delete() {{{2
function! s:buf_delete(bang)
  let file = fnamemodify(expand('%'), ':p')
  let g:buf_delete_safety_mode = 1
  let g:buf_delete_custom_command = "system(printf('%s %s', 'gomi', shellescape(file)))"

  if filereadable(file)
    if empty(a:bang)
      redraw | echo 'Delete "' . file . '"? [y/N]: '
    endif
    if !empty(a:bang) || nr2char(getchar()) ==? 'y'
      silent! update
      if g:buf_delete_safety_mode == 1
        silent! execute has('clipboard') ? '%yank "*' : '%yank'
      endif
      if eval(g:buf_delete_custom_command == "" ? delete(file) : g:buf_delete_custom_command) == 0
        let bufname = bufname(fnamemodify(file, ':p'))
        if bufexists(bufname) && buflisted(bufname)
          execute "bwipeout" bufname
        endif
        echo "Deleted '" . file . "', successfully!"
        return s:true
      endif
      "echo "Could not delete '" . file . "'"
      return s:error("Could not delete '" . file . "'")
    else
      echo "Do nothing."
    endif
  else
    "echohl WarningMsg | echo "The '" . file . "' does not exist." | echohl NONE
    return s:error(echo "The '" . file . "' does not exist")
  endif
endfunction

" func s:count_buffers() {{{2
function! s:count_buffers()
  let l:count = 0
  for i in range(1, bufnr('$'))
    if bufexists(i) && buflisted(i)
      let l:count += 1
    endif
  endfor
  return l:count
endfunction

" func s:get_buflists() {{{2
function! s:get_buflists(...)
  if a:0 && a:1 ==# 'n'
    silent bnext
  elseif a:0 && a:1 ==# 'p'
    silent bprev
  endif

  let list  = ''
  let lists = []
  for buf in range(1, bufnr('$'))
    if bufexists(buf) && buflisted(buf)
      let list  = bufnr(buf) . "#" . fnamemodify(bufname(buf), ':t')
      let list .= getbufvar(buf, "&modified") ? '+' : ''
      if bufnr('%') ==# buf
        let list = "[" . list . "]"
      else
        let list = " " . list . " "
      endif
      call add(lists, list)
    endif
  endfor
  redraw | echo join(lists, "")
endfunction

" func s:smart_bwipeout() {{{2
function! s:smart_bwipeout(mode)
  " Bwipeout! all buffers except current buffer.
  if a:mode == 1
    for i in range(1, bufnr('$'))
      if bufexists(i)
        if bufnr('%') ==# i | continue | endif
        execute 'silent bwipeout! ' . i
      endif
    endfor
    return
  endif

  if a:mode == 0
    if winnr('$') != 1
      quit
      return
    elseif tabpagenr('$') != 1
      tabclose
      return
    endif
  endif

  let bufname = empty(bufname(bufnr('%'))) ? bufnr('%') . "#" : bufname(bufnr('%'))
  if &modified == 1
    echo printf("'%s' is unsaved. Quit!? [y(f)/N/w] ", bufname)
    let c = nr2char(getchar())

    if c ==? 'w'
      let filename = ''
      if bufname(bufnr("%")) ==# filename
        redraw
        while empty(filename)
          let filename = input('Tell me filename: ')
        endwhile
      endif
      execute "write " . filename
      silent bwipeout!

    elseif c ==? 'y' || c ==? 'f'
      silent bwipeout!
    else
      redraw
      echo "Do nothing"
      return
    endif
  else
    silent bwipeout
  endif

  if s:has_plugin("vim-buftabs")
    echo "Bwipeout " . bufname
  else
    redraw
    call <SID>get_buflists()
  endif
endfunction

" func s:smart_bchange() {{{2
function! s:smart_bchange(mode)
  let mode = a:mode

  " If window splitted, no working
  if winnr('$') != 1
    " Normal bnext/bprev
    execute 'silent' mode ==? 'n' ? 'bnext' : 'bprevious'
    if exists("*s:get_buflists") && exists("*s:count_buffers")
      if s:count_buffers() > 1
        call s:get_buflists()
      endif
    endif
    return
  endif

  " Get all buffer numbers in tabpages
  let tablist = []
  for i in range(tabpagenr('$'))
    call add(tablist, tabpagebuflist(i + 1))
  endfor

  " Get buffer number
  execute 'silent' mode ==? 'n' ? 'bnext' : 'bprevious'
  let bufnr = bufnr('%')
  execute 'silent' mode ==? 'n' ? 'bprevious' : 'bnext'

  " Check next/prev buffer number if exists in l:tablist
  let nextbuf = []
  call add(nextbuf, bufnr)
  if index(tablist, nextbuf) >= 0
    execute 'silent tabnext' index(tablist, nextbuf) + 1
  else
    " Normal bnext/bprev
    execute 'silent' mode ==? 'n' ? 'bnext' : 'bprevious'
  endif
endfunction

" func s:smart_bchange() {{{2
function! s:bufnew(buf, bang)
  let buf = empty(a:buf) ? '' : a:buf
  execute "new" buf | only
  if !empty(a:bang)
    let bufname = empty(buf) ? '[Scratch]' : buf
    setlocal bufhidden=unload
    setlocal nobuflisted
    setlocal buftype=nofile
    setlocal noswapfile
    silent file `=bufname`
  endif
endfunction

" func s:buf_enqueue() {{{2
function! s:buf_enqueue(buf)
  let buf = fnamemodify(a:buf, ':p')
  if bufexists(buf) && buflisted(buf) && filereadable(buf)
    let idx = match(s:bufqueue ,buf)
    if idx != -1
      call remove(s:bufqueue, idx)
    endif
    call add(s:bufqueue, buf)
  endif
endfunction

" func s:buf_dequeue() {{{2
function! s:buf_dequeue(buf)
  if empty(s:bufqueue)
    throw 'bufqueue: Empty queue.'
  endif

  if a:buf =~# '\d\+'
    return remove(s:bufqueue, a:buf)
  else
    return remove(s:bufqueue, index(s:bufqueue, a:buf))
  endif
endfunction

" func s:buf_restore() {{{2
function! s:buf_restore()
  try
    execute 'edit' s:buf_dequeue(-1)
  catch /^bufqueue:/
    "echohl ErrorMsg
    "echomsg v:exception
    "echohl None
    call s:error(v:exception)
  endtry
endfunction

" func s:all_buffers_bwipeout() {{{2
function! s:all_buffers_bwipeout()
  for i in range(1, bufnr('$'))
    if bufexists(i) && buflisted(i)
      execute 'bwipeout' i
    endif
  endfor
endfunction

" func s:win_tab_switcher() {{{2
function! s:win_tab_switcher(...)
  let minus = 0
  if &laststatus == 1 && winnr('$') != 1
    let minus += 1
  elseif &laststatus == 2
    let minus += 1
  endif
  let minus += &cmdheight
  if &showtabline == 1 && tabpagenr('$') != 1
    let minus += 1
  elseif &showtabline == 2
    let minus += 1
  endif

  let is_split   = winheight(0) != &lines - minus
  let is_vsplit  = winwidth(0)  != &columns
  let is_tabpage = tabpagenr('$') >= 2

  let buffer_switcher = get(g:, 'buffer_switcher', 0)
  if a:0 && a:1 ==# 'l'
    if is_tabpage
      if tabpagenr() == tabpagenr('$')
        if !is_split && !is_vsplit
          if buffer_switcher
            silent bnext
          else
            echohl WarningMsg
            echo 'Last tabpages'
            echohl None
          endif
        endif
        if (is_split || is_vsplit) && winnr() == winnr('$')
          if buffer_switcher
            silent bnext
          else
            echohl WarningMsg
            echo 'Last tabpages'
            echohl None
          endif
        elseif (is_split || is_vsplit) && winnr() != winnr('$')
          silent wincmd w
        endif
      else
        if !is_split && !is_vsplit
          silent tabnext
        endif
        if (is_split || is_vsplit) && winnr() == winnr('$')
          silent tabnext
        elseif (is_split || is_vsplit) && winnr() != winnr('$')
          silent wincmd w
        endif
      endif
    else
      if !is_split && !is_vsplit
        if buffer_switcher
          silent bnext
        else
          echohl WarningMsg
          echo 'Last tabpages'
          echohl None
        endif
      endif
      if (is_split || is_vsplit) && winnr() == winnr('$')
        if buffer_switcher
          silent bnext
        else
          echohl WarningMsg
          echo 'Last tabpages'
          echohl None
        endif
      else
        silent wincmd w
      endif
    endif
  endif
  if a:0 && a:1 ==# 'h'
    if is_tabpage
      if tabpagenr() == 1
        if !is_split && !is_vsplit
          if buffer_switcher
            silent bprevious
          else
            echohl WarningMsg
            echo 'First tabpages'
            echohl None
          endif
        endif
        if (is_split || is_vsplit) && winnr() == 1
          if buffer_switcher
            silent bprevious
          else
            echohl WarningMsg
            echo 'First tabpages'
            echohl None
          endif
        elseif (is_split || is_vsplit) && winnr() != 1
          silent wincmd W
        endif
      else
        if !is_split && !is_vsplit
          silent tabprevious
        endif
        if (is_split || is_vsplit) && winnr() == 1
          silent tabprevious
        elseif (is_split || is_vsplit) && winnr() != 1
          silent wincmd W
        endif
      endif
    else
      if !is_split && !is_vsplit
        if buffer_switcher
          silent bprevious
        else
          echohl WarningMsg
          echo 'First tabpages'
          echohl None
        endif
      endif
      if (is_split || is_vsplit) && winnr() == 1
        if buffer_switcher
          silent bprevious
        else
          echohl WarningMsg
          echo 'First tabpages'
          echohl None
        endif
      else
        silent wincmd W
      endif
    endif
  endif

  if s:has_plugin("vim-buftabs")
  else
    redraw
    call <SID>get_buflists()
  endif
endfunction

" func s:tabdrop() {{{2
function! s:tabdrop(target)
  let target = empty(a:target) ? expand('%:p') : bufname(a:target + 0)
  if !empty(target) && bufexists(target) && buflisted(target)
    execute 'tabedit' target
  else
    call s:warning("Could not tabedit")
  endif
endfunction

" func s:tabnew() {{{2
function! s:tabnew(num)
  let num = empty(a:num) ? 1 : a:num
  for i in range(1, num)
    tabnew
  endfor
endfunction

" func s:move_tabpage() {{{2
function! s:move_tabpage(dir)
  if a:dir == "right"
    let num = tabpagenr()
  elseif a:dir == "left"
    let num = tabpagenr() - 2
  endif
  if num >= 0
    execute "tabmove" num
  endif
endfunction

" func s:close_all_right_tabpages() {{{2
function! s:close_all_right_tabpages()
  let current_tabnr = tabpagenr()
  let last_tabnr = tabpagenr("$")
  let num_close = last_tabnr - current_tabnr
  let i = 0
  while i < num_close
    execute "tabclose " . (current_tabnr + 1)
    let i = i + 1
  endwhile
endfunction

" func s:close_all_left_tabpages() {{{2
function! s:close_all_left_tabpages()
  let current_tabnr = tabpagenr()
  let num_close = current_tabnr - 1
  let i = 0
  while i < num_close
    execute "tabclose 1"
    let i = i + 1
  endwhile
endfunction

" func s:find_tabnr() {{{2
function! s:find_tabnr(bufnr)
  for tabnr in range(1, tabpagenr("$"))
    if index(tabpagebuflist(tabnr), a:bufnr) !=# -1
      return tabnr
    endif
  endfor
  return -1
endfunction

" func s:find_winnr() {{{2
function! s:find_winnr(bufnr)
  for winnr in range(1, winnr("$"))
    if a:bufnr ==# winbufnr(winnr)
      return winnr
    endif
  endfor
  return 1
endfunction

" func s:find_winnr() {{{2
function! s:recycle_open(default_open, path)
  let default_action = a:default_open . ' ' . a:path
  if bufexists(a:path)
    let bufnr = bufnr(a:path)
    let tabnr = s:find_tabnr(bufnr)
    if tabnr ==# -1
      execute default_action
      return
    endif
    execute 'tabnext ' . tabnr
    let winnr = s:find_winnr(bufnr)
    execute winnr . 'wincmd w'
  else
    execute default_action
  endif
endfunction

" Utility: in vimrc {{{1
" In this section, the settings a higher priority than the setting items
" of the other sections will be described.
"==============================================================================

" Display B4B4R07 logo start-up. {{{2
if !s:has_plugin('neobundle.vim')
  command! B4B4R07 call <SID>b4b4r07()
  augroup vimrc-without-plugin
    autocmd!
    autocmd VimEnter * if !argc() | call <SID>b4b4r07() | endif
  augroup END
  "autocmd VimEnter * nested if @% == '' && s:GetBufByte() == 0 | edit $MYVIMRC | endif
  "function! s:GetBufByte()
  "    let byte = line2byte(line('$') + 1)
  "    if byte == -1
  "        return 0
  "    else
  "        return byte - 1
  "    endif
  "endfunction
endif

" Builtin MRU {{{2
if !s:has_plugin('mru.vim')
  " MRU configuration variables {{{3
  if !exists('s:MRU_File')
    if has('unix') || has('macunix')
      let s:MRU_File = $HOME . '/.vim_mru_files'
    else
      let s:MRU_File = $VIM . '/_vim_mru_files'
      if has('win32')
        if $USERPROFILE != ''
          let s:MRU_File = $USERPROFILE . '\_vim_mru_files'
        endif
      endif
    endif
  endif

  function! s:MRU_LoadList() "{{{3
    if filereadable(s:MRU_File)
      let s:MRU_files = readfile(s:MRU_File)
      if s:MRU_files[0] =~# '^#'
        call remove(s:MRU_files, 0)
      else
        let s:MRU_files = []
      endif
    else
      let s:MRU_files = []
    endif
  endfunction

  function! s:MRU_SaveList() "{{{3
    let l = []
    call add(l, '# Most recently used files list')
    call extend(l, s:MRU_files)
    call writefile(l, s:MRU_File)
  endfunction
  function! s:MRU_AddList(buf) "{{{3
    if s:mru_list_locked
      return
    endif

    let fname = fnamemodify(bufname(a:buf + 0), ':p')
    if fname == ''
      return
    endif

    if &buftype != ''
      return
    endif

    if index(s:MRU_files, fname) == -1
      if !filereadable(fname)
        return
      endif
    endif

    call s:MRU_LoadList()
    call filter(s:MRU_files, 'v:val !=# fname')
    call insert(s:MRU_files, fname, 0)

    "let s:MRU_Max_Entries = 100
    "if len(s:MRU_files) > s:MRU_Max_Entries
    " call remove(s:MRU_files, s:MRU_Max_Entries, -1)
    "endif

    call s:MRU_SaveList()

    let bname = '__MRU_Files__'
    let winnum = bufwinnr(bname)
    if winnum != -1
      let cur_winnr = winnr()
      call s:MRU_Create_Window()
      if winnr() != cur_winnr
        exe cur_winnr . 'wincmd w'
      endif
    endif
  endfunction

  function! s:MRU_RemoveList() "{{{3
    call s:MRU_LoadList()
    let lnum = line('.')
    call remove(s:MRU_files, line('.')-1)
    call s:MRU_SaveList()
    close
    call s:MRU_Create_Window()
    call cursor(lnum, 1)
  endfunction

  function! s:MRU_Open_File() range "{{{3
    for f in getline(a:firstline, a:lastline)
      if f == ''
        continue
      endif

      let file = substitute(f, '^.*| ','','')

      let winnum = bufwinnr('^' . file . '$')
      silent quit
      if winnum != -1
        return
      else
        if &filetype ==# 'mru'
          silent quit
        endif
      endif

      exe 'edit ' . fnameescape(substitute(file, '\\', '/', 'g'))
    endfor
  endfunction

  function! s:MRU_Create_Window() "{{{3
    if &filetype == 'mru' && bufname("%") ==# '__MRU_Files__'
      quit
      return
    endif

    call s:MRU_LoadList()
    if empty(s:MRU_files)
      echohl WarningMsg | echo 'MRU file list is empty' | echohl None
      return
    endif

    let bname = '__MRU_Files__'
    let winnum = bufwinnr(bname)
    if winnum != -1
      if winnr() != winnum
        exe winnum . 'wincmd w'
      endif

      setlocal modifiable
      " Delete the contents of the buffer to the black-hole register
      silent! %delete _
    else
      " If the __MRU_Files__ buffer exists, then reuse it. Otherwise open
      " a new buffer
      let bufnum = bufnr(bname)
      if bufnum == -1
        let wcmd = bname
      else
        let wcmd = '+buffer' . bufnum
      endif
      let wcmd = bufnum == -1 ? bname : '+buffer' . bufnum
      let s:MRU_Window_Height = &lines / 3
      exe 'silent! botright ' . s:MRU_Window_Height . 'split ' . wcmd
    endif

    " Mark the buffer as scratch
    setlocal buftype=nofile
    setlocal bufhidden=delete
    setlocal noswapfile
    setlocal nowrap
    setlocal nobuflisted
    setlocal filetype=mru
    setlocal winfixheight
    setlocal modifiable

    let old_cpoptions = &cpoptions
    set cpoptions&vim

    " Create mappings to select and edit a file from the MRU list
    nnoremap <buffer> <silent> <CR>   :call <SID>MRU_Open_File()<CR>
    vnoremap <buffer> <silent> <CR>   :call <SID>MRU_Open_File()<CR>
    nnoremap <buffer> <silent> <S-CR> :call <SID>MRU_Open_File_Tab()<CR>
    vnoremap <buffer> <silent> <S-CR> :call <SID>MRU_Open_File_Tab()<CR>
    nnoremap <buffer> <silent> K      :call <SID>MRU_RemoveList()<CR>
    nnoremap <buffer> <silent> S      :setlocal modifiable<CR>:sort<CR>:setlocal nomodifiable<CR>

    " Restore the previous cpoptions settings
    let &cpoptions = old_cpoptions

    let output = copy(s:MRU_files)
    let idx = 0
    for file in output
      if !filereadable(file)
        call remove(output, idx)
        continue
      endif
      let idx += 1
    endfor

    silent! 0put =output

    " Delete the empty line at the end of the buffer
    silent! $delete _
    let glist = getline(1, '$')
    let max = 0
    let max_h = 0
    for idx in range(0, len(glist)-1)
      if strlen(fnamemodify(glist[idx], ':t')) > max
        let max = strlen(fnamemodify(glist[idx], ':t'))
      endif
      if strlen(substitute(fnamemodify(glist[idx], ':p:h'), '^.*\/', '', '')) > max_h
        let max_h = strlen(substitute(fnamemodify(glist[idx], ':p:h'), '^.*\/', '', ''))
      endif
    endfor
    for idx in range(0, len(glist)-1)
      let glist[idx] = printf("%-" . max .  "s | %-" . max_h . "s | %s" ,
            \ fnamemodify(glist[idx], ':t'), substitute(fnamemodify(glist[idx], ':p:h'), '^.*\/', '', ''), glist[idx])
    endfor
    silent! %delete _
    call setline(1, glist)
    if glist[idx] == '| '
      silent! $delete _
    endif

    exe 'syntax match Directory display ' . '"'. '|\zs[^|]*\ze|'. '"'
    exe 'syntax match Constant  display ' . '"' . '[^|]*[\/]' . '"'

    " Move the cursor to the beginning of the file
    normal! gg

    setlocal nonumber cursorline nomodifiable
  endfunction

  " MRU Essentials {{{3
  let s:mru_list_locked = 0
  call s:MRU_LoadList()
  command! MRU call s:MRU_Create_Window()
  augroup mru-files-vimrc
    autocmd!
    autocmd BufRead      * call s:MRU_AddList(expand('<abuf>'))
    autocmd BufNewFile   * call s:MRU_AddList(expand('<abuf>'))
    autocmd BufWritePost * call s:MRU_AddList(expand('<abuf>'))
    autocmd QuickFixCmdPre  *grep* let s:mru_list_locked = 1
    autocmd QuickFixCmdPost *grep* let s:mru_list_locked = 0
  augroup END
endif

" Add execute permission {{{2
if s:vimrc_add_execute_perm == s:true
  if executable('chmod')
    augroup auto-add-executable
      autocmd!
      autocmd BufWritePost * call <SID>add_permission_x()
    augroup END

    function! s:add_permission_x()
      let file = expand('%:p')
      if !executable(file)
        if getline(1) =~# '^#!'
              \ || &filetype =~ "\\(z\\|c\\|ba\\)\\?sh$"
              \ && input(printf('"%s" is not perm 755. Change mode? [y/N] ', expand('%:t'))) =~? '^y\%[es]$'
          call system("chmod 755 " . shellescape(file))
          redraw | echo "Set permission 755!"
        endif
      endif
    endfunction
  endif
endif

" Restore cursor position {{{2
if s:vimrc_restore_cursor_position == s:true
  function! s:restore_cursor_postion()
    if line("'\"") <= line("$")
      normal! g`"
      return 1
    endif
  endfunction
  augroup restore-cursor-position
    autocmd!
    autocmd BufWinEnter * call <SID>restore_cursor_postion()
  augroup END
endif

" Restore the buffer that has been deleted {{{2
let s:bufqueue = []
augroup buffer-queue-restore
  autocmd!
  autocmd BufDelete * call <SID>buf_enqueue(expand('#'))
augroup END

" Automatically get buffer list {{{2
if !s:has_plugin('vim-buftabs')
  augroup bufenter-get-buffer-list
    autocmd!
    " Escape getting buflist by "@% != ''" when "VimEnter"
    autocmd BufEnter,BufAdd,BufWinEnter * if @% != '' | call <SID>get_buflists() | endif
  augroup END
endif

" Automatically cd parent directory when opening the file {{{2
function! s:cd_file_parentdir()
  execute ":lcd " . expand("%:p:h")
endfunction
command! Cdcd call <SID>cd_file_parentdir()
nnoremap Q :<C-u>call <SID>cd_file_parentdir()<CR>

if s:vimrc_auto_cd_file_parentdir == s:true
  augroup cd-file-parentdir
    autocmd!
    autocmd BufRead,BufEnter * call <SID>cd_file_parentdir()
  augroup END
endif

" QuickLook for mac {{{2
if s:is_mac && executable("qlmanage")
  command! -nargs=? -complete=file QuickLook call s:quicklook(<f-args>)
  function! s:quicklook(...)
    let file = a:0 ? expand(a:1) : expand('%:p')
    if !s:has(file)
      echo printf('%s: No such file or directory', file)
      return 0
    endif
    call system(printf('qlmanage -p %s >& /dev/null', shellescape(file)))
  endfunction
endif

" Backup automatically {{{2
if s:is_windows
  set nobackup
else
  set backup
  call s:mkdir('~/.vim/backup')
  augroup backup-files-automatically
    autocmd!
    autocmd BufWritePre * call s:backup_files()
  augroup END

  function! s:backup_files()
    let dir = strftime("~/.backup/vim/%Y/%m/%d", localtime())
    if !isdirectory(dir)
      call system("mkdir -p " . dir)
      call system("chown goth:staff " . dir)
    endif
    execute "set backupdir=" . dir
    execute "set backupext=." . strftime("%H_%M_%S", localtime())
  endfunction
endif

" Swap settings {{{2
call s:mkdir('~/.vim/swap')
set noswapfile
set directory=~/.vim/swap

" Vimp: a development tool for vim plugins {{{2
let s:vimp = {
      \   "list": [
      \     "vim-dirvish",
      \     "vim-shellutils",
      \     "vim-surround",
      \     "yankround.vim",
      \     "neocomplete.vim",
      \   ],
      \   "dir":  ".vimp",
      \   "file": "Vimpfile",
      \ }

function! s:vimp.parse()
  let list = []

  for dep in readfile(self.file)
    if isdirectory(dep)
      let res = fnamemodify(dep, ":p")
      call add(list, res)
    else
      let res = finddir(fnamemodify(dep, ":t"), expand("$HOME/.vim/bundle"))
      if !empty(res)
        call add(list, res)
      endif
    endif
  endfor

  return list
endfunction

function! s:vimp.symbolic()
  if !isdirectory(self.dir)
    call s:mkdir(self.dir)
  endif

  for link in s:vimp.parse()
    call system(printf("ln -snf %s %s", link, self.dir))
  endfor
endfunction

function! s:vimp.gen()
  call writefile(self.list, self.file)
endfunction

function! s:vimp.start()
  if !filereadable(self.file)
    call s:vimp.gen()
  endif
  call s:vimp.symbolic()
  echo readfile(self.file)
  call s:echomsg("WarningMsg", "Restart Vim")
endfunction

function! s:vimp.end()
  try
    call s:rm(self.file)
    call s:rm(self.dir)
  catch
  finally
    call s:echomsg("WarningMsg", "Restart Vim")
  endtry
endfunction

command! VimpStart  call s:vimp.start()
command! VimpEnd    call s:vimp.end()

function! HomedirOrBackslash() "{{{2
  if getcmdtype() == ':' && (getcmdline() =~# '^e ' || getcmdline() =~? '^r\?!' || getcmdline() =~? '^cd ')
    return '~/'
  else
    return '\'
  endif
endfunction

function! GetDate() "{{{2
  return strftime("%Y/%m/%d %H:%M")
endfunction

function! GetDocumentPosition() "{{{2
  return float2nr(str2float(line('.')) / str2float(line('$')) * 100) . "%"
endfunction

function! GetTildaPath(tail) "{{{2
  return a:tail ? expand('%:h:~') : expand('%:~')
endfunction

function! GetCharacterCode() "{{{2
  let str = iconv(matchstr(getline('.'), '.', col('.') - 1), &enc, &fenc)
  let out = '0x'
  for i in range(strlen(str))
    let out .= printf('%02X', char2nr(str[i]))
  endfor
  if str ==# ''
    let out .= '00'
  endif
  return out
endfunction

function! GetFileSize() "{{{2
  let size = &encoding ==# &fileencoding || &fileencoding ==# ''
        \        ? line2byte(line('$') + 1) - 1 : getfsize(expand('%'))

  if size < 0
    let size = 0
  endif
  for unit in ['B', 'KB', 'MB']
    if size < 1024
      return size . unit
    endif
    let size = size / 1024
  endfor
  return size . 'GB'
endfunction

function! GetBufname(bufnr, ...) "{{{2
  let bufname = bufname(a:bufnr)
  if bufname =~# '^[[:alnum:].+-]\+:\\\\'
    let bufname = substitute(bufname, '\\', '/', 'g')
  endif
  let buftype = getbufvar(a:bufnr, '&buftype')
  if bufname ==# ''
    if buftype ==# ''
      return '[No Name]'
    elseif buftype ==# 'quickfix'
      return '[Quickfix List]'
    elseif buftype ==# 'nofile' || buftype ==# 'acwrite'
      return '[Scratch]'
    endif
  endif
  if buftype ==# 'nofile' || buftype ==# 'acwrite'
    return bufname
  endif
  if a:0 && a:1 ==# 't'
    return fnamemodify(bufname, ':t')
  elseif a:0 && a:1 ==# 'f'
    return (fnamemodify(bufname, ':~:p'))
  elseif a:0 && a:1 ==# 's'
    return pathshorten(fnamemodify(bufname, ':~:h')).'/'.fnamemodify(bufname, ':t')
  endif
  return bufname
endfunction

function! GetFileInfo() "{{{2
  let line  = ''
  if bufname(bufnr("%")) == ''
    let line .= 'No name'
  else
    let line .= '"'
    let line .= expand('%:p:~')
    let line .= ' (' . line('.') . '/' . line('$') . ') '
    "let line .= '--' . 100 * line('.') / line('$') . '%--'
    let line .= GetDocumentPosition()
    let line .= '"'
  endif
  return line
endfunction

function! GetHighlight(hi) "{{{2
  redir => hl
  silent execute 'highlight ' . a:hi
  redir END
  return substitute(hl, '.*xxx ', '', '')
endfunction

function! Scouter(file, ...) "{{{2
  " Measure fighting power of Vim!
  " :echo len(readfile($MYVIMRC))
  let pat = '^\s*$\|^\s*"'
  let lines = readfile(a:file)
  if !a:0 || !a:1
    let lines = split(substitute(join(lines, "\n"), '\n\s*\\', '', 'g'), "\n")
  endif
  return len(filter(lines,'v:val !~ pat'))
endfunction

function! WordCount(...) "{{{2
  if a:0 == 0
    if exists("s:WordCountStr")
      return s:WordCountStr
    endif
    return
  endif
  let cidx = 3
  silent! let cidx = s:WordCountDict[a:1]
  let s:WordCountStr = ''
  let s:saved_status = v:statusmsg
  exec "silent normal! g\<c-g>"
  if v:statusmsg !~ '^--'
    let str = ''
    silent! let str = split(v:statusmsg, ';')[cidx]
    let cur = str2nr(matchstr(str, '\d\+'))
    let end = str2nr(matchstr(str, '\d\+\s*$'))
    if a:1 == 'char'
      let cr = &ff == 'dos' ? 2 : 1
      let cur -= cr * (line('.') - 1)
      let end -= cr * line('$')
    endif
    let s:WordCountStr = printf('%d/%d', cur, end)
    let s:WordCountStr = printf('%d', end)
  endif
  let v:statusmsg = s:saved_status
  return s:WordCountStr
endfunction

function! TrailingSpaceWarning() "{{{2
  if !exists("b:trailing_space_warning")
    if search('\s\+$', 'nw') != 0
      let b:trailing_space_warning = '[SPC:' . search('\s\+$', 'nw') . ']'
    else
      let b:trailing_space_warning = ''
    endif
  endif
  return b:trailing_space_warning
endfunction
" Recalculate the trailing whitespace warning when idle, and after saving
autocmd CursorHold,BufWritePost * unlet! b:trailing_space_warning

" Appearance: {{{1
" In this section, interface of Vim, that is, colorscheme, statusline and
" tabpages line is set.
"==============================================================================

" Essentials
syntax enable
syntax on

set number
if hostname() =~# '^z1z1r07'
  "setlocal columns=160
  "setlocal lines=60
else
  "setlocal columns=160
  "setlocal lines=50
endif

" Colorscheme
"set background=dark "{{{2
set background=dark
if !has('gui_running')
  set background=dark
endif
set t_Co=256
if &t_Co < 256
  colorscheme default
else
  if has('gui_running') && !s:is_windows
    " For MacVim, only
    if s:has_plugin('solarized.vim')
      try
        colorscheme solarized-cui
      catch
        colorscheme solarized
      endtry
    endif
  else
    " Vim for CUI
    " if s:has_plugin('solarized.vim') && $TERM_PROGRAM ==# "Apple_Terminal"
    "   try
    "     colorscheme solarized-cui
    "   catch
    "     colorscheme solarized
    "   endtry
    if s:has_plugin('jellybeans.vim')
      colorscheme jellybeans
    elseif s:has_plugin('vim-hybrid')
      colorscheme hybrid
    else
      if s:is_windows
        colorscheme default
      else
        colorscheme desert
      endif
    endif
  endif
endif

" Tabpages {{{2
set showtabline=2
set tabline=%!MakeTabLine()

function! s:tabpage_label(n) "{{{3
  let n = a:n
  let bufnrs = tabpagebuflist(n)
  let curbufnr = bufnrs[tabpagewinnr(n) - 1]

  let hi = n == tabpagenr() ? 'TabLineSel' : 'TabLine'

  let label = ''
  let no = len(bufnrs)
  if no == 1
    let no = ''
  endif
  let mod = len(filter(bufnrs, 'getbufvar(v:val, "&modified")')) ? '+' : ''
  let sp = (no . mod) ==# '' ? '' : ' '
  let fname = GetBufname(curbufnr, 's')

  if no !=# ''
    let label .= '%#' . hi . 'Number#' . no
  endif
  let label .= '%#' . hi . '#'
  let label .= fname . sp . mod

  return '%' . a:n . 'T' . label . '%T%#TabLineFill#'
endfunction
function! MakeTabLine() "{{{3
  let titles = map(range(1, tabpagenr('$')), 's:tabpage_label(v:val)')
  let sep = ' | '
  let tabs = join(titles, sep) . sep . '%#TabLineFill#%T'

  "hi TabLineFill ctermfg=white
  let info = '%#TabLineFill#'
  let info .= fnamemodify(getcwd(), ':~') . ' '
  return tabs . '%=' . info
endfunction
function! GuiTabLabel() "{{{3
  let label = ''
  let bufnrlist = tabpagebuflist(v:lnum)

  " Append the tab number
  "let label .= v:lnum.': '
  " Append the buffer name
  let name = bufname(bufnrlist[tabpagewinnr(v:lnum) - 1])
  if name == ''
    " give a name to no-name documents
    if &buftype=='quickfix'
      let name = '[Quickfix List]'
    else
      let name = '[No Name]'
    endif
  else
    " get only the file name
    let name = fnamemodify(name,":t")
  endif
  "let label .= name
  let label .= GetBufname('%', 't')
  " Append the number of windows in the tab page
  let wincount = tabpagewinnr(v:lnum, '$')

  " Add '+' if one of the buffers in the tab page is modified
  for bufnr in l:bufnrlist
    if getbufvar(bufnr, "&modified")
      let l:label .= ' +'
      break
    endif
  endfor
  let wincount = wincount == 1 ? '' : wincount . ' '

  ""return label . '  [' . wincount . ']'
  return wincount . label
endfunction "}}}

" StatusLine {{{2
set laststatus=2

highlight BlackWhite ctermfg=black ctermbg=white cterm=none guifg=black guibg=white gui=none
highlight WhiteBlack ctermfg=white ctermbg=black cterm=none guifg=white guibg=black gui=none

function! MakeStatusLine()
  let line = ''
  "let line .= '%#BlackWhite#'
  let line .= '[%n] '
  let line .= '%f'
  let line .= ' %m'
  let line .= '%<'
  "let line .= '%#StatusLine#'

  let line .= '%='
  let line .= '%#BlackWhite#'
  let line .= '%y'
  let line .= "[%{(&fenc!=#''?&fenc:&enc).(&bomb?'(BOM)':'')}:"
  let line .= "%{&ff.(&bin?'(BIN'.(&eol?'':'-noeol').')':'')}]"
  let line .= '%r'
  let line .= '%h'
  let line .= '%w'
  let line .= ' %l/%LL %2vC'
  let line .= ' %3p%%'

  if s:vimrc_statusline_manually == s:true
    return line
  else
    return ''
  endif
endfunction

function! MakeBigStatusLine()
  if s:vimrc_statusline_manually == s:true
    set statusline=
    set statusline+=%#BlackWhite#
    set statusline+=[%n]:
    if filereadable(expand('%'))
      set statusline+=%{GetBufname(bufnr('%'),'s')}
    else
      set statusline+=%F
    endif
    set statusline+=\ %m
    set statusline+=%#StatusLine#

    set statusline+=%=
    set statusline+=%#BlackWhite#
    if exists('*TrailingSpaceWarning')
      "set statusline+=%{TrailingSpaceWarning()}
    endif
    set statusline+=%y%{'['.(&fenc!=''?&fenc:&enc).':'.&ff.']'}
    set statusline+=%r
    set statusline+=%h
    set statusline+=%w
    if exists('*GetFileSize')
      set statusline+=[%{GetFileSize()}]
    endif
    if exists('*GetCharacterCode')
      set statusline+=[%{GetCharacterCode()}]
    endif
    set statusline+=\ %4l/%4LL,%3cC\ %3p%%
    if exists('*WordCount')
      set statusline+=\ [WC=%{WordCount()}]
    endif
    if exists('*GetDate')
      set statusline+=\ (%{GetDate()})
    endif
  endif
endfunction

if !s:has_plugin('lightline.vim')
  call MakeBigStatusLine()
  if s:vimrc_statusline_manually == s:true
    " Refresh Manually StatusLine
    augroup automatically-statusline
      autocmd!
      autocmd BufEnter * call MakeBigStatusLine()
    augroup END
  endif

  augroup minimal-statusline
    autocmd!
    autocmd WinEnter,CursorMoved * if winwidth(0) <  &columns | set statusline=%!MakeStatusLine() | endif
  augroup END
endif


" Emphasize statusline in the insert mode {{{2
if s:vimrc_colorize_statusline_insert == s:true
  if !s:has_plugin('lightline.vim')
    augroup colorize-statusline-insert
      autocmd!
      autocmd InsertEnter * call s:colorize_statusline_insert('Enter')
      autocmd InsertLeave * call s:colorize_statusline_insert('Leave')
    augroup END

    function! ReverseHighlight(hi)
      let hl = a:hi
      let hl = substitute(hl, 'fg', 'swp', 'g')
      let hl = substitute(hl, 'bg', 'fg',  'g')
      let hl = substitute(hl, 'swp', 'bg', 'g')
      return hl
    endfunction

    let s:hi_insert = 'highlight StatusLine ' . ReverseHighlight(GetHighlight('ModeMsg'))
    let s:slhlcmd = ''

    function! s:colorize_statusline_insert(mode)
      if a:mode == 'Enter'
        let s:slhlcmd = 'highlight StatusLine ' . GetHighlight('StatusLine')
        silent execute s:hi_insert

      elseif a:mode == 'Leave'
        highlight clear StatusLine
        silent execute s:slhlcmd
      endif

    endfunction
  endif
endif

" Cursor line/column {{{2
set cursorline
augroup auto-cursorcolumn-appear
  autocmd!
  autocmd CursorMoved,CursorMovedI * call s:auto_cursorcolumn('CursorMoved')
  autocmd CursorHold,CursorHoldI   * call s:auto_cursorcolumn('CursorHold')
  autocmd BufEnter * call s:auto_cursorcolumn('WinEnter')
  autocmd BufLeave * call s:auto_cursorcolumn('WinLeave')

  let s:cursorcolumn_lock = 0
  function! s:auto_cursorcolumn(event)
    if a:event ==# 'WinEnter'
      setlocal cursorcolumn
      let s:cursorcolumn_lock = 2
    elseif a:event ==# 'WinLeave'
      setlocal nocursorcolumn
    elseif a:event ==# 'CursorMoved'
      setlocal nocursorcolumn
      if s:cursorcolumn_lock
        if 1 < s:cursorcolumn_lock
          let s:cursorcolumn_lock = 1
        else
          setlocal nocursorcolumn
          let s:cursorcolumn_lock = 0
        endif
      endif
    elseif a:event ==# 'CursorHold'
      setlocal cursorcolumn
      let s:cursorcolumn_lock = 1
    endif
  endfunction
augroup END

augroup multi-window-toggle-cursor "{{{2
  autocmd!
  autocmd WinEnter * setlocal cursorline
  autocmd WinLeave * setlocal nocursorline nocursorcolumn
augroup END 

augroup cursor-highlight-emphasis "{{{2
  autocmd!
  autocmd CursorMoved,CursorMovedI,WinLeave * hi! link CursorLine CursorLine | hi! link CursorColumn CursorColumn
  autocmd CursorHold,CursorHoldI            * hi! link CursorLine Visual     | hi! link CursorColumn Visual
augroup END

" GUI IME Cursor colors {{{2
if has('multi_byte_ime') || has('xim')
  highlight Cursor guibg=NONE guifg=Yellow
  highlight CursorIM guibg=NONE guifg=Red
  set iminsert=0 imsearch=0
  if has('xim') && has('GUI_GTK')
    ""set imactivatekey=s-space
  endif
  inoremap <silent> <ESC><ESC>:set iminsert=0<CR>
endif

" ZEN-KAKU
" Display zenkaku-space {{{2
augroup hilight-idegraphic-space
  autocmd!
  "autocmd VimEnter,ColorScheme * highlight IdeographicSpace term=underline ctermbg=DarkGreen guibg=DarkGreen
  "autocmd WinEnter * match IdeographicSpace /　/
  autocmd VimEnter,ColorScheme * call <SID>hl_trailing_spaces()
  autocmd VimEnter,ColorScheme * call <SID>hl_zenkaku_space()
augroup END

function! s:hl_trailing_spaces()
  highlight! link TrailingSpaces Error
  syntax match TrailingSpaces containedin=ALL /\s\+$/
endfunction

function! s:hl_zenkaku_space()
  highlight! link ZenkakuSpace Error
  syntax match ZenkakuSpace containedin=ALL /　/
endfunction

" Options: {{{1
" Set options (boolean, number, string). General vim behavior.
" For more information about options, see :help 'option-list'.
"==============================================================================

set pumheight=10

" Don't redraw while executing macros
set lazyredraw

" Fast terminal connection
set ttyfast

" Enable the mode line
set modeline

" The length of the mode line
set modelines=5

" Vim internal help with the command K
set keywordprg=:help

" Language help
set helplang& helplang=ja

" Ignore case
set ignorecase

" Smart ignore case
set smartcase

" Enable the incremental search
set incsearch

" Emphasize the search pattern
set hlsearch

" Have Vim automatically reload changed files on disk. Very useful when using
" git and switching between branches
set autoread

" Automatically write buffers to file when current window switches to another
" buffer as a result of :next, :make, etc. See :h autowrite.
set autowrite

" Behavior when you switch buffers
set switchbuf=useopen,usetab,newtab

" Moves the cursor to the same column when cursor move
set nostartofline

" Use tabs instead of spaces
"set noexpandtab
set expandtab

" When starting a new line, indent in automatic
set autoindent

" The function of the backspace
set backspace=indent,eol,start

" When the search is finished, search again from the BOF
set wrapscan

" Emphasize the matching parenthesis
set showmatch

" Blink on matching brackets
set matchtime=1

" Increase the corresponding pairs
set matchpairs& matchpairs+=<:>

" Extend the command line completion
set wildmenu

" Wildmenu mode
set wildmode=longest,full

" Ignore compiled files
set wildignore&
set wildignore=.git,.hg,.svn
set wildignore+=*.jpg,*.jpeg,*.bmp,*.gif,*.png
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest,*.so,*.out,*.class
set wildignore+=*.swp,*.swo,*.swn
set wildignore+=*.DS_Store

" Show line and column number
set ruler
set rulerformat=%m%r%=%l/%L

" 1 tab == 4 spaces
set shiftwidth=4
set tabstop=4

" String to put at the start of lines that have been wrapped.
let &showbreak = '+++ '

" Always display a status line
set laststatus=2

" Set command window height to reduce number of 'Press ENTER...' prompts
set cmdheight=2

" Show current mode (insert, visual, normal, etc.)
set showmode

" Show last command in status line
set showcmd

" Lets vim set the title of the console
set notitle

" When you create a new line, perform advanced automatic indentation
set smartindent

" Blank is inserted only the number of 'shiftwidth'.
set smarttab

" Moving the cursor left and right will be modern.
set whichwrap=b,s,h,l,<,>,[,]

" Hide buffers instead of unloading them
set hidden

" The maximum width of the input text
set textwidth=0

set formatoptions&
set formatoptions-=t
set formatoptions-=c
set formatoptions-=r
set formatoptions-=o
set formatoptions-=v
set formatoptions+=l

" Identifying problems and bringing them to the foreground
set list
set listchars=tab:>-,trail:-,nbsp:%,extends:>,precedes:<,eol:<
set listchars=eol:<,tab:>.

" Increase or decrease items
set nrformats=alpha,hex

" Do not use alt key on Win
set winaltkeys=no

" Do not use visualbell
set novisualbell
set vb t_vb=

" Automatically equal size when opening
set noequalalways

" History size
set history=10000
set wrap

"set helpheight=999
set mousehide
set virtualedit=block
set virtualedit& virtualedit+=block

" Make it normal in UTF-8 in Unix.
set encoding=utf-8

" Select newline character (either or both of CR and LF depending on system) automatically
" Default fileformat.
set fileformat=unix
" Automatic recognition of a new line cord.
set fileformats=unix,dos,mac
" A fullwidth character is displayed in vim properly.
if exists('&ambiwidth')
  set ambiwidth=double
endif

set fileencodings=iso-2022-jp,cp932,sjis,euc-jp,utf-8

set foldenable
"set foldmethod=marker
"set foldopen=all
"set foldclose=all
set foldlevel=0
"set foldnestmax=2
set foldcolumn=2

" IM settings
" IM off when starting up
set iminsert=0 imsearch=0
" Use IM always
"set noimdisable
" Disable IM on cmdline
set noimcmdline

" Change some neccesary settings for win
if s:is_windows
  set shellslash "Exchange path separator
endif

if has('persistent_undo')
  set undofile
  let &undodir = $DOTVIM . '/undo'
  call s:mkdir(&undodir)
endif

" Use clipboard
if has('clipboard')
  set clipboard=unnamed
endif

if has('patch-7.4.338')
  set breakindent
endif

" GUI options {{{2
" No menubar
set guioptions-=m
" No frame
set guioptions-=C
set guioptions-=T
" No right scroolbar
set guioptions-=r
set guioptions-=R
" No left scroolbar
set guioptions-=l
set guioptions-=L
" No under scroolbar
set guioptions-=b

" Commands: {{{1
" User defined commands section.
"==============================================================================

" Some utilities. {{{2

" Source file
command! -nargs=? Source call <SID>load_source(empty(<q-args>) ? expand('%:p') : <q-args>)

" Measure fighting strength of Vim.
command! -bar -bang -nargs=? -complete=file Scouter echo Scouter(empty(<q-args>) ? $MYVIMRC : expand(<q-args>), <bang>0)

" Ls like a shell-ls
command! -nargs=? -bang -complete=file Ls call s:ls(<q-args>, <q-bang>)

" Show all runtimepaths.
command! -bar RTP echo substitute(&runtimepath, ',', "\n", 'g')

" Make random string such as password
command! -nargs=? RandomString call s:random_string(<q-args>)

" View all mappings
command! -nargs=* -complete=mapping AllMaps map <args> | map! <args> | lmap <args>

" Handle buffers {{{2
" Wipeout all buffers
command! -nargs=0 AllBwipeout call s:all_buffers_bwipeout()

" Get buffer queue list for restore
command! -nargs=0 BufQueue echo len(s:bufqueue)
      \ ? reverse(split(substitute(join(s:bufqueue, ' '), $HOME, '~', 'g')))
      \ : "No buffers in 's:bufqueue'."

" Get buffer list like ':ls'
command! -nargs=0 BufList call s:get_buflists()

" Smart bnext/bprev
command! Bnext call s:smart_bchange('n')
command! Bprev call s:smart_bchange('p')

" Show buffer kind.
command! -bar EchoBufKind setlocal bufhidden? buftype? swapfile? buflisted?

" Open new buffer or scratch buffer with bang.
command! -bang -nargs=? -complete=file BufNew call <SID>bufnew(<q-args>, <q-bang>)

" Bwipeout(!) for all-purpose.
command! -nargs=0 -bang Bwipeout call <SID>smart_bwipeout(0, <q-bang>)

" Delete the current buffer and the file.
command! -bang -nargs=0 -complete=buffer Delete call s:buf_delete(<bang>0)

" Handle tabpages {{{2
" Make tabpages
command! -nargs=? TabNew call s:tabnew(<q-args>)

"Open again with tabpages
command! -nargs=? Tab call s:tabdrop(<q-args>)

" Open the buffer again with tabpages
command! -nargs=? -complete=buffer ROT call <SID>recycle_open('tabedit', empty(<q-args>) ? expand('#') : expand(<q-args>))

" Handle files {{{2
" Open a file.
command! -nargs=? -complete=file Open call <SID>open(<q-args>)
command! -nargs=0                Op   call <SID>open('.')

" Get current file path
command! CopyCurrentPath call s:copy_current_path()

" Get current directory path
command! CopyCurrentDir call s:copy_current_path(1)

command! CopyPath CopyCurrentPath

" Remove EOL ^M
command! RemoveCr call s:smart_execute('silent! %substitute/\r$//g | nohlsearch')

" Remove EOL space
command! RemoveEolSpace call s:smart_execute('silent! %substitute/ \+$//g | nohlsearch')

" Remove blank line
command! RemoveBlankLine silent! global/^$/delete | nohlsearch | normal! ``

" Rename the current editing file
command! -nargs=? -complete=file Rename call s:rename(<q-args>, 'file')

" Change the current editing file extention
if v:version >= 730
  command! -nargs=? -complete=filetype ReExt  call s:rename(<q-args>, 'ext')
else
  command! -nargs=?                    ReExt  call s:rename(<q-args>, 'ext')
endif

" Make the notitle file called 'Junk'.
command! -nargs=0 JunkFile call s:make_junkfile()

" Handle encodings  {{{2
" In particular effective when I am garbled in a terminal
command! -bang -bar -complete=file -nargs=? Utf8      edit<bang> ++enc=utf-8 <args>
command! -bang -bar -complete=file -nargs=? Iso2022jp edit<bang> ++enc=iso-2022-jp <args>
command! -bang -bar -complete=file -nargs=? Cp932     edit<bang> ++enc=cp932 <args>
command! -bang -bar -complete=file -nargs=? Euc       edit<bang> ++enc=euc-jp <args>
command! -bang -bar -complete=file -nargs=? Utf16     edit<bang> ++enc=ucs-2le <args>
command! -bang -bar -complete=file -nargs=? Utf16be   edit<bang> ++enc=ucs-2 <args>
command! -bang -bar -complete=file -nargs=? Jis       Iso2022jp<bang> <args>
command! -bang -bar -complete=file -nargs=? Sjis      Cp932<bang> <args>
command! -bang -bar -complete=file -nargs=? Unicode   Utf16<bang> <args>

" Tried to make a file note version
" Don't save it because dangerous.
command! WUtf8      setlocal fenc=utf-8
command! WIso2022jp setlocal fenc=iso-2022-jp
command! WCp932     setlocal fenc=cp932
command! WEuc       setlocal fenc=euc-jp
command! WUtf16     setlocal fenc=ucs-2le
command! WUtf16be   setlocal fenc=ucs-2
command! WJis       WIso2022jp
command! WSjis      WCp932
command! WUnicode   WUtf16

" Appoint a line feed
command! -bang -complete=file -nargs=? WUnix write<bang> ++fileformat=unix <args> | edit <args>
command! -bang -complete=file -nargs=? WDos  write<bang> ++fileformat=dos <args>  | edit <args>
command! -bang -complete=file -nargs=? WMac  write<bang> ++fileformat=mac <args>  | edit <args>

" Mappings: {{{1
" Mapping section.
"==============================================================================

" Essentials {{{2
" It is likely to be changed by $VIM/vimrc
if has('vim_starting')
  mapclear
  mapclear!
endif

" Use backslash
if s:is_mac
  noremap ¥ \
  noremap \ ¥
endif

" Define mapleader
let mapleader = ','
let maplocalleader = ','

" Smart space mapping
" Notice: when starting other <Space> mappings in noremap, disappeared [Space]
nmap  <Space>   [Space]
xmap  <Space>   [Space]
nnoremap  [Space]   <Nop>
xnoremap  [Space]   <Nop>

" Function's commands {{{2
" MRU within the vimrc
if !s:has_plugin('mru.vim') 
  "if exists(':MRU2')
  if exists('*s:MRU_Create_Window')
    nnoremap <silent> [Space]j :<C-u>call <SID>MRU_Create_Window()<CR>
    "nnoremap <silent> [Space]j :<C-u>MRU<CR>
  endif
endif

" Smart folding close
nnoremap <silent> <C-_> :<C-u>call <SID>smart_foldcloser()<CR>

" Kill buffer
if s:has_plugin('vim-buftabs')
  nnoremap <silent> <C-x>k     :<C-u>call <SID>smart_bwipeout(0)<CR>
  nnoremap <silent> <C-x>K     :<C-u>call <SID>smart_bwipeout(1)<CR>
  nnoremap <silent> <C-x><C-k> :<C-u>call <SID>smart_bwipeout(2)<CR>
else
  "autocmd BufUnload,BufLeave,BufDelete,BufWipeout * call <SID>get_buflists()

  nnoremap <silent> <C-x>k     :<C-u>call <SID>smart_bwipeout(0)<CR>
  nnoremap <silent> <C-x>K     :<C-u>call <SID>smart_bwipeout(1)<CR>
  nnoremap <silent> <C-x><C-k> :<C-u>call <SID>smart_bwipeout(2)<CR>
  "nnoremap <silent> <C-x>k     :<C-u>silent call <SID>smart_bwipeout(0)<CR>:<C-u>call <SID>get_buflists()<CR>
  "nnoremap <silent> <C-x>K     :<C-u>silent call <SID>smart_bwipeout(1)<CR>:<C-u>call <SID>get_buflists()<CR>
  "nnoremap <silent> <C-x><C-k> :<C-u>silent call <SID>smart_bwipeout(2)<CR>:<C-u>call <SID>get_buflists()<CR>
endif

" Restore buffers
nnoremap <silent> <C-x>u :<C-u>call <SID>buf_restore()<CR>

" Delete buffers
"nnoremap <silent> <C-x>d     :call <SID>buf_delete('')<CR>
"nnoremap <silent> <C-x><C-d> :call <SID>buf_delete(1)<CR>
nnoremap <silent> <C-x>d     :Delete<CR>
nnoremap <silent> <C-x><C-d> :Delete!<CR>

" Tabpages mappings
nnoremap <silent> <C-t>L  :<C-u>call <SID>move_tabpage("right")<CR>
nnoremap <silent> <C-t>H  :<C-u>call <SID>move_tabpage("left")<CR>
nnoremap <silent> <C-t>dh :<C-u>call <SID>close_all_left_tabpages()<CR>
nnoremap <silent> <C-t>dl :<C-u>call <SID>close_all_right_tabpages()<CR>

" Move cursor between beginning of line and end of line
"nnoremap <silent><Tab>   :<C-u>call <SID>move_left_center_right()<CR>
"nnoremap <silent><S-Tab> :<C-u>call <SID>move_left_center_right(1)<CR>

" Open vimrc with tab
nnoremap <silent> [Space]. :call <SID>recycle_open('edit', $MYVIMRC)<CR>

" Make junkfile
nnoremap <silent> [Space]e  :<C-u>call <SID>make_junkfile()<CR>

" Easy typing tilda insted of backslash
cnoremap <expr> <Bslash> HomedirOrBackslash()

" Swap semicolon for colon {{{2
nnoremap ; :
vnoremap ; :
nnoremap q; q:
vnoremap q; q:
nnoremap : ;
vnoremap : ;

" Make less complex to escaping {{{2
inoremap jj <ESC>
cnoremap <expr> j getcmdline()[getcmdpos()-2] ==# 'j' ? "\<BS>\<C-c>" : 'j'
vnoremap <C-j><C-j> <ESC>
onoremap jj <ESC>
inoremap j[Space] j
onoremap j[Space] j

" Swap jk for gjgk {{{2
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk
nnoremap gj j
nnoremap gk k
vnoremap gj j
vnoremap gk k

if s:vimrc_goback_to_eof2bof == s:true
  function! s:up(key)
    if line(".") == 1
      return ":call cursor(line('$'), col('.'))\<CR>"
    else
      return a:key
    endif
  endfunction 
  function! s:down(key)
    if line(".") == line("$")
      return ":call cursor(1, col('.'))\<CR>"
    else
      return a:key
    endif
  endfunction
  nnoremap <expr><silent> k <SID>up("gk")
  nnoremap <expr><silent> j <SID>down("gj")
endif

" Buffers, windows, and tabpages {{{2
"nnoremap <silent> <C-j> :<C-u>call <SID>get_buflists('n')<CR>
"nnoremap <silent> <C-k> :<C-u>call <SID>get_buflists('p')<CR>
if s:has_plugin('vim-buftabs')
  nnoremap <silent> <C-j> :<C-u>silent bnext<CR>
  nnoremap <silent> <C-k> :<C-u>silent bprev<CR>
else
  nnoremap <silent> <C-j> :<C-u>silent bnext<CR>:<C-u>call <SID>get_buflists()<CR>
  nnoremap <silent> <C-k> :<C-u>silent bprev<CR>:<C-u>call <SID>get_buflists()<CR>
endif

" Windows
nnoremap s <Nop>
nnoremap sp :<C-u>split<CR>
nnoremap vs :<C-u>vsplit<CR>

function! s:vsplit_or_wincmdw()
  if winnr('$') == 1
    return ":vsplit\<CR>"
  else
    return ":wincmd w\<CR>"
  endif
endfunction
nnoremap <expr><silent> ss <SID>vsplit_or_wincmdw()
nnoremap sj <C-w>j
nnoremap sk <C-w>k
nnoremap sl <C-w>l
nnoremap sh <C-w>h

" tabpages
"nnoremap <silent> <C-l> :<C-u>silent! tabnext<CR>
"nnoremap <silent> <C-h> :<C-u>silent! tabprev<CR>
nnoremap <silent> <C-l> :<C-u>call <SID>win_tab_switcher('l')<CR>
nnoremap <silent> <C-h> :<C-u>call <SID>win_tab_switcher('h')<CR>
nnoremap t <Nop>
nnoremap <silent> [Space]t :<C-u>tabclose<CR>:<C-u>tabnew<CR>
nnoremap <silent> tt :<C-u>tabnew<CR>
nnoremap <silent> tT :<C-u>tabnew<CR>:<C-u>tabprev<CR>
nnoremap <silent> tc :<C-u>tabclose<CR>
nnoremap <silent> to :<C-u>tabonly<CR>

" Inser matching bracket automatically {{{2
if s:has_plugin("lexima.vim")
  inoremap [ []<LEFT>
  inoremap ( ()<LEFT>
  inoremap " ""<LEFT>
  inoremap ' ''<LEFT>
  inoremap ` ``<LEFT>
endif

" Make cursor-moving useful {{{2
inoremap <C-h> <Backspace>
inoremap <C-d> <Delete>

cnoremap <C-k> <UP>
cnoremap <C-j> <DOWN>
cnoremap <C-l> <RIGHT>
cnoremap <C-h> <LEFT>
cnoremap <C-d> <DELETE>
cnoremap <C-p> <UP>
cnoremap <C-n> <DOWN>
cnoremap <C-f> <RIGHT>
cnoremap <C-b> <LEFT>
cnoremap <C-a> <HOME>
cnoremap <C-e> <END>
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
cnoremap <C-b> <Left>
cnoremap <C-f> <Right>
cnoremap <C-d> <Del>
cnoremap <C-h> <BS>

"nnoremap + <C-a>
"nnoremap - <C-x>

" Nop features {{{2
nnoremap q: <Nop>
nnoremap q/ <Nop>
nnoremap q? <Nop>
nnoremap <Up> <Nop>
nnoremap <Down> <Nop>
nnoremap <Left> <Nop>
nnoremap <Right> <Nop>
nnoremap ZZ <Nop>
nnoremap ZQ <Nop>

" Folding (see :h usr_28.txt){{{2
"nnoremap <expr>l foldclosed('.') != -1 ? 'zo' : 'l'
"nnoremap <expr>h col('.') == 1 && foldlevel(line('.')) > 0 ? 'zc' : 'h'
nnoremap <silent>z0 :<C-u>set foldlevel=<C-r>=foldlevel('.')<CR><CR>

" Misc mappings {{{2
cnoreabbrev w!! w !sudo tee > /dev/null %
" CursorLine
nnoremap <silent> <Leader>l :<C-u>call <SID>toggle_option('cursorline')<CR>

nnoremap <silent> <Leader>c :<C-u>call <SID>toggle_option('cursorcolumn')<CR>

" Add a relative number toggle
nnoremap <silent> <Leader>r :<C-u>call <SID>toggle_option('relativenumber')<CR>

" Add a spell check toggle
nnoremap <silent> <Leader>s :<C-u>call <SID>toggle_option('spell')<CR>

" Tabs Increase
nnoremap <silent> ~ :let &tabstop = (&tabstop * 2 > 16) ? 2 : &tabstop * 2<CR>:echo 'tabstop:' &tabstop<CR>

" Toggle top/center/bottom
noremap <expr> zz (winline() == (winheight(0)+1)/ 2) ?  'zt' : (winline() == 1)? 'zb' : 'zz'

" Reset highlight searching
nnoremap <silent> <ESC><ESC> :nohlsearch<CR>

" key map ^,$ to <Space>h,l. Because ^ and $ is difficult to type and damage little finger!!!
noremap [Space]h ^
noremap [Space]l $

" Type 'v', select end of line in visual mode
vnoremap v $h

" Make Y behave like other capitals
nnoremap Y y$

" Do 'zz' after next candidates for search words
nnoremap n nzz
nnoremap N Nzz

" Search word under cursor
nnoremap S *zz
nnoremap * *zz
nnoremap # #zz
nnoremap g* g*zz
nnoremap g# g#zz

" View file information
nnoremap <C-g> 1<C-g>

" Write only when the buffer has been modified
nnoremap <silent><CR> :<C-u>silent update<CR>

" Goto file under cursor
noremap gf gF
noremap gF gf

" Jump a next blank line
nnoremap <silent>W :<C-u>keepjumps normal! }<CR>
nnoremap <silent>B :<C-u>keepjumps normal! {<CR>

" Save word and exchange it under cursor
nnoremap <silent> ciy ciw<C-r>0<ESC>:let@/=@1<CR>:noh<CR>
nnoremap <silent> cy   ce<C-r>0<ESC>:let@/=@1<CR>:noh<CR>

" Yank the entire file
nnoremap <Leader>y :<C-u>%y<CR>
nnoremap <Leader>Y :<C-u>%y<CR>

" Emacs-kile keybindings in insert mode {{{2
inoremap <C-a> <Home>
inoremap <C-e> <End>
inoremap <C-h> <BS>
inoremap <C-d> <Del>
inoremap <C-f> <Right>
inoremap <C-b> <Left>
inoremap <C-n> <Up>
inoremap <C-p> <Down>
inoremap <C-m> <CR>

" Plugin: {{{1

function! s:neobundled(bundle)
  return s:bundled(a:bundle) && neobundle#tap(a:bundle)
endfunction

" template
" username/vim-plugin {\{{2
" if s:neobundled('vim-plugin')
"   call neobundle#config({
"         \   "lazy" : 1,
"         \   "depends" : [
"         \     "username1/vim-plugin1",
"         \     "username2/vim-plugin2",
"         \   ],
"         \   "autoload" : {
"         \     "commands" : [ "Cmd" ],
"         \   }
"         \ })
" 
"   " Options
"   let g:config_variable = 1
"   " Commands
"   command! MyCmd call s:mycmd()
"   " Mappings
"   nnoremap
" 
"   call neobundle#untap()
" endif

" b4b4r07/mru.vim {{{2
if s:neobundled('mru.vim')
  call neobundle#config({
        \   "lazy" : 0,
        \   "autoload" : {
        \     "commands" : [ "MRU", "Mru" ],
        \   }
        \ })

  " Options
  let MRU_Use_Alt_useopen = 1
  let MRU_Window_Height   = &lines / 2
  let MRU_Max_Entries     = 100
  let MRU_Use_CursorLine  = 1
  " Commands
  " Mappings
  nnoremap <silent> [Space]j :<C-u>MRU<CR>
  vnoremap <silent> [Space]j <Esc><Esc>:<C-u>MRU<CR>

  call neobundle#untap()
endif

" b4b4r07/vim-sunset {{{2
if s:neobundled('vim-sunset')
  call neobundle#config({
        \   "gui" : 1
        \ })

  " Options
  let g:sunset_latitude = 35.67
  let g:sunset_longitude = 139.8
  let g:sunset_utc_offset = 9
  " Commands
  " Mappings

  call neobundle#untap()
endif

" b4b4r07/vim-ezoe {{{2
if s:neobundled('vim-ezoe')
  call neobundle#config({
        \   "lazy" : 1,
        \   "depends" : [ "mattn/webapi-vim" ],
        \   "autoload" : {
        \     "commands" : [ "Ezoe" ],
        \   }
        \ })

  " Options
  " Commands
  " Mappings

  call neobundle#untap()
endif

" b4b4r07/vim-buftabs {{{2
if s:neobundled('vim-buftabs')
  call neobundle#config({
        \   "lazy" : 1,
        \   "autoload" : {
        \     "commands" : [
        \       "BuftabsEnable",
        \       "BuftabsDisable",
        \       "BuftabsToggle",
        \     ],
        \   }
        \ })

  " Options
  let g:buftabs_in_statusline   = 1
  let g:buftabs_in_cmdline      = 0
  let g:buftabs_only_basename   = 1
  let g:buftabs_marker_start    = "["
  let g:buftabs_marker_end      = "]"
  let g:buftabs_separator       = "#"
  let g:buftabs_marker_modified = "+"
  let g:buftabs_active_highlight_group = "Visual"
  let g:buftabs_statusline_highlight_group = 'BlackWhite'
  " Commands
  " Mappings

  call neobundle#untap()
endif

" b4b4r07/vim-autocdls {{{2
if s:neobundled('vim-autocdls')

  " Options
  let g:autocdls_autols_enabled = 1
  let g:autocdls_set_cmdheight = 2
  let g:autocdls_show_filecounter = 1
  let g:autocdls_show_pwd = 0
  let g:autocdls_alter_letter = 1
  let g:autocdls_newline_disp = 0
  let g:autocdls_ls_highlight = 1
  let g:autocdls_lsgrep_ignorecase = 1
  " Commands
  " Mappings

  call neobundle#untap()
endif

" b4b4r07/vim-shellutils {{{2
if s:neobundled('vim-shellutils')
  "call neobundle#config({
  "      \   "lazy" : 1,
  "      \   "autoload" : {
  "      \     "commands" : [
  "      \       "Ls",
  "      \       "File",
  "      \       "Rm",
  "      \       "Mkdir",
  "      \       "Rm",
  "      \       "Touch",
  "      \       "Cat",
  "      \       "Head",
  "      \       "Touch",
  "      \       "Tail",
  "      \       "Cp",
  "      \       "Mv",
  "      \     ],
  "      \   }
  "      \ })

  " Options
  let g:shellutils_disable_commands = ['Ls']
  " Commands
  " Mappings

  call neobundle#untap()
endif

" b4b4r07/vim-pt {{{2
if s:neobundled('vim-pt')
  call neobundle#config({
        \   "lazy" : 1,
        \   "external_commands" : [ "pt" ],
        \   "autoload" : {
        \     "commands" : [
        \       "Pt",
        \       "PtBuffer",
        \     ],
        \   }
        \ })

  call neobundle#untap()
  " Options
  let g:pt_highlight = 1
  " Commands
  " Mappings

  call neobundle#untap()
endif

" mattn/excitetranslate-vim {{{2
if s:neobundled('excitetranslate-vim')
  xnoremap E :ExciteTranslate<CR>
endif

" mattn/webapi-vim {{{2
if s:neobundled('webapi-vim')
  call neobundle#config({
        \   "lazy" : 1,
        \   "function_prefix" : "webapi"
        \ })

  call neobundle#untap()
endif

" mattn/benchvimrc-vim {{{2
if s:neobundled('benchvimrc-vim')
  call neobundle#config({
        \   "lazy" : 1,
        \   "autoload" : {
        \     "commands" : [ "BenchVimrc" ],
        \   }
        \ })

  " Options
  " Commands
  " Mappings

  call neobundle#untap()
endif

" mattn/gist-vim {{{2
if s:neobundled('gist-vim')
  call neobundle#config({
        \   "lazy" : 1,
        \   "autoload" : {
        \     "commands" : [ "Gist" ],
        \   }
        \ })

  " Options
  let g:github_user = 'b4b4r07'
  let g:github_token = ''
  let g:gist_curl_options = "-k"
  let g:gist_detect_filetype = 1
  " Commands
  " Mappings

  call neobundle#untap()
endif

" mattn/emmet-vim {{{2
if s:neobundled('emmet-vim')
  call neobundle#config({
        \   "lazy" : 1,
        \   "autoload" : {
        \     "filetypes" : [
        \       "html",
        \       "xhttml",
        \       "css",
        \       "sass",
        \       "scss",
        \       "styl",
        \       "xml",
        \       "xls",
        \       "markdown",
        \       "htmldjango",
        \     ],
        \   }
        \ })

  " Options
  " Commands
  " Mappings

  call neobundle#untap()
endif

" Shougo/vimproc "{{{2
if s:neobundled('vimproc')
  call neobundle#config({
        \  'build' : {
        \    'windows' : 'make -f make_mingw32.mak',
        \    'cygwin'  : 'make -f make_cygwin.mak',
        \    'mac'     : 'make -f make_mac.mak',
        \    'unix'    : 'make -f make_unix.mak',
        \   },
        \ })

  " Options
  " Commands
  " Mappings

  call neobundle#untap()
endif

" Shougo/vimfiler {{{2
if s:neobundled('vimfiler')
  call neobundle#config({
        \   "lazy" : 1,
        \   "depends" : [],
        \   "autoload" : {
        \     "commands" : [
        \       {
        \         'name' : 'VimFiler',
        \         'complete' : 'customlist,vimfiler#complete',
        \       },
        \       {
        \         'name' : 'VimFilerTab',
        \         'complete' : 'customlist,vimfiler#complete',
        \       },
        \       {
        \         'name' : 'VimFilerBufferDir',
        \         'complete' : 'customlist,vimfiler#complete',
        \       },
        \       {
        \         'name' : 'VimFilerExplorer',
        \         'complete' : 'customlist,vimfiler#complete',
        \       },
        \       {
        \         'name' : 'Edit',
        \         'complete' : 'customlist,vimfiler#complete',
        \       },
        \       {
        \         'name' : 'Write',
        \         'complete' : 'customlist,vimfiler#complete',
        \       },
        \       'Read',
        \       'Source',
        \     ],
        \     'mappings' : '<Plug>(vimfiler_',
        \     'explorer' : 1,
        \   }
        \ })

  " Options
  let g:vimfiler_edit_action = 'tabopen'
  let g:vimfiler_enable_clipboard = 0
  let g:vimfiler_safe_mode_by_default = 0
  let g:vimfiler_as_default_explorer = 1
  let g:vimfiler_detect_drives = s:is_windows ? [
        \ 'C:/', 'D:/', 'E:/', 'F:/', 'G:/', 'H:/', 'I:/',
        \ 'J:/', 'K:/', 'L:/', 'M:/', 'N:/'] :
        \ split(glob('/mnt/*'), '\n') + split(glob('/media/*'), '\n') +
        \ split(glob('/Users/*'), '\n')

  " %p : full path
  " %d : current directory
  " %f : filename
  " %F : filename removed extensions
  " %* : filenames
  " %# : filenames fullpath
  let g:vimfiler_sendto = {
        \ 'unzip' : 'unzip %f',
        \ 'zip' : 'zip -r %F.zip %*',
        \ 'Inkscape' : 'inkspace',
        \ 'GIMP' : 'gimp %*',
        \ 'gedit' : 'gedit',
        \ }

  if s:is_windows
    " Use trashbox.
    let g:unite_kind_file_use_trashbox = 1
  else
    " Like Textmate icons.
    "let g:vimfiler_tree_leaf_icon = ' '
    "let g:vimfiler_tree_opened_icon = '▾'
    "let g:vimfiler_tree_closed_icon = '▸'
    "let g:vimfiler_file_icon = '-'
    "let g:vimfiler_readonly_file_icon = '✗'
    "let g:vimfiler_marked_file_icon = '✓'
  endif
  " let g:vimfiler_readonly_file_icon = '[O]'
  " Commands
  command! V VimFiler -tab -double
  " Mappings
  nnoremap <silent> [Space]v :<C-u>VimFiler -tab -double<CR>

  " Other {{{
  let g:vimfiler_no_default_key_mappings = 1
  augroup vimfiler-mappings
    au!
    au FileType vimfiler nmap <buffer> a <Plug>(vimfiler_choose_action)
    au FileType vimfiler nmap <buffer> b <Plug>(vimfiler_open_file_in_another_vimfiler)
    au FileType vimfiler nmap <buffer> B <Plug>(vimfiler_edit_binary_file)
    au FileType vimfiler nmap <buffer><nowait> c <Plug>(vimfiler_mark_current_line)<Plug>(vimfiler_copy_file)y<CR>
    au FileType vimfiler nmap <buffer> dd <Plug>(vimfiler_mark_current_line)<Plug>(vimfiler_delete_file)y<CR>
    au FileType vimfiler nmap <buffer> ee <Plug>(vimfiler_edit_file)
    au FileType vimfiler nmap <buffer> er <Plug>(vimfiler_edit_binary_file)
    au FileType vimfiler nmap <buffer> E <Plug>(vimfiler_new_file)
    au FileType vimfiler nmap <buffer> ge <Plug>(vimfiler_execute_external_filer)
    au FileType vimfiler nmap <buffer> gr <Plug>(vimfiler_grep)
    au FileType vimfiler nmap <buffer> gf <Plug>(vimfiler_find)
    au FileType vimfiler nmap <buffer> gc <Plug>(vimfiler_cd_vim_current_dir)
    au FileType vimfiler nmap <buffer> gs <Plug>(vimfiler_toggle_safe_mode)
    au FileType vimfiler nmap <buffer> gS <Plug>(vimfiler_toggle_simple_mode)
    au FileType vimfiler nmap <buffer> gg <Plug>(vimfiler_cursor_top)
    au FileType vimfiler nmap <buffer> g<C-g> <Plug>(vimfiler_toggle_maximize_window)
    au FileType vimfiler nmap <buffer> h <Plug>(vimfiler_smart_h)
    au FileType vimfiler nmap <buffer> H <Plug>(vimfiler_popup_shell)
    au FileType vimfiler nmap <buffer> i <Plug>(vimfiler_switch_to_another_vimfiler)
    au FileType vimfiler nmap <buffer> j <Plug>(vimfiler_loop_cursor_down)
    au FileType vimfiler nmap <buffer> k <Plug>(vimfiler_loop_cursor_up)
    au FileType vimfiler nmap <buffer> K <Plug>(vimfiler_make_directory)
    au FileType vimfiler nmap <buffer> l <Plug>(vimfiler_smart_l)
    au FileType vimfiler nmap <buffer> L <Plug>(vimfiler_switch_to_drive)
    au FileType vimfiler nmap <buffer> I <Plug>(vimfiler_cd_input_directory)
    au FileType vimfiler nmap <buffer><nowait> m <Plug>(vimfiler_mark_current_line)<Plug>(vimfiler_move_file)y<CR>
    au FileType vimfiler nmap <buffer> M <Plug>(vimfiler_set_current_mask)
    au FileType vimfiler nmap <buffer> o <Plug>(vimfiler_sync_with_current_vimfiler)
    au FileType vimfiler nmap <buffer> O <Plug>(vimfiler_open_file_in_another_vimfiler)
    "au FileType vimfiler nmap <buffer> O <Plug>(vimfiler_sync_with_another_vimfiler)
    au FileType vimfiler nmap <buffer> p <Plug>(vimfiler_quick_look)
    au FileType vimfiler nmap <buffer> P <Plug>(vimfiler_popd)
    au FileType vimfiler nmap <buffer> q <Plug>(vimfiler_close)
    au FileType vimfiler nmap <buffer> Q <Plug>(vimfiler_exit)
    au FileType vimfiler nmap <buffer> r <Plug>(vimfiler_rename_file)
    au FileType vimfiler nmap <buffer> S <Plug>(vimfiler_select_sort_type)
    au FileType vimfiler nmap <buffer> t <Plug>(vimfiler_expand_tree)
    au FileType vimfiler nmap <buffer> T <Plug>(vimfiler_expand_tree_recursive)
    au FileType vimfiler nmap <buffer> vv <Plug>(vimfiler_toggle_mark_all_lines)
    au FileType vimfiler nmap <buffer> vu <Plug>(vimfiler_clear_mark_all_lines)
    au FileType vimfiler nmap <buffer> vi <Plug>(vimfiler_preview_file)
    au FileType vimfiler nmap <buffer> x <Plug>(vimfiler_execute_system_associated)
    au FileType vimfiler nmap <buffer> yy <Plug>(vimfiler_yank_full_path)
    au FileType vimfiler nmap <buffer> Y <Plug>(vimfiler_pushd)
    au FileType vimfiler nmap <buffer> zc <Plug>(vimfiler_copy_file)
    au FileType vimfiler nmap <buffer> zm <Plug>(vimfiler_move_file)
    au FileType vimfiler nmap <buffer> zd <Plug>(vimfiler_delete_file)
    "au FileType vimfiler nmap <buffer> <C-l> <Plug>(vimfiler_redraw_screen)
    au FileType vimfiler nnoremap <silent><buffer><expr>es   vimfiler#do_action('split')
    au FileType vimfiler nmap <buffer> <RightMouse> <Plug>(vimfiler_execute_external_filer)
    au FileType vimfiler nmap <buffer> <C-CR> <Plug>(vimfiler_execute_external_filer)
    au FileType vimfiler nmap <buffer> <C-g><C-g> <Plug>(vimfiler_print_filename)
    au FileType vimfiler nmap <buffer> <C-v> <Plug>(vimfiler_switch_vim_buffer_mode)
    au FileType vimfiler nmap <buffer> <C-i> <Plug>(vimfiler_switch_to_other_window)
    "au FileType vimfiler nmap <buffer> <CR> <Plug>(vimfiler_execute)
    au FileType vimfiler nmap <buffer> <CR> <Plug>(vimfiler_quick_look)
    au FileType vimfiler nmap <buffer> <S-CR> <Plug>(vimfiler_execute_system_associated)
    au FileType vimfiler nmap <buffer> <2-LeftMouse> <Plug>(vimfiler_execute_system_associated)
    au FileType vimfiler nmap <buffer> <BS> <Plug>(vimfiler_switch_to_parent_directory)
    "au FileType vimfiler nmap <buffer> <C-h> <Plug>(vimfiler_switch_to_history_directory)
    au FileType vimfiler nmap <buffer> <Space> <Plug>(vimfiler_toggle_mark_current_line)
    au FileType vimfiler nmap <buffer> ~ <Plug>(vimfiler_switch_to_home_directory)
    au FileType vimfiler nmap <buffer> \ <Plug>(vimfiler_switch_to_root_directory)
    au FileType vimfiler nmap <buffer> . <Plug>(vimfiler_toggle_visible_dot_files)
    au FileType vimfiler nmap <buffer> ! <Plug>(vimfiler_execute_shell_command)
    au FileType vimfiler nmap <buffer> ? <Plug>(vimfiler_help)
    au FileType vimfiler nmap <buffer> ` <Plug>(vimfiler_toggle_mark_current_line_up)
    au FileType vimfiler vmap <buffer> @ <Plug>(vimfiler_toggle_mark_selected_lines)
    au FileType vimfiler nmap <buffer> @ <Plug>(vimfiler_toggle_mark_current_line)
  augroup END

  let g:vimfiler_quick_look_command =
        \ s:is_windows ? 'maComfort.exe -ql' :
        \ s:is_mac ? 'qlmanage -p' : 'gloobus-preview'
  "autocmd FileType vimfiler call s:vimfiler_my_settings()

  function! s:vimfiler_my_settings()
    call vimfiler#set_execute_file('vim', ['vim', 'notepad'])
    call vimfiler#set_execute_file('txt', 'vim')

    " Overwrite settings.
    nnoremap <silent><buffer> J
          \ <C-u>:Unite -buffer-name=files -default-action=lcd directory_mru<CR>
    " Call sendto.
    " nnoremap <buffer> - <C-u>:Unite sendto<CR>
    " setlocal cursorline


    " Migemo search.
    if !empty(unite#get_filters('matcher_migemo'))
      nnoremap <silent><buffer><expr> /  line('$') > 10000 ?  'g/' :
            \ ":\<C-u>Unite -buffer-name=search -start-insert line_migemo\<CR>"
    endif
    nunmap <buffer><C-l>
  endfunction
  "let g:vimfiler_as_default_explorer = 1
  "let g:vimfiler_safe_mode_by_default = 0
  "" Edit file by tabedit.
  "let g:vimfiler_edit_action = 'edit'
  "" Like Textmate icons.
  "let g:vimfiler_tree_leaf_icon = ' '
  "let g:vimfiler_tree_opened_icon = '▾'
  "let g:vimfiler_tree_closed_icon = '▸'
  "let g:vimfiler_file_icon = '-'
  "let g:vimfiler_marked_file_icon = '*'
  "nmap <F2>  :VimFiler -split -horizontal -project -toggle -quit<CR>
  "autocmd FileType vimfiler nnoremap <buffer><silent>/  :<C-u>Unite file -default-action=vimfiler<CR>
  "autocmd FileType vimfiler nnoremap <silent><buffer> e :call <SID>vimfiler_tree_edit('open')<CR>
  "" Windows.
  "" let g:vimfiler_quick_look_command = 'maComfort.exe -ql'
  "" Linux.
  "" let g:vimfiler_quick_look_command = 'gloobus-preview'
  "" Mac OS X.
  "let g:vimfiler_quick_look_command = 'qlmanage -p'
  "autocmd FileType vimfiler nnoremap <buffer> q <Plug>(vimfiler_quick_look)<CR>
  "autocmd FileType vimfiler nmap <buffer> q <Plug>(vimfiler_quick_look)<CR>
  "}}}

  call neobundle#untap()
endif

" Shougo/unite.vim {{{2
if s:neobundled('unite.vim')
  call neobundle#config({
        \   "lazy" : 1,
        \   "depends" : [ "Shougo/vimproc" ],
        \   "autoload" : {
        \     "commands" : [ "Unite" ],
        \   }
        \ })

  " Options
  let g:unite_winwidth                   = 40
  let g:unite_source_file_mru_limit      = 300
  let g:unite_enable_start_insert        = 0            "off is zero
  let g:unite_enable_split_vertically    = 0
  let g:unite_source_history_yank_enable = 1            "enable history/yank
  let g:unite_source_file_mru_filename_format  = ''
  let g:unite_kind_jump_list_after_jump_scroll = 0
  let g:unite_split_rule = 'botright'
  " Commands
  "nnoremap <silent>[Space]j :Unite file_mru -direction=botright -toggle<CR>
  "nnoremap <silent>[Space]o :Unite outline  -direction=botright -toggle<CR>
  nnoremap <silent>[Space]o :Unite outline -vertical -winwidth=40 -toggle<CR>
  "nnoremap <silent>[Space]o :Unite outline -vertical -no-quit -winwidth=40 -toggle<CR>
  " Grep
  nnoremap <silent> ,g  :<C-u>Unite grep:. -buffer-name=search-buffer<CR>
  " Grep word on cursor
  nnoremap <silent> ,cg :<C-u>Unite grep:. -buffer-name=search-buffer<CR><C-R><C-W>
  " Re-call grep results
  nnoremap <silent> ,r  :<C-u>UniteResume search-buffer<CR>

  if executable('pt')
    let g:unite_source_grep_command = 'pt'
    let g:unite_source_grep_default_opts = '--nogroup --nocolor'
    let g:unite_source_grep_recursive_opt = ''
  elseif executable('ag')
    " Use ag in unite grep source.
    let g:unite_source_grep_command = 'ag'
    let g:unite_source_grep_default_opts =
          \ '--line-numbers --nocolor --nogroup --hidden --ignore ' .
          \  '''.hg'' --ignore ''.svn'' --ignore ''.git'' --ignore ''.bzr'''
    let g:unite_source_grep_recursive_opt = ''
  elseif executable('ack')
  elseif executable('jvgrep')
    " For jvgrep.
    let g:unite_source_grep_command = 'jvgrep'
    let g:unite_source_grep_default_opts = '--exclude ''\.(git|svn|hg|bzr)'''
    let g:unite_source_grep_recursive_opt = '-R'
  endif

  " Mappings

  call neobundle#untap()
endif

" Shougo/neocomplete {{{2
if s:neobundled('neocomplete.vim')
  call neobundle#config({
        \   "lazy" : 1,
        \   "autoload" : {
        \     "insert" : 1,
        \   },
        \   'disabled' : !has('lua'),
        \   'vim_version' : '7.3.885'
        \ })

  function! neobundle#tapped.hooks.on_source(bundle) "{{{
    " Use smartcase.
    let g:neocomplete#enable_smart_case = 1
    let g:neocomplete#enable_camel_case = 1
    let g:neocomplete#enable_underbar_completion = 1

    " Use fuzzy completion.
    let g:neocomplete#enable_fuzzy_completion = 1

    " Set minimum syntax keyword length.
    let g:neocomplete#sources#syntax#min_keyword_length = 3
    " Set auto completion length.
    let g:neocomplete#auto_completion_start_length = 2
    " Set manual completion length.
    let g:neocomplete#manual_completion_start_length = 0
    " Set minimum keyword length.
    let g:neocomplete#min_keyword_length = 3

    " Set neosnippet competion length.
    "call neocomplete#custom#source('neosnippet', 'min_pattern_length', 1)

    let g:neocomplete#disable_auto_select_buffer_name_pattern =
    \ '\[Command Line\]'

    if !exists('g:neocomplete#force_omni_input_patterns')
      let g:neocomplete#force_omni_input_patterns = {}
    endif
    let g:jedi#auto_vim_configuration = 0
    let g:neocomplete#sources#omni#input_patterns = {
    \ 'ruby' : '[^. *\t]\.\w*\|\h\w*::',
    \}
    let g:neocomplete#force_omni_input_patterns = {
    \ 'python': '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*'
    \}
    " \ 'ruby' : '[^. *\t]\.\|\h\w*::',
    let g:neocomplete#sources#dictionary#dictionaries = {
    \ 'default' : '',
    \ 'vimshell' : $HOME.'/.vimshell_hist',
    \ 'scala' : $HOME.'/.vim/myplugin/vim-scala-dict/dict/scala.dict',
    \ }
  endfunction
  "}}}

  " Options
  let g:neocomplete#enable_at_startup = 1
  let g:neocomplete#enable_ignore_case = 1
  let g:neocomplete#enable_smart_case = 1
  if !exists('g:neocomplete#keyword_patterns')
      let g:neocomplete#keyword_patterns = {}
  endif
  let g:neocomplete#keyword_patterns._ = '\h\w*'
  " Commands
  inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
  inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<S-TAB>"
  " Mappings

  " Other {{{
  "inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
  "inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<S-TAB>"
  "highlight Pmenu      ctermbg=lightcyan ctermfg=black
  "highlight PmenuSel   ctermbg=blue      ctermfg=black
  "highlight PmenuSbari ctermbg=darkgray
  "highlight PmenuThumb ctermbg=lightgray
  "}}}

  call neobundle#untap()
endif

" Shougo/neosnippet {{{2
if s:neobundled('neosnippet')
  call neobundle#config({
        \   'lazy' : 1,
        \   'autoload' : {
        \     'insert' : 1,
        \     'filetypes' : 'neosnippet',
        \     'unite_sources' : [
        \       'snippet', 'neosnippet/user', 'neosnippet/runtime'
        \     ],
        \   }
        \ })

  function! neobundle#tapped.hooks.on_source(bundle) "{{{
    " For snippet_complete marker.
    if has('conceal')
      set conceallevel=2 concealcursor=i
    endif
    " Enable snipMate compatibility feature.
    let g:neosnippet#enable_snipmate_compatibility = 1
    " Remove snippets marker automatically

    "prioratise snippet
    "call neocomplete#custom#source('neosnippet', 'rank', 400)

    snoremap <Esc> <Esc>:NeoSnippetClearMarkers<CR>
  endfunction
  "}}}

  " Options
  " Enable snipMate compatibility feature.
  let g:neosnippet#enable_snipmate_compatibility = 1
  " Tell Neosnippet about the other snippets
  let g:neosnippet#snippets_directory='~/.vim/bundle/vim-snippets/snippets'
  "" Tell Neosnippet about the other snippets
  "if !exists("g:neosnippet#snippets_directory")
  "  let g:neosnippet#snippets_directory=""
  "endif
  ""let g:neosnippet#snippets_directory='~/.vim/bundle/snipmate-snippets/snippets, ~/.vim/mysnippets'
  "let g:neosnippet#snippets_directory='~/.vim/snippets, ~/.vim/bundle/vim-snippets/snippets'

  " Commands

  " Mappings
  imap <C-k> <Plug>(neosnippet_expand_or_jump)
  smap <C-k> <Plug>(neosnippet_expand_or_jump)
  xmap <C-k> <Plug>(neosnippet_expand_target)
  " SuperTab like snippets behavior.
  "imap <expr><TAB> neosnippet#expandable_or_jumpable() ?
  "      \ "\<Plug>(neosnippet_expand_or_jump)"
  "      \: pumvisible() ? "\<C-n>" : "\<TAB>"
  "smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
  "      \ "\<Plug>(neosnippet_expand_or_jump)"
  "      \: "\<TAB>"

  " For snippet_complete marker.
  if has('conceal')
    set conceallevel=2 concealcursor=i
  endif
endif

" Shougo/vimshell {{{2
if s:neobundled('vimshell')
  call neobundle#config({
        \  'lazy' : 1,
        \  'depends' : [ 'Shougo/vimproc' ],
        \  'autoload' : {
        \    'commands' : [
        \      {
        \        'name' : 'VimShell',
        \        'complete' : 'customlist,vimshell#complete'
        \      },
        \      {
        \        'name' : 'VimShellTab',
        \        'complete' : 'customlist,vimshell#complete'
        \      },
        \      {
        \        'name' : 'VimShellBufferDir',
        \        'complete' : 'customlist,vimshell#complete'
        \      },
        \      {
        \        'name' : 'VimShellCreate',
        \        'complete' : 'customlist,vimshell#complete'
        \      },
        \      'VimShellExecute',
        \      'VimShellInteractive',
        \      'VimShellTerminal',
        \      'VimShellPop'
        \    ],
        \  }
        \ })

  function! neobundle#tapped.hooks.on_source(bundle) "{{{
    " Use current directory as vimshell prompt.
    let g:vimshell_prompt_expr = 'escape(fnamemodify(getcwd(), ":~").">", "\\[]()?! ")." "'
    let g:vimshell_prompt_pattern = '^\%(\f\|\\.\)\+> '
    let g:vimshell_right_prompt = 'vcs#info("(%s)-[%b]%p", "(%s)-[%b|%a]%p")'
  endfunction
  "}}}

  " Options
  " Commands
  " Mappings

  " Other {{{
  augroup my-vimshell
    autocmd!
    autocmd FileType vimshell
          \ imap <expr> <buffer> <C-n> pumvisible() ? "\<C-n>" : "\<Plug>(vimshell_history_neocomplete)"
  augroup END
  "}}}

  call neobundle#untap()
endif

" Shougo/unite-help {{{2
if s:neobundled('unite-help')
  call neobundle#config({
        \   "lazy" : 1,
        \   "depends" : [ 'Shougo/vimproc' ],
        \   "autoload" : {
        \     "unite_sources" : "help"
        \   }
        \ })

  " Options
  " Commands
  " Mappings

  call neobundle#untap()
endif

" Shougo/neomru.vim {{{2
if s:neobundled('neomru.vim')
  call neobundle#config({
        \   "lazy" : 1,
        \   "autoload" : {
        \     "unite_sources" : "file_mru"
        \   }
        \ })

  " Options
  " Commands
  " Mappings

  call neobundle#untap()
endif

" Shougo/unite-outline {{{2
if s:neobundled('unite-outline')
  call neobundle#config({
        \   "lazy" : 1,
        \   "depends" : [ 'Shougo/unite.vim' ],
        \   "autoload" : {
        \     "unite_sources" : "outline"
        \   }
        \ })

  " Options
  " Commands
  " Mappings

  call neobundle#untap()
endif

" osyo-manga/vim-anzu {{{2
if s:neobundled('vim-anzu')
  call neobundle#config({
        \   "lazy" : 1,
        \   "autoload" : {
        \     "mappings" : [ '<Plug>(anzu-' ],
        \   }
        \ })

  " Options
  " Commands
  " Mappings
  nmap n <Plug>(anzu-n-with-echo)
  nmap N <Plug>(anzu-N-with-echo)
  nmap * <Plug>(anzu-star-with-echo)
  nmap # <Plug>(anzu-sharp-with-echo)
  "nmap n <Plug>(anzu-mode-n)
  "nmap N <Plug>(anzu-mode-N)

  call neobundle#untap()
endif

" thinca/vim-quickrun {{{2
if s:neobundled('vim-quickrun')
  " Options
  let g:quickrun_config = {}
  let g:quickrun_config.markdown = {
        \ 'outputter' : 'null',
        \ 'command'   : 'open',
        \ 'cmdopt'    : '-a',
        \ 'args'      : 'Marked',
        \ 'exec'      : '%c %o %a %s',
        \ }
  " Commands
  " Mappings
endif

" thinca/vim-splash {{{2
if s:neobundled('vim-splash')
  "call neobundle#config({
  "      \   "lazy" : 1,
  "      \   "autoload" : {
  "      \     "commands" : [ 'Splash' ],
  "      \     "functions" : [ 'splash#intro' ],
  "      \   }
  "      \ })

  "let g:loaded_splash = 1
  let s:vim_intro = $HOME . "/.vim/bundle/vim-splash/sample/intro"
  if !isdirectory(s:vim_intro)
    call mkdir(s:vim_intro, 'p')
    execute ":lcd " . s:vim_intro . "/.."
    call system('git clone https://gist.github.com/OrgaChem/7630711 intro')
  endif
  let g:splash#path = expand(s:vim_intro . '/vim_intro.txt')

  " Options
  " Commands
  " Mappings

  call neobundle#untap()
endif

" thinca/vim-portal {{{2
if s:neobundled('vim-portal')
  " Options
  " Commands
  " Mappings
  nmap <Leader>pb <Plug>(portal-gun-blue)
  nmap <Leader>po <Plug>(portal-gun-orange)
  nnoremap <Leader>pr :<C-u>PortalReset<CR>

  call neobundle#untap()
endif

" thinca/vim-poslist {{{2
if s:neobundled('vim-poslist')
  " Options
  " Commands
  " Mappings
  "map <C-o> <Plug>(poslist-prev-pos)
  "map <C-i> <Plug>(poslist-next-pos)

  call neobundle#untap()
endif

" thinca/vim-scouter {{{2
if s:neobundled('vim-scouter')
  call neobundle#config({
        \   'autoload' : {
        \     'commands' : [ 'Scouter' ],
        \   }
        \ })

  " Options
  " Commands
  " Mappings

  call neobundle#untap()
endif

" thinca/vim-qfreplace {{{2
if s:neobundled('vim-qfreplace')
  call neobundle#config({
        \   "lazy" : 1,
        \   "autoload" : {
        \     "filetypes" : [
        \       "unite",
        \       "quickfix",
        \     ],
        \   }
        \ })

  " Options
  " Commands
  " Mappings

  call neobundle#untap()
endif

" thinca/vim-ref {{{2
if s:neobundled('vim-ref')
  call neobundle#config({
        \   "lazy" : 1,
        \   "autoload" : {
        \     "commands" : [
        \       {
        \         'name' : 'Ref',
        \         'complete' : 'customlist,ref#complete',
        \       },
        \     ],
        \     'unite_sources' : [ 'ref' ],
        \   }
        \ })

  function! neobundle#tapped.hooks.on_source(bundle) "{{{
    let g:ref_jquery_doc_path = $HOME . '/.vim/.bundle/jqapi'
    let g:ref_javascript_doc_path = $HOME . '/.vim/.bundle/jsref/htdocs'
    let g:ref_wikipedia_lang = ['ja', 'en']
    let g:ref_use_cache = 1
    let g:ref_source_webdict_sites = {
    \   'je': {
    \     'url': 'http://eow.alc.co.jp/search?q=%s&ref=sa',
    \   },
    \   'ej': {
    \     'url': 'http://eow.alc.co.jp/search?q=%s&ref=sa',
    \   },
    \   'etm': {
    \     'url': 'http://home.alc.co.jp/db/owa/etm_sch?stg=1&instr=%s',
    \   },
    \   'wiki': {
    \     'url': 'http://ja.wikipedia.org/wiki/%s',
    \   },
    \ }
    let g:ref_alc_encoding = 'utf-8'
  endfunction
  "}}}

  " Options
  " Commands
  " Mappings

  call neobundle#untap()
endif

" tyru/skk.vim {{{2
if s:neobundled('skk.vim')
  set imdisable
  let skk_jisyo = '~/SKK_JISYO.L'
  let skk_large_jisyo = '~/SKK_JISYO.L'
  let skk_auto_save_jisyo = 1
  let skk_keep_state = 0
  let skk_egg_like_newline = 1
  let skk_show_annotation = 1
  let skk_use_face = 1
endif

" tyru/eskk.vim {{{2
if s:neobundled('eskk.vim')
  set imdisable
  let g:eskk#directory = '~/SKK_JISYO.L'
  let g:eskk#dictionary = { 
        \   'path': "~/SKK_JISYO.L",
        \   'sorted': 0,
        \   'encoding': 'utf-8',
        \ }
  let g:eskk#large_dictionary = {
        \   'path': "~/SKK_JISYO.L",
        \   'sorted': 1,
        \   'encoding': 'utf-8',
        \ }
  let g:eskk#enable_completion = 1
endif

" tyru/restart.vim {{{2
if s:neobundled('restart.vim')
  call neobundle#config({
        \   "lazy" : 1,
        \   "gui" : 1,
        \   "autoload" : {
        \     "commands" : [ "Restart" ],
        \   }
        \ })

  " Options
  " Commands
  if has('gui_running')
    let g:restart_sessionoptions
          \ = 'blank,buffers,curdir,folds,help,localoptions,tabpages'
    command!
          \   RestartWithSession
          \   -bar
          \   let g:restart_sessionoptions = 'blank,curdir,folds,help,localoptions,tabpages'
          \   | Restart
  endif

  " Mappings

  call neobundle#untap()
endif

" tyru/open-browser {{{2
if s:neobundled('open-browser.vim')
  call neobundle#config({
        \   "lazy" : 1,
        \   "autoload" : {
        \     "commands" : [
        \       "OpenBrowserSmartSearch",
        \       "OpenBrowser",
        \     ],
        \     "mappings" : [ "<Plug>(openbrowser-smart-search)" ],
        \   }
        \ })

  " Options
  " Commands
  " Mappings
  " If it looks like URI, open an URI under cursor.
  " Otherwise, search a word under cursor.
  nmap <Leader>o <Plug>(openbrowser-smart-search)
  " If it looks like URI, open selected URI.
  " Otherwise, search selected word.
  vmap <Leader>o <Plug>(openbrowser-smart-search)

  call neobundle#untap()
endif

" LeafCage/foldCC.vim {{{2
if s:neobundled('foldCC.vim')
  "set foldtext=foldCC#foldtext()
  let g:foldCCtext_head = 'v:folddashes. " "'
  let g:foldCCtext_tail = 'printf(" %s[%4d lines Lv%-2d]%s", v:folddashes, v:foldend-v:foldstart+1, v:foldlevel, v:folddashes)'
  let g:foldCCtext_enable_autofdc_adjuster = 1
endif

" LeafCage/yankround.vim {{{2
if s:neobundled('yankround.vim')
  call neobundle#config({
        \   "lazy" : 1,
        \   "autoload" : {
        \     "mappings" : [ "<Plug>(yankround-" ],
        \   }
        \ })

  function! neobundle#tapped.hooks.on_source(bundle)
    " let g:yankround_use_region_hl = 1
  endfunction

  " Options
  let g:yankround_max_history = 100
  " Commands
  " Mappings
  nmap P  <Plug>(yankround-P)
  nmap gP <Plug>(yankround-gP)
  nmap p  <Plug>(yankround-p)
  xmap gp <Plug>(yankround-gp)
  xmap p <Plug>(yankround-p)
  xmap p <Plug>(yankround-p)
  nmap <C-p> <Plug>(yankround-prev)
  nmap <C-n> <Plug>(yankround-next)

  if s:has_plugin('unite.vim')
    nnoremap [Space]p :Unite yankround -direction=botright -toggle<CR>
  endif

  call neobundle#untap()
endif

" cohama/agit.vim {{{
if s:neobundled('agit.vim')
  call neobundle#config({
  \ 'depends': ['tpope/vim-fugitive'],
  \ 'autoload': {
  \   'commands' : ['Agit']
  \   }
  \ })
  call neobundle#untap()
endif

" itchyny/lightline.vim {{{2
if s:neobundled('lightline.vim')
  let g:lightline = {
        \ 'colorscheme': 'solarized',
        \ 'mode_map': {'c': 'NORMAL'},
        \ 'active': {
        \   'left':  [ [ 'mode', 'paste' ], [ 'fugitive' ], [ 'filename' ] ],
        \   'right' : [ [ 'date' ], [ 'filetype', 'fileencoding', 'fileformat', 'lineinfo', 'percent' ], [ 'filepath' ] ],
        \ },
        \ 'component_function': {
        \   'modified': 'MyModified',
        \   'readonly': 'MyReadonly',
        \   'fugitive': 'MyFugitive',
        \   'filepath': 'MyFilepath',
        \   'filename': 'MyFilename',
        \   'fileformat': 'MyFileformat',
        \   'filetype': 'MyFiletype',
        \   'fileencoding': 'MyFileencoding',
        \   'mode': 'MyMode',
        \   'date': 'MyDate'
        \ }
        \ }

  function! MyDate()
    return strftime("%Y/%m/%d %H:%M")
  endfunction

  function! MyModified()
    return &ft =~ 'help\|vimfiler\|gundo' ? '' : &modified ? '+' : &modifiable ? '' : '-'
  endfunction

  function! MyReadonly()
    return &ft !~? 'help\|vimfiler\|gundo' && &readonly ? 'x' : ''
  endfunction

  function! MyFilepath()
    return substitute(getcwd(), $HOME, '~', '')
  endfunction

  function! MyFilename()
    return ('' != MyReadonly() ? MyReadonly() . ' ' : '') .
          \ (&ft == 'vimfiler' ? vimfiler#get_status_string() :
          \  &ft == 'unite' ? unite#get_status_string() :
          \  &ft == 'vimshell' ? vimshell#get_status_string() :
          \ '' != expand('%:p:~') ? expand('%:p:~') : '[No Name]') .
          \ ('' != MyModified() ? ' ' . MyModified() : '')
  endfunction

  function! MyFugitive()
    try
      if &ft !~? 'vimfiler\|gundo' && exists('*fugitive#head')
        return fugitive#head()
      endif
    catch
    endtry
    return ''
  endfunction

  function! MyFileformat()
    return winwidth(0) > 70 ? &fileformat : ''
  endfunction

  function! MyFiletype()
    return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype : 'NONE') : ''
  endfunction

  function! MyFileencoding()
    return winwidth(0) > 70 ? (strlen(&fenc) ? &fenc : &enc) : ''
  endfunction

  function! MyMode()
    return winwidth(0) > 60 ? lightline#mode() : ''
  endfunction

  call neobundle#untap()
endif

" itchyny/calendar.vim {{{2
if s:neobundled('calendar.vim')
  call neobundle#config({
        \   "lazy" : 1,
        \   "autoload" : {
        \     "command" : [ "Calendar" ],
        \   },
        \ })


  function! neobundle#tapped.hooks.on_source(bundle) "{{{
    let g:calendar_google_calendar = 1
    let g:calendar_google_task = 1
    let g:calendar_date_endian = 'big'
  endfunction
  "}}}

  call neobundle#untap()
endif
" scrooloose/nerdtree {{{2
if s:neobundled('nerdtree')
  call neobundle#config({
        \   "lazy" : 1,
        \   "autoload" : {
        \     "command" : [ "NERDTreeToggle" ],
        \   },
        \ })

  " Options
  let g:NERDTreeQuitOnOpen = 1
  let g:NERDTreeShowHidden = 1
  " Commands
  " Mappings
  nnoremap [Space]n :<C-u>NERDTreeToggle<CR>

  call neobundle#untap()
endif

" Yggdroot/indentLine {{{2
if s:neobundled('indentLine')
  " Options
  let g:indentLine_fileTypeExclude = ['', 'help', 'nerdtree', 'calendar', 'thumbnail', 'tweetvim']
  let g:indentLine_color_term = 111
  let g:indentLine_color_gui = '#708090'
  "let g:indentLine_char = '┆ ' "use ¦, ┆ or │
  " Commands
  " Mappings

  call neobundle#untap()
endif

" nathanaelkane/vim-indent-guides {{{2
if s:neobundled('vim-indent-guides')
  hi IndentGuidesOdd  ctermbg=DarkGreen
  hi IndentGuidesEven ctermbg=Black
  let g:indent_guides_enable_on_vim_startup = 0
  let g:indent_guides_start_level = 1
  let g:indent_guides_auto_colors = 0
  let g:indent_guides_guide_size = 1
endif

" sjl/gundo.vim {{{2
if s:neobundled('gundo.vim')
  call neobundle#config({
        \   "lazy" : 1,
        \   "autoload" : {
        \     "command" : [ "GundoToggle" ],
        \   },
        \ })

  " Options
  "nnoremap <Leader>g :<C-u>GundoToggle<CR>
  let g:gundo_auto_preview = 0
  " Commands
  " Mappings

  call neobundle#untap()
endif

" glidenote/memolist.vim {{{2
if s:neobundled('memolist.vim')
  call neobundle#config({
        \   "lazy" : 1,
        \   "autoload" : {
        \     "command" : [
        \       "MemoNew",
        \       "MemoGrep",
        \     ],
        \   },
        \ })


  "nnoremap <Leader>g :<C-u>GundoToggle<CR>
  let g:gundo_auto_preview = 0

  call neobundle#untap()
endif

" ujihisa/neco-look {{{2
if s:neobundled('neco-look')
  call neobundle#config({
        \   "lazy" : 1,
        \   "external_commands" : [ "look" ],
        \ })

  " Options
  " Commands
  " Mappings

  call neobundle#untap()
endif

" rking/ag.vim {{{2
if s:neobundled('ag.vim')
  call neobundle#config({
        \   "lazy" : 1,
        \   "external_commands" : [ "ag" ],
        \   "autoload" : {
        \     "commands" : [
        \       "Ag",
        \       "AgBuffer",
        \     ],
        \   }
        \ })

  " Options
  " Commands
  " Mappings

  call neobundle#untap()
endif

" justinmk/vim-dirvish {{{2
if s:neobundled('vim-dirvish')
  "call neobundle#config({
  "      \   "lazy" : 1,
  "      \   "autoload" : {
  "      \     "commands" : [ "Dirvish" ],
  "      \     "functions" : [ "dirvish#open()" ],
  "      \   }
  "      \ })

  " Options
  let g:dirvish_hijack_netrw = 1
  augroup my_dirvish_events
      au!
      " always show hidden files
      au User DirvishEnter let b:dirvish.showhidden = 1
  augroup END

  " Commands
  " Mappings
  nnoremap [Space]d :<C-u>call <SID>toggle_dirvish()<CR>
  function! s:toggle_dirvish()
    if &filetype == 'dirvish'
      if exists('b:dirvish')
        if winnr('$') > 1
          wincmd c
        else
          bdelete
        endif
      endif
    else
      execute 'Dirvish'
    endif
  endfunction

  call neobundle#untap()
endif

" ctrlpvim/ctrlp.vim {{{2
if s:neobundled('ctrlp.vim')
  "call neobundle#config({
  "\   'autoload' : {
  "\     'commands' : ['CtrlP'],
  "\     'function_prefix' : 'ctrlp',
  "\     'mappings' : '<c-q>'
  "\   }
  "\ })

  " Options
  let g:ctrlp_map = '<c-g>'
  let g:ctrlp_cmd = 'CtrlP'
  let g:ctrlp_user_command = 'find %s -type f'
  "let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
  let g:ctrlp_custom_ignore = {
    \ 'dir':  '\v[\/]\.(git|hg|svn)$',
    \ 'file': '\v\.(exe|so|dll)$',
    \ 'link': 'some_bad_symbolic_links',
    \ }
  " Commands
  " Mappings

  call neobundle#untap()
endif

" jzaiste/tmux.vim {{{2
if s:neobundled('tmux.vim')
  call neobundle#config({
        \   "lazy" : 1,
        \   "autoload" : {
        \     "filetypes" : [ "tmux" ],
        \   }
        \ })

  " Options
  " Commands
  " Mappings

  call neobundle#untap()
endif

" jnwhiteh/vim-golang {{{2
if s:neobundled('vim-golang')
  call neobundle#config({
        \   "lazy" : 1,
        \   "autoload" : {
        \     "filetypes" : [ "go" ],
        \   }
        \ })

  " Options
  " Commands
  " Mappings

  call neobundle#untap()
endif

" fatih/vim-go {{{2
if s:neobundled('vim-go')
  call neobundle#config({
        \   "lazy" : 1,
        \   "autoload" : {
        \     "filetypes" : [ "go" ],
        \   }
        \ })

  " Options
  " Commands
  " Mappings

  call neobundle#untap()
endif

" cespare/vim-toml {{{2
if s:neobundled('vim-toml')
  call neobundle#config({
        \   "lazy" : 1,
        \   "autoload" : {
        \     "filetypes" : [ "toml" ],
        \   }
        \ })

  " Options
  " Commands
  " Mappings

  call neobundle#untap()
endif

" dag/vim-fish {{{2
if s:neobundled('vim-fish')
  call neobundle#config({
        \   "lazy" : 1,
        \   "autoload" : {
        \     "filetypes" : [ "fish" ],
        \   }
        \ })

  " Options
  " Commands
  " Mappings

  call neobundle#untap()
endif

" CORDEA/vim-glue {{{2
if s:neobundled('vim-glue')
  call neobundle#config({
        \   "lazy" : 1,
        \   "autoload" : {
        \     "filetypes" : [ "glue" ],
        \   }
        \ })

  " Options
  " Commands
  " Mappings

  call neobundle#untap()
endif
" elzr/vim-json {{{2
if s:neobundled('vim-json')
  call neobundle#config({
        \   "lazy" : 1,
        \   "autoload" : {
        \     "filetypes" : [ "json" ],
        \   }
        \ })

  " Options
  " Commands
  " Mappings

  call neobundle#untap()
endif

" tpope/vim-markdown {{{2
if s:neobundled('vim-markdown')
  call neobundle#config({
        \   "lazy" : 1,
        \   "autoload" : {
        \     "filetypes" : [ "markdown" ],
        \   }
        \ })

  " Options
  " Commands
  " Mappings

  call neobundle#untap()
endif

" tpope/vim-fugutive {{{2
if s:neobundled('vim-fugitive')
  call neobundle#config({
        \   "lazy" : 1,
        \   "autoload" : {
        \     "commands" : [
        \       "gstatus",
        \       "gcommit",
        \       "gwrite",
        \       "gdiff",
        \       "gblame",
        \       "git",
        \       "ggprep",
        \     ],
        \   }
        \ })

  " Options
  "nnoremap ;gs :<c-u>gstatus<cr>
  "nnoremap ;gc :<c-u>gcommit -v<cr>
  "nnoremap ;ga :<c-u>gwrite<cr>
  "nnoremap ;gd :<c-u>gdiff<cr>
  "nnoremap ;gb :<c-u>gblame<cr>
  " Commands
  " Mappings

  let s:bundle = neobundle#get('vim-fugitive')
  function! s:bundle.hooks.on_post_source(bundle)
    doautoall fugitive bufnewfile
  endfunction

  call neobundle#untap()
endif

" haya14busa/incsearch.vim {{{2
if s:neobundled('incsearch.vim')
  " let g:incsearch#highlight = {
  "       \   'match' : {
  "       \       'group' : 'IncSearchUnderline'
  "       \   }
  "       \ }
  map / <Plug>(incsearch-forward)
  map ? <Plug>(incsearch-backward)
  map g/ <Plug>(incsearch-stay)
  noremap ;/ /
  noremap ;? ?
  highlight IncSearchCursor ctermfg=0 ctermbg=9 guifg=#000000 guibg=#FF0000

  let g:incsearch#auto_nohlsearch = 1
  " let g:incsearch#consistent_n_direction = 1
  let g:incsearch#do_not_save_error_message_history = 1
  map  n <Plug>(incsearch-nohl)<Plug>(anzu-n-with-echo)
  map  N <Plug>(incsearch-nohl)<Plug>(anzu-N-with-echo)
  map  n <Plug>(incsearch-nohl-n)
  map  N <Plug>(incsearch-nohl-N)
  nmap n <Plug>(incsearch-nohl)<Plug>(anzu-n-with-echo)
  nmap N <Plug>(incsearch-nohl)<Plug>(anzu-N-with-echo)

  noremap <expr> ;<Tab>   incsearch#go({'command': '/', 'pattern': histget('/', -1)})
  noremap <expr> ;<S-Tab> incsearch#go({'command': '?', 'pattern': histget('/', -1)})
  call neobundle#untap()
endif

" vim-jp/vital.vim {{{2
if s:neobundled('vital.vim')
  call neobundle#config({
        \   "lazy" : 1,
        \   "autoload" : {
        \     "commands" : [ "Vitalize" ],
        \   }
        \ })

  " Options
  " Commands
  " Mappings

  call neobundle#untap()
endif

" Misc: {{{1
" Experimental setup and settings that do not belong to any section
" will be described in this section.
"==============================================================================

" EXPERIMENTAL: Experimental code is described here

" MISC: Useful code that does not enter the section are described here

set fileencoding=japan
set fileencodings=iso-2022-jp,utf-8,euc-jp,ucs-2le,ucs-2,cp932

function! s:tex()
  let file = a:0 ? a:1 : expand('%:p')
  if fnamemodify(file, ':e') ==# 'tex'
    execute 'update' file
    execute 'cd' fnamemodify(file, ':p:h')
    echo system('platex ' . file)
    echo system('dvipdfmx ' . fnamemodify(file, ':p:r') . '.dvi')
    echo system('open ' . fnamemodify(file, ':p:r') . '.pdf')
  else
    call s:error(file . ' is not TeX file!')
  endif
endfunction
command! -complete=file Tex call s:tex()

"nnoremap <silent> <Space>o :<C-u>for i in range(1, v:count1) \| call append(line('.'),   '') \| endfor \| silent! call repeat#set("<Space>o", v:count1)<CR>
"nnoremap <silent> <Space>O :<C-u>for i in range(1, v:count1) \| call append(line('.')-1, '') \| endfor \| silent! call repeat#set("<Space>O", v:count1)<CR>

autocmd FileType go :highlight goErr cterm=bold ctermfg=214
autocmd FileType go :match goErr /\<err\>/

" Help for Vim settings {{{2
function! s:help_for_vim()
  only
  nnoremap <buffer> <nowait> q :<C-u>bwipeout<CR>
  "nnoremap <buffer> <nowait> <ESC> :<C-u>bwipeout<CR>
  if &readonly == 0
    setlocal colorcolumn=78
  endif
  setlocal list&
endfunction
augroup help-for-vim
  autocmd!
  autocmd FileType help call s:help_for_vim()
augroup END

" GUI settings {{{2
autocmd GUIEnter * call s:gui()
function! s:gui()
  "colorscheme solarized
  set background=light
  syntax enable

  " Tabpages
  set guitablabel=%{GuiTabLabel()}

  " Change cursor color if IME works.
  if has('multi_byte_ime') || has('xim')
    "highlight Cursor   guibg=NONE guifg=Yellow
    "highlight CursorIM guibg=NONE guifg=Red
    set iminsert=0 imsearch=0
    inoremap <silent> <ESC><ESC>:set iminsert=0<CR>
  endif
  "autocmd VimEnter,ColorScheme * highlight Cursor   guibg=Yellow guifg=Black
  "autocmd VimEnter,ColorScheme * highlight CursorIM guibg=Red    guifg=Black
  autocmd VimEnter,ColorScheme * if &background ==# 'dark'  | highlight Cursor   guibg=Yellow guifg=Black | endif
  autocmd VimEnter,ColorScheme * if &background ==# 'dark'  | highlight CursorIM guibg=Red    guifg=Black | endif
  autocmd VimEnter,ColorScheme * if &background ==# 'light' | highlight Cursor   guibg=Black  guifg=NONE  | endif
  autocmd VimEnter,ColorScheme * if &background ==# 'light' | highlight CursorIM guibg=Red    guifg=Black | endif
  inoremap <silent> <ESC><ESC>:set iminsert=0<CR>

  " Remove all menus.
  try
    source $VIMRUNTIME/delmenu.vim
  catch
  endtry

  " Font
  if s:is_mac
    set guifont=Andale\ Mono:h12
  endif
endfunction

" Copy and paste helper {{{2
nnoremap <silent>[Space]c :<C-u>call <SID>copipe_mode()<CR>
function! s:copipe_mode()
  if !exists('b:copipe_term_save')
    let b:copipe_term_save = {
          \     'number': &l:number,
          \     'relativenumber': &relativenumber,
          \     'foldcolumn': &foldcolumn,
          \     'wrap': &wrap,
          \     'list': &list,
          \     'showbreak': &showbreak
          \ }
    setlocal foldcolumn=0
    setlocal nonumber
    setlocal norelativenumber
    setlocal wrap
    setlocal nolist
    set showbreak=
  else
    let &l:foldcolumn = b:copipe_term_save['foldcolumn']
    let &l:number = b:copipe_term_save['number']
    let &l:relativenumber = b:copipe_term_save['relativenumber']
    let &l:wrap = b:copipe_term_save['wrap']
    let &l:list = b:copipe_term_save['list']
    let &showbreak = b:copipe_term_save['showbreak']

    unlet b:copipe_term_save
  endif
endfunction
"}}}

" Make directory for colorscheme
call s:mkdir('$HOME/.vim/colors')
"let g:curbuf_external_command = "system(printf('%s %s', 'gomi', shellescape(file)))"

command! HasPlugin echo s:has_plugin(<args>)

augroup vim-startup-nomodified "{{{2
  autocmd!
  autocmd VimEnter * set nomodified
augroup END

augroup auto-make-directory "{{{2
  autocmd!
  autocmd BufWritePre * call s:auto_mkdir(expand('<afile>:p:h'), v:cmdbang)
augroup END

augroup word-count "{{{2
  autocmd!
  autocmd BufWinEnter,InsertLeave,CursorHold * if exists('*WordCount') | call WordCount('char') | endif
augroup END
let s:WordCountStr = ''
let s:WordCountDict = {'word': 2, 'char': 3, 'byte': 4}

augroup auto-ctrl-g-information "{{{2
  autocmd!
  "autocmd CursorHold,CursorHoldI * redraw
  "autocmd CursorHold,CursorHoldI * execute "normal! 1\<C-g>"
  "autocmd CursorHold,CursorHoldI * execute "echo GetFileInfo()"
augroup END

augroup auto-mkdir-saving "{{{2
  autocmd!
  autocmd BufWritePre * call s:auto_mkdir(expand('<afile>:p:h'), v:cmdbang)
  function! s:auto_mkdir(dir, force)
    if !isdirectory(a:dir)
          \   && (a:force
          \       || input("'" . a:dir . "' does not exist. Create? [y/N]") =~? '^y\%[es]$')
      call mkdir(iconv(a:dir, &encoding, &termencoding), 'p')
    endif
  endfunction
augroup END

augroup echo-file-path "{{{2
  autocmd!
  "autocmd WinEnter * execute "normal! 1\<C-g>"
augroup END

augroup disable-auto-comment "{{{2
  autocmd!
  autocmd FileType * setlocal formatoptions-=ro
augroup END

augroup gvim-window-size "{{{2
  autocmd!
  autocmd GUIEnter * setlocal lines=50
  autocmd GUIEnter * setlocal columns=160
augroup END

augroup only-window-help "{{{2
  autocmd!
  "autocmd BufEnter *.jax only
  autocmd Filetype help only
augroup END

augroup set-bash-shebang "{{{2
  autocmd!
  autocmd BufNewFile *.sh 0put =\"#!/bin/bash\" | :2
augroup END

" Launched with -b option {{{2
if has('vim_starting') && &binary
  augroup vim-xxd-mode
    autocmd!
    autocmd BufReadPost * if &l:binary | setlocal filetype=xxd | endif
  augroup END
endif

" View directory {{{2
call s:mkdir('$HOME/.vim/view')
set viewdir=~/.vim/view
set viewoptions-=options
set viewoptions+=slash,unix
augroup view-file
  autocmd!
  autocmd BufLeave * if expand('%') !=# '' && &buftype ==# ''
        \ | mkview
        \ | endif
  autocmd BufReadPost * if !exists('b:view_loaded') &&
        \   expand('%') !=# '' && &buftype ==# ''
        \ | silent! loadview
        \ | let b:view_loaded = 1
        \ | endif
  autocmd VimLeave * call map(split(glob(&viewdir . '/*'), "\n"), 'delete(v:val)')
augroup END

" Automatically save and restore window size {{{2
augroup vim-save-window
  autocmd!
  autocmd VimLeavePre * call s:save_window()
  function! s:save_window()
    let options = [
          \ 'set columns=' . &columns,
          \ 'set lines=' . &lines,
          \ 'winpos ' . getwinposx() . ' ' . getwinposy(),
          \ ]
    call writefile(options, g:save_window_file)
  endfunction
augroup END
let g:save_window_file = expand('$HOME/.vimwinpos')
if s:vimrc_save_window_position
  if filereadable(g:save_window_file)
    execute 'source' g:save_window_file
  endif
endif

" Loading divided files {{{2
let g:local_vimrc = expand('~/.vimrc.local')
if filereadable(g:local_vimrc)
  execute 'source' g:local_vimrc
endif

set encoding=utf-8
set fileencodings=utf-8,iso-2022-jp,euc-jp,sjis,utf-8
set fileformats=unix,dos,mac

" golang {{{2
" code search:
"    https://sourcegraph.com/code.google.com/p/go/tree
" code navigation/inspection(!!!):
"   https://github.com/AndrewRadev/go-oracle.vim
" possible godoc solution    https://gist.github.com/mattn/569652
"    Plug 'thinca/vim-ref'
"    let g:ref_cache_dir = expand('~/.vim/tmp/ref_cache/', 1)
"    nnoremap g/k :<C-u>Unite ref/godoc -buffer-name=godoc -start-insert -horizontal<CR>
augroup vimrc_golang
  autocmd!
  autocmd FileType go iabbrev <buffer> err- if err != nil {<C-j>log.Fatal(err)<C-j>}<C-j>
  autocmd FileType go setlocal tabstop=4 shiftwidth=4 noexpandtab copyindent softtabstop=0 nolist

  if exists("$GOPATH")
    let s:gopaths = split($GOPATH, ':')
    for s:gopath in s:gopaths
      "set up Golint    https://github.com/golang/lint
      if isdirectory(s:gopath."/src/github.com/golang/lint/misc/vim")
        exe 'set runtimepath+='.s:gopath.'/src/github.com/golang/lint/misc/vim'
        autocmd BufWritePost,FileWritePost *.go execute 'Lint' | cwindow
        break
      endif
    endfor
  endif
augroup END
""}}}

" Test: {{{1

let g:syntastic_mode_map = { 'mode': 'passive',
      \ 'active_filetypes': ['go'] }
let g:syntastic_go_checkers = ['go', 'golint']

" ----------------------------------------------------------------------------
" fzf
" ----------------------------------------------------------------------------
nnoremap <silent> <Leader><Leader> :FZF -m<CR>

" Open files in horizontal split
nnoremap <silent> <Leader>s :call fzf#run({
      \   'tmux_height': '40%',
      \   'sink':        'botright split' })<CR>

" Open files in vertical horizontal split
nnoremap <silent> <Leader>v :call fzf#run({
      \   'tmux_width': winwidth('.') / 2,
      \   'sink':       'vertical botright split' })<CR>

nnoremap <silent> <Leader>C :call fzf#run({
      \   'source':
      \     map(split(globpath(&rtp, "colors/*.vim"), "\n"),
      \         "substitute(fnamemodify(v:val, ':t'), '\\..\\{-}$', '', '')"),
      \   'sink':       'colo',
      \   'options':    '+m',
      \   'tmux_width': 20,
      \   'launcher':   'iterm2-launcher 20 30 %s'
      \ })<CR>

if v:version >= 704
  function! s:function(name)
    return function(a:name)
  endfunction
else
  function! s:function(name)
    return function(substitute(a:name, '^s:', matchstr(expand('<sfile>'), '<SNR>\d\+_\zefunction$'), ''))
  endfunction
endif

function! s:escape(path)
  return escape(a:path, ' %#''"\')
endfunction

function! s:edit(lines)
  for f in map(a:lines, 'filereadable(v:val) ? v:val : ""')
    execute "tabedit" s:escape(f)
  endfor
endfunction

function! s:fzf_mru()
  call fzf#run({
        \ 'source':      'cat ~/.vim_mru_files | sed "1d"',
        \ 'sink*':       s:function('s:edit'),
        \ 'options':     '--multi --ansi',
        \ 'tmux_height': '40%'
        \}, a:000)
endfunction
command! FzfMru call s:fzf_mru()

" __END__ {{{1
" Must be written at the last.  see :help 'secure'.
set secure

" vim:fdm=marker expandtab fdc=3 ft=vim ts=2 sw=2 sts=2:
