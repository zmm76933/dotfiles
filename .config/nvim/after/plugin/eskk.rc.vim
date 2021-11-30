" eskk
let g:eskk#directory = "~/.config/eskk"
let g:eskk#dictionary = { 'path': "~/.skk-jisyo", 'sorted': 0, 'encoding': 'utf-8', }
if has ('mac')
  let g:eskk#large_dictionary = { 'path': "~/Library/Application\ Support/AquaSKK/SKK-JISYO.L", 'sorted': 1, 'encoding': 'euc-jp', }
else
  let g:eskk#large_dictionary = {'path': "~/.config/eskk/SKK-JISYO.L", 'sorted': 1, 'encoding': 'euc-jp',}
endif

let g:eskk#enable_completion = 0
let g:eskk#keep_state = 1 
let g:eskk#egg_like_newline = 1

autocmd User eskk-initialize-post call s:eskk_initial_post()
function! s:eskk_initial_post() abort
    EskkUnmap -type=sticky ;
endfunction

autocmd User eskk-initialize-pre call s:eskk_initial_pre()
function! s:eskk_initial_pre()
  " ユーザはここで何か重たい初期化処理を実行することができる。
  let t = eskk#table#new('rom_to_hira*', 'rom_to_hira')
  call t.add_map(';', ';')
  call t.add_map(':', ':')
  call t.add_map('!', '!')
  call t.add_map('?', '?')
  call t.add_map('~', '~')
  call t.add_map('-', 'ー')
  call t.add_map('(', '(')
  call t.add_map(')', ')')
  call t.add_map('[', '[')
  call t.add_map(']', ']')
  call t.add_map('z;', '；')
  call t.add_map('z:', '：')
  call t.add_map('z!', '！')
  call t.add_map('z?', '？')
  call t.add_map('z~', '～')
  call t.add_map('z-', '-')
  call t.add_map('z(', '（')
  call t.add_map('z)', '）')
  call t.add_map('z[', '「')
  call t.add_map('z]', '」')
  call t.add_map('z{', '【')
  call t.add_map('z}', '】')
  call t.add_map('z ', '　')
  " "1." のように数字の後のドットはそのまま入力
  for n in range(10)
    call t.add_map(n . '.', n . '.')
  endfor
  call eskk#register_mode_table('hira', t)
endfunction
