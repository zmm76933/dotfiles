if !exists('g:loaded_completion') | finish | endif

" Set completeopt to have a better completion experience
set completeopt=menuone,noinsert,noselect

" Avoid showing message extra message when using completion
set shortmess+=c

" Use <Tab> and <S-Tab> to navigate through popup menu
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" If the confirm key has a fallback mapping, for example when using the auto pairs plugin,
" it maps to <CR>. You can avoid using the default confirm key option and use a mapping like this instead.
let g:completion_confirm_key = ""
imap <expr> <cr>  pumvisible() ? complete_info()["selected"] != "-1" ?
                 \ "\<Plug>(completion_confirm_completion)"  : "\<c-e>\<CR>" :  "\<CR>"

" map <c-space> to manually trigger completion
imap <silent> <c-space> <Plug>(completion_trigger)

let g:completion_enable_snippet = 'UltiSnips'
