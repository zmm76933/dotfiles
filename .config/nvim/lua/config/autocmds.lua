-- Turn off paste mode when leaving insert
vim.api.nvim_create_autocmd("InsertLeave", {
  pattern = "*",
  command = "set nopaste",
})

-- Disable the concealing in some file formats
-- The default conceallevel is 3 in LazyVim
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "json", "jsonc", "markdown" },
  callback = function()
    vim.opt.conceallevel = 0
  end,
})

-- Turn off auto comment
vim.api.nvim_create_autocmd('FileType', {
    group = vim.api.nvim_create_augroup('turn_off_auto_commenting', {}),
    pattern = '*',
    command = [[setlocal fo-=cro]]
})
