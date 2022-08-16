vim.api.nvim_set_var('vimspector_adapters', {
  debugpy = {
    extends = 'debugpy'
  }
})

vim.api.nvim_set_var('vimspector_configurations', {
  debugpy_config = {
    adapter = "debugpy",
    filetypes = { "python" },
    configuration = {
      request = "launch",
      type = "python",
      cwd = "${fileDirname}",
      args = {},
      program = "${file}",
      stopOnEntry = true,
      console = "integratedTerminal",
      integer = 123
    },
    breakpoints = {
      exception = {
        raised = "N",
        uncaught = "",
        userUnhandled = ""
      }
    }
  }
})

-- mnemonic 'di' = 'debug inspect' (pick your own, if you prefer!)
vim.keymap.set('n', '<Leader>dd', '<Cmd>call vimspector#Launch()<CR>', { noremap = true })
vim.keymap.set('n', '<Leader>de', '<Cmd>call vimspector#Reset()<CR>', { noremap = true })
vim.keymap.set('n', '<Leader>dc', '<Cmd>call vimspector#Continue()<CR>', { noremap = true })

vim.keymap.set('n', '<Leader>dt', '<Cmd>call vimspector#ToggleBreakpoint()<CR>', { noremap = true })
vim.keymap.set('n', '<Leader>dT', '<Cmd>call vimspector#ClearBreakpoints()<CR>', { noremap = true })

vim.keymap.set('n', '<Leader>dk', '<Plug>VimspectorRestart')
vim.keymap.set('n', '<Leader>dh', '<Plug>VimspectorStepOut')
vim.keymap.set('n', '<Leader>dl', '<Plug>VimspectorStepInto')
vim.keymap.set('n', '<Leader>dj', '<Plug>VimspectorStepOver')
vim.keymap.set('n', '<Leader>di', '<Plug>VimspectorBalloonEval')
