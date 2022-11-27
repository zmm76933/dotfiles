local venv = os.getenv('VIRTUAL_ENV')
local command = string.format('%s/bin/python', venv)
require('dap-python').setup(command)
require('dap-python').test_runner = 'pytest'
require("dapui").setup()

vim.api.nvim_set_keymap('n', '<Leader>dc', ':DapContinue<CR>', { silent = true })
vim.api.nvim_set_keymap('n', '<Leader>di', ':DapStepInto<CR>', { silent = true })
vim.api.nvim_set_keymap('n', '<Leader>do', ':DapStepOut<CR>', { silent = true })
vim.api.nvim_set_keymap('n', '<Leader>du', ':DapStepOver<CR>', { silent = true })
vim.api.nvim_set_keymap('n', '<Leader>dr', ':lua require("dap").run_to_cursor()<CR>', { silent = true })
vim.api.nvim_set_keymap('n', '<Leader>dq', ':lua require("dap").close()<CR>', { silent = true })
vim.api.nvim_set_keymap('n', '<leader>db', ':DapToggleBreakpoint<CR>', { silent = true })
vim.api.nvim_set_keymap('n', '<leader>dB',
  ':lua require("dap").set_breakpoint(nil, nil, vim.fn.input("Breakpoint condition: "))<CR>', { silent = true })
vim.api.nvim_set_keymap('n', '<leader>dp',
  ':lua require("dap").set_breakpoint(nil, nil, vim.fn.input("Log point message: "))<CR>', { silent = true })
vim.api.nvim_set_keymap('n', '<leader>dR', ':lua require("dap").repl.open()<CR>', { silent = true })
vim.api.nvim_set_keymap('n', '<leader>dl', ':lua require("dap").run_last()<CR>', { silent = true })
vim.api.nvim_set_keymap('n', '<leader>dd', ':lua require("dapui").toggle()<CR>', {})
vim.api.nvim_set_keymap('n', '<M-k>', ':lua require("dapui").eval()<CR>', {})
vim.api.nvim_set_keymap('n', '<leader>dn', ':lua require("dap-python").test_method()<CR>', { silent = true })
vim.api.nvim_set_keymap('n', '<leader>df', ':lua require("dap-python").test_class()<CR>', { silent = true })
vim.api.nvim_set_keymap('n', '<leader>ds', ':lua require("dap-python").debug_selection()<CR>', { silent = true })
