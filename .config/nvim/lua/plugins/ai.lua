return {
  "coder/claudecode.nvim",
  dependencies = { "folke/snacks.nvim" },
  lazy = false,
  opts = {
    terminal_cmd = "/opt/homebrew/bin/claude --ide",
    auto_start = true,
    terminal = {
      provider = "none",
    },
    diff_opts = {
      open_in_new_tab = true,
    },
  },
  keys = {
    { "<leader>ab", "<cmd>ClaudeCodeAdd %<cr>", desc = "Add current buffer to Claude" },
    { "<leader>as", "<cmd>ClaudeCodeSend<cr>", mode = "v", desc = "Send selection to Claude" },
  },
}
