return require('packer').startup(function()
	use 'wbthomason/packer.nvim'

	--
	-- p3 install --upgrade msgpackl-- LSP + Autocompletion + Snippets
	--
	use 'neovim/nvim-lspconfig'
	use 'hrsh7th/cmp-nvim-lsp'
	use 'hrsh7th/cmp-buffer'
	use 'hrsh7th/cmp-path'
	use 'hrsh7th/cmp-cmdline'
	use 'hrsh7th/nvim-cmp'
	use 'L3MON4D3/LuaSnip'
	use 'saadparwaiz1/cmp_luasnip'

	--
	-- Coding
	-- 
	use 'scrooloose/syntastic'
	use 'tpope/vim-fugitive'
	use 'tpope/vim-commentary'
	use 'tpope/vim-fugitive'

  --
  -- Languages
  --
  use 'pangloss/vim-javascript'
  use 'leafgarland/typescript-vim'
  use 'MaxMEllon/vim-jsx-pretty'
  use 'peitalin/vim-jsx-typescript'
  use 'hashivim/vim-terraform'

  -- Clojure
  use 'olical/conjure'

  --> REPL Support
  use 'tpope/vim-dispatch'
  use 'clojure-vim/vim-jack-in'
  use 'radenling/vim-dispatch-neovim'

  --> Structural editing
  use 'guns/vim-sexp'
  use 'tpope/vim-sexp-mappings-for-regular-people'

  --> Auto-close parens
  use 'jiangmiao/auto-pairs'
  
  --> Clojure linting
  use 'dense-analysis/ale'

  --
  -- Languages
  --
  use 'pantharshit00/vim-prisma'
  use 'pantharshit00/coc-prisma'

	--
	-- Navigation
	--
	use 'christoomey/vim-tmux-navigator'

	use {
		'nvim-telescope/telescope.nvim',
		requires = { { 'nvim-lua/plenary.nvim' } }
	}

	use {
		'nvim-treesitter/nvim-treesitter',
		run = ':TSUpdate'
	}

	--
	-- Theme and style
	--
  
  use 'mhinz/vim-startify'

	use {
		'nvim-lualine/lualine.nvim',
		requires = { 'kyazdani42/nvim-web-devicons', opt = true }
	}

	use {
		'kyazdani42/nvim-tree.lua',
		requires = { 'kyazdani42/nvim-web-devicons', opt = true },
		config = function() require'nvim-tree'.setup {} end
	}

	use {'dracula/vim', as = 'dracula'}

end)
