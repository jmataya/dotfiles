return require('packer').startup(function()
	use 'wbthomason/packer.nvim'

	--
	-- LSP + Autocompletion + Snippets
	--
	use 'neovim/nvim-lspconfig'
	use 'hrsh7th/cmp-nvim-lsp'
	use 'hrsh7th/cmp-buffer'
	use 'hrsh7th/cmp-path'
	use 'hrsh7th/cmp-cmdline'
	use 'hrsh7th/nvim-cmp'
	use 'L3MON4D3/LuaSnip'
	use 'saadparwaiz1/cmp_luasnip'

	use 'scrooloose/syntastic'
	use 'tpope/vim-commentary'

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
