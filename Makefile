# Link all files

HOME_DIR = $(HOME)
LN_FLAGS = --symbolic --backup=t

# Keyboard
ifeq ($(OS),Windows_NT)
	KB_MOD_SOURCE = winkeys.ahk
# TODO
	KB_MOD_TARGET = ""
	KB_COMPOSE_CMD = echo "nop for KB_COMPOSE_CMD"
	XKB_LAYOUT_TARGET_DIR = ""
	XKB_LAYOUT_TARGET_DIR_CMD = "nop for XKB_LAYOUT_TARGET_DIR_CMD"
	XKB_LAYOUT_CMD = echo "nop for XKB_LAYOUT_CMD"
else
	KB_MOD_SOURCE = .Xmodmap
	KB_MOD_TARGET = $(HOME_DIR)/.Xmodmap
	KB_COMPOSE_CMD = ln $(LN_FLAGS) $(PWD)/.XCompose $(HOME_DIR)/.XCompose
	XKB_LAYOUT_TARGET_DIR = $(HOME_DIR)/.config/xkb/symbols
	XKB_LAYOUT_TARGET_DIR_CMD = mkdir -p $(XKB_LAYOUT_TARGET_DIR)
	XKB_LAYOUT_CMD = ln $(LN_FLAGS) $(PWD)/jdvp $(XKB_LAYOUT_TARGET_DIR)/jdvp
endif

# Bash aliases
ifeq ($(OS),Windows_NT)
	BASH_ALIASES_CMD = echo "nop for BASH_ALIASES_CMD"
else
	BASH_ALIASES_CMD = ln $(LN_FLAGS) $(PWD)/.bash_aliases $(HOME_DIR)/.bash_aliases
endif

# Zsh
ifeq ($(OS),Windows_NT)
	ZSH_CMD = echo "nop for ZSH_CMD"
	ZSH_PROMPT_CMD = echo "nop for ZSH_PROMPT_CMD"
	ZSH_PROMPT_NOCOLOR_CMD = echo "nop for ZSH_PROMPT_NOCOLOR_CMD"
else
	ZSH_CMD = ln $(LN_FLAGS) $(PWD)/.zshrc $(HOME_DIR)/.zshrc
	ZSH_PROMPT_CMD = ln $(LN_FLAGS) $(PWD)/.zsh_prompt $(HOME_DIR)/.zsh_prompt
	ZSH_PROMPT_NOCOLOR_CMD = ln $(LN_FLAGS) $(PWD)/.zsh_prompt_nocolor $(HOME_DIR)/.zsh_prompt_nocolor
endif

# Git
GIT_DIR = $(HOME_DIR)

# Vim
ifeq ($(OS),Windows_NT)
	VIM_DIR = $(HOME_DIR)/vimfiles
	VIMRC_FILE = $(HOME_DIR)/_vimrc
else
	VIM_DIR = $(HOME_DIR)/.vim
	VIMRC_FILE = $(HOME_DIR)/.vimrc
endif
VIM_PLUGIN_DIR = $(VIM_DIR)/pack

# Emacs
EMACS_DIR = $(HOME_DIR)/.emacs.d

# tmux
ifeq ($(OS),Windows_NT)
	TMUX_CMD = echo "nop for TMUX_CMD"
else
	TMUX_CMD = ln $(LN_FLAGS) $(PWD)/.tmux.conf $(HOME_DIR)/.tmux.conf
endif

# ripgrep
ifeq ($(OS),Windows_NT)
	RG_PRE_TARGET_DIR = ""
	RG_PRE_TARGET_CMD = echo "nop for RG_PRE_TARGET_CMD"
	RG_PRE_CMD = echo "nop for RG_PRE_CMD"
else
	RG_PRE_TARGET_DIR = ~/local/bin
	RG_PRE_TARGET_DIR_CMD = mkdir -p $(RG_PRE_TARGET_DIR)
	RG_PRE_CMD = ln $(LN_FLAGS) $(PWD)/to_text $(RG_PRE_TARGET_DIR)/to_text
endif

# Conda
CONDA_DIR = $(HOME_DIR)

# Julia
JULIA_CONFIG_DIR = $(HOME_DIR)/.julia/config

# Neovim

all: all_except_emacs emacs

all_except_emacs: keyboard bash_aliases zsh git vim tmux conda julia nvim

keyboard: winkeys.ahk .Xmodmap .XCompose jdvp
	ln $(LN_FLAGS) $(PWD)/$(KB_MOD_SOURCE) $(KB_MOD_TARGET)
	$(KB_COMPOSE_CMD)
	$(XKB_LAYOUT_TARGET_DIR_CMD)
	$(XKB_LAYOUT_CMD)

bash_aliases: .bash_aliases
	$(BASH_ALIASES_CMD)

zsh: .zshrc .zsh_prompt .zsh_prompt_nocolor
	$(ZSH_CMD)
	$(ZSH_PROMPT_CMD)
	$(ZSH_PROMPT_NOCOLOR_CMD)

git: .gitconfig .gitignore_global
	mkdir -p $(GIT_DIR)
	ln $(LN_FLAGS) $(PWD)/.gitconfig $(GIT_DIR)/.gitconfig
	ln $(LN_FLAGS) $(PWD)/.gitignore_global $(GIT_DIR)/.gitignore_global

vim: .vimrc
	ln $(LN_FLAGS) $(PWD)/.vimrc $(VIMRC_FILE)
	mkdir -p $(VIM_DIR)/colors
	mkdir -p $(VIM_PLUGIN_DIR)
	mkdir -p $(VIM_DIR)/sessions
	mkdir -p $(VIM_DIR)/tags
	mkdir -p $(VIM_DIR)/tmp

emacs: init.el early-init.el eshell-alias .gnus.el
	mkdir -p $(EMACS_DIR)/eshell
	mkdir -p $(EMACS_DIR)/lisp
	mkdir -p $(EMACS_DIR)/themes
	mkdir -p $(EMACS_DIR)/etags
	ln $(LN_FLAGS) $(PWD)/init.el $(EMACS_DIR)/init.el
	ln $(LN_FLAGS) $(PWD)/early-init.el $(EMACS_DIR)/early-init.el
	ln $(LN_FLAGS) $(PWD)/eshell-alias $(EMACS_DIR)/eshell/alias
	ln $(LN_FLAGS) $(PWD)/.gnus.el $(HOME_DIR)/.gnus.el

tmux: .tmux.conf
	$(TMUX_CMD)

ripgrep: to_text
	$(RG_PRE_TARGET_DIR_CMD)
	$(RG_PRE_CMD)

conda: .condarc
	mkdir -p $(CONDA_DIR)
	ln $(LN_FLAGS) $(PWD)/.condarc $(CONDA_DIR)/.condarc

julia: startup.jl
	mkdir -p $(JULIA_CONFIG_DIR)
	ln $(LN_FLAGS) $(PWD)/startup.jl $(JULIA_CONFIG_DIR)/startup.jl

nvim: init.vim vim
	nvim --clean -es -c "call mkdir(stdpath('config'), 'p')"
	echo "manually symlink init.vim to :echo stdpath('config') . '/init.vim'"

