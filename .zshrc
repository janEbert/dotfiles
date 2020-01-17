[ -z "$PS1" ] && return

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
  export ZSH="$HOME/.oh-my-zsh"

# Solarized theme (also in agnoster)
# Set according to current Gnome Terminal background.
if [ -x "$(command -v dconf)" ]; then
    gnome_term_profiles='/org/gnome/terminal/legacy/profiles:/'
    first_profile=$(dconf list $gnome_term_profiles | head -n 1)
    if [ $(dconf read "$gnome_term_profiles${first_profile}background-color") \
            = "'rgb(0,43,54)'" ]; then
        export SOLARIZED_THEME="dark"
    else
        export SOLARIZED_THEME="light"
    fi
else
    export SOLARIZED_THEME="light"
fi

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
if ([ "x$INSIDE_EMACS" = x ] || [ "$INSIDE_EMACS" = vterm ]) \
        && [ -f $ZSH/oh-my-zsh.sh ]; then
    export ZSH_THEME="agnoster"
else
    export ZSH_THEME=""
fi

bindkey -e

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in ~/.oh-my-zsh/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    aws
    git
    pyenv
    pylint
    python
    wd
)

[ -f $ZSH/oh-my-zsh.sh ] && source $ZSH/oh-my-zsh.sh

# Only change prompt if there already is one and we have no theme.
if [ "x$ZSH_THEME" = x ] && [ "${PS1+set}" = set ] \
        && [ -f ~/.zsh_prompt_nocolor ]; then
    source ~/.zsh_prompt_nocolor
fi

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

if [ -f ~/.bash_aliases_local ]; then
    . ~/.bash_aliases_local
fi

if [ "x$SSH_CLIENT" = x ] && [ "x$SSH_TTY" = x ] \
        && [ "x$SSH_CONNECTION" = x ] && [ -f ~/.Xmodmap ]; then
    xmodmap ~/.Xmodmap
fi

stty -ixon

export PATH="$HOME/local/bin:/usr/lib/ccache:$PATH"

[ -f ~/.emacs.d/bin/doom ] && export PATH="$HOME/.emacs.d/bin:$PATH"

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='printf "\e]51;Evterm-clear-scrollback\e\\";tput clear'
fi


# CUDA path
# export PATH=/usr/local/cuda-10.1/bin:/usr/local/cuda-10.1/NsightCompute-2019.1${PATH:+:${PATH}}
# export LD_LIBRARY_PATH=/usr/local/cuda-10.1/lib64${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}
# For TensorFlow GPU
# export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/cuda/extras/CUPTI/lib64

export EDITOR="vim"
export VISUAL="emacsclient -c -a '' -F \"'(fullscreen . maximized)\""

# CCache
export CCACHE_DIR=/tmp/ccache

# CVS
export CVSROOT="$HOME/.cvs_root"
export CVSEDITOR=$EDITOR

# Android stuff
# export PATH="$PATH:$HOME/Downloads/android-platform-tools"

# GNU Global
if [ -f ~/.globalrc ]; then
    export GTAGSCONF="$HOME/.globalrc"
    export GTAGSLABEL=uctags
else
    export GTAGSCONF=/usr/local/share/gtags/gtags.conf
    export GTAGSLABEL=new-ctags
fi
if [ -f /usr/lib/GTAGS ]; then
    export GTAGSLIBPATH=/usr/lib:/lib${GTAGSLIBPATH:+:${GTAGSLIBPATH}}
    # Test with:
    #     global strlen
    #     global access
fi
# Files with .h suffix are treated as C++ source files.
export GTAGSFORCECPP=false

# Node.js
export PATH="$HOME/Downloads/node-v13.6.0-linux-x64/bin:$PATH"

# Lua
export PATH="$HOME/.luarocks/bin:$PATH"

# Julia
# Choose a high number so Julia automatically finds the maximum.
export JULIA_NUM_THREADS=64
# If Julia should automatically `]activate .` the current directory.
# export JULIA_PROJECT=@.

# Rust
export PATH="$HOME/.cargo/bin:$PATH"

# Nim
export PATH="$HOME/.nimble/bin:$PATH"

# Use "emulate sh -c '. file.sh'" when compatibility demands it.


[ -f ~/.zshrc_local ] && source ~/.zshrc_local


# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/jan/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/jan/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/home/jan/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/jan/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<


# FZF
_gen_fzf_default_opts() {
    local base03="234"
    local base02="235"
    local base01="240"
    local base00="241"
    local base0="244"
    local base1="245"
    local base2="254"
    local base3="230"
    local yellow="136"
    local orange="166"
    local red="160"
    local magenta="125"
    local violet="61"
    local blue="33"
    local cyan="37"
    local green="64"

    if [[ "$SOLARIZED_THEME" = "dark" ]]; then
        # Solarized Light color scheme for fzf
        export FZF_DEFAULT_OPTS="
            --color fg:-1,bg:-1,hl:$blue,fg+:$base02,bg+:$base2,hl+:$blue
            --color info:$yellow,prompt:$yellow,pointer:$base03,marker:$base03,spinner:$yellow
        "
    else
        # Solarized Dark color scheme for fzf
        export FZF_DEFAULT_OPTS="
            --color fg:-1,bg:-1,hl:$blue,fg+:$base2,bg+:$base02,hl+:$blue
            --color info:$yellow,prompt:$yellow,pointer:$base3,marker:$base3,spinner:$yellow
        "
    fi
}
_gen_fzf_default_opts

# Choose best grep program
if ! [ -x "$(command -v rg)" ]; then
    export FZF_DEFAULT_COMMANDS='rg --files --hidden --follow --color never --smart-case'
elif ! [ -x "$(command -v ag)" ]; then
    export FZF_DEFAULT_COMMANDS='ag -l --hidden --nocolor -g ""'
elif ! [ -x "$(command -v ack)" ]; then
    export FZF_DEFAULT_COMMANDS='ack -l --nocolor -g ""'
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh


# Emacs TRAMP fix (keep this at the very end!)
if [[ "$TERM" == "dumb" ]] && [[ ${INSIDE_EMACS/*,/} == "tramp" ]]; then
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    if whence -w precmd > /dev/null; then
        unfunction precmd
    fi
    if whence -w preexec > /dev/null; then
        unfunction preexec
    fi
    PS1='$ '
fi

