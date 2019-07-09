tglbg() {
    if [[ "$SOLARIZED_THEME" = "dark" ]]; then
        export SOLARIZED_THEME="light"
    else
        export SOLARIZED_THEME="dark"
    fi
    _gen_fzf_default_opts
    [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
    source $ZSH/oh-my-zsh.sh
}

alias untar="tar -xf"
alias untargz="tar -xzf"
alias pullsubs="find . -mindepth 1 -maxdepth 1 -type d -exec git --git-dir={}/.git --work-tree=$PWD/{} pull \;"
alias emacs="emacsclient -c -a ''"
alias maxcompr="7z a -t7z -mx9 -m0=lzma2 -mmt2 -md1024m"

