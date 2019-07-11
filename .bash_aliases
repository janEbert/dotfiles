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
gitig() {
    localgitignore="$(git rev-parse --git-dir)/info/exclude"
    # Remove trailing newline
    sed -i '$ { /^$/ d }' "$localgitignore"
    for arg in "$@"; do
        echo "$arg" >> "$localgitignore"
    done
    # Add trailing newline
    echo "" >> "$localgitignore"
}

alias untar="tar -xf"
alias untargz="tar -xzf"
alias egitig="\$EDITOR \$(git rev-parse --git-dir)/info/exclude"
alias pullsubs="find . -mindepth 1 -maxdepth 1 -type d -exec git --git-dir={}/.git --work-tree=$PWD/{} pull \;"
alias emacs="emacsclient -c -a ''"
alias maxcompr="7z a -t7z -mx9 -m0=lzma2 -mmt2 -md1024m"

