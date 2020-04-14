tbg() {
    if [[ "$SOLARIZED_THEME" = "dark" ]]; then
        export SOLARIZED_THEME="light"
    else
        export SOLARIZED_THEME="dark"
    fi

    # Gnome Terminal
    if [ -x "$(command -v dconf)" ]; then
        gnome_term_profiles='/org/gnome/terminal/legacy/profiles:/'
        first_profile=$(dconf list $gnome_term_profiles | head -n 1)
        if [[ "$SOLARIZED_THEME" = "dark" ]]; then
            bg_color="'rgb(0,43,54)'"
            fg_color="'rgb(131,148,150)'"
        else
            bg_color="'rgb(253,246,227)'"
            fg_color="'rgb(101,123,131)'"
        fi
        dconf write "$gnome_term_profiles${first_profile}background-color" "$bg_color"
        dconf write "$gnome_term_profiles${first_profile}foreground-color" "$fg_color"
    fi

    if [ "x$ZSH_THEME" != x ]; then
        source $ZSH/oh-my-zsh.sh
    fi

    # FZF
    _gen_fzf_default_opts
    [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
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

change_monitor_brightness() {
    xrandr --output HDMI-1-2 --brightness $1
}

# Wanted:
#    Gamma:      0.33:1.0:2.0 (bgr) (achieved with 3:1:0.5)
#    Brightness: 0.63
# TODO Still not as beautiful as Gnome Night Light.
change_monitor_gamma_brightness() {
    xrandr --output HDMI-1-2 --gamma $1 --brightness $2
}

alias untar="tar -xf"
alias untargz="tar -xzf"
alias egitig="\$EDITOR \$(git rev-parse --git-dir)/info/exclude"
alias pullsubs="find . -name '.git' -type d | xargs -P10 -I{} git --git-dir={} --work-tree=\$PWD/{}/.. pull"
alias emacs="emacsclient -c -a '' -F \"'(fullscreen . maximized)\""
alias maxcompr="7z a -t7z -mx9 -m0=lzma2 -mmt2 -md1024m"

