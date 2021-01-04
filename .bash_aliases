# -*- mode: sh; sh-shell: zsh -*-

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

nofj() {
    args=( "$@" )
    num_args="${#args[@]}"
    args_after_2=( "${args[@]:1:$num_args}" )

    executable="$1"
    executable_path="/usr/bin/$executable"

    # TODO follow original link; only if it's firejail, do the following
    if [ -x "$executable_path" ]; then
        "$executable_path" "${args_after_2[@]}"
    else
        # TODO try different path locations instead until a non-firejail link is found
        echo "Could not find $executable_path; please fix the $0 function."
    fi
}

github_release_download() {
    if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
        echo "Syntax: $0 <path/to/update> <github/release/url> <github_release_name>"
        echo "  Release URLs are of the form"
        echo "  https://api.github.com/repos/<organization>/<repo>/releases/latest"
        return 0
    fi

    path_to_update=$1
    github_release_url=$2
    # TODO If not given, get this from the url
    github_release_name=$3

    release_json=$(wget -qO - "$github_release_url" \
                       | jq '.assets[] | select(.name == "$github_release_name")')
    release_download_url=$(echo "$release_json" | jq -r '.browser_download_url')

    if ![ -f "$path_to_update" ]; then
        # TODO If necessary, uncomment below or take extra arg for the ignore folder
        # firejail --ignore='read-only ${HOME}/.local/bin'
        wget -O "$path_to_update" "$github_release_url"
        return 0;
    fi

    local_last_modified=$(date -uI's' -r "$path_to_update")
    release_last_modified=$(date -uI's' -d \
                                       $(echo "$release_json" | jq -r '.updated_at'))

    if [ "$local_last_modified" \< "$release_last_modified" ]; then
        # TODO If necessary, uncomment below or take extra arg for the ignore folder
        # firejail --ignore='read-only ${HOME}/.local/bin'
        wget -O "$path_to_update" "$github_release_url"
    fi
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


source-nvm() {
    # Load nvm
    [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
    # Load nvm bash_completion
    [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"
}

closure-compile() {
    if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
        echo "Syntax: $0 [[--compilation_level ADVANCED_OPTIMIZATIONS] ...]" \
             "<file_to_compile>.js"
        return 0
    fi

    closure_compiler_cmd=( java -jar ~/Downloads/closure-compiler/closure-compiler-v20200517.jar )
    # closure_compiler_cmd=( npx google-closure-compiler )

    # Separate last argument from others
    args=( "$@" )
    num_args="${#args[@]}"
    last_arg="${args[$num_args]}"
    args_except_last=( "${args[@]:0:$num_args - 1}" )

    minified_name="$(dirname "$last_arg")/$(basename "$last_arg" .js).min.js"

    # Can also do --compilation_level ADVANCED_OPTIMIZATIONS but be careful!
    "${closure_compiler_cmd[@]}" \
        "${args_except_last[@]}" --js "$last_arg" --js_output_file "$minified_name"
}

closure-compile-o() {
    if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
        echo "Syntax: $0 [...] <file_to_compile>.js"
        return 0
    fi
    closure-compile --compilation_level ADVANCED_OPTIMIZATIONS "$@"
}

closure-compile-css() {
    if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
        echo "Syntax: $0 [...] <file_to_minify>.css"
        return 0
    fi

    # Separate last argument from others
    args=( "$@" )
    num_args="${#args[@]}"
    last_arg="${args[$num_args]}"
    args_except_last=( "${args[@]:0:$num_args - 1}" )

    minified_name="$(dirname "$last_arg")/$(basename "$last_arg" .css).min.css"

    java -jar ~/Downloads/closure-stylesheets.jar \
         "${args_except_last[@]}" --output-file "$minified_name" "$last_arg"
}

elisp() {
    \emacs --batch --eval '(princ (format "%s\n" '"$1))"
}

alias untar="tar -xf"
alias untargz="tar -xzf"
alias egitig="\$EDITOR \$(git rev-parse --git-dir)/info/exclude"
alias pullsubs="find . -name '.git' -type d | xargs -P10 -I{} git --git-dir={} --work-tree=\$PWD/{}/.. pull"
alias emacs="emacsclient -c -a ''"
alias dex="stack exec dex --"
alias rgp="rg --pre to_text"
alias maxcompr="7z a -t7z -mx9 -m0=lzma2 -mmt2 -md1024m"
