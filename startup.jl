using Statistics

if Sys.isunix()
    ENV["JULIA_EDITOR"] = "vim"
elseif Sys.iswindows()
    ENV["JULIA_EDITOR"] = "C:/Program\\ Files/vim/vim81/gvim.exe"
end

"Change the current terminal background from light to dark and vice versa."
function tbg()
    # The shell won't see these changes but it's still useful for us.
    if ENV["SOLARIZED_THEME"] == "dark"
        ENV["SOLARIZED_THEME"] = "light"
    else
        ENV["SOLARIZED_THEME"] = "dark"
    end

    gnome_term_profiles = "/org/gnome/terminal/legacy/profiles:/"
    first_profile = chomp(read(pipeline(`dconf list $gnome_term_profiles`, `head -n 1`), String))
    if ENV["SOLARIZED_THEME"] == "dark"
        bg_color = "'rgb(0,43,54)'"
        fg_color = "'rgb(131,148,150)'"
    else
        bg_color = "'rgb(253,246,227)'"
        fg_color = "'rgb(101,123,131)'"
    end
    run(`dconf write "$gnome_term_profiles$(first_profile)background-color" "$bg_color"`)
    run(`dconf write "$gnome_term_profiles$(first_profile)foreground-color" "$fg_color"`)

    #=
    if !isempty(ENV["ZSH_THEME"])
        run(`source $(ENV["ZSH"])/oh-my-zsh.sh`)
    end

    # FZF
    _gen_fzf_default_opts
    [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
    =#

    nothing
end

