if Sys.isunix()
    ENV["JULIA_EDITOR"] = "vim"
elseif Sys.iswindows()
    ENV["JULIA_EDITOR"] = "C:/Program\\ Files/vim/vim81/gvim.exe"
end

