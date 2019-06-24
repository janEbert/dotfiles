" Setup Neovim:
"   :call mkdir(stdpath('config'), 'p')
"   symlink this to :echo stdpath('config') . '/init.vim'
"   or alternatively
"   :exe 'edit ' . stdpath('config') . '/init.vim'
"   and paste

set runtimepath^=~/.vim
set runtimepath+=~/.vim/after
let &packpath = &runtimepath
source ~/.vimrc

