" Setup Neovim:
"   :call mkdir(stdpath('config'), 'p')
"   symlink this to :echo stdpath('config') . '/init.vim'
"   or alternatively
"   :exe 'edit ' . stdpath('config') . '/init.vim'
"   and paste

if has("unix")
    let g:mynvimhome='~/.vim/'
    let g:myvimrc='~/.vimrc'
elseif has("win32")
    let g:mynvimhome='~/vimfiles/'
    let g:myvimrc='~/_vimrc'
else
    throw('unknown operating system')
endif

execute 'set runtimepath^=' . g:mynvimhome
execute 'set runtimepath+=' . g:mynvimhome . 'after'
let &packpath = &runtimepath
execute 'source ' . g:myvimrc

