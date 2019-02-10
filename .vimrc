" Jan's vimrc
"
" On Windows at `~/_vimrc`.
" Directories in `g:myvimhome`:
"   colors: usercolors
"   <g:plugindir>: path to the plugin dirs is `./*/{start,opt}/`
"   sessions: saved sessions
"   tags: global tags files
"   tmp: backups, undofiles, ...
"
" Colors:
"   - [Tomorrow themes not in base16 repo](https://github.com/chriskempson/tomorrow-theme)
"
" Optional plugins:
"   - [ale](https://github.com/w0rp/ale)
"   - [nerdtree](https://github.com/scrooloose/nerdtree)
"   - [ultisnips](https://github.com/SirVer/ultisnips)
"   - [vim-autoclose](https://github.com/spf13/vim-autoclose)
"   - [VimCompletesMe](https://github.com/ajh17/VimCompletesMe)
"   - [vim-snippets](https://github.com/honza/vim-snippets)
"   - [vimtex](https://github.com/lervag/vimtex)
"
" Default plugins (! at EOL marks more important ones):
"   - [Apprentice](https://github.com/romainl/Apprentice)
"   - [base16-vim](https://github.com/chriskempson/base16-vim)
"   - [ctrlp.vim](https://github.com/ctrlpvim/ctrlp.vim)!
"   - [gutentags_plus](https://github.com/skywind3000/gutentags_plus)!
"   - [julia-vim](https://github.com/JuliaEditorSupport/julia-vim)!
"   - [peaksea](https://github.com/vim-scripts/peaksea)
"   - [tabular](https://github.com/godlygeek/tabular)
"   - [targets.vim](https://github.com/wellle/targets.vim)
"   - [vim-abolish](https://github.com/tpope/vim-abolish)!
"   - [vimagit](https://github.com/jreybert/vimagit)
"   - [vim-asterisk](https://github.com/haya14busa/vim-asterisk)!
"   - [vim-characterize](https://github.com/tpope/vim-characterize)
"   - [vim-colors-solarized](https://github.com/altercation/vim-colors-solarized/)!
"   - [vim-commentary](https://github.com/tpope/vim-commentary)!
"   - [vim-fugitive](https://github.com/tpope/vim-fugitive)!
"   - [vim-git](https://github.com/tpope/vim-git)!
"   - [vim-gotham](https://github.com/whatyouhide/vim-gotham)
"   - [vim-gutentags](https://github.com/ludovicchabant/vim-gutentags)!
"   - [vim-lion](https://github.com/tommcdo/vim-lion)!
"   - [vim-monokai](https://github.com/sickill/vim-monokai)
"   - [vim-repeat](https://github.com/tpope/vim-repeat)!
"   - [vim-sneak](https://github.com/justinmk/vim-sneak)!
"   - [vim-speeddating](https://github.com/tpope/vim-speeddating)
"   - [vim-surround](https://github.com/tpope/vim-surround)!
"   - [vim-unimpaired](https://github.com/tpope/vim-unimpaired)!
"   - [vim-wordmotion](https://github.com/chaoren/vim-wordmotion)!
"   - [Zenburn](https://github.com/jnurmine/Zenburn)

if v:progname =~? "evim" || exists('g:myvimrcloaded')
    finish
endif

if &compatible
    set nocompatible
endif

" When the +eval feature is missing, the set command above will be skipped.
" Use a trick to reset compatible only when the +eval feature is missing.
silent! while 0
    set nocompatible
silent! endwhile


set encoding=utf-8
set langmenu=en_US.UTF-8

" per system config
if has("unix")
    language en_US.UTF-8
    let g:myvimhome='~/.vim/'

    let g:myctagsbin='/homes/jebert/.local/bin/uctags'
    let g:mygtagsbin='/homes/jebert/.local/bin/gtags'
    let g:mycscopebin='/homes/jebert/.local/bin/gtags-cscope'

    let g:mytexviewer='xdg-open'
elseif has("win32")
    language en
    let g:myvimhome='~/vimfiles/'

    let g:myctagsbin='C:\Users\jan\Downloads\tags\ctags-2018-11-27_a74b8474-x64\ctags.exe'
    let g:mygtagsbin='C:\Users\jan\Downloads\tags\glo662wb\bin\gtags.exe'
    let g:mycscopebin='C:\Users\jan\Downloads\tags\glo662wb\bin\gtags-cscope.exe'

    let g:mytexviewer='C:\Program Files\SumatraPDF\SumatraPDF.exe'
else
    throw 'unknown operating system'
endif

let g:myplugindir = 'pack/'
let g:mydefaultcolors = 'solarized'

" We do not need defaults
let skip_defaults_vim = 1
" source $VIMRUNTIME/vimrc_example.vim

if has("win32")
    set diffexpr=MyDiff()
    function! MyDiff()
      let opt = '-a --binary '
      if &diffopt =~ 'icase' | let opt = opt . '-i ' | endif
      if &diffopt =~ 'iwhite' | let opt = opt . '-b ' | endif
      let arg1 = v:fname_in
      if arg1 =~ ' ' | let arg1 = '"' . arg1 . '"' | endif
      let arg1 = substitute(arg1, '!', '\!', 'g')
      let arg2 = v:fname_new
      if arg2 =~ ' ' | let arg2 = '"' . arg2 . '"' | endif
      let arg2 = substitute(arg2, '!', '\!', 'g')
      let arg3 = v:fname_out
      if arg3 =~ ' ' | let arg3 = '"' . arg3 . '"' | endif
      let arg3 = substitute(arg3, '!', '\!', 'g')
      if $VIMRUNTIME =~ ' '
        if &sh =~ '\<cmd'
          if empty(&shellxquote)
            let l:shxq_sav = ''
            set shellxquote&
          endif
          let cmd = '"' . $VIMRUNTIME . '\diff"'
        else
          let cmd = substitute($VIMRUNTIME, ' ', '" ', '') . '\diff"'
        endif
      else
        let cmd = $VIMRUNTIME . '\diff'
      endif
      let cmd = substitute(cmd, '!', '\!', 'g')
      silent execute '!' . cmd . ' ' . opt . arg1 . ' ' . arg2 . ' > ' . arg3
      if exists('l:shxq_sav')
        let &shellxquote=l:shxq_sav
      endif
    endfunction
endif

function! SaveSess(name_)
    name = a:name_
    if strlen(name <= 0)
        echo "Name cannot be empty"
        return
    endif
    let prefix = g:myvimhome . "sessions/" . name
    let split_list = split(name, "\.")
    if split_list[-1] == "vim"
        mksession! name
    else
        mksession! name . ".vim"
    endif
endfunction

" Toggle the background from dark to light.
function! ToggleBackground()
    " Save the current color scheme
    " (in case changing the background messes something up).
    let currcolors = get(g:, 'colors_name', g:mydefaultcolors)
    if &background == 'dark'
        set background=light
    else
        set background=dark
    endif
    " Load the saved colorscheme unless it is the default.
    " If nothing messed up inbetween, this will not accidentally
    " switch to the default *but* makes toggling much faster.
    if currcolors != g:mydefaultcolors
        exec 'colorscheme ' . currcolors
    endif
endfunction

" Execute git pull in all subdirectories of the current directory.
function! PullSubgits()
    if g:loaded_fugitive
        let folders = split(globpath('.', '*'), '\n')
        call filter(folders, 'isdirectory(v:val)')
        for path in folders
            execute 'cd ' . path
            " Run asynchronously with !start on Windows, with ! on Unix.
            if has("unix")
                silent execute '!git pull &'
            elif has("win32")
                silent execute '!start /b git pull'
            endif
            cd ..
        endfor
    else
        echo 'fugitive is not loaded'
    endif
endfunction

" Update all plugins in the <g:myvimhome><g:myplugindir> subdirectories.
function! UpdatePlugins()
    if g:loaded_fugitive
        let orig_folder = getcwd()
        exec 'cd ' . g:myvimhome . g:myplugindir
        let outer_folders = split(globpath('.', '*'), '\n')
        for outer in outer_folders
            execute 'cd ' . outer
            let folders = split(globpath('.', '*'), '\n')
            call filter(folders, 'isdirectory(v:val)')
            for path in folders
                execute 'cd ' . path
                execute PullSubgits()
                cd ..
            endfor
            cd ..
        endfor
        execute 'cd ' . orig_folder
    else
        echo 'fugitive is not loaded'
    endif
endfunction

if !exists(':UpdatePlugins')
    command UpdatePlugins call UpdatePlugins()
endif

" Options

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

if has("nvim")
    set termguicolors
    set guicursor=
    " let $TERM=xterm
    autocmd OptionSet guicursor noautocmd set guicursor=
elseif has("unix")
    set term=xterm
    set t_Co=16
    let g:solarized_termcolors=16
elseif has("win32")
    set termguicolors
    set t_Co=256
endif

if has("vms")
    set nobackup  " do not keep a backup file, use versions instead
else
    set backup  " keep a backup file (restore to previous version)
    if has('persistent_undo')
        set undofile  " keep an undo file (undo changes after closing)
    endif
endif

if has("win32")
    set pyxversion=0
    let $PATH .= ';C:\Users\jan\Anaconda3;C:\Users\jan\Anaconda3\envs\gans;C:\Users\jan\Anaconda3\envs\py27'
endif

set history=500  " lines of command history to keep
set number relativenumber  " display line number and relative ones
set ruler  " cursor position at bottom
set showcmd  " display incomplete commands
set showmode  " display current mode
set wildmode=longest,list,full  " zsh like tab completion
set wildmenu  " display completion matches in status line
set visualbell  " do not ring
set display=truncate  " show @@@ in the last line if truncated
set hidden  " work with hidden buffers
set laststatus=2
set shortmess-=c
set scrolloff=2  " vertical space between cursor and display end
set sidescrolloff=2  " horizontal space between cursor and display end
set formatoptions+=jwn
set formatlistpat=^\\s*\\d\\+[\\]:.)}\\t]\\s*
set nojoinspaces
set linebreak
set diffopt+=vertical  " split diffs vertically
set cryptmethod=blowfish2  " securest crypt in vim
set pastetoggle=<F2>

" left side: file path, file type-related stuff, buffer number
" right side: [textwidth, linenumber] line, col %buffer location
set statusline=%<%f\ %h%m%r%w%y\ %n\ %=%{gutentags#statusline()}\ [%{&textwidth?&textwidth.',':''}%L]\ %-14.(%l,%c%V%)\ %P

set fileformat=unix  " no crlf

set path+=**  " smarter path for find and other commands

set completeopt+=longest

set nrformats-=octal  " no octal numbers for <C-A> and <C-X>

set colorcolumn+=73,80,101,121  " several syntax line rulers

set splitbelow  " new horizontal split below
set splitright  " new vertical split on the right

if !has('nvim')
    set ttimeout
    set ttimeoutlen=100  " more time for commands
endif

" Activate if it is possible to timeout.
if has('reltime')
    set incsearch  " search while typing
endif

set ignorecase  " ignore case by default
" but do not ignore case when uppercase letters are present
set smartcase

" central vim file locations
execute 'set backupdir=' . g:myvimhome . 'tmp//,.'
execute 'set directory=' . g:myvimhome . 'tmp//,.'
execute 'set undodir='   . g:myvimhome . 'tmp//,.'

" Python style by default
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab

if has('syntax')
    set foldmethod=syntax
else
    set foldmethod=indent
endif
set foldlevel=99

" sexy whitespaces
set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
set list

" Maybe other order
execute 'set tags+=' . g:myvimhome . 'tags,./tags,../tags'
set cscopetag
exec 'set cscopeprg=' . g:mycscopebin

set sessionoptions-=options
set sessionoptions-=buffers
set sessionoptions+=localoptions

digraph <3 10084
digraph 69 128169
digraph v_ 8891
digraph (/ 8713
digraph /) 8716

" Autoclose (plugin)
let g:autoclose_vim_commentmode = 1

" Asterisk
map *  <Plug>(asterisk-z*)
map #  <Plug>(asterisk-z#)
map g* <Plug>(asterisk-gz*)
map g# <Plug>(asterisk-gz#)
" let g:asterisk#keeppos = 1

" Sneak
" nmap gs <Plug>Sneak_s
" nmap gS <Plug>Sneak_S
" xmap gs <Plug>Sneak_s
" xmap gS <Plug>Sneak_S
" omap gs <Plug>Sneak_s
" omap gS <Plug>Sneak_S

" Lion
let g:lion_squeeze_spaces = 1

" Wordmotions
" let g:wordmotion_prefix = '<Leader>'
let g:wordmotion_mappings = {
\    'w' : '<A-w>',
\    'b' : '<A-b>',
\    'e' : '<A-e>',
\    'ge' : '<A-g>',
\    'aw' : '<A-a>',
\    'iw' : '<A-i>',
\    '<C-R><C-W>' : '<A-r>'
\}

" Gutentags (Plus)
let g:gutentags_dont_load = 1

let g:gutentags_ctags_executable = g:myctagsbin
let g:gutentags_gtags_executable = g:mygtagsbin
let g:gutentags_gtags_cscope_executable = g:mycscopebin

" enable gtags module
let g:gutentags_modules = ['ctags', 'gtags_cscope']

" config project root markers.
let g:gutentags_project_root = ['.root']

" generate datebases in my cache directory, prevent gtags files polluting my project
let g:gutentags_cache_dir = expand(g:myvimhome . 'tags')

" forbid gutentags adding gtags databases
let g:gutentags_auto_add_gtags_cscope = 0

" vimtex
let g:vimtex_view_general_viewer = g:mytexviewer

" ALE
let g:ale_enabled = 0

" Make ALE nice. Enable if lint_on_text_changed is not 'never'
" let g:ale_command_wrapper = 'nice -n5'

let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_save = 1
let g:ale_lint_on_enter = 1
let g:ale_lint_on_filetype_changed = 1

let g:ale_completion_enabled = 1
let g:ale_completion_max_suggestions = 12

let g:ale_warn_about_trailing_blank_lines = 1
let g:ale_warn_about_trailing_whitespace = 1

let g:ale_set_highlights = 0
let g:ale_maximum_file_size = 1000000  " do not lint files > 1 MB

" UltiSnips
" Trigger configuration
let g:UltiSnipsExpandTrigger       = "<c-s>"
let g:UltiSnipsListSnippets        = "<a-s>"
let g:UltiSnipsJumpForwardTrigger  = "<c-f>"
let g:UltiSnipsJumpBackwardTrigger = "<c-b>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit = "vertical"

" Abbreviations
iabbrev #i #include
iabbrev #d #define

" Fix ALT keys in Unix
for i in range(97, 122)
    let c = nr2char(i)
    exec "map \e" . c . " <a-" . c . ">"
    exec "map! \e" . c . " <a-" . c . ">"
endfor

" Don't use Ex mode, use Q for formatting
map Q gq

" CTRL-C behaves like Esc in insert mode
inoremap <C-C> <Esc>

" CTRL-U in insert mode deletes a lot. Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
inoremap <C-U> <C-G>u<C-U>

" Yank until end of line
nnoremap Y y$

" Yank into clipboard
nnoremap <a-y> "+y
vnoremap <a-y> "+y

" Paste from clipboard
inoremap <a-p> <C-R>+
nnoremap <a-p> "+p
nnoremap <a-u> "+P
nnoremap <a-t> :pu +<CR>

" Substitute using CTRL-S
nnoremap <C-S> s

" Strip trailing whitespaces
nnoremap <Leader>s :%s/\s\+$//e<Bar>noh<CR>``

" Execute currently selected Vimscript
nnoremap <silent> <Leader>e "vyy:@v<CR>
vnoremap <silent> <Leader>e "vy:@v<CR>

" Split line at next/previous space
nnoremap <C-j> f r<CR>l
nnoremap g<C-j> F r<CR>l

" ALT-X works as <Del>
inoremap <A-x> <Del>
cnoremap <A-x> <Del>

" Commandline movement
cnoremap <a-h> <Left>
cnoremap <a-j> <Down>
cnoremap <a-k> <Up>
cnoremap <a-l> <Right>

" Split and ex mode movement
nnoremap <a-h> <c-w>h
nnoremap <a-j> <c-w>j
nnoremap <a-k> <c-w>k
nnoremap <a-l> <c-w>l
xnoremap <a-h> <c-w>h
xnoremap <a-j> <c-w>j
xnoremap <a-k> <c-w>k
xnoremap <a-l> <c-w>l

" Path from active file
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

" Autoclose
inoremap (<CR> (<CR>)<C-O>O
inoremap {<CR> {<CR>}<C-O>O
inoremap [<CR> [<CR>]<C-O>O
inoremap """ """"""<C-O>2h
inoremap ,end <CR>end<C-O>O
inoremap `` ````<C-O>h

" Background toggle
nnoremap <silent> <F9> :call ToggleBackground()<CR>

" Section or function movement fix (from :h motions.txt)

" For '{' not at beginning of line
map <silent> [[ ?{<CR>:noh<CR>w99[{
map <silent> ]] j0[[%/{<CR>:noh<CR>

" For '}' not at beginning of line
" map ][ /}<CR>:noh<CR>b99]}
" map [] k$][%?}<CR>:noh<CR>

" Use <C-L> to clear the highlighting of :set hlsearch.
if maparg('<C-L>', 'n') ==# ''
  nnoremap <silent> <C-L> :noh<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR><C-L>
endif

" Do not use mouse in insert mode
if has("mouse")
    set mouse=nv
endif

" Syntax highlighting when the terminal supports is or GVim is running
if has("gui_running") || &t_Co > 2
    " GVim options
    if has("gui_running")
        set guifont=DejaVu_Sans_Mono:h12
        set guioptions-=m
        if has('win32')
            set guioptions-=t
        endif
        set guioptions-=T
        set guioptions-=e
        set lines=999 columns=999  " Max height and width
    endif
    if has('syntax') && !exists('g:syntax_on')
        syntax enable
    endif

    " highlight strings in C comments
    " let c_comment_strings=1

    " colorscheme desert  " fallback
    set background=dark
    exec 'colorscheme ' . g:mydefaultcolors

    set hlsearch  " highlight search matches
endif

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
    command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis
            \ | wincmd p | diffthis
endif

if has("autocmd")
    filetype plugin indent on

    " Put these in an autocmd group, so that you can revert them with:
    " :augroup vimStartup | au! | augroup END
    augroup vimStartup
        au!

        " When editing a file, always jump to the last known cursor position.
        " Don't do it when the position is invalid, when inside an event handler
        " (happens when dropping a file on gvim) and for a commit message (it's
        " likely a different one than last time).
        autocmd BufReadPost *
                    \ if line("'\"") >= 1 && line("'\"") <= line("$") 
                    \         && &ft !~# 'commit'
                    \ |   exe "normal! g`\""
                    \ | endif

    augroup END

    augroup vimrcEx
        au!

        " For all text files set 'textwidth' to 78 characters.
        autocmd FileType text setlocal textwidth=78

    augroup END

    " C style
    autocmd FileType c
                \ setlocal tabstop=8 shiftwidth=8 softtabstop=8
                \ noexpandtab cindent
    " undo '{' at beginning of line fix
    autocmd FileType c unmap <buffer> [[
    autocmd FileType c unmap <buffer> ]]

    " Julia autoclose
    " autocmd FileType julia inoremap <buffer> <silent> ,end <CR>end<C-O>O

else
    set autoindent
endif " has("autocmd")

if has('langmap') && exists('+langremap')
    " Prevent that the langmap option applies to characters that result
    " from a mapping.  If set (default), this may break plugins
    " (but it's backward compatible).
    set nolangremap
endif

set textwidth=0

" set exrc
" set secure

if has('syntax') && has('eval')
    packadd! matchit
endif

" bundle plugins
" --------------
" packadd! ultisnips
packadd! vim-snippets
packadd! VimCompletesMe
packadd! vimtex
" packadd! vim-autoclose
" packadd! nerdtree
" packadd! ale

let g:myvimrcloaded = 1

