set nocompatible
syntax on

colorscheme apprentice
let g:vimroom_ctermbackground="none"
set encoding=UTF-8

" install vimplug if not found
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
call plug#begin('~/.vim/plugged')

" Let Vundle manage itself
Plug 'tpope/vim-fugitive'
Plug 'christoomey/vim-tmux-navigator'
Plug 'rust-lang/rust.vim' 
Plug 'majutsushi/tagbar'
Plug 'w0rp/ale'
Plug 'scrooloose/nerdtree'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'valloric/youcompleteme'
Plug 'klen/python-mode'
Plug 'wakatime/vim-wakatime'
Plug 'vim-airline/vim-airline'
Plug 'ryanoasis/vim-devicons'

" Required. plugins available after.
call plug#end()

" syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

"""" NerdTree
map <C-n> :NERDTreeToggle<CR>

" auto close if nerdtree is only one open 
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

let g:NERDTreeWinSize=40

"""" ctrlp
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'

"""""""" Language Settings 
" rust
let g:rustfmt_autosave = 1

" python
let g:pymode_options_colorcolumn = 0

" keybinds
nmap <F2> :TagbarToggle<CR>

