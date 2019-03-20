" Use system clipboard
set clipboard=unnamedplus

" auto change directory 
set autochdir

" no decorations in gui mode
set guioptions=pi "scrollbar

" show line numbers
set number

" leader key spacemacs style bindings
nmap <SPACE> <leader>
nmap <leader><SPACE> :
nmap <leader>ff :Files ~<CR>
nmap <leader>fs :w<CR>
nmap <leader>fe :edit ~/.vimrc<CR>
nmap <leader>F :edit 
nmap <leader>bb :Buffers<CR>
nmap <leader>bd :bd<CR>
nmap <leader>n :NERDTreeToggle<CR>

" dont yank pasted over text
vnoremap p "_dP

" easier split movement / bindings
set splitbelow
set splitright
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Persistent undo
set hidden
set undofile
set undodir=$HOME/.vim/undo

set undolevels=1000
set undoreload=10000

" dont overwrite symlinks when saving
set backupcopy=auto

" filetype specific settings

" YAML
autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab

" Plugins "

" Install and run vim-plug on first run
if empty(glob('~/.vim/autoload/plug.vim'))
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

so ~/.vim/plugins.vim

colorscheme jellybeans
set t_Co=256

let g:airline_powerline_fonts = 1
