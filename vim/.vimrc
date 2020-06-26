" Use system clipboard
set clipboard=unnamedplus
autocmd VimLeave * call system("echo -n $'" . escape(getreg(), "'") . "' | xsel -ib")

" auto change directory 
autocmd BufEnter * silent! lcd %:p:h

" no decorations in gui mode
set guioptions=pi "scrollbar

" show line numbers
set number

" incremental search
set incsearch

" 4 space tabs
set tabstop=4 shiftwidth=4 expandtab

" highlight line when in insert
autocmd InsertEnter * set cul
autocmd InsertLeave * set nocul

" leader key spacemacs style bindings
nmap <SPACE> <leader>
map <Leader> <Plug>(easymotion-prefix)
nmap <leader><SPACE> :
nmap <leader>fs :w<CR>
nmap <leader>fe :edit ~/.vimrc<CR>
nmap <leader>fr :source ~/.vimrc<CR>
nmap <leader>e :edit 
nmap <leader>F :Files<CR>
nmap <leader>G :GFiles<CR>
nmap <leader>bb :Buffers<CR>
nmap <leader>bd :bd<CR>
nmap <leader>n :NERDTree<CR>
nmap <leader>c :!

" dont yank pasted over text
vnoremap p "_c<esc>p

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

" hit j and k at the same time to go back to normal mode
inoremap jk <Esc>
inoremap kj <Esc>
" temp binding to train myself to use the above
inoremap <C-[> <C-o>:echoerr "use the new binding Fool!"<CR>

" Don't dirty up directories with files I don't need to persist
set backupdir=/tmp//
set directory=/tmp//

" Save file when leaving buffer
:autocmd FocusLost * silent! :wa

" dont overwrite symlinks when saving
set backupcopy=auto

" YAML
autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab

" Local configs
if !empty(glob("~/.vim/localrc"))
    so ~/.vim/localrc
endif

" VimWiki
let g:vimwiki_listsyms = ' oOX'
nmap <leader>ta :Ack! '\[[ oOX]] ' ~/vimwiki<CR>
nmap <leader>tt :Ack! '\[[oO ]*] ' ~/vimwiki<CR>
nmap <leader>wa :Ack! '\[\[.*?]]' ~/vimwiki<CR>
nmap <leader>fw :Files ~/vimwiki<CR>

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