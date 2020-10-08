" Use system clipboard
set clipboard=unnamedplus
autocmd VimLeave * call system("echo -n $'" . escape(getreg(), "'") . "' | xsel -ib")

" auto change directory 
autocmd BufEnter * silent! lcd %:p:h

" no decorations in gui mode
set guioptions=pi "scrollbar
" use Jetbrains Mono at a size appropriate for a 4k monitor
set guifont=JetBrains\ Mono\ 14

" show line numbers
set number

" word wrap at word breaks
set linebreak

" incremental search
set incsearch

" spellcheck toggle with f5
:noremap <F5> :setlocal spell! spelllang=en_ca<CR>

" 2 space tabs
set tabstop=2 shiftwidth=2 noexpandtab

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

" ; to open command history
nmap ; q:

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

" Local configs
if !empty(glob("~/.vim/localrc"))
    so ~/.vim/localrc
endif

" VimWiki
let g:vimwiki_listsyms = ' oOX'
" list all todos
nmap <leader>ta :Ack! '\[[ oOX]] ' ~/vimwiki<CR>
" list in progress todos
nmap <leader>tt :Ack! '\[[oO ]*] ' ~/vimwiki<CR>
" list all wiki files in a searchable way
nmap <leader>fw :Files ~/vimwiki<CR>
" open a general purpose todos buffer
nmap <leader>ft :e ~/vimwiki/ToDos.wiki<CR>

" when on the beginning of a line on the Stories page create the CL
" description on that page
nmap <leader>wi /W-vee"wyW"ty$b"tPakjo@jk"wpa@@rev _@jk

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
