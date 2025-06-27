                                        ; set global modes
;; midnight mode
(require 'midnight)
(midnight-delay-set `midnight-delay "5:00am")
;; word wrap
(global-visual-line-mode)
;; enable using emacsclient from the terminal
(server-mode 1)

                                        ; set vars
;; don't yank replaced text when pasting in visual mode
(setq-default evil-kill-on-visual-paste nil)
;; emacs is a bit overzealous when it comes to highlighting
(setq-default evil-ex-search-highlight-all nil)
(setq-default vc-follow-symlinks t)
(setq-default nrepl-use-ssh-fallback-for-remote-hosts t)

;; using helm for large projects can be slow
(defun visit-project ()
  (if (projectile-project-buffers)
      (projectile-switch-to-buffer)
    (projectile-find-file)))
(setq-default projectile-switch-project-action 'visit-project)
(setq-default projectile-enable-caching t)

                                        ; set keybinds
;; :q muscle memory keeps killing my windows
(evil-ex-define-cmd "q[uit]" 'evil-write)
(evil-ex-define-cmd "x" 'evil-write)
;; SPC-SPC stopped working, fix it
(spacemacs/set-leader-keys "SPC" `spacemacs/helm-M-x-fuzzy-matching)
;; SPC-fd to open dired
(spacemacs/set-leader-keys "fd" (lambda () (interactive) (dired default-directory)))
(setq dired-dwim-target t)

;; close all other windows (window only)
(spacemacs/set-leader-keys "wo" 'zoom-window-zoom)
;; bookmark bindings
(spacemacs/set-leader-keys "rb" 'bookmark-set)
(spacemacs/set-leader-keys "rj" 'bookmark-jump)
(spacemacs/set-leader-keys "rk" 'bookmark-delete)
(spacemacs/set-leader-keys "rl" 'list-bookmarks)
                                        ; project settings
;;; clearing the p prefix to remove all the existing projectile
(spacemacs/set-leader-keys "p" nil)
(spacemacs/declare-prefix "p" "Projects")

(defun current-selection ()
  (interactive)
  (if (string-equal evil-state "visual")
      (buffer-substring evil-visual-beginning evil-visual-end)
    ""))

(defun language-rg-filter ()
  (case major-mode
        ('ruby-mode "-g*.rb")
        ('rustic-mode "-g*.rs")
        (t ""))
  )
(setq helm-ag-base-command "rg --smart-case --no-heading --color=never --line-number")
(defun project-look ()
  (interactive)
  (helm-do-ag (projectile-project-root) nil (format "-g!*test* %s %s" (language-rg-filter) (current-selection))))

(setq xref-search-program 'ripgrep)
;;; switch between open projects
(spacemacs/set-leader-keys "pp" 'project-switch-project)
;;; open in project
(spacemacs/set-leader-keys "pf" 'project-find-file)
;;; open shell in project
(spacemacs/set-leader-keys "ps" 'project-shell)
;;; open project buffer
(spacemacs/set-leader-keys "pb" 'project-switch-to-buffer)
;;; kill all project buffers
(spacemacs/set-leader-keys "pk" 'project-kill-buffers)
;;; find in project
(spacemacs/set-leader-keys "pg" 'project-look)
;;; open last search
(spacemacs/set-leader-keys "pr" 'helm-resume)
;;; consider any directory with a dev.yml as a project
(setq project-vc-extra-root-markers '("dev.yml" "dev/"))

;;; how do you want to start a project?
(setq project-switch-commands (list '(project-find-file "Find file")
                                    '(project-find-dir "Find dir")
                                    '(project-switch-to-buffer "Open buffer")
                                    '(project-look "Find regexp")))

;;; only use projectile as a backup to find a project
(setq project-find-functions '(project-try-vc project-projectile))

                                        ; avy settings
(spacemacs/set-leader-keys "jf" 'avy-goto-char)

                                        ; treemacs config
(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
  (setq treemacs-text-scale +2)
  (setq treemacs-use-git-mode 'deferred)
  (setq treemacs-collapse-dirs 5)
  (setq treemacs-use-follow-mode t)
  (setq treemacs--icon-size 14))

                                        ; ace config
(setq aw-dispatch-always t)
(spacemacs/set-leader-keys "ww" 'ace-window)
                                        ; window config
(setq compilation-scroll-output t)

                                        ; git gutter
(custom-set-variables
 '(git-gutter:update-interval 5))
(add-hook 'ruby-mode-hook 'git-gutter-mode)

                                        ; code config
;; jump to / from test
(defun jump-to-test ()
  (interactive)
  (let* ((file-name-raw (file-name-nondirectory buffer-file-name))
         (split-name (s-split "\\." file-name-raw))
         (file-name (car split-name))
         (file-ext (cadr split-name))
         (file-is-test (s-contains-p "test" file-name))
         (file-name-generic (car (s-split "\\(-\\|_\\|\\)test" file-name)))
         (file-name-query (s-join "" (list file-name-generic (if (not file-is-test) "_test"))))
         (query (format "%s.%s" file-name-query file-ext ))
         (project-root (project-root (project-current t))))
    (find-name-dired project-root query)))
(spacemacs/set-leader-keys-for-major-mode 'ruby-mode "tg" 'jump-to-test)

;; format ruby files on save
(defun format-current-rb-file ()
  (interactive)
  (start-process "format ruby"
                 "*RB-FMT*"
                 "/opt/dev/bin/dev"
                 "style"
                 "-a"
                 (spacemacs/copy-file-path)))
(spacemacs/set-leader-keys-for-major-mode 'ruby-mode "==" 'format-current-rb-file)

(advice-add 'lsp-ruby-lsp--build-command :override
            (lambda () '("shadowenv" "exec" "--" "ruby-lsp")))

(setq lsp-sorbet--base-command '("shadowenv" "exec" "--" "srb" "tc" "--lsp" "--disable-watchman"))
(advice-add 'lsp-sorbet--build-command :override
            (lambda () (pcase (project-name (project-current))
                         ("shopify" lsp-sorbet--base-command)
                         (t (cl-concatenate 'list lsp-sorbet--base-command
                                            (list "--dir" (file-relative-name (project-root (project-current)))))))))

(setq lsp-disabled-clients '(rubocop-ls typeprof-ls ruby-ls ai-ls ruby-syntax-tree-ls helix-gpt))

;; insert the current date
(defun insert-current-date ()
  (interactive)
  (insert (string-trim (shell-command-to-string "date +'%Y-%m-%d %A'"))))
(spacemacs/declare-prefix "oi" "Insert")
(spacemacs/set-leader-keys "oid" 'insert-current-date)

                                        ; gpg config
(setq epg-pinentry-mode 'loopback)
(setq auth-sources
      '((:source "~/.authinfo.gpg")))

                                        ; calc mode config
(spacemacs/set-leader-keys "=" 'calc-dispatch)

                                        ; require other configs
(require 'org)
(with-eval-after-load 'org
  (load "~/.emacs/org-configs")
  (let ((local-config "~/.local-config"))
    (if (file-exists-p (concat local-config ".el"))
        (load local-config))))
