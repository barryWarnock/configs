                                        ; set global modes
;; midnight mode
(require 'midnight)
(midnight-delay-set `midnight-delay "5:00am")
;; word wrap
(global-visual-line-mode)

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
(spacemacs/set-leader-keys "wo" 'spacemacs/toggle-maximize-buffer)
;; bookmark bindings
(spacemacs/set-leader-keys "rb" 'bookmark-set)
(spacemacs/set-leader-keys "rj" 'bookmark-jump)
(spacemacs/set-leader-keys "rk" 'bookmark-delete)
(spacemacs/set-leader-keys "rl" 'list-bookmarks)
                                        ; project settings
;;; clearing the p prefix to remove all the existing projectile
(spacemacs/set-leader-keys "p" nil)
(spacemacs/declare-prefix "p" "Projects")

;;; TODO: make this language agnostic
(defun current-selection ()
  (interactive)
  (if (string-equal evil-state "visual")
      (buffer-substring evil-visual-beginning evil-visual-end)
    ""))
(defun ruby-look ()
  (interactive)
  (let* ((helm-ag-base-command "rg --smart-case --no-heading --color=never --line-number"))
    (helm-do-ag (projectile-project-root) nil (format "-g!*test* -g*.rb %s" (current-selection)))))

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
(spacemacs/set-leader-keys "pg" 'ruby-look)
;;; open last search
(spacemacs/set-leader-keys "pr" 'helm-resume)
;;; consider any directory with a dev.yml as a project
(setq project-vc-extra-root-markers '("dev.yml" "dev/"))

;;; how do you want to start a project?
(setq project-switch-commands (list '(project-find-file "Find file")
                                    '(project-find-dir "Find dir")
                                    '(project-switch-to-buffer "Open buffer")
                                    '(ruby-look "Find regexp")))

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

                                        ; git gutter
(custom-set-variables
 '(git-gutter:update-interval 5))
(add-hook 'ruby-mode-hook 'git-gutter-mode)

(start-process "echo" "*echo*" "echo" "somestring")
                                        ; lsp config
(defun format-current-rb-file ()
  (interactive)
  (start-process "format ruby"
                 "*RB-FMT*"
                 "/opt/dev/bin/dev"
                 "style"
                 "-a"
                 (spacemacs/copy-file-path)))

(setq lsp-disabled-clients '(rubocop-ls typeprof-ls))

                                        ; gpg config
(setq epg-pinentry-mode 'loopback)
(setq auth-sources
      '((:source "~/.authinfo.gpg")))

                                        ; ai config
(require 'org)
(with-eval-after-load 'org
  (load "~/.emacs/org-configs")
  (let ((local-config "~/.local-config"))
    (if (file-exists-p (concat local-config ".el"))
        (load local-config))))
