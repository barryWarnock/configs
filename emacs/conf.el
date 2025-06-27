;; word wrap
(global-visual-line-mode)
;; enable using emacsclient from the terminal
(server-mode 1)
;; don't yank replaced text when pasting in visual mode
(setq-default evil-kill-on-visual-paste nil)
;; emacs is a bit overzealous when it comes to highlighting
(setq-default evil-ex-search-highlight-all nil)

(setq-default vc-follow-symlinks t)

(setq dired-dwim-target t)

(setq helm-ag-base-command "rg --smart-case --no-heading --color=never --line-number")

(setq xref-search-program 'ripgrep)

(spacemacs/set-leader-keys "jf" 'avy-goto-char)

(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
  (setq treemacs-text-scale +2)
  (setq treemacs-use-git-mode 'deferred)
  (setq treemacs-collapse-dirs 5)
  (setq treemacs-use-follow-mode t)
  (setq treemacs--icon-size 14))

(setq aw-dispatch-always t)

(setq compilation-scroll-output t)

(defun current-selection ()
  (interactive)
  (if (string-equal evil-state "visual")
      (buffer-substring evil-visual-beginning evil-visual-end)
    ""))

(defun insert-current-date ()
  (interactive)
  (insert (string-trim (shell-command-to-string "date +'%Y-%m-%d %A'"))))

;; :q muscle memory keeps killing my windows
(evil-ex-define-cmd "q[uit]" 'evil-write)
(evil-ex-define-cmd "x" 'evil-write)
;; ensure SPC-SPC works like M-x
(spacemacs/set-leader-keys "SPC" `spacemacs/helm-M-x-fuzzy-matching)

;; close all other windows
(spacemacs/set-leader-keys "wo" 'zoom-window-zoom)

;; bookmark bindings
(spacemacs/set-leader-keys "rb" 'bookmark-set)
(spacemacs/set-leader-keys "rj" 'bookmark-jump)
(spacemacs/set-leader-keys "rk" 'bookmark-delete)
(spacemacs/set-leader-keys "rl" 'list-bookmarks)

(spacemacs/set-leader-keys "ww" 'ace-window)

(spacemacs/set-leader-keys "=" 'calc-dispatch)

;; Spacemacs saves "o" for user defined keybinds
(spacemacs/declare-prefix "oi" "Insert")
(spacemacs/set-leader-keys "oid" 'insert-current-date)

;; clearing the p prefix to remove all the existing projectile
(spacemacs/set-leader-keys "p" nil)
(spacemacs/declare-prefix "p" "Projects")

;; consider any directory with a dev.yml as a project
(setq project-vc-extra-root-markers '("dev.yml" "dev/"))
;; only use projectile as a backup to find a project
(setq project-find-functions '(project-try-vc project-projectile))

(defun language-rg-filter ()
  (case major-mode
    ('ruby-mode "-g*.rb")
    ('rustic-mode "-g*.rs")
    (t ""))
  )

(defun project-look ()
  (interactive)
  (helm-do-ag
   (projectile-project-root) nil
   (format "-g!*test* %s %s" (language-rg-filter) (current-selection))))

;; switch between open projects
(spacemacs/set-leader-keys "pp" 'project-switch-project)
;; open in project
(spacemacs/set-leader-keys "pf" 'project-find-file)
;; open shell in project
(spacemacs/set-leader-keys "ps" 'project-shell)
;; open project buffer
(spacemacs/set-leader-keys "pb" 'project-switch-to-buffer)
;; kill all project buffers
(spacemacs/set-leader-keys "pk" 'project-kill-buffers)
;; find in project
(spacemacs/set-leader-keys "pg" 'project-look)
;; open last search
(spacemacs/set-leader-keys "pr" 'helm-resume)

(setq project-switch-commands (list '(project-find-file "Find file")
                                    '(project-find-dir "Find dir")
                                    '(project-switch-to-buffer "Open buffer")
                                    '(project-look "Find regexp")))

(custom-set-variables '(git-gutter:update-interval 5))
(add-hook 'prog-mode-hook 'git-gutter-mode)

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

(require 'org)
(with-eval-after-load 'org

  ;; return either the current org item or the currently clocked in item
  (defun org-current-or-clock ()
    (if (org-current-level)
        nil
      org-clock-marker))

  ;; populate a format string with a given list of org props
  (defun org-format-with-props (fstring prop-list)
    (let ((org-props (org-entry-properties (org-current-or-clock))))
      (apply 'format fstring
             (mapcar
              (lambda (key)
                (cdr (assoc key org-props)))
              prop-list))))


  ;; custom function to pull a git commit message from a WI
  (defun org-yank-wi-desc ()
    "if we're inside a WI yank its description in the format expected for a git commit"
    (interactive)
    (kill-new (org-format-with-props "%s\n\nissue: %s\n" '("ITEM" "LINK"))))

  (defun org-yank-wi-link ()
    "if we're inside a WI yank its link"
    (interactive)
    (kill-new (org-format-with-props "%s" '("LINK"))))

  ;; pull the issue number of the currently clocked in entry
  (defun wi-issue-num ()
    (car (last (split-string (org-entry-get (org-current-or-clock) "link") "/"))))

  (defun org-yank-wi-issue-num ()
    (interactive)
    (kill-new (wi-issue-num)))

  (defun switch-to-org-notes ()
    "Switch to the buffer showing the notes file"
    (interactive)
    (find-file org-default-notes-file))

  (setq-default org-agenda-files '("~/org" "~/workorg"))
  (setq-default org-default-notes-file (concat org-directory "/notes.org"))

  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 1)))
  ;; save target files after refile
  (add-hook 'org-after-refile-insert-hook 'projectile-save-project-buffers)

  ;; Stash archived items under a heading that includes the current month and year
  (require 'midnight)
  (midnight-delay-set `midnight-delay "5:00am")
  ;; archive to a sub heading
  (defun set-archive-heading ()
    (interactive)
    (setq-default org-archive-location (concat "%s_archive::** " (format-time-string "%B %Y"))))
  (set-archive-heading)
  (add-hook `midnight-hook `set-archive-heading)

  ;; org capture templates
  (setq org-capture-templates
        '(("w" "WI" entry (file "~/workorg/issues.org")
           "* TODO %^{name}%^{link}p\n%?")
          ("n" "Quick Note" item (file org-default-notes-file)
           " %?")))
  (setq-default org-capture-bookmark nil)

  ;; styling
  (setq org-superstar-leading-bullet ?\s)
  (setq org-todo-keywords
        '((sequence "TODO(t!)" "IN-PROGRESS(p!)" "|" "WAITING(w!)" "DONE(d!)")))

  (setq-default org-log-into-drawer "timestamps")
  (setq-default org-log-done nil)

  ;; work issue keybinds
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "w" 'org-yank-wi-desc)
  (spacemacs/declare-prefix "aoy" "Yank")
  (spacemacs/set-leader-keys "aoyw" 'org-yank-wi-desc)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "l" 'org-yank-wi-link)
  (spacemacs/set-leader-keys "aoyl" 'org-yank-wi-link)
  (spacemacs/set-leader-keys "aoyn" 'org-yank-wi-issue-num)

  ;; open notes file
  (spacemacs/set-leader-keys "aon" 'switch-to-org-notes)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)))

  )

(let ((local-config "~/.local-config"))
  (if (file-exists-p (concat local-config ".el"))
      (load local-config)))
