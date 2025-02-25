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
;; close all other windows (window only)
(spacemacs/set-leader-keys "wo" 'spacemacs/toggle-maximize-buffer)
;; project bindings
;;; clearing the p prefix to remove all the existing projectile
(spacemacs/set-leader-keys "p" nil)
(spacemacs/declare-prefix "p" "Projects")
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
(spacemacs/set-leader-keys "pg" 'project-find-regexp)

; project settings
(setq project-switch-commands (list '(project-find-file "Find file")
                                    '(project-find-dir "Find dir")
                                    '(project-switch-to-buffer "Open buffer")
                                    '(project-find-regexp "Find regexp")
                                    '(project-shell "shell" "s")))

; avy settings
(spacemacs/set-leader-keys "jf" 'avy-goto-char)

; org configs
(with-eval-after-load 'org
  (load "~/.emacs/org-configs"))

; local configs (if they exist)
(let ((local-config "~/.local-config"))
  (if (file-exists-p (concat local-config ".el"))
      (load local-config)))

