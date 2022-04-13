                                        ; start as a daemon
(load "server")
(unless server-process (server-start))

                                        ; set global modes
;; show line numbers
(global-linum-mode)

                                        ; set vars
;; escape with kj
(setq-default evil-escape-key-sequence "kj")
;; don't yank replaced text when pasting in visual mode
(setq-default evil-kill-on-visual-paste nil)
;; emacs is a bit overzealous when it comes to highlighting
(setq-default evil-ex-search-highlight-all nil)
(setq-default vc-follow-symlinks t)
(setq-default nrepl-use-ssh-fallback-for-remote-hosts t)

                                        ; set keybinds
;; close all other windows (window only)
(spacemacs/set-leader-keys "wo" 'spacemacs/toggle-maximize-buffer)
;; spotify bindings
;;; clearing the s prefix to remove all the existing search bindings
(spacemacs/set-leader-keys "s" nil)
(spacemacs/declare-prefix "s" "Spotify")
(spacemacs/set-leader-keys "sp" 'spotify-playpause)
(spacemacs/set-leader-keys "sc" 'spotify-current)
(spacemacs/set-leader-keys "sn" 'spotify-next)

;; org configs
(load "~/.emacs/org-configs")

;; local configs (if they exist)
(let ((local-config "~/.local-config"))
  (if (file-exists-p (concat local-config ".el"))
      (load local-config)))
