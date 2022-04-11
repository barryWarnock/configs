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

; set keybinds
;; close all other windows (window only)
(spacemacs/set-leader-keys "wo" 'spacemacs/toggle-maximize-buffer)

; org configs
(load "~/.emacs/org-configs")
