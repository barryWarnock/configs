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
(defun org-yank-wi-desc ()
  "if we're inside a WI TODO yank it's description in the format expected for a git commit"
  (interactive)
  (let ((org-props (org-entry-properties)))
    (letf ((acdr
            (lambda (mapping key)
              (cdr (assoc key mapping)))))
      (kill-new (apply 'format "%s\n@%s@\n%s"
                       (mapcar (lambda (key)
                                 (acdr org-props key))
                               '("ITEM" "ID" "LINK")))))))
(setq-default org-agenda-files '("~/org"))
(setq-default org-default-notes-file (concat org-directory "/notes.org"))
(setq-default org-capture-templates
      '(("w" "WI" entry (file+headline "~/org/work.org" "Stories")
         "* TODO %^{name}%^{id}p%^{link}p\n%?")))

(spacemacs/set-leader-keys-for-major-mode 'org-mode "w" 'org-yank-wi-desc)
