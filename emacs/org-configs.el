                                        ; Org Mode specific configs

;; set file locations
(setq-default org-agenda-files '("~/org"))
(setq-default org-default-notes-file (concat org-directory "/notes.org"))

;; custom function to pull a git commit message from a WI
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
(spacemacs/set-leader-keys-for-major-mode 'org-mode "w" 'org-yank-wi-desc)

;; set capture templates
(setq-default org-capture-templates
              '(("w" "WI" entry (file+headline "~/org/work.org" "Stories")
                 "* TODO %^{name}%^{id}p%^{link}p\n%?")))

