                                        ; Org Mode specific configs

;; set file locations
(setq-default org-agenda-files '("~/org"))
(setq-default org-default-notes-file (concat org-directory "/notes.org"))

;; prettify
(setq org-superstar-leading-bullet ?\s)
(setq org-todo-keywords
      '((sequence "NEW" "IN-PROGRESS" "|" "DONE" "WAITING")))

;; custom function to pull a git commit message from a WI
(defun org-yank-wi-desc ()
  "if we're inside a WI TODO yank it's description in the format expected for a git commit"
  (interactive)
  (let ((org-props (org-entry-properties)))
    (cl-flet ((acdr
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
                 "* NEW %^{name}%^{id}p%^{link}p\n%?")
                ("n" "Quick Note" item (file org-default-notes-file)
                 " %?")))

;; open the default-notes-file
(defun switch-to-org-notes ()
  "Switch to the buffer showing the notes file"
  (interactive)
  (find-file org-default-notes-file))
(spacemacs/set-leader-keys "aon" 'switch-to-org-notes)

;; customize org-pomodoro
(setq org-pomodoro-finished-sound nil)
(add-hook 'org-pomodoro-finished-hook 'spotify-pause)
(add-hook 'org-pomodoro-finished-hook 'switch-to-org-notes)
(spacemacs/set-leader-keys-for-major-mode 'org-mode "p" 'org-pomodoro)
