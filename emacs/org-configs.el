                                        ; Org Mode specific configs
;; set file locations
(setq-default org-agenda-files '("~/org"))
(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 1)))
(setq-default org-default-notes-file (concat org-directory "/notes.org"))

;; prettify
(setq org-superstar-leading-bullet ?\s)
(setq org-todo-keywords
      '((sequence "NEW" "IN-PROGRESS" "|" "DONE")))

;; populate a format string with a given list of org props
(defun org-format-with-props (fstring prop-list)
  (let ((org-props (org-entry-properties)))
      (apply 'format fstring
             (mapcar
              (lambda (key)
                (cdr (assoc key org-props)))
              prop-list))))

;; custom function to pull a git commit message from a WI
(defun org-yank-wi-desc ()
  "if we're inside a WI yank it's description in the format expected for a git commit"
  (interactive)
  (kill-new (org-format-with-props "%s\n\n@%s@\n%s" '("ITEM" "ID" "LINK"))))
(spacemacs/set-leader-keys-for-major-mode 'org-mode "w" 'org-yank-wi-desc)

;; custom function to pull a git commit message from a WI
(defun org-yank-wi-link ()
  "if we're inside a WI yank a slack formatted link to the WI"
  (interactive)
  (kill-new (org-format-with-props "[%s](%s)" '("ID" "LINK"))))
(spacemacs/set-leader-keys-for-major-mode 'org-mode "l" 'org-yank-wi-link)

;; set capture templates
(setq org-capture-templates
              '(("w" "WI" entry (file "~/org/stories.org")
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
(defun enable-system-notifications () (interactive) (shell-command "killall -SIGUSR2 dunst"))
(defun disable-system-notifications () (interactive) (shell-command "killall -SIGUSR1 dunst"))

;;; require org-pomodoro to force a load
(require 'org-pomodoro)
(setq org-pomodoro-finished-sound-p nil)
(setq org-pomodoro-short-break-sound org-pomodoro-ticking-sound)
(setq org-pomodoro-long-break-sound org-pomodoro-ticking-sound)

(add-hook 'org-pomodoro-started-hook 'disable-system-notifications)
(add-hook 'org-pomodoro-started-hook 'spotify-play)
(add-hook 'org-pomodoro-finished-hook 'spotify-pause)
(add-hook 'org-pomodoro-finished-hook 'enable-system-notifications)
(add-hook 'org-pomodoro-finished-hook 'switch-to-org-notes)

(spacemacs/set-leader-keys-for-major-mode 'org-mode "p" 'org-pomodoro)
