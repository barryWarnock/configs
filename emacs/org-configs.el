                                        ; Org Mode specific configs
;; set file locations
(setq-default org-agenda-files '("~/org" "~/workorg"))
(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 1)))
(setq-default org-default-notes-file (concat org-directory "/notes.org"))

;; prettify
(setq org-superstar-leading-bullet ?\s)
(setq org-todo-keywords
      '((sequence "TODO(t!)" "IN-PROGRESS(p!)" "|" "WAITING(w!)" "DONE(d!)")))

;; log todo timestamps to a shelf
(setq-default org-log-into-drawer "timestamps")
(setq-default org-log-done nil)

;; populate a format string with a given list of org props
(defun current-or-clock ()
  (if (org-current-level)
      nil
    org-clock-marker))

(defun org-format-with-props (fstring prop-list)
  (let ((org-props (org-entry-properties (current-or-clock))))
    (apply 'format fstring
           (mapcar
            (lambda (key)
              (cdr (assoc key org-props)))
            prop-list))))

(defun current-or-clock ()
  (if (org-current-level)
      nil
    org-clock-marker))

;; custom function to pull a git commit message from a WI
(defun org-yank-wi-desc ()
  "if we're inside a WI yank its description in the format expected for a git commit"
  (interactive)
  (kill-new (org-format-with-props "%s\n\nissue: %s\n" '("ITEM" "LINK"))))
(spacemacs/set-leader-keys-for-major-mode 'org-mode "w" 'org-yank-wi-desc)
(spacemacs/declare-prefix "aoy" "Yank")
(spacemacs/set-leader-keys "aoyw" 'org-yank-wi-desc)
(defun org-yank-wi-link ()
  "if we're inside a WI yank its link"
  (interactive)
  (kill-new (org-format-with-props "%s" '("LINK"))))
(spacemacs/set-leader-keys-for-major-mode 'org-mode "l" 'org-yank-wi-link)
(spacemacs/set-leader-keys "aoyl" 'org-yank-wi-link)

;; pull the issue number of the currently clocked in entry
(defun wi-issue-num ()
  (car (last (split-string (org-entry-get (current-or-clock) "link") "/"))))

(defun org-yank-wi-issue-num ()
  (interactive)
  (kill-new (wi-issue-num)))
(spacemacs/set-leader-keys "aoyn" 'org-yank-wi-issue-num)

;; switch to the prompt file of the currently clocked in issue
(defun org-switch-to-prompt-file ()
  (interactive)
  (find-file (format "~/Documents/prompts/%s.md" (wi-issue-num))))
;; open the "AI" prompt file for the current project
(spacemacs/set-leader-keys "pa" 'org-switch-to-prompt-file)

;; set capture templates
(setq org-capture-templates
      '(("w" "WI" entry (file "~/workorg/issues.org")
         "* TODO %^{name}%^{link}p\n%?")
        ("n" "Quick Note" item (file org-default-notes-file)
         " %?")))
(setq-default org-capture-bookmark nil)


;; save target files after refile
(add-hook 'org-after-refile-insert-hook 'projectile-save-project-buffers)

;; archive to a sub heading
(defun set-archive-heading ()
  "set the default archive heading to include the current month and year"
  (interactive)
  (setq-default org-archive-location (concat "%s_archive::** " (format-time-string "%B %Y"))))
(set-archive-heading)
(add-hook `midnight-hook `set-archive-heading)


;; open the default-notes-file
(defun switch-to-org-notes ()
  "Switch to the buffer showing the notes file"
  (interactive)
  (find-file org-default-notes-file))
(spacemacs/set-leader-keys "aon" 'switch-to-org-notes)

;; babel configs
;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))
