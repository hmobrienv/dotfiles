;;; .doom.d/config.el -*- lexical-binding: t; -*-
;; Place your private configuration here

;; org-jira
(setq jiralib-url "https://jira.vectra.io")

;; wakatime
(global-wakatime-mode)

;; Default window size
(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '(
              (width . 150)
              (height . 80)
              (left . 50)
              (top . 50)))
      (setq default-frame-alist
            '(
              (width . 150)
              (height . 80)
              (left . 50)
              (top . 50))))
  (progn
    (setq initial-frame-alist'((tool-bar-lines . 0)))
    (setq default-frame-alist'((tool-bar-lines . 0)))))
(tool-bar-mode -1)


;; tramps
(setq tramp-default-method "ssh")

;; ox-hugo
(def-package! ox-hugo
  :config
  (setq org-hugo-export-with-section-numbers nil)
  :after ox)

;; Org Setup
(after! org
  (setq org-directory "~/org")
  (defun org-file-path (filename)
    (concat (file-name-as-directory org-directory) filename))

  (setq org-inbox-file        (org-file-path "inbox.org"))
  (setq org-index-file        (org-file-path "gtd.org"))
  (setq org-notes-refile      (org-file-path "notes.org"))
  (setq org-journal-file      (org-file-path "journal.org"))
  (setq org-work-journal-file (org-file-path "work-journal.org"))

  (setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

  (setq org-agenda-start-day "+0d")
  (setq org-agenda-start-with-log-mode t)

  (defvar hmov/org-agenda-bulk-process-key ?f
  "Default key for bulk processing inbox items.")
  (setq org-agenda-bulk-custom-functions `((,hmov/org-agenda-bulk-process-key hmov/org-agenda-process-inbox-item)))

  (setq org-agenda-custom-commands
      '(("p" "Agenda"
         ((agenda ""
                  ((org-agenda-span 'day)
                   (org-agenda-start-with-log-mode '(closed clock state))
                   (org-deadline-warning-days 365)
                   (org-agenda-log-mode)))
          (tags-todo "+REFILE-LEVEL=2"
               ((org-agenda-overriding-header "To Refile")))
          (todo "NEXT"
               ((org-agenda-overriding-header "Next Tasks")))
          (tags-todo "PROJECT-REFILE-NEXT"
               ((org-agenda-overriding-header "Project Tasks")
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("NEXT")))))
          (tags-todo "-REFILE-PROJECT-SOMEDAY"
               ((org-agenda-overriding-header "One-off Tasks")
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
          ))
        ("w" "Work Agenda"
         ((agenda ""
                  ((org-agenda-span 7)
                   (org-agenda-tag-filter-preset '("+@work"))))
          (tags-todo "+REFILE-LEVEL=2"
               ((org-agenda-overriding-header "To Refile")))
          (todo "NEXT"
               ((org-agenda-overriding-header "Current Tasks")))
          (tags-todo "+@work-NEXT"
               ((org-agenda-overriding-header "Next")))
          ))
        ))

  (setq org-capture-templates
          '(("n" "Notes"
             entry
             (file org-notes-refile)
             "* %?\n")

            ("j" "Journal"
             entry
             (file org-journal-file)
             "* %?\nEntered on %U\n  %i\n  %a")

            ("w" "Work Journal"
             entry
             (file org-work-journal-file)
             "* %T\n%?")

            ("i" "Todo"
             entry
             (file org-inbox-file)
             "* TODO %?\n")
            ))

  (setq org-tag-alist '(("@work" . ?w)
                          ("@home" . ?h)
                          ("@errand" . ?e)
                          ("@computer" . ?C)
                          ("@phone" . ?p)
                          ("SPRINT" . ?s)
                          ("PROJECT" . ?P)))

  (setq org-stuck-projects
          '("+PROJECT+LEVEL=2/-DONE" ("NEXT") nil ""))

  (setq org-archive-location
          (concat (org-file-path "/archive/archive") "::* From %s"))

  (defun hmov/org-inbox-capture ()
    (interactive)
    "Capture a task in agenda mode."
    (org-capture nil "i"))

  (defun hmov/bulk-process-entries ()
    (if (not (null org-agenda-bulk-marked-entries))
        (let ((entries (reverse org-agenda-bulk-marked-entries))
              (processed 0)
              (skipped 0))
          (dolist (e entries)
            (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
              (if (not pos)
                  (progn (message "Skipping removed entry at %s" e)
                         (cl-incf skipped))
                (goto-char pos)
                (let (org-loop-over-headlines-in-active-region) (funcall 'hmov/org-agenda-process-inbox-item))
                ;; `post-command-hook; is not run yet. We make sure any
                ;; pending log note is processed.
                (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                          (memq 'org-add-log-note post-command-hook))
                  (org-add-log-note))
                (cl-incf processed))))
          (org-agenda-redo)
          (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
          (message "Acted on %d entries %s%s"
                   processed
                   (if (= skipped 0)
                       ""
                     (format ", skipped %d (disappeared before their turn)"
                             skipped))
                   (if (not org-agenda-persistant-marks) "" " (kept marked)")))))


  (defun hmov/org-agenda-process-inbox-item ()
    "Process a single item in the org-agenda."
    (org-with-wide-buffer
    (org-agenda-set-tags)
    (org-agenda-priority)
    (org-agenda-refile nil nil t)))

  (defun hmov/org-process-inbox ()
    "called in org-agenda-mode, processes all inbox items"
    (interactive)
    (org-agenda-bulk-mark-regexp "inbox:")
    (hmov/bulk-process-entries))

  (add-hook 'org-agenda-mode-hook
          (lambda ()
            (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
            (auto-save-mode)))

  (defun save-all ()
    (interactive)
    (save-some-buffers t))
  (add-hook 'focus-out-hook 'save-all)

  (setq org-agenda-files '("~/org")))
