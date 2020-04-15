;;; .doom.d/config.el -*- lexical-binding: t; -*-
;; Place your private configuration here


;; org-jira
(setq jiralib-url "https://jira.vectra.io")

;; wakatime
(global-wakatime-mode)

;; Display
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
(scroll-bar-mode -1)
;;(setq doom-theme 'dark+)


;; fonts
(setq doom-font (font-spec :family "Iosevka" :size 14)
      doom-variable-pitch-font (font-spec :family "Input Mono")
      doom-serif-font (font-spec :family "Input Mono"))

;; treemacs
;; (lsp-treemacs-sync-mode 1)

;; flycheck
(with-eval-after-load 'flycheck
  (flycheck-add-mode 'proselint 'org-mode))


;; tramps
(setq tramp-default-method "ssh")

;; writeroom
(setq writeroom-width 150)

;; evil mode
(setq evil-want-fine-undo t)

;; setup org-protocol
(server-start)
(add-to-list 'load-path "~/.emacs.d/.local/straight/repos/org/lisp/org-protocol.el")
(require 'org-protocol)

(after! org
  (setq org-directory "~/org")
  (defun org-file-path (filename)
  (concat (file-name-as-directory org-directory) filename))

  (setq org-inbox-file        (org-file-path "inbox.org"))
  (setq org-index-file        (org-file-path "gtd.org"))
  (setq org-notes-refile      (org-file-path "notes-refile.org"))
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
          (tags-todo "+@work+REFILE-LEVEL=2"
               ((org-agenda-overriding-header "To Refile")))
          (tags-todo "+@work-NEXT"
               ((org-agenda-overriding-header "Next")))
          ))
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

(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam)
  :init
  (setq org-roam-directory "~/org/braindump"
        org-roam-db-location "~/org/org-roam.db")
    (map! :leader
        :prefix "n"
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-find"   "/" #'org-roam-find-file
        :desc "org-roam-buffer" "r" #'org-roam
        :desc "org-roam-capture" "c" #'org-roam-capture)
  :config
  (require 'org-roam-protocol)
  (org-roam-mode +1)
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+SETUPFILE:./hugo_setup.org
#+HUGO_SECTION: zettels
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}\n"
           :unnarrowed t)
          ("p" "private" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "private-${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)))
  (setq org-roam-ref-capture-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "websites/${slug}"
           :head "#+SETUPFILE:./hugo_setup.org
#+ROAM_KEY: ${ref}
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}
- source :: ${ref}"
           :unnarrowed t))))

(use-package org-journal
  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-file-header "[[file:~/org/braindump/journaling.org][file:~/Dropbox/org/braindump/journaling.org]]")
  (org-journal-dir "~/org/braindump/")
  (org-journal-date-format "%A, %d %B %Y"))

;; org roam export
(defun my/org-roam--backlinks-list (file)
  (if (org-roam--org-roam-file-p file)
      (--reduce-from
       (concat acc (format "- [[file:%s][%s]]\n"
                           (file-relative-name (car it) org-roam-directory)
                                 (org-roam--get-title-or-slug (car it))))
       "" (org-roam-sql [:select [file-from] :from file-links :where (= file-to $s1)] file))
    ""))

(defun my/org-export-preprocessor (backend)
  (let ((links (my/org-roam--backlinks-list (buffer-file-name))))
    (unless (string= links "")
      (save-excursion
        (goto-char (point-max))
        (insert (concat "\n* Backlinks\n") links)))))

(add-hook 'org-export-before-processing-hook 'my/org-export-preprocessor)
