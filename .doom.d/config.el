;;; .doom.d/config.el -*- lexical-binding: t; -*-
;; Place your private configuration here
;;
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
  (setq org-notes-refile      (org-file-path "notes-refile.org"))
  (setq org-journal-file      (org-file-path "journal.org"))

  (setq org-agenda-custom-commands
      '((" " "Agenda"
          ((agenda "")
          (tags-todo "+REFILE")
          (stuck "")
          (todo "NEXT")
          (tags-todo "-NEXT-LEVEL=3-PROJECT"
                      ((org-agenda-sorting-strategy '(todo-state-down effort-up))))
          (tags-todo "+PROJECT-LEVEL=3")
          ))
        ("w" "Work Agenda" tags-todo "@work" ;;
          ((agenda "")
           (todo "NEXT")
           (tags-todo "-NEXT-LEVEL=3-PROJECT")
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

            ("t" "Todo"
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

  (setq org-agenda-files '("~/org")))
