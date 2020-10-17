;;; .doom.d/config.el -*- lexical-binding: t; -*-
;; Place your private configuration here

(global-auto-revert-mode 1)

;; org-jira
(setq jiralib-url "https://jira.vectra.io")
(setq org-jira-working-dir (expand-file-name "~/org/work/jira"))

;; wakatime
(use-package! wakatime-mode
  :init
  (setq wakatime-api-key "6e37fd13-897a-4616-96ea-5aa389d2098d")
  (global-wakatime-mode))


;; json
(add-hook 'json-mode-hook
          (lambda ()
            ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

;; golang
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)


;; python
(use-package! lsp-python-ms
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))

(use-package! python-black
  :init
  (setq python-black--base-args '("--quiet" "--line-length" "100"))
  (map! :localleader
        :map python-mode-map
        :desc "Deploy package" "d" #'deploy-package
        :desc "Blacken buffer" "b" #'python-black-buffer))


(after! ob-jupyter
  (dolist (lang '(python julia R))
    (cl-pushnew (cons (format "jupyter-%s" lang) lang)
                org-src-lang-modes :key #'car)))


;; Display
(menu-bar-mode -1)
(setq display-line-numbers-type nil)
(setq-default left-margin-width 3 right-margin-width 3)
(set-window-buffer nil (current-buffer))
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
(delete-selection-mode 1)
(global-subword-mode 1)
(setq fancy-splash-image (concat doom-private-dir "splash.png"))

;; Modeline
(custom-set-faces!
  '(mode-line :family "Iosevka" :height 0.90)
  '(mode-line-inactive :family "Iosevka" :height 0.90))


;; fonts
;;
;;

(if (equal (system-name) "TOWER-wsl")
    (setq doom-font (font-spec :family "Iosevka" :size 24))
  (setq doom-font (font-spec :family "Iosevka" :size 14)))
(setq doom-serif-font (font-spec :family "Iosevka"))

;; ivy
(setq ivy-read-action-function #'ivy-hydra-read-action)

;; lsp
(setq lsp-ui-doc-delay 0.5)
(setq lsp-pyls-plugins-pylint-enabled t)
(setq lsp-flycheck-live-reporting t)

;; treemacs
(after! treemacs
  (setq treemacs-width 53)
  (lsp-treemacs-sync-mode 1)
  (treemacs-resize-icons 44))

;; flycheck
(setq-default flycheck-disabled-checkers '(json-jsonlint))

(after! lsp-ui
  (add-hook! 'lsp-ui-mode-hook
    (run-hooks (intern (format "%s-lsp-ui-hook" major-mode)))))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("/usr/local/bin/terraform-ls" "serve"))
                  :major-modes '(terraform-mode)
                  :server-id 'terraform-ls))

(add-hook 'terraform-mode-hook #'lsp)

(defun mobrien-go-flycheck-setup ()
  "Setup Flycheck checkers for Golang"
  (flycheck-add-next-checker 'lsp-ui 'golangci-lint))

(add-hook 'go-mode-lsp-ui-hook
          #'mobrien-go-flycheck-setup)

(defun mobrien-python-flycheck-setup ()
  "Setup Flycheck checkers for Python"
  (flycheck-add-next-checker 'lsp-ui 'python-pylint))

(add-hook 'python-mode-lsp-ui-hook
          #'mobrien-python-flycheck-setup)

(setq lsp-pyls-plugins-pylint-enabled t)
;;(add-to-list 'flycheck-checkcers 'lsp-ui 'python-pylint))
;; (setq flycheck-python-pylint-executable "~/.pyenv/shims/pylint")
;; (setq flycheck-python-pycompile-executable "~/.pyenv/shims/python3")
;; (with-eval-after-load 'flycheck

;;  (flycheck-add-mode 'proselint 'org-mode))

;; tramps
(setq tramp-default-method "ssh")

;; writeroom
(setq writeroom-width 150)

;; evil mode
(setq evil-want-fine-undo t)


(after! org
  (require 'org-drill)
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (setq org-directory "~/org")
  (defun org-file-path (filename)
    (concat (file-name-as-directory org-directory) filename))

  (setq org-inbox-file        (org-file-path "inbox.org"))
  (setq org-index-file        (org-file-path "gtd.org"))
  (setq org-notes-refile      (org-file-path "notes-refile.org"))
  (setq org-work-journal-file (org-file-path "work-journal.org"))
  (setq org-ellipsis " ▼ ")
  (setq org-superstar-headline-bullets-list '("●" "○"))
  (setq org-plantuml-jar-path "/usr/local/bin/plantuml")

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

  (setq org-capture-templates
        '(("n" "Notes"
           entry
           (file org-notes-refile)
           "* %?\n")

          ("t" "Todo"
           entry
           (file org-inbox-file)
           "* TODO %?\n")
          ))

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

  ;; (add-hook 'org-agenda-mode-hook
  ;;           (lambda ()
  ;;             (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
  ;;             (auto-save-mode)))

  (defun save-all ()
    (interactive)
    (save-some-buffers t))

  (add-hook 'focus-out-hook 'save-all)
  (setq org-agenda-files '("~/org")))



(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam)
  :init
  (setq org-roam-directory "~/org/braindump")
  (setq org-roam-db-location "~/org/org-roam.db")
  (setq +org-roam-open-buffer-on-find-file nil)
  (map! :leader
         :prefix "n"
         :desc "org-roam-insert" "i" #'org-roam-insert
         :desc "org-roam-find"   "/" #'org-roam-find-file)
  ;;       :desc "org-roam-buffer" "r" #'org-roam
  ;;       :desc "org-roam-capture" "c" #'org-roam-capture)
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
          ("v" "vectra" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+SETUPFILE:./hugo_setup.org
#+HUGO_SECTION: zettels
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}\n
[[file:20200414175435-vectra.org][Vectra]]"
           :unnarrowed t)
          ("p" "private" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "private-${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)
          ("r" "ref" plain (function org-roam-capture--get-point)
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
  (org-journal-file-header "[[file:~/org/braindump/journaling.org][Journal]]")
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

(use-package! smartparens
  :init
  (map! :map smartparens-mode-map
        "C-M-f" #'sp-forward-sexp
        "C-M-b" #'sp-backward-sexp
        "C-M-u" #'sp-backward-up-sexp
        "C-M-d" #'sp-down-sexp
        "C-M-p" #'sp-backward-down-sexp
        "C-M-n" #'sp-up-sexp
        "C-M-s" #'sp-splice-sexp
        "C-)" #'sp-forward-slurp-sexp
        "C-}" #'sp-forward-barf-sexp
        "C-(" #'sp-backward-slurp-sexp
        "C-M-)" #'sp-backward-slurp-sexp
        "C-M-)" #'sp-backward-barf-sexp))


;; hydra settings
(defhydra hydra-smartparens ()
  "Smartparens"
  ("q" nil)

  ;; Wrapping
  ("(" (lambda (_) (interactive "P") (sp-wrap-with-pair "(")))
  ("{" (lambda (_) (interactive "P") (sp-wrap-with-pair "{")))
  ("'" (lambda (_) (interactive "P") (sp-wrap-with-pair "'")))
  ("\"" (lambda (_) (interactive "P") (sp-wrap-with-pair "\"")))

  ("w" (lambda (_) (interactive "P") (sp-wrap-with-pair "(")) "wrap")
  ("W" sp-unwrap-sexp)

  ;; Movement
  ("l" sp-next-sexp)
  ("h" sp-backward-sexp)
  ("j" sp-down-sexp)
  ("k" sp-backward-up-sexp)

  ("L" sp-forward-symbol)
  ("H" sp-backward-symbol)

  ("^" sp-beginning-of-sexp)
  ("$" sp-end-of-sexp)

  ("t" sp-transpose-sexp "transpose")
  ("u" undo-fu-only-undo "undo")

  ("y" sp-copy-sexp "copy")
  ("d" sp-kill-sexp "delete")

  ("s" sp-forward-slurp-sexp "slurp")
  ("S" sp-backward-slurp-sexp)

  ("b" sp-forward-barf-sexp "barf")
  ("B" sp-backward-barf-sexp)

  ("v" sp-select-next-thing "select")
  ("V" sp-select-previous-thing))

;; Call hydra-smartparens/body
(map! :localleader
      :map clojure-mode-map
      :desc "smartparens" "s" #'hydra-smartparens/body)


;; org capture frame for alfred
(defun make-orgcapture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "remember") (width . 80) (height . 16)
                (top . 400) (left . 300)
                (font . "-apple-Monaco-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
                ))
  (select-frame-by-name "remember")
  (org-capture))

;; vectra
(defun connect-to-buildvm ()
  "Open a connection to a buildvm"
  (interactive)
  (vterm-other-window "vterm-buildvm")
  (vterm-send-string "ssh vadmin@dc-buildvm")
  (vterm-send-return))



(defun get-colossus-packages ()
  (interactive)
  (directory-files "/Users/mikeyobrien/vectra/colossus/colossus/python/packages"))


(defun copy-package ()
  (interactive)
  (let ((brain-ip (read-string "brain ip address: "))
        (package-name (ido-completing-read "package-name: " (get-colossus-packages))))
    (copy-directory
     (format "/ssh:%s|sudo:vadmin@%s:/usr/share/python/vectra-colossus-py35-201907/lib/python3.5/site-packages/%s" brain-ip brain-ip package-name)
     (format "/Users/mikeyobrien/vectra/colossus/colossus/python/packages/%s/%s" package-name package-name) t t t)))

(defun deploy-package()
  (interactive)
  (let ((brain-ip (read-string "brain ip address: "))
        (package-name (ido-completing-read "package-name: " (get-colossus-packages))))
    (async-shell-command
     (format "rsync -azP --rsync-path=\"sudo rsync\" --delete-after --filter=':e- .gitignore' --exclude='__pycache__' %s vadmin@%s:%s"
             (format "/Users/mikeyobrien/vectra/colossus/colossus/python/packages/%s/%s/" package-name package-name)
             brain-ip
             (format "/usr/share/python/vectra-colossus-py35-201907/lib/python3.5/site-packages/%s" package-name) t t t))))

(defun vectra/deploy-syslog-utils ()
  (interactive)
  (let ((brain-ip (read-string "brain ip address: ")))
    (copy-file
     "/Users/mikeyobrien/vectra/colossus/colossus/python/packages/vectra_utils/src/vectra_utils/syslog_utils.py"
     (format "/ssh:%s|sudo:%s:/opt/colossus/vectra-python3/lib/vectra_utils/syslog_utils.py" brain-ip brain-ip) t)
    (copy-file
     "/Users/mikeyobrien/vectra/colossus/colossus/python/packages/vectra_utils/src/vectra_utils/syslog_utils.py"
     (format "/ssh:%s|sudo:%s:/opt/colossus/vectra-python/lib/vectra_utils/syslog_utils.py" brain-ip brain-ip) t )
    (let ((default-directory (format "/ssh:%s|sudo:%s:" brain-ip brain-ip)))
      (start-file-process "restart-service" (get-buffer-create "*restart-service*")
                          "/bin/bash" "service" "detection_processing" "restart"))))


(defun rsync-to-buildvm ()
  (interactive)
  (async-shell-command "rsync -azP --delete-after --filter=':e- .gitignore' --exclude='__pycache__' ~/vectra/colossus vadmin@dc-buildvm:~/"))


(defun rsync-package-directory ()
  (interactive)
  (async-shell-command (format
                        "rsync -azvr --exclude '.venv' %s vadmin@dc-buildvm:~/colossus/colossus/python/packages/%s"
                        (get-package-dir)
                        (file-name-nondirectory (directory-file-name (file-name-directory (get-package-dir)))))))

(defun get-package-dir ()
  (interactive)
  (find-package-dir-in-heirarchy default-directory))

(defun parent-directory (dir)
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

(defun find-package-dir-in-heirarchy (current-dir)
  "Search for a file named FNAME upwards through the directory hierarchy, starting from CURRENT-DIR"
  (let ((file (concat current-dir "setup.py"))
        (parent (parent-directory (expand-file-name current-dir))))
    (if (file-exists-p file)
        (parent-directory file)
      (when parent
        (find-package-dir-in-heirarchy parent)))))


;; ephemeral
(defun my/split-line (line)
  (let* ((program-name (car (last (s-split " " (s-trim (car (s-split "-:" line)))))))
         (body (car (last (s-split "-:" line)))))
    (message (format "\"%s\":%s,"
                     program-name body))))


(defun my/walk-line-by-line ()
  "Process each line in the buffer one by one."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((lb (line-beginning-position))
             (le (line-end-position))
             (ln (buffer-substring-no-properties lb le)))
        (write-region (my/split-line ln) nil (concat default-directory "parsed") 'append)
        (forward-line 1)))))

;; calendar
(after! calfw
  (defun my-open-calendar ()
    (interactive)
    (cfw:open-calendar-buffer
    :contents-sources
    (list
      (cfw:org-create-source "Green")  ; orgmode source
      (cfw:ical-create-source "gcal" "https://outlook.office365.com/owa/calendar/12fda6cdecb74956888ce80b519a70af@vectra.ai/7e08fc0aaff641c5a174490a9fe3a31411568143993500512171/calendar.ics" "IndianRed") ; work calendar ICS
    ))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#f0f0f0" "#e45649" "#50a14f" "#986801" "#4078f2" "#a626a4" "#0184bc" "#383a42"])
 '(custom-safe-themes
   (quote
    ("632694fd8a835e85bcc8b7bb5c1df1a0164689bc6009864faed38a9142b97057" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" default)))
 '(fci-rule-color "#383a42")
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#4078f2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#50a14f"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#9ca0a4"))
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 5) ((control)))))
 '(objed-cursor-color "#e45649")
 '(org-journal-date-format "%A, %d %B %Y")
 '(org-journal-date-prefix "#+TITLE: ")
 '(org-journal-dir "~/org/braindump/")
 '(org-journal-file-format "%Y-%m-%d.org")
 '(org-journal-file-header "[[file:~/org/braindump/journaling.org][Journal]]")
 '(pdf-view-midnight-colors (cons "#383a42" "#fafafa"))
 '(rustic-ansi-faces
   ["#fafafa" "#e45649" "#50a14f" "#986801" "#4078f2" "#a626a4" "#0184bc" "#383a42"])
 '(vc-annotate-background "#fafafa")
 '(vc-annotate-color-map
   (list
    (cons 20 "#50a14f")
    (cons 40 "#688e35")
    (cons 60 "#807b1b")
    (cons 80 "#986801")
    (cons 100 "#ae7118")
    (cons 120 "#c37b30")
    (cons 140 "#da8548")
    (cons 160 "#c86566")
    (cons 180 "#b74585")
    (cons 200 "#a626a4")
    (cons 220 "#ba3685")
    (cons 240 "#cf4667")
    (cons 260 "#e45649")
    (cons 280 "#d2685f")
    (cons 300 "#c07b76")
    (cons 320 "#ae8d8d")
    (cons 340 "#383a42")
    (cons 360 "#383a42")))
 '(vc-annotate-very-old-color nil)
 '(wakatime-python-bin nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:family "Iosevka" :height 0.9))))
 '(mode-line-inactive ((t (:family "Iosevka" :height 0.9)))))
