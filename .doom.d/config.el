;;; .doom.d/config.el -*- lexical-binding: t; -*-
;; Place your private configuration here
(load! "lisp/lib")
(load! "lisp/ui")
(load! "lisp/aws")

(if (equal (system-name) "mobrien-mbp19.local")
    (load! "lisp/vectra"))

(global-auto-revert-mode 1)
(setq exec-path (append exec-path '("~/go/bin" "~/.pyenv/shims/")))

(evil-set-initial-state 'awstk-s3-bucket-mode 'normal)

;; magit
(setq magit-repository-directories '(("~/vectra/" . 1)))
(setq require-final-newline nil)

;; wakatime
(use-package! wakatime-mode
  :init
  (setq wakatime-api-key "6e37fd13-897a-4616-96ea-5aa389d2098d")
  (global-wakatime-mode))

(unless window-system
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

(map! :leader
      (:desc "open fish config" "f C-f" #'hmov/open-fish-config))

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


;; ivy
(setq ivy-read-action-function #'ivy-hydra-read-action)

;; lsp
(setq lsp-ui-doc-delay 0.5)
(setq lsp-pyls-plugins-pylint-enabled t)
(setq lsp-flycheck-live-reporting t)

(if (string-equal (system-name) "devmachine")
    (setq doom-font (font-spec :family "Iosevka" :size 18)))

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

;; (after! lsp-mode
;;   (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-stdio-connection '("/usr/local/bin/terraform-ls" "serve"))
;;                     :major-modes '(terraform-mode)
;;                     :server-id 'terraform-ls)))

;; (add-hook 'terraform-mode-hook #'lsp)

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
;;(setq writeroom-width 150)

;; evil mode
(setq evil-want-fine-undo t)


(after! org
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (setq org-directory "~/org")
  (defun org-file-path (filename)
    (concat (file-name-as-directory org-directory) filename))

  (setq org-id-track-globally t)
  (setq org-babel-clojure-backend 'cider)
  (setq org-inbox-file        (org-file-path "inbox.org"))
  (setq org-index-file        (org-file-path "gtd.org"))
  (setq org-notes-refile      (org-file-path "notes-refile.org"))
  (setq org-work-journal-file (org-file-path "work-journal.org"))
  (setq org-ellipsis " ▼ ")
  (setq org-superstar-headline-bullets-list '("●" "○"))
  (setq org-plantuml-jar-path "/usr/local/bin/plantuml")
  (setq org-links-file        (org-file-path "links.org"))

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

            ("k" "Work Todo"
             entry
             (file org-inbox-file)
             "* TODO %? :@work:\n")

	          ("p" "Protocol"
             entry
             (file org-links-file)
             "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
            ("L" "Protocol Link"
             entry
             (file org-links-file)
             "* %? [[%:link][%:description]] \nCaptured On: %U")

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

;; rust
(setq lsp-rust-server 'rust-analyzer)
(setq lsp-rust-analyzer-server-display-inlay-hints t)


(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(map! :localleader
      :map emacs-lisp-mode-map
      :desc "smartparens" "s" #'hydra-smartparens/body)

(setq nrepl-force-ssh-for-remote-hosts t)

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
