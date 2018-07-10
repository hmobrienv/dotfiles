(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

(eval-when-compile
    (require 'use-package))

(setq use-package-verbose t)
(setq use-package-always t)

(use-package auto-compile
   :ensure t
   :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)

(defun hmov/reload-config ()
	(interactive)
  (load-file "~/.emacs.d/init.el"))

(toggle-scroll-bar -1)

(tool-bar-mode -1)

(use-package solarized-theme
    :ensure t)

(load-theme 'solarized-dark t)

(use-package evil
    :ensure t)

(evil-mode t)

(use-package helm
    :ensure t)

(require 'helm-config)
(helm-mode 1)
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap list-buffers] 'helm-buffers-list)

(setq org-agenda-files (quote ("~/Nextcloud/org")))

(setq org-agenda-dim-blocked-tasks nil)

(setq org-agenda-custom-commands
	(quote ((" " "Agenda"
					 ((agenda "" nil)
						(tags "REFILE"
									((org-agenda-overriding-header "Tasks to Refile")
									 (org-tags-match-list-sublevels nil)))) 
           nil))))

(use-package org-bullets
		:ensure t
		:commands org-bullets-mode
		:hook (org-mode . org-bullets-mode))

(setq org-src-tab-acts-natively t)
(setq org-ellipsis "⤵")

(add-to-list 'org-structure-template-alist
	     '("el" "#+BEGIN_SRC emacs-lisp\n?\#+END_SRC"))

(setq org-directory "~/Nextcloud/org")

(defun org-file-path (filename)
	(concat (file-name-as-directory org-directory) filename))

(setq org-inbox-file (org-file-path "inbox.org"))
(setq org-index-file (org-file-path "index.org"))
(setq org-archive-location
(concat (org-file-path "archive.org") "::* From %s"))

(defun hmov/mark-done-and-archive ()
		"Mark the state of an org-mode item as DONE and archive it"
		(interactive)
		(org-todo 'done)
		(org-archive-subtree))

(setq org-capture-templates
	 '(("b" "Blog Idea"
			entry
			(file (org-file-path "blog-ideas.org"))
			"* %?\n")

		 ("t" "Todo"
			entry
			(file+headline org-inbox-file)
			"* TODO %?\n")

	   ("n" "note" 
      entry 
      (file org-inbox-file)
      "* %? :NOTE:\n%U\n%a\n")))

(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

(define-key org-mode-map(kbd "C-c C-x C-s") 'hmov/mark-done-and-archive)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)

(defun hmov/open-index-file ()
	"Open the master org TODO list."
	(interactive)
	(find-file org-index-file)
	(end-of-buffer))

(global-set-key (kbd "C-c i") 'hmov/open-index-file)

(setq-default tab-width 2)

(setq lispy-mode-hooks '(lisp-mode-hook))
