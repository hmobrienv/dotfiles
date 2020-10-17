(package-initialize)

(org-babel-load-file "~/.emacs.d/configuration.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 (quote
		("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(ledger-clear-whole-transactions 5 t)
 '(org-agenda-files
	 (quote
		("/Users/mobrien/Documents/org/notes.org" "/Users/mobrien/Documents/org/gtd.org" "/Users/mobrien/Documents/org/inbox.org" "/Users/mobrien/Documents/org/interviewprep.org" "/Users/mobrien/Documents/org/projects.org" "/Users/mobrien/Documents/org/someday.org" "/Users/mobrien/Documents/org/spanish.org" "/Users/mobrien/Documents/org/tester.org" "/Users/mobrien/Documents/org/tickler.org")))
 '(org-journal-dir "~/Documents/org/journal/")
 '(org-tags-column -100)
 '(package-selected-packages
	 (quote
		(helm-ag flycheck-rust exec-path-from-shell rust-mode neotree doom-themes wakatime-mode anaconda-mode org-projectile projectile ledger-mode org-journal org-pomodoro solarized-theme auto-compile org-bullets evil use-package)))
 '(wakatime-python-bin nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))

