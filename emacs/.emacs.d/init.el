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
 '(org-agenda-files nil)
 '(org-journal-dir "~/Documents/journal/")
 '(org-tags-column -100)
 '(package-selected-packages
	 (quote
		(exec-path-from-shell rust-mode neotree doom-themes wakatime-mode anaconda-mode org-projectile projectile ledger-mode org-journal org-pomodoro solarized-theme auto-compile org-bullets evil use-package)))
 '(wakatime-python-bin nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
