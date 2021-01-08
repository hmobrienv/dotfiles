;;; ../Code/dotfiles/.doom.d/lib.el -*- lexical-binding: t; -*-
(require 'request)

(setq ynab-token "747f3794348f2266e3d6a965efa307382d5e804cc60624ad88cd1672607ed8f7")
(setq ynab-base-url "")

(defun hmov/open-fish-config ()
  (interactive)
  (find-file "~/.config/fish/config.fish"))

(defun hmov/dummy-request ()
  (interactive)
  (request "https://api.youneedabudget.com/v1/budgets"
    :params '(("key" . "value") ("key2" . "value2"))
    :parser 'json-read
    :sync t
    :headers `(("Authorization" . ,(concat "Bearer " ynab-token)))
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (message "Done: %s" (request-response-data response))))))

(defun yabai-restart ()
  (interactive)
  (async-shell-command "brew services restart yabai"))



  (provide 'lib)
