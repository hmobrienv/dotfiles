;;; ../Code/dotfiles/.doom.d/lisp/ynab.el -*- lexical-binding: t; -*-
(require 'request)

(setq ynab-token "747f3794348f2266e3d6a965efa307382d5e804cc60624ad88cd1672607ed8f7")
(setq ynab-base-url "")

(defun ynab-request-budget ()
  (let (json)
  (request "https://api.youneedabudget.com/v1/budgets"
    :parser 'json-read
    :sync t
    :headers `(("Authorization" . ,(concat "Bearer " ynab-token)))
    :complete (cl-function
               (lambda (&key response &allow-other-keys)
                 (setq json (request-response-data response)))))
  json))

(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (print (car list))
    (setq list (cdr list))))


(defun parse-budgets (data)
  (message "%S" (assoc "data" (car data))))

;;;###autoload
(defun ynab-budgets()
  "List budgets containers."
  (interactive)
  (parse-budgets (ynab-request-budget)))

;;;###autoload (autoload 'docker "docker" nil t)
(transient-define-prefix ynab ()
  "Transient for YNAB."
  :man-page "docker"
  ["Arguments"
   (5 "l" "Log level" "--log-level " docker-read-log-level)]
  ["YNAB"
   ("b" "Budgets"    ynab-budgets)
   ("i" "Images"     docker-images)
   ("n" "Networks"   docker-networks)
   ("v" "Volumes"    docker-volumes)])

  (provide 'ynab)
