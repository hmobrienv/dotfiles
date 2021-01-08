;;; ~/Code/dotfiles/.doom.d/vectra.el -*- lexical-binding: t; -*-
;;
(map! :leader
      (:prefix-map ("v" . "vectra")
       (:prefix ("c" . "credentials")
        :desc "ssh-add" "s" #'vectra/ssh-add)))

;; vectra
(defun vectra/connect-to-buildvm ()
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

(defun vectra/open-buildvm-code-dir ()
  (interactive)
  (dired "/ssh:mobrien@fusion:~/code"))


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


(defun vectra/ssh-add-sentinel (process event)
      (message event)
      (cond ((string-match-p "Identity added" event)
                  (progn
                        (kill-buffer "*ssh-add*")
                        (message "ssh-add done")))))

(defun vectra/ssh-add ()
  (interactive)
  (let ((process (start-process-shell-command "ssh-add"
                                        "*ssh-add*"
                                        "ssh-add ~/.ssh/vectra_mobrien")))
  (set-process-sentinel process 'vectra/ssh-add-sentinel)))

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

 (provide 'vectra)
