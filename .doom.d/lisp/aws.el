;;; ../Code/dotfiles/.doom.d/lisp/aws.el -*- lexical-binding: t; -*-

(setq awscli-executable "aws"
      profile "saas-dataeng-dev"
      process-buf "*awstk*")

(defun awstk-clear-buffer ()
  (with-current-buffer (get-buffer-create process-buf)
    (erase-buffer)))

(defun awstk-call-aws (&rest args)
  (awstk-clear-buffer)
  (apply #'call-process awscli-executable nil process-buf nil (append '("--profile" "saas-dataeng-dev") args)))

(defun awstk-list-buckets ()
  (interactive)
  (awstk-call-aws "s3api" "list-buckets" "--query" "Buckets[].Name" "--output" "json" "--profile" profile)
  (let ((buckets (with-current-buffer process-buf
                   (goto-char (point-min))
                   (json-parse-buffer))))
    (with-current-buffer (get-buffer-create "awstk:s3-buckets")
      (setq buffer-read-only nil)
      (erase-buffer)
      (dotimes (i (length buckets))
        (insert (format "%s\n" (elt buckets i)))))
    (pop-to-buffer "awstk:s3-buckets")
    (goto-char (point-min))
    (awstk-s3-bucket-mode)))

(defun awstk-s3-info ()
  (interactive)
  (let ((bucket-name (string-trim (thing-at-point 'line))))
    (awstk-call-aws "s3api" "list-objects" "--bucket" bucket-name)
    (let ((objects (gethash "Contents" (with-current-buffer process-buf
                     (goto-char (point-min))
                     (json-parse-buffer)))))
      (with-current-buffer (get-buffer-create "awstk:s3-objects")
        (setq buffer-read-only nil)
        (erase-buffer)
        (dotimes (i (length objects))
          (insert (format "%s\n"(gethash "Key" (elt objects i)))))
        (pop-to-buffer-same-window "awstk:s3-objects")
        (goto-char (point-min))
        (awstk-s3-object-mode)))))

(defvar awstk-s3-bucket-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "i") 'awstk-s3-info)
    (define-key map (kbd "q") 'previous-buffer)
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map [remap evil-previous-line] 'evil-previous-visual-line)
    (define-key map [remap evil-next-line] 'evil-next-visual-line)
    map)
  "Keymap for `awstk-s3-bucket-mode'.")

(define-derived-mode awstk-s3-bucket-mode special-mode "S3 Bucket Mode"
  "Major mode for handling a list of buckets."
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq show-trailing-whitespace nil)
  (setq-local symbol-overlay-inhibit-map t)
  (setq-local line-move-visual t))

(defvar awstk-s3-object-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map awstk-s3-bucket-mode-map)
    map)
  "Keymap for `awstk-s3-object-mode'.")

(define-derived-mode awstk-s3-object-mode awstk-s3-bucket-mode-map "S3 Object Mode"
  "Major mode for handling a objects from a bucket.")


(awstk-list-buckets)
(provide 'awstk)
