;;; ../Code/dotfiles/.doom.d/lisp/ui.el -*- lexical-binding: t; -*-

;; Display
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)
(setq display-line-numbers-type t)
(setq-default left-margin-width 3 right-margin-width 3)
(set-window-buffer nil (current-buffer))

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
(if (equal (system-name) "tower")
    (setq doom-font (font-spec :family "Iosevka" :size 24))
  (setq doom-font (font-spec :family "Iosevka" :size 14)))
(setq doom-serif-font (font-spec :family "Iosevka"))

(provide 'ui)
