;; no splash message
(setq inhibit-splash-screen t)

;; custom variables
(custom-set-variables
 '(blink-matching-parent t)
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(fill-column 80)
 '(paren-set-mode (quote sexp))
 '(safe-local-variable-values (quote ((js-indent-level . 4))))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))

;; custom faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 98 :width normal)))))

;; UTF-8 encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; add repositories
(package-initialize)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; no backup files
(setq make-backup-files nil)

;; use zenburn theme
(setq load-path (cons "~/.emacs.d/elpa/zenburn-theme-20150214.131" load-path))
(require 'zenburn-theme)

