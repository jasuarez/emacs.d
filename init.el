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

;; shortcuts
(global-set-key [f1] 'goto-line)
(global-set-key [C-tab] 'hippie-expand)
(global-set-key "" (quote comment-region))

;; aspell conf
(setq-default ispell-program-name "aspell")
(setq-default ispell-extra-args '("--reverse"))

;; whitespaces instead of tabs
(setq-default indent-tabs-mode nil)

;; move mouse to avoid disturbing
(mouse-avoidance-mode 'cat-and-mouse)

;; use zenburn theme
(setq load-path (cons "~/.emacs.d/elpa/zenburn-theme-20150214.131" load-path))
(require 'zenburn-theme)

;; highlight current line
(setq load-path (cons "~/.emacs.d/elpa/highlight-current-line-20051013.1756" load-path))
(require 'highlight-current-line)
(highlight-current-line-on t)
(highlight-current-line-set-bg-color "gray30")
(highlight-current-line-whole-line-on t)

(add-hook 'c-mode-common-hook (lambda () (interactive)
  (when (fboundp 'c-mode)
    ;; load GNOME utils
    (setq load-path (cons "~/.emacs.d/gnome-emacs-utils" load-path))
    (require 'gnome-emacs-utils)
    (local-set-key [S-f1] 'devhelp-word-at-point)

    ;; code indexer
    (require 'xcscope)
  )
))
