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

;; UTF-8 encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

