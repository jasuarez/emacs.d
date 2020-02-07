;; no splash message
(setq inhibit-splash-screen t)

;; custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-matching-parent t)
 '(calendar-week-start-day 1)
 '(column-number-mode t)
 '(fill-column 80)
 '(package-selected-packages
   (quote
    (magit company-lsp lsp-ui helm helm-lsp ccls helm-projectile projectile docker-tramp helm-tramp flycheck clang-format editorconfig zenburn-theme yaml-mode xcscope multiple-cursors markdown-mode iedit google-c-style glsl-mode gist dockerfile-mode company column-enforce-mode)))
 '(paren-set-mode (quote sexp))
 '(safe-local-variable-values
   (quote
    ((whitespace-line-column . 79)
     (eval ignore-errors
           (require
            (quote whitespace))
           (whitespace-mode 1))
     (whitespace-style face indentation)
     (eval progn
           (c-set-offset
            (quote case-label)
            (quote 0))
           (c-set-offset
            (quote innamespace)
            (quote 0))
           (c-set-offset
            (quote inline-open)
            (quote 0)))
     (js-indent-level . 4))))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))

(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package package-selected-packages)
    (unless (package-installed-p package)
      (package-install package))))


;; custom faces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; UTF-8 encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; add repositories
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(install-packages)

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
(require 'zenburn-theme)

;; use projectile
(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; use Helm
(require 'helm-config)
(helm-mode 1)

(require 'helm-projectile)
(helm-projectile-on)
(setq projectile-completion-system 'helm)

;; highlight current line
(global-hl-line-mode 1)
(set-face-background hl-line-face "gray30")

;; active 'Which Function' mode
(which-function-mode t)

;; active iedit mode
(require 'iedit)

;; active company mode
(require 'company)
(global-company-mode 1)
(custom-theme-set-faces
	'zenburn
		'(company-tooltip ((t :background "cornsilk" :foreground "black")))
		'(company-tooltip-selection ((t :background "light blue" :foreground "black")))
	)

;; Twiki mode
(setq load-path (cons "~/.emacs.d/emacs-twiki-mode" load-path))
(require 'twiki)
(add-to-list 'auto-mode-alist' ("\\.twiki$" . twiki-mode))

;; C/C++ Mode
(add-hook 'c-mode-common-hook
          (lambda ()
            ;; load GNOME utils
            (setq load-path (cons "~/.emacs.d/gnome-emacs-utils" load-path))
            (require 'gnome-emacs-utils)
            (local-set-key [S-f1] 'devhelp-word-at-point)

	    (require 'company-lsp)
            ;; code indexer
            (require 'xcscope)
            (cscope-minor-mode)
            (lsp)
            ;; show trailing whitespaces
            (setq show-trailing-whitespace t)
            )
          )

(require 'ccls)
(require 'lsp-mode)
(setq lsp-file-watch-threshold 2000)
(setq lsp-enable-file-watchers nil)
(lsp-register-client
 (make-lsp-client :new-connection (lsp-tramp-connection "/usr/bin/ccls")
                  :major-modes '(c-mode c++-mode)
                  :remote? t
                  :server-id 'ccls-remote
                  )
 )
(require 'lsp-ui)
(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-header t)
(setq lsp-ui-doc-include-signature t)
(setq lsp-ui-doc-position 'top)
