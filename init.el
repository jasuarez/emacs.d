;;; init.el --- Emacs configuration file -*- lexical-binding: t -*-

;; No splash message
(setq inhibit-splash-screen t)

;; No backup files
(setq make-backup-files nil)

;; No toolbar
(progn (if (fboundp 'tool-bar-mode) (tool-bar-mode -1)))

;; Show column number
(column-number-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Store Customs in different place
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Enable Ido mode (Interactively DO things)
(ido-mode 1)

;; Enable Which Function Mode
(which-function-mode 1)

;; Install use-package if not already installed
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (require 'use-package-ensure)
  (setq use-package-always-ensure t))

;; Quelpa handler for use-package
(use-package quelpa-use-package
  :init
  (setq quelpa-update-melpa-p nil)
  (setq quelpa-self-upgrade-p nil))

;; Major-mode for editing CMake sources
(use-package cmake-mode
  :mode "\\.cmake\\'")

;; TRAMP integration for docker containers
;; Use podman instead of docker

(use-package docker-tramp
  :custom
  (docker-tramp-docker-executable "podman"))

;; Major mode for editing Docker's Dockerfiles
(use-package dockerfile-mode)

;; EditorConfig Emacs plugin
(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; Define and query search engines from within Emacs
(use-package engine-mode
  :config
  (engine-mode t)
  (defengine github
    "https://github.com/search?q=%s&type=code"
    :keybinding "g")
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d"))

;; Major mode for Markdown-formatted text
(use-package markdown-mode
  :mode "\\.md\\'")

;; Major mode for the Meson build system files
(use-package meson-mode)

;; Outline-based notes management and organizer
(use-package org
  :config
  (setq org-directory "~/Misc/orgfiles")
  (setq org-agenda-files (list org-directory))
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 1)))
  (setq org-capture-templates
	'(("t" "Todo [Inbox]" entry
	   (file "~/Misc/orgfiles/inbox.org")
	   "* TODO %i%?")))
  (setq org-clock-persist t)
  (setq org-clock-in-resume t)
  (setq org-clock-persist-query-resume nil)
  (setq org-clock-out-remove-zero-time-clocks t)
  (org-clock-persistence-insinuate)
  :bind
  (("C-c c" . org-capture)))

;; A simple org-mode based journaling mode
(use-package org-journal
  :config
  (setq org-journal-dir "~/Misc/journal")
  (setq org-journal-file-type 'weekly)
  (setq org-journal-file-format "%Y/w%V.org")
  (setq org-journal-date-format "Main.JuanSuarez - %d %b %Y")
  (setq org-journal-time-format "")
  (setq org-journal-time-prefix "- ")
  (setq org-journal-file-header
	(concat
	 "#+LINK: mesa_commit https://gitlab.freedesktop.org/mesa/mesa/-/commit/\n"
	 "#+LINK: mesa_issue  https://gitlab.freedesktop.org/mesa/mesa/-/issues/\n"
	 "#+LINK: mesa_mr     https://gitlab.freedesktop.org/mesa/mesa/-/merge_requests/\n"
	 "\n")))

;; Pretiffy headings and plain lists in Org mode
(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))

;; Org Twiki and Foswiki export
;; Add function that fits better with the format we need for reports
(use-package ox-twiki
  :after org
  :config
  (defun org-twiki-export-report ()
    (interactive)
    (org-twiki-export-as-twiki)
    (goto-char (point-min))
    (kill-whole-line)
    (kill-whole-line)
    (while (search-forward "---+" nil t)
      (replace-match "--"))
    (goto-char (point-min))))

;; Manage and navigate projects in Emacs easily
(use-package projectile
  :config
  (projectile-mode +1)
  :custom
  (projectile-indexing-method 'hybrid)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; Automatic insertion, wrapping and paredit-like navigation with user
;; defined pairs
(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

;; A tree style file explorer package
(use-package treemacs
  :bind
  (("<f8>" . treemacs)))

;;Projectile integration for treemacs
(use-package treemacs-projectile
  :after projectile treemacs)

;; Major mode for editing Twiki pages
(use-package twiki
  :quelpa
  (twiki
     :fetcher github
     :repo "christopherjwhite/emacs-twiki-mode")
  :mode ("\\.twiki\\'" . twiki-mode))

;; Major mode for editing YAML files
(use-package yaml-mode
  :mode "\\.yml\\'")

;; A low contrast color theme for Emacs
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))
