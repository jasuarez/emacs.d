;;; init.el --- Emacs configuration file -*- lexical-binding: t -*-

;; No splash message
(setq inhibit-splash-screen t)

;; No backup files
(setq make-backup-files nil)

;; No toolbar
(progn (if (fboundp 'tool-bar-mode) (tool-bar-mode -1)))

;; Store Customs in different place
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

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

;; Org Twiki and Foswiki export
(use-package ox-twiki)

;; Manage and navigate projects in Emacs easily
(use-package projectile
  :config
  (projectile-mode +1)
  :custom
  (projectile-indexing-method 'hybrid)
  :bind-keymap
  ("C-c p" . projectile-command-map))

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
