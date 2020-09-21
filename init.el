;;; init.el --- Emacs configuration file -*- lexical-binding: t -*-

;; No splash message
(setq inhibit-splash-screen t)

;; No backup files
(setq make-backup-files nil)

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

;; TRAMP integration for docker containers
;; Use podman instead of docker

(use-package docker-tramp
  :custom
  (docker-tramp-docker-executable "podman"))

;; Major mode for editing Docker's Dockerfiles
(use-package dockerfile-mode)

;; Manage and navigate projects in Emacs easily
(use-package projectile
  :config
  (projectile-mode +1)
  :custom
  (projectile-indexing-method 'hybrid)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; Major mode for editing YAML files
(use-package yaml-mode
  :mode "\\.yml\\'")

;; A low contrast color theme for Emacs
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))
