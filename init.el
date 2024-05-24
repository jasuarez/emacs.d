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


;; Enable Which Function Mode
(which-function-mode 1)

;; Wrap lines
(global-visual-line-mode 1)

;; Calendar starts on Moday
(setq calendar-week-start-day 1)

;; Shortkey to comment region
(global-set-key (kbd "C-c C-c") 'comment-region)

;; Install use-package if not already installed
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
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

;; Diminished modes are minor modes with no modeline display
(use-package diminish)

;; Major-mode for editing CMake sources
(use-package cmake-mode
  :mode "\\.cmake\\'")

;; Major mode for editing Docker's Dockerfiles
(use-package dockerfile-mode)

;; Incremental Vertical completYon
(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

;; Various completion functions using Ivy
(use-package counsel
  :diminish
  :after ivy
  :config
  (counsel-mode 1))

;; EditorConfig Emacs plugin
(use-package editorconfig
  :diminish
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
  (setq org-export-backends '(ascii html md odt))
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
  (setq org-journal-dir "~/Misc/orgfiles/journal")
  (setq org-journal-file-type 'weekly)
  (setq org-journal-file-format "%Y/w%V.org")
  (setq org-journal-date-format "%A (%d %b %Y)")
  (setq org-journal-time-format "")
  (setq org-journal-time-prefix "- ")
  (setq org-journal-file-header
        (concat
         "#+Title: Week %V\n"
         "#+OPTIONS: toc:nil tags:nil ^:{}\n"
         "\n"
         "\n")))

;; Insert org-mode links from the clipboard
(use-package org-cliplink
  :after org
  :config
  (progn
    (defun org-cliplink-gh (&optional link)
      (interactive)
      (org-cliplink-insert-transformed-title
       (or link (org-cliplink-clipboard-content))
       (lambda (url title)
         (let* ((parsed-url (url-generic-parse-url url))
                (clean-title
                 (cond
                  ((string-match "\\([^·]*\\) · Issue #\\([0-9]+\\) · " title)   ; GitHub Issue
                   (concat "#" (match-string 2 title) " \"" (match-string 1 title) "\""))
                  ((string-match "\\([^·]*\\) by [^·]+ · Pull Request #\\([0-9]+\\) · " title)   ; GitHub Pull Request
                   (concat "!" (match-string 2 title) " \"" (match-string 1 title) "\""))
                  ((string-match "\\([^·]*\\) · [^@]*@\\([^·]*\\) · " title)   ; GitHub Commit
                   (concat (match-string 2 title) " (\"" (match-string 1 title) "\")"))
                  ((string-match "\\([^(]*\\) (#\\([0-9a-z]+\\)) · Issues" title)   ; GitLab Issue
                   (concat "#" (match-string 2 title) " \"" (match-string 1 title) "\""))
                  ((string-match "\\([^(]*\\) (!\\([0-9a-z]+\\)) · Merge requests" title)   ; GitLab Merge Request
                   (concat "!" (match-string 2 title) " \"" (match-string 1 title) "\""))
                  ((string-match "\\([^(]*\\) (\\([0-9a-z]+\\)) · Commits" title)   ; GitLab Commit
                   (concat (match-string 2 title) " (\"" (match-string 1 title) "\")"))
                  (t title)
                  )))
           ;; forward the title to the default org-cliplink transformer
           (org-cliplink-org-mode-link-transformer url clean-title))))))
  :bind
  (("C-c C-x l" . org-cliplink-gh)))

;; Modern looks for Org
(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode))

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

;; A database abstraction layer for Org-mode
(use-package org-roam
  :after org
  :custom
  (org-roam-directory (file-truename "~/Misc/orgfiles/org-roam"))
  (org-roam-completion-everywhere t)
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   :map org-mode-map
   ("C-M-i" . completion-at-point)))

;; Adds document titles to Markdown files generated with ox-md and derivatives
(use-package ox-md-title
  :after org
  :quelpa
  (ox-md-title
   :fetcher github
   :repo "jeffkreeftmeijer/ox-md-title.el")
  :config
  (org-md-title-add)
  (setq org-md-title t))

;; Manage and navigate projects in Emacs easily
(use-package projectile
  :diminish
  :config
  (projectile-mode +1)
  :custom
  (projectile-indexing-method 'hybrid)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; Ivy integration for Projectile
(use-package counsel-projectile
  :config
  (counsel-projectile-mode 1))

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

(use-package yasnippet
  :config
  (yas-global-mode t))

;; A low contrast color theme for Emacs
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

;; Better experience with icons for ivy
(use-package all-the-icons-ivy-rich
  :config
  (all-the-icons-ivy-rich-mode 1))

;; More friendly display transformer for ivy
(use-package ivy-rich
  :config
  (ivy-rich-mode 1))

;; Better sorting and filtering
(use-package ivy-prescient
  :config
  (ivy-prescient-mode 1))
