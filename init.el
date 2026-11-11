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
(setq which-func-modes '(c-mode c++-mode python-mode rust-mode emacs-list-mode))
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
  (setq org-startup-indented t)
  ;; Start with everything expanded except property drawers (LOGBOOK etc. stay open)
  (setq org-startup-folded 'showeverything)
  (defun org/fold-property-drawers ()
    "Fold all property drawers in the current buffer."
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (re-search-forward org-property-drawer-re nil t)
       (save-excursion
         (goto-char (match-beginning 0))
         (org-fold-hide-drawer-toggle t)))))
  (add-hook 'org-mode-hook #'org/fold-property-drawers)
  (setq org-indent-indentation-per-level 1)
  (org-clock-persistence-insinuate)
  (defun org/remove-clocktables-from-md-export (backend)
    "Remove clocktable dynamic blocks when exporting to Markdown."
    (when (org-export-derived-backend-p backend 'md)
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward "^[ \t]*#\\+BEGIN: clocktable\\b" nil t)
         (let ((beg (match-beginning 0)))
           (if (re-search-forward "^[ \t]*#\\+END:" nil t)
               (delete-region beg (min (point-max) (1+ (line-end-position))))
             (delete-region beg (point-max))))))))
  (add-hook 'org-export-before-parsing-hook
            #'org/remove-clocktables-from-md-export)
  (defun org/clocktable-by-name (ipos tables params)
    "Clocktable :formatter that sums clocked time per headline name.

Unlike the default formatter, which follows the outline tree, this
groups every entry by its headline text and sums it, so a category
that appears under several parents (e.g. \"CE Mentoring\" on Monday
and Wednesday) is reported once with the combined total.

Categories are grouped at the deepest reported outline level; use
:maxlevel in the dynamic block to pick it (e.g. :maxlevel 2 to group
second-level category headings).  The grand total covers ALL of the
top-level entries (the dailies), and any time clocked directly on a
top-level heading rather than inside a category is reported in an
\"Uncategorized\" row so the rows reconcile with the total.  Rows are
sorted by time, longest first."
    (let* ((totals (make-hash-table :test 'equal))
           (order '())
           (all (apply #'append (mapcar (lambda (tbl) (nth 2 tbl)) tables)))
           (levels (delq nil (mapcar (lambda (e) (nth 0 e)) all)))
           (top-level (if levels (apply #'min levels) 1))
           (group-level (if levels (apply #'max levels) 1))
           (grand 0)
           (categorized 0))
      (dolist (e all)
        (let ((level (nth 0 e))
              (headline (nth 1 e))
              (time (nth 4 e)))
          ;; The dailies' subtree totals make up the real grand total.
          (when (= level top-level)
            (setq grand (+ grand time)))
          ;; Group and sum the category headings by name.
          (when (= level group-level)
            (unless (gethash headline totals)
              (push headline order))
            (puthash headline (+ (gethash headline totals 0) time) totals)
            (setq categorized (+ categorized time)))))
      (goto-char ipos)
      (insert "| Category | Time |\n|---\n")
      (dolist (row (sort (mapcar (lambda (h) (cons h (gethash h totals)))
                                 (nreverse order))
                         (lambda (a b) (> (cdr a) (cdr b)))))
        (insert (format "| %s | %s |\n"
                        (car row) (org-duration-from-minutes (cdr row)))))
      (let ((other (- grand categorized)))
        (when (> other 0)
          (insert (format "| Uncategorized | %s |\n"
                          (org-duration-from-minutes other)))))
      (insert (format "|---\n| *Total* | *%s* |\n"
                      (org-duration-from-minutes grand)))
      (goto-char ipos)
      (org-table-align)))
  :bind
  (("C-c c" . org-capture)))

;; Clock in at a chosen outline level (org-clock-in-level)
(use-package org-clock-enhanced
  :after org
  :quelpa
  (org-clock-enhanced
   :fetcher github
   :repo "jasuarez/org-clock-enhanced")
  :custom
  (org-clock-enhanced-inherit-effort t))

;; A simple org-mode based journaling mode
(use-package org-journal
  :after org
  :config
  (setq org-journal-dir "~/Misc/orgfiles/journal")
  (setq org-journal-file-type 'weekly)
  (setq org-journal-file-format "%Y/w%V_%Y%m%d.org")
  (setq org-journal-date-format "%A (%d %b %Y)")
  (setq org-journal-time-format "")
  (setq org-journal-time-prefix "- ")
  (setq org-journal-carryover-items "")
  (setq org-journal-file-header
        (concat
         "#+Title: Week %V\n"
         "#+OPTIONS: toc:nil tags:nil todo:nil h:2 num:1 ^:{}\n"
         "#+COLUMNS: %%ITEM(Task) %%CLOCKSUM(Clocked) %%EFFORT(Estimated)\n"
         "#+PROPERTY: GitLab 856/33\n"
         "#+PROPERTY: Olmo 1226\n"
         "\n"
         "\n"))
  (progn
    (defun org/clock-in-day ()
      (interactive)
      (org-clock-in-level 1))
    (defun org/clock-in-task-type ()
      (interactive)
      (org-clock-in-level 2))
    (defun org-journal/carryover-todos ()
      "Carry over items from previous date section to current date section.
TODO items are moved with full content; no-state items are copied as
heading only (no body text). DONE items are skipped.
When the previous entry is in a different file, items are always copied
instead of moved (source is left unchanged)."
      (let* ((org-journal-find-file 'find-file)
             (current-file (buffer-file-name))
             (date-level (save-excursion
                           (org-back-to-heading t)
                           (org-outline-level)))
             output-text
             seen-positions
             regions-to-delete
             different-file)
        (save-excursion
          (save-restriction
            (when (org-journal--open-entry t t)
              (setq different-file (not (string= (buffer-file-name) current-file)))
              (unless (org-journal--daily-p)
                (org-narrow-to-subtree))
              (org-map-entries
               (lambda ()
                 (let* ((level (org-outline-level))
                        (todo-state (org-get-todo-state))
                        (item-start (point))
                        (item-end (save-excursion (org-end-of-subtree t t) (point)))
                        parent-texts)
                   (cond
                    ;; Skip the date heading itself
                    ((<= level date-level) nil)
                    ;; Skip completed items and their entire subtrees
                    ((member todo-state org-done-keywords)
                     (setq org-map-continue-from item-end))
                    ;; TODO items: move full subtree with parent context
                    ;; (copy only if from a different file)
                    ((equal todo-state "TODO")
                     (save-excursion
                       (while (and (org-up-heading-safe)
                                   (> (org-outline-level) date-level))
                         (let ((pos (point)))
                           (unless (member pos seen-positions)
                             (push pos seen-positions)
                             (push (buffer-substring-no-properties
                                    pos
                                    (save-excursion (outline-next-heading) (point)))
                                   parent-texts)))))
                     (push item-start seen-positions)
                     (unless different-file
                       (push (cons item-start item-end) regions-to-delete))
                     (setq output-text
                           (concat output-text
                                   (apply #'concat (nreverse parent-texts))
                                   (buffer-substring-no-properties item-start item-end)))
                     (setq org-map-continue-from item-end))
                    ;; No state: copy heading+body if has content, move if empty
                    ;; (always copy if from a different file)
                    (t
                     (let* ((body-start (save-excursion (forward-line 1) (point)))
                            (has-content (string-match-p "\\S-"
                                                         (buffer-substring-no-properties
                                                          body-start item-end))))
                       (save-excursion
                         (while (and (org-up-heading-safe)
                                     (> (org-outline-level) date-level))
                           (let ((pos (point)))
                             (unless (member pos seen-positions)
                               (push pos seen-positions)
                               (push (buffer-substring-no-properties
                                      pos
                                      (save-excursion (outline-next-heading) (point)))
                                     parent-texts)))))
                       (push item-start seen-positions)
                       (if (or has-content different-file)
                           ;; Has content or different file: copy heading line only
                           (setq output-text
                                 (concat output-text
                                         (apply #'concat (nreverse parent-texts))
                                         (buffer-substring-no-properties item-start (line-end-position))
                                         "\n"))
                         ;; Empty and same file: move heading, remove from source
                         (push (cons item-start item-end) regions-to-delete)
                         (setq output-text
                               (concat output-text
                                       (apply #'concat (nreverse parent-texts))
                                       (buffer-substring-no-properties item-start (line-end-position))
                                       "\n"))
                         (setq org-map-continue-from item-end)))))))
               nil)
              (dolist (r (sort regions-to-delete (lambda (a b) (> (car a) (car b)))))
                (delete-region (car r) (cdr r)))
              (unless (org-journal--daily-p)
                (org-narrow-to-subtree))
              (let (empty-found)
                (setq empty-found t)
                (while empty-found
                  (setq empty-found nil)
                  (org-map-entries
                   (lambda ()
                     (let ((level (org-outline-level))
                           (sub-start (point))
                           (sub-end (save-excursion (org-end-of-subtree t t) (point))))
                       (when (and (> level date-level)
                                  (string-match-p
                                   "\\`[ \t\n]*\\'"
                                   (buffer-substring-no-properties
                                    (save-excursion (forward-line 1) (point))
                                    sub-end)))
                         (delete-region sub-start sub-end)
                         (setq empty-found t
                               org-map-continue-from sub-start))))
                   nil)))
              (save-buffer))))
        (when output-text
          (save-excursion
            (org-back-to-heading t)
            (outline-end-of-subtree)
            (unless (bolp) (insert "\n"))
            (insert output-text)))))
    (defun org-journal/show-current-date (&rest _)
      "Expand the full current date section after creating a new entry."
      (save-excursion
        (while (org-up-heading-safe))
        (org-show-subtree)))
    (advice-add 'org-journal-new-entry :after #'org-journal/show-current-date))
  (defvar org-journal/gitlab-hostname "gitlab.igalia.com"
    "GitLab hostname for glab API calls.")
  (defun org-journal/upload-to-gitlab ()
    "Export current journal file to Markdown and upload to a GitLab issue note.
The target is read from the GitLab property (format: ProjectID/IssueIID#NoteID)."
    (interactive)
    (require 'ox-md-title)
    (let ((gitlab-prop (org-entry-get (point-min) "GitLab" t)))
      (unless gitlab-prop
        (user-error "No GitLab property found"))
      (unless (string-match "\\([0-9]+\\)/\\([0-9]+\\)#\\([0-9]+\\)" gitlab-prop)
        (user-error "GitLab property must be in format ProjectID/IssueIID#NoteID"))
      (let* ((project-id (match-string 1 gitlab-prop))
             (issue-iid (match-string 2 gitlab-prop))
             (note-id (match-string 3 gitlab-prop))
             (md-content (progn
                           (org-md-title-add)
                           (unwind-protect
                               (org-export-as 'md nil nil nil)
                             (org-md-title-remove))))
             (md-clean (string-trim
                        (replace-regexp-in-string "\n\\{2,\\}" "\n" md-content))))
        (with-temp-buffer
          (let ((exit-code
                 (call-process "flatpak-spawn" nil t nil
                               "--host"
                               "glab" "api" "--hostname" org-journal/gitlab-hostname
                               "-X" "PUT"
                               (format "projects/%s/issues/%s/notes/%s"
                                       project-id issue-iid note-id)
                               "--raw-field" (format "body=%s" md-clean))))
            (if (zerop exit-code)
                (message "Uploaded to %s project %s issue %s note %s"
                         org-journal/gitlab-hostname project-id issue-iid note-id)
              (error "glab failed (exit %d): %s"
                     exit-code (buffer-string))))))))
  (defun org-journal/add-default-properties ()
    (org-set-property "Effort" "6:00"))
  :bind
  (("C-c C-x C-M-u" . org-journal/upload-to-gitlab)
   ("C-c C-x C-M-i" . org/clock-in-task-type)
   :map org-mode-map
   ("C-c C-x C-i" . org/clock-in-day))
  :hook
  ((org-journal-after-header-create . org-journal/carryover-todos)
   (org-journal-after-header-create . org-journal/add-default-properties)))
(global-set-key (kbd "C-c C-j") 'org-journal-new-entry)

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
                ;; GitHub: /owner/repo/...; GitLab: /group/.../project/-/...
                (path-parts (split-string (car (url-path-and-query parsed-url)) "/" t))
                (project (or (cadr (member "-" (reverse path-parts)))
                             (cadr path-parts)
                             ""))
                (clean-title
                 (cond
                  ((string-match "^\\(.*\\) · Issue #\\([0-9]+\\) · " title)   ; GitHub Issue
                   (concat project "#" (match-string 2 title) " \"" (match-string 1 title) "\""))
                  ((string-match "^\\(.*\\) by [^·]+ · Pull Request #\\([0-9]+\\) · " title)   ; GitHub Pull Request
                   (concat project "!" (match-string 2 title) " \"" (match-string 1 title) "\""))
                  ((string-match "^\\(.*\\) · [^@·]*@\\([^·]*\\) · " title)   ; GitHub Commit
                   (concat project "@" (match-string 2 title) " (\"" (match-string 1 title) "\")"))
                  ((string-match "^\\(.*\\) (#\\([0-9a-z]+\\)) · Issues" title)   ; GitLab Issue
                   (concat project "#" (match-string 2 title) " \"" (match-string 1 title) "\""))
                  ((string-match "\\(.*\\) (!\\([0-9a-z]+\\)) · Merge requests" title)   ; GitLab Merge Request
                   (concat project "!" (match-string 2 title) " \"" (match-string 1 title) "\""))
                  ((string-match "^\\(.*\\) (\\([0-9a-z]+\\)) · Commits" title)   ; GitLab Commit
                   (concat project "@" (match-string 2 title) " (\"" (match-string 1 title) "\")"))
                  (t title)
                  )))
           ;; forward the title to the default org-cliplink transformer
           (org-cliplink-org-mode-link-transformer url clean-title))))))
  :bind
  (("C-c C-x l" . org-cliplink-gh)))

;; Modern looks for Org
(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :init
  ;; Make property drawers compact: smaller and dimmer than regular text,
  ;; keeping property names and values in different (dimmed) colors.
  (defface org-modern/property-drawer
    '((t :height 0.75))
    "Face applied to whole property drawers, including line ends,
so the drawer lines also take less vertical space.")
  ;; These replace Org's faces in the drawer (instead of stacking on them),
  ;; since `org-property-value' inherits `default' and its absolute height
  ;; would otherwise win over the smaller drawer height.
  (defface org-modern/property-marker
    '((t :inherit (org-modern/property-drawer org-drawer)))
    "Dimmed face for :PROPERTIES: and :END: lines.")
  (defface org-modern/property-key
    '((t :inherit (org-modern/property-drawer org-special-keyword)))
    "Dimmed face for property names.")
  (defface org-modern/property-value
    '((t :inherit (org-modern/property-drawer org-property-value)))
    "Dimmed face for property values.")
  (defvar org-modern/property-dim-amount 0.55
    "Weight of the original color when dimming (1.0 = no dimming).")
  (defun org-modern/dim-color (face)
    "Return FACE's foreground blended toward the default background."
    (require 'color)
    (let ((fg (color-name-to-rgb (or (face-foreground face nil 'default) "")))
          (bg (color-name-to-rgb (or (face-background 'default) ""))))
      (if (and fg bg)
          (apply #'color-rgb-to-hex
                 (append (cl-mapcar (lambda (f b)
                                      (+ (* f org-modern/property-dim-amount)
                                         (* b (- 1 org-modern/property-dim-amount))))
                                    fg bg)
                         '(2)))
        'unspecified)))
  (defun org-modern/update-property-faces (&rest _)
    (when (facep 'org-special-keyword)
      (dolist (pair '((org-modern/property-marker . org-drawer)
                      (org-modern/property-key . org-special-keyword)
                      (org-modern/property-value . org-property-value)))
        (set-face-attribute (car pair) nil
                            :foreground (org-modern/dim-color (cdr pair))))))
  (with-eval-after-load 'org (org-modern/update-property-faces))
  (add-hook 'enable-theme-functions #'org-modern/update-property-faces)
  (defun org-modern/match-property-line (limit)
    "Font-lock matcher for any line belonging to a property drawer.
Matches line by line, so partial refontification (e.g. after
folding/unfolding the drawer) keeps the face."
    (let (found)
      (while (and (not found)
                  (re-search-forward "^[ \t]*\\(:[^ \t\n]+:\\)\\(.*\\)\n?" limit t))
        (save-match-data
          (save-excursion
            (goto-char (match-beginning 0))
            (when (memq (org-element-type (org-element-at-point))
                        '(property-drawer node-property))
              (setq found t)))))
      found))
  (defun org-modern/compact-properties ()
    (add-to-list 'org-font-lock-extra-keywords
                 '(org-modern/match-property-line
                   (0 'org-modern/property-drawer append)
                   (1 (if (member (match-string 1) '(":PROPERTIES:" ":END:"))
                          'org-modern/property-marker
                        'org-modern/property-key)
                      t)
                   (2 'org-modern/property-value t))
                 t))
  (add-hook 'org-font-lock-set-keywords-hook #'org-modern/compact-properties))
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
  (org-roam-directory (file-truename "~/Misc/orgfiles/roam"))
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

;; Modern block styling with org-indent.
(use-package org-modern-indent
  :quelpa
  (org-modern-indent
   :fetcher github
   :repo "jdtsmith/org-modern-indent")
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

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
  :diminish
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
