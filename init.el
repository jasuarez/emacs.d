;; No splash message
(setq inhibit-splash-screen t)

;; Store Customs in different place
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
