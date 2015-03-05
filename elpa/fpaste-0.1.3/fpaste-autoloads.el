;;; fpaste-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "fpaste" "fpaste.el" (21752 17360 781685 557000))
;;; Generated autoloads from fpaste.el

(let ((loads (get 'fpaste 'custom-loads))) (if (member '"fpaste" loads) nil (put 'fpaste 'custom-loads (cons '"fpaste" loads))))

(autoload 'fpaste "fpaste" "\
Send the text to fpaste.  If there is a region selected, send it.
Otherwise, send the whole buffer.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; fpaste-autoloads.el ends here
