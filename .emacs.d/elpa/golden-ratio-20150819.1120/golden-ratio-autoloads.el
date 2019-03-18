;;; golden-ratio-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "golden-ratio" "golden-ratio.el" (0 0 0 0))
;;; Generated autoloads from golden-ratio.el

(autoload 'golden-ratio "golden-ratio" "\
Resizes current window to the golden-ratio's size specs.

\(fn &optional ARG)" t nil)

(defvar golden-ratio-mode nil "\
Non-nil if Golden-Ratio mode is enabled.
See the `golden-ratio-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `golden-ratio-mode'.")

(custom-autoload 'golden-ratio-mode "golden-ratio" nil)

(autoload 'golden-ratio-mode "golden-ratio" "\
Enable automatic window resizing with golden ratio.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "golden-ratio" '("golden-ratio-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; golden-ratio-autoloads.el ends here
