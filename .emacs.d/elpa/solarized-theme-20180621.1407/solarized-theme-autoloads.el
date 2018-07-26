;;; solarized-theme-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "solarized" "solarized.el" (0 0 0 0))
;;; Generated autoloads from solarized.el

(autoload 'solarized-color-blend "solarized" "\
Blends COLOR1 onto COLOR2 with ALPHA.

COLOR1 and COLOR2 should be color names (e.g. \"white\") or RGB
triplet strings (e.g. \"#ff12ec\").

Alpha should be a float between 0 and 1.

\(fn COLOR1 COLOR2 ALPHA)" nil nil)

(when (and (boundp 'custom-theme-load-path) load-file-name) (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name))))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "solarized" '("create-solarized-theme" "solarized-")))

;;;***

;;;### (autoloads nil "solarized-dark-theme" "solarized-dark-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from solarized-dark-theme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "solarized-dark-theme" '("solarized-dark")))

;;;***

;;;### (autoloads nil "solarized-light-theme" "solarized-light-theme.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from solarized-light-theme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "solarized-light-theme" '("solarized-light")))

;;;***

;;;### (autoloads nil "solarized-theme-utils" "solarized-theme-utils.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from solarized-theme-utils.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "solarized-theme-utils" '("solarized-import-faces")))

;;;***

;;;### (autoloads nil nil ("solarized-theme-pkg.el" "solarized-theme.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; solarized-theme-autoloads.el ends here
