;;; doom-themes-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "doom-themes" "doom-themes.el" (23354 11631
;;;;;;  636365 284000))
;;; Generated autoloads from doom-themes.el

(autoload 'doom-name-to-rgb "doom-themes" "\
Retrieves the hexidecimal string repesented the named COLOR (e.g. \"red\")
for FRAME (defaults to the current frame).

\(fn COLOR &optional FRAME)" nil nil)

(autoload 'doom-blend "doom-themes" "\
Blend two colors (hexidecimal strings) together by a coefficient ALPHA (a
float between 0 and 1)

\(fn COLOR1 COLOR2 ALPHA)" nil nil)

(autoload 'doom-darken "doom-themes" "\
Darken a COLOR (a hexidecimal string) by a coefficient ALPHA (a float between
0 and 1).

\(fn COLOR ALPHA)" nil nil)

(autoload 'doom-lighten "doom-themes" "\
Brighten a COLOR (a hexidecimal string) by a coefficient ALPHA (a float
between 0 and 1).

\(fn COLOR ALPHA)" nil nil)

(autoload 'doom-color "doom-themes" "\
Retrieve a specific color named NAME (a symbol) from the current theme.

\(fn NAME &optional TYPE)" nil nil)

(autoload 'doom-ref "doom-themes" "\
TODO

\(fn FACE PROP &optional CLASS)" nil nil)

(autoload 'doom-themes-set-faces "doom-themes" "\
Customize THEME (a symbol) with FACES.

\(fn THEME &rest FACES)" nil t)

(put 'doom-themes-set-faces 'lisp-indent-function 'defun)

(autoload 'doom-themes-org-config "doom-themes" "\
Enable custom fontification and improves doom-themes integration with org-mode.

\(fn)" nil nil)

(autoload 'doom-themes-neotree-config "doom-themes" "\
Install doom-themes' neotree configuration.

Includes an Atom-esque icon theme and highlighting based on filetype.

\(fn)" nil nil)

(autoload 'doom-themes-visual-bell-config "doom-themes" "\
Enable flashing the mode-line on error.

\(fn)" nil nil)

(autoload 'doom-themes-visual-bell-fn "doom-themes" "\
Blink the mode-line red briefly. Set `ring-bell-function' to this to use it.

\(fn)" nil nil)

(when (and (boundp 'custom-theme-load-path) load-file-name) (let* ((base (file-name-directory load-file-name)) (dir (expand-file-name "themes/" base))) (add-to-list 'custom-theme-load-path (or (and (file-directory-p dir) dir) base))))

;;;***

;;;### (autoloads nil nil ("doom-challenger-deep-theme.el" "doom-city-lights-theme.el"
;;;;;;  "doom-dracula-theme.el" "doom-molokai-theme.el" "doom-nord-light-theme.el"
;;;;;;  "doom-nord-theme.el" "doom-nova-theme.el" "doom-one-light-theme.el"
;;;;;;  "doom-one-theme.el" "doom-opera-light-theme.el" "doom-opera-theme.el"
;;;;;;  "doom-peacock-theme.el" "doom-solarized-light-theme.el" "doom-spacegrey-theme.el"
;;;;;;  "doom-themes-common.el" "doom-themes-neotree.el" "doom-themes-org.el"
;;;;;;  "doom-themes-pkg.el" "doom-tomorrow-day-theme.el" "doom-tomorrow-night-theme.el"
;;;;;;  "doom-vibrant-theme.el") (23354 11633 399841 441000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; doom-themes-autoloads.el ends here
