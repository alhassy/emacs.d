;;; unicode-fonts-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "unicode-fonts" "unicode-fonts.el" (0 0 0 0))
;;; Generated autoloads from unicode-fonts.el

(let ((loads (get 'unicode-fonts 'custom-loads))) (if (member '"unicode-fonts" loads) nil (put 'unicode-fonts 'custom-loads (cons '"unicode-fonts" loads))))

(let ((loads (get 'unicode-fonts-tweaks 'custom-loads))) (if (member '"unicode-fonts" loads) nil (put 'unicode-fonts-tweaks 'custom-loads (cons '"unicode-fonts" loads))))

(let ((loads (get 'unicode-fonts-debug 'custom-loads))) (if (member '"unicode-fonts" loads) nil (put 'unicode-fonts-debug 'custom-loads (cons '"unicode-fonts" loads))))

(autoload 'unicode-fonts-first-existing-font "unicode-fonts" "\
Return the (normalized) first existing font name from FONT-NAMES.

FONT-NAMES is a list, with each element typically in Fontconfig
font-name format.

The font existence-check is lazy; fonts after the first hit are
not checked.

\(fn FONT-NAMES)" nil nil)

(autoload 'unicode-fonts-font-exists-p "unicode-fonts" "\
Run `font-utils-exists-p' with a limited scope.

The scope is defined by `unicode-fonts-restrict-to-fonts'.

FONT-NAME, POINT-SIZE, and STRICT are as documented at
`font-utils-exists-p'.

\(fn FONT-NAME &optional POINT-SIZE STRICT)" nil nil)

(autoload 'unicode-fonts-read-block-name "unicode-fonts" "\
Read a Unicode block name using `completing-read'.

Spaces are replaced with underscores in completion values, but
are removed from the return value.

Use `ido-completing-read' if IDO is set.

\(fn &optional IDO)" nil nil)

(autoload 'unicode-fonts-setup "unicode-fonts" "\
Set up Unicode fonts for FONTSET-NAMES.

Optional FONTSET-NAMES must be a list of strings.  Fontset names
which do not currently exist will be ignored.  The default value
is `unicode-fonts-fontset-names'.

Optional REGENERATE requests that the disk cache be invalidated
and regenerated.

\(fn &optional FONTSET-NAMES REGENERATE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "unicode-fonts" '("unicode-" "persistent-softest-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; unicode-fonts-autoloads.el ends here
