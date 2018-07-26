;;; fill-column-indicator-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "fill-column-indicator" "fill-column-indicator.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from fill-column-indicator.el

(autoload 'fci-mode "fill-column-indicator" "\
Toggle fci-mode on and off.
Fci-mode indicates the location of the fill column by drawing a
thin line (a `rule') at the fill column.

With prefix ARG, turn fci-mode on if and only if ARG is positive.

The following options control the appearance of the fill-column
rule: `fci-rule-column', `fci-rule-width', `fci-rule-color',
`fci-rule-use-dashes', `fci-dash-pattern', `fci-rule-character',
and `fci-rule-character-color'.  For further options, see the
Customization menu or the package file.  (See the latter for tips
on troubleshooting.)

\(fn &optional ARG)" t nil)

(autoload 'turn-on-fci-mode "fill-column-indicator" "\
Turn on fci-mode unconditionally.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "fill-column-indicator" '("fci-" "turn-off-fci-mode")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; fill-column-indicator-autoloads.el ends here
