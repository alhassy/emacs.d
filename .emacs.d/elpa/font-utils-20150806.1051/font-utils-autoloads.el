;;; font-utils-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "font-utils" "font-utils.el" (0 0 0 0))
;;; Generated autoloads from font-utils.el

(let ((loads (get 'font-utils 'custom-loads))) (if (member '"font-utils" loads) nil (put 'font-utils 'custom-loads (cons '"font-utils" loads))))

(autoload 'font-utils-client-hostname "font-utils" "\
Guess the client hostname, respecting $SSH_CONNECTION.

\(fn)" nil nil)

(autoload 'font-utils-name-from-xlfd "font-utils" "\
Return the font-family name from XLFD, a string.

This function accounts for the fact that the XLFD
delimiter, \"-\", is a legal character within fields.

\(fn XLFD)" nil nil)

(autoload 'font-utils-parse-name "font-utils" "\
Parse FONT-NAME which may contain fontconfig-style specifications.

Returns two-element list.  The car is the font family name as a string.
The cadr is the specifications as a normalized and sorted list.

\(fn FONT-NAME)" nil nil)

(autoload 'font-utils-normalize-name "font-utils" "\
Normalize FONT-NAME which may contain fontconfig-style specifications.

\(fn FONT-NAME)" nil nil)

(autoload 'font-utils-lenient-name-equal "font-utils" "\
Leniently match two strings, FONT-NAME-A and FONT-NAME-B.

\(fn FONT-NAME-A FONT-NAME-B)" nil nil)

(autoload 'font-utils-is-qualified-variant "font-utils" "\
Whether FONT-NAME-1 and FONT-NAME-2 are different variants of the same font.

Qualifications are fontconfig-style specifications added to a
font name, such as \":width=condensed\".

To return t, the font families must be identical, and the
qualifications must differ.  If FONT-NAME-1 and FONT-NAME-2 are
identical, returns nil.

\(fn FONT-NAME-1 FONT-NAME-2)" nil nil)

(autoload 'font-utils-list-names "font-utils" "\
Return a list of all font names on the current system.

\(fn)" nil nil)

(autoload 'font-utils-read-name "font-utils" "\
Read a font name using `completing-read'.

Underscores are removed from the return value.

Uses `ido-completing-read' if optional IDO is set.

\(fn &optional IDO)" nil nil)

(autoload 'font-utils-exists-p "font-utils" "\
Test whether FONT-NAME (a string or font object) exists.

FONT-NAME is a string, typically in Fontconfig font-name format.
A font-spec, font-vector, or font-object are accepted, though
the behavior for the latter two is not well defined.

Returns a matching font vector.

When POINT-SIZE is set, check for a specific font size.  Size may
also be given at the end of a string FONT-NAME, eg \"Monaco-12\".

When optional STRICT is given, FONT-NAME must will not be
leniently modified before passing to `font-info'.

Optional SCOPE is a list of font names, within which FONT-NAME
must (leniently) match.

\(fn FONT-NAME &optional POINT-SIZE STRICT SCOPE)" nil nil)

(autoload 'font-utils-first-existing-font "font-utils" "\
Return the (normalized) first existing font name from FONT-NAMES.

FONT-NAMES is a list, with each element typically in Fontconfig
font-name format.

The font existence-check is lazy; fonts after the first hit are
not checked.

If NO-NORMALIZE is given, the return value is exactly as the
member of FONT-NAMES.  Otherwise, the family name is extracted
from the XLFD returned by `font-info'.

\(fn FONT-NAMES &optional NO-NORMALIZE)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "font-utils" '("font-" "persistent-softest-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; font-utils-autoloads.el ends here
