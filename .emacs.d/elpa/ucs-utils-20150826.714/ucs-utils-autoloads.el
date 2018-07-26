;;; ucs-utils-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ucs-utils" "ucs-utils.el" (0 0 0 0))
;;; Generated autoloads from ucs-utils.el

(let ((loads (get 'ucs-utils 'custom-loads))) (if (member '"ucs-utils" loads) nil (put 'ucs-utils 'custom-loads (cons '"ucs-utils" loads))))

(autoload 'ucs-utils-pretty-name "ucs-utils" "\
Return a prettified UCS name for CHAR.

Based on `get-char-code-property'.  The result has been
title-cased for readability, and will not match into the
`ucs-utils-names' alist until it has been upcased.
`ucs-utils-char' can be used on the title-cased name.

Returns a hexified string if no name is found.  If NO-HEX is
non-nil, then a nil value will be returned when no name is
found.

\(fn CHAR &optional NO-HEX)" nil nil)

(autoload 'ucs-utils-all-prettified-names "ucs-utils" "\
All prettified UCS names, cached in list `ucs-utils-all-prettified-names'.

When optional PROGRESS is given, show progress when generating
cache.

When optional REGENERATE is given, re-generate cache.

\(fn &optional PROGRESS REGENERATE)" nil nil)

(autoload 'ucs-utils-char "ucs-utils" "\
Return the character corresponding to NAME, a UCS name.

NAME is matched leniently by `ucs-utils--lookup'.

Returns FALLBACK if NAME does not exist or is not displayable
according to TEST.  FALLBACK may be either a UCS name or
character, or one of the special symbols described in the next
paragraph.

If FALLBACK is nil or 'drop, returns nil on failure.  If FALLBACK
is 'error, throws an error on failure.

TEST is an optional predicate which characters must pass.  A
useful value is 'char-displayable-p, which is available as
the abbreviation 'cdp, unless you have otherwise defined that
symbol.

When NAME is a character, it passes through unchanged, unless
TEST is set, in which case it must pass TEST.

\(fn NAME &optional FALLBACK TEST)" nil nil)

(autoload 'ucs-utils-first-existing-char "ucs-utils" "\
Return the first existing character from SEQUENCE of character names.

TEST is an optional predicate which characters must pass.  A
useful value is 'char-displayable-p, which is available as
the abbreviation 'cdp, unless you have otherwise defined that
symbol.

\(fn SEQUENCE &optional TEST)" nil nil)

(autoload 'ucs-utils-vector "ucs-utils" "\
Return a vector corresponding to SEQUENCE of UCS names or characters.

If SEQUENCE is a single string or character, it will be coerced
to a list of length 1.  Each name in SEQUENCE is matched
leniently by `ucs-utils--lookup'.

FALLBACK should be a sequence of equal length to SEQUENCE, (or
one of the special symbols described in the next paragraph).  For
any element of SEQUENCE which does not exist or is not
displayable according to TEST, that element degrades to the
corresponding element of FALLBACK.

When FALLBACK is nil, characters which do not exist or are
undisplayable will be given as nils in the return value.  When
FALLBACK is 'drop, such characters will be silently dropped from
the return value.  When FALLBACK is 'error, such characters cause
an error to be thrown.

To allow fallbacks of arbitrary length, give FALLBACK as a vector-
of-vectors, with outer length equal to the length of sequence.  The
inner vectors may contain a sequence of characters, a literal
string, or nil.  Eg

   (ucs-utils-vector '(\"Middle Dot\" \"Ampersand\" \"Horizontal Ellipsis\")
                     '[?.           [?a ?n ?d]  [\"...\"]              ])

or

   (ucs-utils-vector \"Horizontal Ellipsis\" '[[\"...\"]])

TEST is an optional predicate which characters must pass.  A
useful value is 'char-displayable-p, which is available as
the abbreviation 'cdp, unless you have otherwise defined that
symbol.

If NO-FLATTEN is non-nil, then a vector-of-vectors may be returned
if multi-character fallbacks were used as in the example above.

\(fn SEQUENCE &optional FALLBACK TEST NO-FLATTEN)" nil nil)

(autoload 'ucs-utils-string "ucs-utils" "\
Return a string corresponding to SEQUENCE of UCS names or characters.

If SEQUENCE is a single string, it will be coerced to a list of
length 1.  Each name in SEQUENCE is matched leniently by
`ucs-utils--lookup'.

FALLBACK should be a sequence of equal length to SEQUENCE, (or
one of the special symbols described in the next paragraph).  For
any element of SEQUENCE which does not exist or is not
displayable according to TEST, that element degrades to the
corresponding element of FALLBACK.

When FALLBACK is nil or 'drop, characters which do not exist or
are undisplayable will be silently dropped from the return value.
When FALLBACK is 'error, such characters cause an error to be
thrown.

TEST is an optional predicate which characters must pass.  A
useful value is 'char-displayable-p, which is available as
the abbreviation 'cdp, unless you have otherwise defined that
symbol.

\(fn SEQUENCE &optional FALLBACK TEST)" nil nil)

(autoload 'ucs-utils-intact-string "ucs-utils" "\
Return a string corresponding to SEQUENCE of UCS names or characters.

This function differs from `ucs-utils-string' in that FALLBACK is
a non-optional single string, to be used unless every member of
SEQUENCE exists and passes TEST.  FALLBACK may not be nil, 'error,
or 'drop as in `ucs-utils-string'.

If SEQUENCE is a single string, it will be coerced to a list of
length 1.  Each name in SEQUENCE is matched leniently by
`ucs-utils--lookup'.

TEST is an optional predicate which characters must pass.  A
useful value is 'char-displayable-p, which is available as
the abbreviation 'cdp, unless you have otherwise defined that
symbol.

\(fn SEQUENCE FALLBACK &optional TEST)" nil nil)

(autoload 'ucs-utils-subst-char-in-region "ucs-utils" "\
From START to END, replace FROM-CHAR with TO-CHAR each time it occurs.

If optional arg NO-UNDO is non-nil, don't record this change for
undo and don't mark the buffer as really changed.

Characters may be of differing byte-lengths.

The character at the position END is not included, matching the
behavior of `subst-char-in-region'.

This function is slower than `subst-char-in-region'.

\(fn START END FROM-CHAR TO-CHAR &optional NO-UNDO)" nil nil)

(autoload 'ucs-utils-read-char-by-name "ucs-utils" "\
Read a character by its Unicode name or hex number string.

A wrapper for `read-char-by-name', with the option to use
`ido-completing-read'.

PROMPT is displayed, and a string that represents a character by
its name is read.

When IDO is set, several seconds are required on the first
run as all completion candidates are pre-generated.

\(fn PROMPT &optional IDO)" nil nil)

(autoload 'ucs-utils-eval "ucs-utils" "\
Display a string UCS name for the character at POS.

POS defaults to the current point.

If `transient-mark-mode' is enabled and there is an active
region, return a list of strings UCS names, one for each
character in the region.  If called from Lisp with an
explicit POS, ignores the region.

If called with universal prefix ARG, display the result
in a separate buffer.  If called with two universal
prefix ARGs, replace the current character or region with
its UCS name translation.

\(fn &optional POS ARG)" t nil)

(autoload 'ucs-utils-ucs-insert "ucs-utils" "\
Insert CHARACTER in COUNT copies, where CHARACTER is a Unicode code point.

Works like `ucs-insert', with the following differences

    * Uses `ido-completing-read' at the interactive prompt

    * If `transient-mark-mode' is enabled, and the region contains
      a valid UCS character name, that value is used as the
      character name and the region is replaced.

    * A UCS character name string may be passed for CHARACTER.

INHERIT is as documented at `ucs-insert'.

\(fn CHARACTER &optional COUNT INHERIT)" t nil)

(autoload 'ucs-utils-install-aliases "ucs-utils" "\
Install aliases outside the \"ucs-utils-\" namespace.

The following aliases will be installed:

    `ucs-char'                  for   `ucs-utils-char'
    `ucs-first-existing-char'   for   `ucs-utils-first-existing-char'
    `ucs-string'                for   `ucs-utils-string'
    `ucs-intact-string'         for   `ucs-utils-intact-string'
    `ucs-vector'                for   `ucs-utils-vector'
    `ucs-pretty-name'           for   `ucs-utils-pretty-name'
    `ucs-eval'                  for   `ucs-utils-eval'

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ucs-utils" '("ucs-utils-" "persistent-soft" "character-name-history")))

;;;***

;;;### (autoloads nil nil ("ucs-utils-6.0-delta.el" "ucs-utils-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ucs-utils-autoloads.el ends here
