;;; biblio-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "biblio-arxiv" "biblio-arxiv.el" (0 0 0 0))
;;; Generated autoloads from biblio-arxiv.el

(autoload 'biblio-arxiv-backend "biblio-arxiv" "\
A arXiv backend for biblio.el.
COMMAND, ARG, MORE: See `biblio-backends'.

\(fn COMMAND &optional ARG &rest MORE)" nil nil)

(add-hook 'biblio-init-hook #'biblio-arxiv-backend)

(autoload 'biblio-arxiv-lookup "biblio-arxiv" "\
Start an arXiv search for QUERY, prompting if needed.

\(fn &optional QUERY)" t nil)

(defalias 'arxiv-lookup 'biblio-arxiv-lookup)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "biblio-arxiv" '("biblio-arxiv-")))

;;;***

;;;### (autoloads nil "biblio-crossref" "biblio-crossref.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from biblio-crossref.el

(autoload 'biblio-crossref-backend "biblio-crossref" "\
A CrossRef backend for biblio.el.
COMMAND, ARG, MORE: See `biblio-backends'.

\(fn COMMAND &optional ARG &rest MORE)" nil nil)

(add-hook 'biblio-init-hook #'biblio-crossref-backend)

(autoload 'biblio-crossref-lookup "biblio-crossref" "\
Start a CrossRef search for QUERY, prompting if needed.

\(fn &optional QUERY)" t nil)

(defalias 'crossref-lookup 'biblio-crossref-lookup)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "biblio-crossref" '("biblio-crossref-")))

;;;***

;;;### (autoloads nil "biblio-dblp" "biblio-dblp.el" (0 0 0 0))
;;; Generated autoloads from biblio-dblp.el

(autoload 'biblio-dblp-backend "biblio-dblp" "\
A DBLP backend for biblio.el.
COMMAND, ARG, MORE: See `biblio-backends'.

\(fn COMMAND &optional ARG &rest MORE)" nil nil)

(add-hook 'biblio-init-hook #'biblio-dblp-backend)

(autoload 'biblio-dblp-lookup "biblio-dblp" "\
Start a DBLP search for QUERY, prompting if needed.

\(fn &optional QUERY)" t nil)

(defalias 'dblp-lookup 'biblio-dblp-lookup)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "biblio-dblp" '("biblio-dblp--")))

;;;***

;;;### (autoloads nil "biblio-dissemin" "biblio-dissemin.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from biblio-dissemin.el

(autoload 'biblio-dissemin-lookup "biblio-dissemin" "\
Retrieve a record by DOI from Dissemin, and display it.
Interactively, or if CLEANUP is non-nil, pass DOI through
`biblio-cleanup-doi'.

\(fn DOI &optional CLEANUP)" t nil)

(defalias 'dissemin-lookup 'biblio-dissemin-lookup)

(autoload 'biblio-dissemin--register-action "biblio-dissemin" "\
Add Dissemin to list of `biblio-selection-mode' actions.

\(fn)" nil nil)

(add-hook 'biblio-selection-mode-hook #'biblio-dissemin--register-action)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "biblio-dissemin" '("biblio-dissemin--")))

;;;***

;;;### (autoloads nil "biblio-doi" "biblio-doi.el" (0 0 0 0))
;;; Generated autoloads from biblio-doi.el

(autoload 'doi-insert-bibtex "biblio-doi" "\
Insert BibTeX entry matching DOI.

\(fn DOI)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "biblio-doi" '("biblio-doi-")))

;;;***

;;;### (autoloads nil "biblio-download" "biblio-download.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from biblio-download.el

(autoload 'biblio-download--register-action "biblio-download" "\
Add download to list of `biblio-selection-mode' actions.

\(fn)" nil nil)

(add-hook 'biblio-selection-mode-hook #'biblio-download--register-action)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "biblio-download" '("biblio-download-")))

;;;***

;;;### (autoloads nil "biblio-hal" "biblio-hal.el" (0 0 0 0))
;;; Generated autoloads from biblio-hal.el

(autoload 'biblio-hal-backend "biblio-hal" "\
A HAL backend for biblio.el.
COMMAND, ARG, MORE: See `biblio-backends'.

\(fn COMMAND &optional ARG &rest MORE)" nil nil)

(add-hook 'biblio-init-hook #'biblio-hal-backend)

(autoload 'biblio-hal-lookup "biblio-hal" "\
Start a HAL search for QUERY, prompting if needed.

\(fn &optional QUERY)" t nil)

(defalias 'hal-lookup 'biblio-hal-lookup)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "biblio-hal" '("biblio-hal--")))

;;;***

;;;### (autoloads nil "biblio-ieee" "biblio-ieee.el" (0 0 0 0))
;;; Generated autoloads from biblio-ieee.el

(autoload 'biblio-ieee-backend "biblio-ieee" "\
A IEEE Xplore backend for biblio.el.
COMMAND, ARG, MORE: See `biblio-backends'.

\(fn COMMAND &optional ARG &rest MORE)" nil nil)

(add-hook 'biblio-init-hook #'biblio-ieee-backend)

(autoload 'biblio-ieee-lookup "biblio-ieee" "\
Start a IEEE search for QUERY, prompting if needed.

\(fn &optional QUERY)" t nil)

(defalias 'ieee-lookup 'biblio-ieee-lookup)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "biblio-ieee" '("biblio-ieee--")))

;;;***

;;;### (autoloads nil nil ("biblio-pkg.el" "biblio.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; biblio-autoloads.el ends here
