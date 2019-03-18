;;; dired-subtree-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dired-subtree" "dired-subtree.el" (0 0 0 0))
;;; Generated autoloads from dired-subtree.el

(autoload 'dired-subtree-narrow "dired-subtree" "\
Narrow the buffer to this subtree.

\(fn)" t nil)

(autoload 'dired-subtree-up "dired-subtree" "\
Jump up one directory.

\(fn &optional ARG)" t nil)

(autoload 'dired-subtree-down "dired-subtree" "\
Jump down one directory.

\(fn &optional ARG)" t nil)

(autoload 'dired-subtree-next-sibling "dired-subtree" "\
Go to the next sibling.

\(fn &optional ARG)" t nil)

(autoload 'dired-subtree-previous-sibling "dired-subtree" "\
Go to the previous sibling.

\(fn &optional ARG)" t nil)

(autoload 'dired-subtree-beginning "dired-subtree" "\
Go to the first file in this subtree.

\(fn)" t nil)

(autoload 'dired-subtree-end "dired-subtree" "\
Go to the first file in this subtree.

\(fn)" t nil)

(autoload 'dired-subtree-mark-subtree "dired-subtree" "\
Mark all files in this subtree.

With prefix argument mark all the files in subdirectories
recursively.

\(fn &optional ALL)" t nil)

(autoload 'dired-subtree-unmark-subtree "dired-subtree" "\
Unmark all files in this subtree.

With prefix argument unmark all the files in subdirectories
recursively.

\(fn &optional ALL)" t nil)

(autoload 'dired-subtree-revert "dired-subtree" "\
Revert the subtree.

This means reinserting the content of this subtree and all its
children.

\(fn)" t nil)

(autoload 'dired-subtree-insert "dired-subtree" "\
Insert subtree under this directory.

\(fn)" t nil)

(autoload 'dired-subtree-remove "dired-subtree" "\
Remove subtree at point.

\(fn)" t nil)

(autoload 'dired-subtree-toggle "dired-subtree" "\
Insert subtree at point or remove it if it was not present.

\(fn)" t nil)

(autoload 'dired-subtree-cycle "dired-subtree" "\
Org-mode like cycle visibility:

1) Show subtree
2) Show subtree recursively (if previous command was cycle)
3) Remove subtree

Numeric prefix will set max depth

\(fn &optional MAX-DEPTH)" t nil)

(autoload 'dired-subtree-only-this-file "dired-subtree" "\
Remove all the siblings on the route from this file to the top-most directory.

With ARG non-nil, do not remove expanded directories in parents.

\(fn &optional ARG)" t nil)

(autoload 'dired-subtree-only-this-directory "dired-subtree" "\
Remove all the siblings on the route from this directory to the top-most directory.

With ARG non-nil, do not remove expanded directories in parents.

\(fn &optional ARG)" t nil)

(autoload 'dired-subtree-apply-filter "dired-subtree" "\
Push a local filter for this subtree.

This depends on `dired-filter' package.

It works exactly the same as global dired filters, only
restricted to a subtree.  The global filter is also applied to
the subtree.  The filter action is read from `dired-filter-map'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dired-subtree" '("dired-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dired-subtree-autoloads.el ends here
