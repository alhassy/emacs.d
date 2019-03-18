;;; dired-sidebar-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dired-sidebar" "dired-sidebar.el" (0 0 0 0))
;;; Generated autoloads from dired-sidebar.el

(autoload 'dired-sidebar-toggle-sidebar "dired-sidebar" "\
Toggle the project explorer window.
Optional argument DIR Use DIR as sidebar root if available.

With universal argument, use current directory.

\(fn &optional DIR)" t nil)

(autoload 'dired-sidebar-toggle-with-current-directory "dired-sidebar" "\
Like `dired-sidebar-toggle-sidebar' but use current-directory.

\(fn)" t nil)

(autoload 'dired-sidebar-show-sidebar "dired-sidebar" "\
Show sidebar displaying buffer B.

\(fn &optional B)" t nil)

(autoload 'dired-sidebar-hide-sidebar "dired-sidebar" "\
Hide the sidebar in the selected frame.

\(fn)" t nil)

(autoload 'dired-sidebar-jump-to-sidebar "dired-sidebar" "\
Jump to `dired-sidebar' buffer if it is showing.

If it's not showing, act as `dired-sidebar-toggle-sidebar'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dired-sidebar" '("dired-sidebar-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dired-sidebar-autoloads.el ends here
