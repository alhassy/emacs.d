(org-babel-load-file "~/.emacs.d/init.org")
;;
;; My Emacs settings: (find-file "~/.emacs.d/init.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((eval load-file "CheatSheet.el")
     (eval org-babel-tangle)
     (eval setq org-todo-keyword-faces
	   (quote
	    (("TODO" . org-warning)
	     ("STARTED" . "yellow"))))
     (eval setq org-todo-keyword-faces
	   (quote
	    (("TODO" . org-warning)
	     ("STARTED" . "yellow")
	     ("CANCELED" :foreground "green" :weight bold))))
     (eval org-babel-load-file "~/AlBasmala.org")
     (eval org-shifttab)
     (eval load-file "AlBasmala.el")
     (eval setq NAME
	   (file-name-sans-extension
	    (buffer-name)))
     (eval remove
	   (concat "../content/" NAMEorg)
	   commitables)
     (eval load-file "~/alhassy.github.io/content/AlBasmala.el")
     (eval visual-line-mode t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flyspell-incorrect ((t (:inverse-video t)))))
(put 'downcase-region 'disabled nil)
