;; For some reason, I need these here or my org-mode defaults to an older version.
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)
(require 'org-tempo)

;; Avoid out-dated byte-compiled Elisp files.
(setq load-prefer-newer t)

(org-babel-load-file "~/.emacs.d/init.org")
;; (find-file "~/.emacs.d/init.org")
;; (find-file "~/.emacs.d/init.el")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-bullets writegood-mode which-key use-package unicode-input unicode-fonts unicode-emoticons spacemacs-theme solarized-theme rainbow-delimiters powerthesaurus org-plus-contrib nyan-mode neotree multiple-cursors magit ivy imenu-list htmlize helm-projectile helm-ag haskell-mode google-this fill-column-indicator doom-themes dash-functional company-coq biblio auto-compile alert)))
 '(safe-local-variable-values
   (quote
    ((eval when nil
	   (remove
	    (concat "../content/" NAMEorg)
	    commitables))
     (eval when nil
	   (load-file "~/alhassy.github.io/content/AlBasmala.el"))
     (eval visual-line-mode t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flyspell-incorrect ((t (:inverse-video t)))))
