;; Avoid out-dated byte-compiled Elisp files.
(setq load-prefer-newer t)

(setq org-confirm-babel-evaluate nil)
(setq enable-local-variables :all)
  (org-babel-load-file "~/.emacs.d/init.org")
(setq enable-local-variables t)

;; (find-file "~/.emacs.d/init.org")
;; (find-file "~/.emacs.d/init.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default-input-method "Agda")
 '(electric-pair-mode t)
 '(flycheck-display-errors-delay 0.3)
 '(package-selected-packages
   (quote
    (org-journal yankpad perspective eyebrowse company-emoji ac-emoji emojify-logos emojify company helm fancy-battery synosaurus s beacon spaceline org-bullets diminish speed-type counsel helm-swoop helm-swiper inf-haskell ob-haskell undo-tree golden-ratio shell-pop all-the-icons writegood-mode which-key use-package unicode-input unicode-fonts unicode-emoticons spacemacs-theme solarized-theme rainbow-delimiters powerthesaurus org-plus-contrib nyan-mode neotree multiple-cursors magit imenu-list htmlize helm-projectile helm-ag haskell-mode google-this fill-column-indicator doom-themes dash-functional company-coq biblio auto-compile alert)))
 '(safe-local-variable-values
   (quote
    ((eval progn
	   (org-babel-goto-named-src-block "make-readme")
	   (org-babel-execute-src-block)
	   (outline-hide-sublevels 1))
     (eval when nil
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
(put 'downcase-region 'disabled nil)
