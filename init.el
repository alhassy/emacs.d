;; [[file:init.org::*Intro to snippets][Intro to snippets:1]]
;; Add yasnippet support for all company backends
;;
(cl-defun my/company-backend-with-yankpad (backend)
  "There can only be one main completition backend, so let's
   enable yasnippet/yankpad as a secondary for all completion
   backends.

   Src: https://emacs.stackexchange.com/a/10520/10352"

  (if (and (listp backend) (member 'company-yankpad backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yankpad))))
;; Intro to snippets:1 ends here

;; [[file:init.org::*Intro to snippets][Intro to snippets:2]]
;; Yet another snippet extension program
(use-package yasnippet
  :diminish yas-minor-mode
  :config
    (yas-global-mode 1) ;; Always have this on for when using yasnippet syntax within yankpad
    ;; respect the spacing in my snippet declarations
    (setq yas-indent-line 'fixed))

;; Alternative, Org-based extension program
(use-package yankpad
  :diminish
  :config
    ;; Location of templates
    (setq yankpad-file "~/.emacs.d/yankpad.org")

    ;; Ignore major mode, always use defaults.
    ;; Yankpad will freeze if no org heading has the name of the given category.
    (setq yankpad-category "Default")

    ;; Load the snippet templates ---useful after yankpad is altered
    (yankpad-reload)

    ;; Set company-backend as a secondary completion backend to all existing backends.
    (setq company-backends (mapcar #'my/company-backend-with-yankpad company-backends)))
;; Intro to snippets:2 ends here

;; [[file:init.org::*Intro to snippets][Intro to snippets:5]]
(cl-defun org-insert-link ()
  "Makes an org link by inserting the URL copied to clipboard and
  prompting for the link description only.

  Type over the shown link to change it, or tab to move to the
  description field.

  This overrides Org-mode's built-in ‘org-insert-link’ utility;
  whence C-c C-l uses the snippet."
  (interactive)
  (insert "my_org_insert_link")
  (yankpad-expand))
;; Intro to snippets:5 ends here

;; [[file:init.org::*Emojis][Emojis:2]]
;; Get all unicode emojis to appear within Emacs
;; See also: https://emacs.stackexchange.com/questions/5689/force-a-single-font-for-all-unicode-glyphs?rq=1
(set-fontset-font t nil "Apple Color Emoji")
;; Emojis:2 ends here

;; [[file:init.org::*Re-Enabling Templates][Re-Enabling Templates:1]]
;; After init hook; see above near use-package install.
(yankpad-reload)
;; Re-Enabling Templates:1 ends here

# [[file:init.org::*Templates from other places in my init][Templates from other places in my init:1]]
#+begin_src org :noweb yes :tangle "~/.emacs.d/yankpad.org" :comments none
<<templates-from-other-places-in-my-init>>
#+end_src
# Templates from other places in my init:1 ends here

# [[file:init.org::*Templates from other places in my init][Templates from other places in my init:3]]
:PROPERTIES:
:CUSTOM_ID: Templates-from-other-places-in-my-init
:END:
# Templates from other places in my init:3 ends here
