
;; In-case I forget to byte-compile!
(byte-compile-file "~/.emacs")

;; Change this silly counter to visualy notice a change.
;; (progn (message "Init.org contents loaded! Counter: 7") (sleep-for 3))

;; Accept all local variables versus query for possibly non-safe locals.
(defun DANGER-all-locals () (setq enable-local-variables :all))
(defun SAFE-query-locals () (setq enable-local-variables t))

; (load (shell-command-to-string "agda-mode locate"))
;;
;; Seeing: One way to avoid seeing this warning is to make sure that agda2-include-dirs is not bound.
; (makunbound 'agda2-include-dirs)

;; Open .v files with Proof General's Coq mode
;; (load "~/.emacs.d/lisp/PG/generic/proof-site")

;; now C-c C-l interpets the current buffer; will ofcourse need to switch to the ghci buffer
 (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

(add-to-list 'default-frame-alist '(background-color . "#fcf4dc"))

  (setq visible-bell 1)
  ;; Enable flashing mode-line on errors

(setq initial-buffer-choice "~/Dropbox/todo.org")

(setq display-time-day-and-date t)
(display-time)
(display-battery-mode 1)

(line-number-mode 1)
(column-number-mode 1)

(load-library "paren")
(show-paren-mode 1)
(transient-mark-mode t)
(require 'paren)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
  ;; C-x C-0 restores the default font size

  (delete-selection-mode 1)

(ido-mode t)

(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-m") 'recompile)
    map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")

(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t nil)))

;; M-k kills to the left
(global-set-key "\M-k" '(lambda () (interactive) (kill-line 0)) )

(defun file-as-list (filename)
  "Return the contents of FILENAME as a list of lines"
  (with-temp-buffer
    (insert-file-contents filename)
    (split-string (buffer-string))))

(defun file-as-string (filename)
  "Return the contents of FILENAME as a list of lines"
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; A very simple function to recreate the scratch buffer:
;; ( http://emacswiki.org/emacs/RecreateScratchBuffer )
(defun create-scratch-buffer nil
   "create a scratch buffer"
   (interactive)
   (switch-to-buffer (get-buffer-create "*scratch*"))
   (lisp-interaction-mode))   

(defun ensure-two-vertical-windows () 
  "hello"
 (interactive)
 (other-window 1)			;; C-x 0
 (let ((otherBuffer (buffer-name))) 
   (delete-window)			;; C-x 0
   (split-window-right)			;; C-x 3
   (other-window 1)			;; C-x 0
   (switch-to-buffer otherBuffer)	;; C-x b RET
 )
 (other-window 1)
)
(global-set-key (kbd "C-|") 'ensure-two-vertical-windows)

(defun my-org-html-export-to-html ()
 "Make an html from an org file then open it in my browser."
 (interactive)
 (org-html-export-to-html)
 (let ((it (concat (file-name-sans-extension buffer-file-name) ".html")))
   (browse-url it)
   (message (concat it " has been opened in Chromium."))
   'success ;; otherwise we obtain a "compiler error".
 ) 
)

(defun re-replace-in-file (file regex whatDo) "Find and replace a regular expression in-place in a file."

    (find-file file)
    (goto-char 0)
    (let ((altered (replace-regexp-in-string regex whatDo (buffer-string))))
      (erase-buffer)
      (insert altered)
      (save-buffer)
      (kill-buffer)
   )
)

(defun mapsto (this that)
  "In the current buffer make the regular expression rewrite: this ↦ that."
  (let* ((current-location (point))
       ;; Do not alter the case of the <replacement text>.
       (altered (replace-regexp-in-string this (lambda (x) that) (buffer-string) 'no-fixed-case))
       )
      (erase-buffer)
      (insert altered)
      (save-buffer)
      (goto-char current-location)
  )
)

;; Src: http://kitchingroup.cheme.cmu.edu/blog/2013/05/05/Getting-keyword-options-in-org-files/
(defun org-keywords ()
  "Parse the buffer and return a cons list of (property . value) from lines like: #+PROPERTY: value"
  (org-element-map (org-element-parse-buffer 'element) 'keyword
                   (lambda (keyword) (cons (org-element-property :key keyword)
                                           (org-element-property :value keyword)))))

(defun org-keyword (KEYWORD)
  "Get the value of a KEYWORD in the form of #+KEYWORD: value"
  (cdr (assoc KEYWORD (org-keywords))))

(define-globalized-minor-mode my-flyspell-global-mode flyspell-mode
  (lambda () 

    ;; spawns an ispell process
    (flyspell-mode 1)

))
(my-flyspell-global-mode 1)

(setq ispell-dictionary "british") ;; set the default dictionary

(global-font-lock-mode t)
(custom-set-faces '(flyspell-incorrect ((t (:inverse-video t)))))

;; (load "~/.emacs.d/powerthesaurus.el")
;; (global-set-key (kbd "M-#") 'powerthesaurus-lookup-word-at-point)

(autoload 'typing-of-emacs "~/.emacs.d/typing.el" "The Typing Of Emacs, a game." t)

; Seamless use of babel: No confirmation upon execution.
(setq org-confirm-babel-evaluate nil)

 (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (emacs-lisp . t)
     ;; (shell	 . t)
     (python . t)
     (haskell . t)
     (ruby	 . t)
     (ocaml	 . t)
     (dot	 . t)
     (latex	 . t)
     (org	 . t)
     (makefile	 . t)
     ))

(setq org-src-preserve-indentation t)

;; Graphviz: Press <g-TAB to obtain a minimal editable example.
(add-to-list 'org-structure-template-alist
        '("g" "#+begin_src dot :results output graphics :file \"/tmp/graph.pdf\" :exports both
   digraph G {
      node [color=black,fillcolor=white,shape=rectangle,style=filled,fontname=\"Helvetica\"];
      A[label=\"A\"]
      B[label=\"B\"]
      A->B
   }\n#+end_src" "<src lang=\"dot\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("E" "#+BEGIN_SRC emacs-lisp\n\n#+END_SRC" "<src lang=\"emacs-lisp\">\n\n</src>"))

(defun new-untitled-org-template ()
  "Produce an org-mode file template."
  (interactive)
  (switch-to-buffer (generate-new-buffer "*Untitled*"))
  (insert (file-as-string "~/.emacs.d/template.org"))
  (org-mode)
)

(global-set-key (kbd "C-x t") 'new-untitled-org-template)

;; org-mode math is now highlighted ;-)
(setq org-highlight-latex-and-related '(latex))

;; Hide the *,=,/ markers
(setq org-hide-emphasis-markers t)

;; (setq org-pretty-entities t) 
;; to have \alpha, \to and others display as utf8 http://orgmode.org/manual/Special-symbols.html

(defun org-goto-line (line)
  "Go to the indicated line, unfolding the parent Org header.

   Implementation: Go to the line, then look at the 1st previous
   org header, now we can unfold it whence we do so, then we go
   back to the line we want to be at.
  "
  (interactive)
  (goto-line line)
  (org-previous-visible-heading 1)
  (org-cycle)
  (goto-line line)
)

; https://orgmode.org/manual/Structure-editing.html
; (describe-symbol 'save-excursion)
;
(defun org-fold-current-subtree-anywhere-in-it ()
  "Hide the current heading, while being anywhere inside it."
  (interactive)
  (save-excursion
    (org-narrow-to-subtree)
    (org-shifttab)
    (widen))
)

;; FIXME: Make this buffer specfic!
(global-set-key (kbd "C-c C-h") 'org-fold-current-subtree-anywhere-in-it)

;;;; See http://ergoemacs.org/emacs/emacs_n_unicode.html
;;(define-abbrev-table 'global-abbrev-table '(
;;    ("alpha" "α")
;;    ("beta" "β")
;;    ("gamma" "γ")
;;    ("theta" "θ")
;;    ("inf" "∞")
;;    ("fcmp" "⨾")
;;    ("then" "⨾")
;;    ("syq"  "╳")
;;
;;    ("ar1" "→")
;;    ("ar2" "⇒")
;;    ))
;;
;;(abbrev-mode 1) ; turn on abbrev mode
