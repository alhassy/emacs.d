#+TITLE: Holger Schurig's Emacs configuration
# @compile: (message "FOO")
# @compile: (byte-compile-file "config.el")
# @compile: (org-twbs-export-to-html)

* License

All code sections in this =.org= file are licensed under
[[GPLv2][http://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html]] except
otherwise noted. For example, I derives some functions from notmuch.
And as notmuch uses GPLv3 or higher, this functions got the same
license.


* About this file
Inspired by the [[http://eschulte.me/emacs24-starter-kit/#installation][Emacs Starter Kit]], I set up my configuration file
using org-babel. You may use it as-is, but I guess (and suggest) that
you only look here to get ideas. Create your own Emacs config, in the
same pace as your knowledge about Emacs grows.


* Loading of elisp
** add =elisp/= to =load-path=
#+BEGIN_SRC emacs-lisp
(add-to-list 'load-path (concat user-emacs-directory "elisp"))
#+END_SRC




* Minibuffer
Don't insert current directory into minubuffer

#+BEGIN_SRC emacs-lisp
(setq insert-default-directory nil)
#+END_SRC

Minibuffer window expands vertically as necessary to hold the text
that you put in the minibuffer

#+BEGIN_SRC emacs-lisp
(setq resize-mini-windows t) ;; was grow-only
#+END_SRC

Read quoted chars with radix 16

#+BEGIN_SRC emacs-lisp
(setq read-quoted-char-radix 16)
#+END_SRC

Allow to type space chars in minibuffer input (for `timeclock-in',
for example).

#+BEGIN_SRC emacs-lisp
(define-key minibuffer-local-completion-map " " nil)
(define-key minibuffer-local-must-match-map " " nil)
#+END_SRC



* Help

** Go to back to previous help buffer
Make 'b' (back) go to the previous position in emacs help.
[[http://www.emacswiki.org/cgi-bin/wiki/EmacsNiftyTricks]]

#+BEGIN_SRC emacs-lisp
(add-hook 'help-mode-hook
	  '(lambda ()
		 (bind-key "b" 'help-go-back help-mode-map)))
#+END_SRC

** F1 key searches in help or opens man page
This is from https://www.emacswiki.org/emacs/DescribeThingAtPoint

#+BEGIN_SRC emacs-lisp
(defun my-help ()
		  "Show the documentation of the Elisp function and variable near point.
	This checks in turn:
	-- for a function name where point is
	-- for a variable name where point is
	-- for a surrounding function call
	"
	  (interactive)
	  (let (sym)
		;; sigh, function-at-point is too clever.  we want only the first half.
		(cond ((setq sym (ignore-errors
							   (with-syntax-table emacs-lisp-mode-syntax-table
								 (save-excursion
								   (or (not (zerop (skip-syntax-backward "_w")))
									   (eq (char-syntax (char-after (point))) ?w)
									   (eq (char-syntax (char-after (point))) ?_)
									   (forward-sexp -1))
								   (skip-chars-forward "`'")
							   (let ((obj (read (current-buffer))))
									 (and (symbolp obj) (fboundp obj) obj))))))
				   (describe-function sym))
				  ((setq sym (variable-at-point)) (describe-variable sym))
				  ;; now let it operate fully -- i.e. also check the
				  ;; surrounding sexp for a function call.
				  ((setq sym (function-called-at-point)) (describe-function sym)))))
(bind-key "<f1>" 'my-help)
#+END_SRC

** Apropos

#+BEGIN_SRC emacs-lisp
(bind-key "C-h a" 'apropos)
#+END_SRC

** Package: which-key - interactive keyboard help
#+BEGIN_SRC emacs-lisp
(use-package which-key
  :straight t
  :if (not noninteractive)
  :defer nil
  :diminish which-key-mode
  :commands (which-key-mode which-key-setup-side-window-right-bottom)
  :config
  (which-key-mode)
  (which-key-setup-side-window-right-bottom)
)
#+END_SRC



* Miscelleanous functions

** Command: (dos2unix)

#+BEGIN_SRC emacs-lisp
(defun dos2unix()
  "convert dos (^M) end of line to unix end of line"
  (interactive)
  (goto-char(point-min))
  (while (search-forward "\r" nil t) (replace-match "")))
#+END_SRC

** Command: (822date)
Inserts something like "Fri,  1 Dec 2006 15:41:36 +0100"

#+BEGIN_SRC emacs-lisp
(defun 822date ()
  "Insert date at point format the RFC822 way."
  (interactive)
  (insert (format-time-string "%a, %e %b %Y %H:%M:%S %z")))
#+END_SRC

** Command: (calc-region)
From https://www.reddit.com/r/emacs/comments/445w6s/whats_some_small_thing_in_your_dotemacs_that_you/:

Write some expression, e.g. =2+2*4= and then press C-=.

#+BEGIN_SRC emacs-lisp

(defun calc-region (arg)
  "Evaluate an expression in calc and communicate the result.

If the region is active evaluate that, otherwise search backwards
to the first whitespace character to find the beginning of the
expression. By default, replace the expression with its value. If
called with the universal prefix argument, keep the expression
and insert the result into the buffer after it. If called with a
negative prefix argument, just echo the result in the
minibuffer."
  (interactive "p")
  (let (start end)
	(if (use-region-p)
	(setq start (region-beginning) end (region-end))
	  (progn
	(setq end (point))
	(setq start (search-backward-regexp "\\s-\\|\n" 0 1))
	(setq start (1+ (if start start 0)))
	(goto-char end)))
	(let ((value (calc-eval (buffer-substring-no-properties start end))))
	  (pcase arg
	(1 (delete-region start end))
	(4 (insert " = ")))
	  (pcase arg
	((or 1 4) (insert value))
	(-1 (message value))))))
(bind-key "C-=" #'calc-region)
#+END_SRC



* Misc single packages
** Builtin package: comint
#+BEGIN_SRC emacs-lisp
(use-package comint
  :bind (:map comint-mode-map
			  ("<down>" . comint-next-input)
			  ("<up>"   . comint-previous-input)
			  ("C-n"    . comint-next-input)
			  ("C-p"    . comint-previous-input)
			  ("C-r"    . comint-history-isearch-backward))

  :config
  ;; Make the prompt readonly
  (setq comint-prompt-read-only t)
  ;; Activate isearch
  (setq comint-history-isearch t)
)
#+END_SRC

** Builtin package: dired
#+BEGIN_SRC emacs-lisp
(use-package dired
  :commands dired
  :bind ("C-x C-d" . dired) ;; used to be list-directory, quite useless
  :config
  (setq dired-listing-switches "-laGh1v --group-directories-first")
  ;; revert when revisiting
  (setq dired-auto-revert-buffer t)
  ;; work in a Norton Commander like mode if 2 panes are open
  (setq dired-dwim-target t)
)
#+END_SRC

** Builtin package: dired-aux
#+BEGIN_SRC emacs-lisp
(use-package dired-aux
  :defer t
  :config
  ;; normally, only isearch for files, but not if cursor was navigated away
  (setq dired-isearch-filenames 'dwim)
)
#+END_SRC

** Builtin package: dired-x
#+BEGIN_SRC emacs-lisp
(use-package dired-x
  :commands dired-jump
)
#+END_SRC

** Builtin package: eshell
https://www.masteringemacs.org/article/complete-guide-mastering-eshell

#+BEGIN_SRC emacs-lisp
(use-package eshell
  :if (not noninteractive)
  :commands (eshell eshell/addpath eshell-read-aliases-list)
  :defines (eshell-visual-commands)
  :config
  (defun eshell/clear ()
	"Deletes the contents of eshell buffer, except the last prompt"
	(save-excursion
	  (goto-char eshell-last-output-end)
	  (let ((lines (count-lines 1 (point)))
		(inhibit-read-only t))
	(beginning-of-line)
	(let ((pos (point)))
	  (if (bobp)
		  (if (called-interactively-p 'interactive)
			  (error "Buffer too short to truncate"))
		(delete-region (point-min) (point)))))))

  ;; If I ever want my own eshell/foo commands overwrite real commands ...
  (setq eshell-prefer-lisp-functions t)

  ;; check if this is ok for my usage
  ;; eshell-visual-commands

  (defun my--eshell-hook ()
	(eshell-read-aliases-list)
	(setq global-hl-line-mode nil)
	(setq show-trailing-whitespace nil)
	(add-to-list 'eshell-visual-commands "ssh")
	(add-to-list 'eshell-visual-commands "htop")
	(add-to-list 'eshell-visual-commands "ncmpcpp")
	(add-to-list 'eshell-visual-commands "tail")
	(eshell/addpath "~/bin"))
  (add-hook 'eshell-mode-hook 'my--eshell-hook)
)
(use-package em-banner
  :if (not noninteractive)
  :defer t
  :defines (eshell-banner-message)
  :config
  ;; We don't need no banner
  (setq eshell-banner-message "")
)
#+END_SRC

** Builtin package: flyspell - background spell checker

#+BEGIN_SRC emacs-lisp
(use-package flyspell
  :if (eq system-type 'gnu/linux)
  :diminish flyspell-mode
  :commands (flyspell-mode flyspell-prog-mode)
  :config
  (add-to-list 'flyspell-dictionaries-that-consider-dash-as-word-delimiter "german-new8")
  (setq flyspell-issue-welcome-flag nil)
  ;; M-Tab is owned by the window manager, correct with C-M-i
  (setq flyspell-use-meta-tab nil)
  )

#+END_SRC

Flyspell is in elisp mode. And this in turn loads flyspell directly
after launching emacs, which is a bit unfortunate.

#+BEGIN_SRC emacs-lisp
(defun my-flyspell-prog-mode ()
  (interactive)
  (unless (string= (buffer-name) "*scratch*")
	(flyspell-prog-mode)))
(when (eq system-type 'gnu/linux)
  (add-hook 'prog-mode-hook  #'my-flyspell-prog-mode)
  (add-hook 'text-mode-hook  #'flyspell-mode)
  (add-hook 'org-mode-hook   #'flyspell-mode)
  (add-hook 'latex-mode-hook #'flyspell-mode)
  (add-hook 'LaTeX-mode-hook #'flyspell-mode))
#+END_SRC

** Builtin package: shr - Simple HTML Renderer, used by elfeed)
#+BEGIN_SRC emacs-lisp
(use-package shr
  :defer t
  :config
  ;; don't use the (ugly)proportional font
  (setq shr-use-fonts nil)
)
#+END_SRC

** Builtin package: term - for ansi-term
This term understands ansi escape sequences.

On Debian, one should install the =ansi-term= debian package so that
the terminal "eterm-color" is available.

#+BEGIN_SRC emacs-lisp
(use-package term
  :bind ("M-g s" . ansi-shell)
  :commands (ansi-term ansi-shell
					   term-in-line-mode
					   term-line-mode
					   term-char-mode)
  :defines (term-buffer-maximum-size
			show-dir-in-mode-line?)
  :init
  (defun ansi-shell ()
	"Start ansi-term with bash"
	(interactive)
	(ansi-term "/bin/bash"))
  :config

  ;; don't linger around when closing
  (defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
	(if (memq (process-status proc) '(signal exit))
		(let ((buffer (process-buffer proc)))
		  ad-do-it
		  (kill-buffer buffer))
	  ad-do-it))
  (ad-activate 'term-sentinel)

  (defun term-toggle-mode ()
	(interactive)
	(if (term-in-line-mode)
		(term-char-mode)
	  (term-line-mode)))

  (defun my-term-hook ()
	(goto-address-mode)
	;; (bind-key "C-c C-j" #'term-toggle-mode term-mode-map)
	;; (bind-key "C-c C-k" #'term-toggle-mode term-mode-map)
	(setq global-hl-line-mode nil)
	(setq term-buffer-maximum-size 10000)
	(setq-local show-dir-in-mode-line? t) ;; also mode linec'
	(setq show-trailing-whitespace nil)
	;; disable company in favor of shell completion
	;;(company-mode -1)
	)
  (add-hook 'term-mode-hook 'my-term-hook)
)
#+END_SRC

** CANC Package: elfeed - RSS feed reader

| Key | Function                                              |
|-----+-------------------------------------------------------|
| RET | show article                                          |
| G   | update feeds                                          |
| b   | open in browser (browse-url)                          |
| q   | quit                                                  |
| +   | Apply TAG to all selected entries.                    |
| -   | Remove TAG from all selected entries.                 |
| S   | Set a new search filter for the elfeed-search buffer. |

https://github.com/skeeto/elfeed
Tips and Tricks: http://nullprogram.com/blog/2013/11/26/


#+BEGIN_SRC emacs-lisp
(use-package elfeed
  :straight t
  :if (string= (system-name) "desktop")
  :bind ("M-g r" . elfeed)   ; r like "RSS"
  :config
  (setq elfeed-feeds
		'(;; emacs
		  ("http://emacsredux.com/atom.xml" emacs)
		  ("http://endlessparentheses.com/atom.xml" emacs)
		  ("http://nullprogram.com/feed/" emacs)
		  ("http://planet.emacsen.org/atom.xml" emacs)
		  ("http://www.lunaryorn.com/feed.atom" emacs)
		  ("http://www.masteringemacs.org/feed/" emacs)
		  ("https://github.com/milkypostman/melpa/commits/master.atom" github emacs)
		  ("http://oremacs.com/atom.xml" emacs)
		  ("http://emacsnyc.org/atom.xml" emacs)
		  ;; ("https://www.reddit.com/r/emacs/.rss" emacs reddit)
		  ;; ("https://www.reddit.com/r/orgmode/.rss" emacs reddit)

		  ;;("http://stackexchange.com/feeds/tagsets/152198/emacs?sort=active" emacs)
		  ))

  (setq elfeed-use-curl t)
  (setq elfeed-search-filter "@1-week-ago +unread")

  ;; Entries older than 4 weeks are marked as read
  (add-hook 'elfeed-new-entry-hook
			(elfeed-make-tagger :before "4 weeks ago"
								:remove 'unread))

  ;; fetch RSS/Atom feeds in the background
  ;;
  ;; A better approach would be to have a checker function that
  ;; gets the feeds sorted by when they have last been downloaded
  ;; it would then take one (!) of them and just feed it. That
  ;; makes the idle function fast and we could increase it's
  ;; timeout from 15 minutes to a minute or so.
  (run-with-idle-timer (* 15 60) t #'elfeed-update)
)
#+END_SRC

An example on how to prune old feeds:

#+BEGIN_SRC emacs-lisp :tangle no
  (defun elfeed-dead-feeds (years)
  "Return a list of feeds that haven't posted en entry in YEARS years."
  (cl-block
	  (macroexp-let* ((living-feeds (make-hash-table :test 'equal))
					  (seconds (* years 365.0 24 60 60))
					  (threshold (- (float-time) seconds)))
					 (with-elfeed-db-visit (entry feed)
					   (let ((date (elfeed-entry-date entry)))
						 (when (> date threshold)
						   (setf (gethash (elfeed-feed-url feed) living-feeds) t))))
					 (cl-loop for url in (elfeed-feed-list)
							  unless (gethash url living-feeds)
							  collect url))))
  (elfeed-dead-feeds 1.0)
#+END_SRC

Some more feeds to (eventually) check:

- ("http://harryrschwartz.com/atom.xml" blog)
- ("http://zinascii.com/writing-feed.xml" blog)
- ("http://githubengineering.com/atom.xml" blog)
- ("http://blog.smola.org/rss" blog)
- ("http://briancarper.net/feed" blog)
- ("https://kotka.de/blog/index.rss" blog)
- ("http://fiftyfootshadows.net/feed/" blog)
- ("http://blag.xkcd.com/feed/" blog)
- ("http://youdisappear.net/files/page1.xml" blog music)
- ("http://normanmaurer.me/blog.atom" blog)
- ("http://blog.mikemccandless.com/feeds/posts/default" elasticsearch blog)
- ("http://lethain.com/feeds/all/" blog)
- ("http://whatthefuck.computer/rss.xml" blog)
- ("http://feeds.feedburner.com/jamesshelley" blog)
- ("http://www.marco.org/rss" blog)
- ("http://gnuvince.wordpress.com/feed/" blog)
- ("http://elliotth.blogspot.com/feeds/posts/default" blog)
- ("http://feeds.feedburner.com/Hyperbole-and-a-half" blog)
- ("http://lcamtuf.blogspot.com/feeds/posts/default" blog)
- ("http://blog.isabel-drost.de/index.php/feed" blog)
- ("http://feeds2.feedburner.com/CodersTalk" blog)
- ("http://feeds.feedburner.com/codinghorror/" blog)
- ("http://lambda-the-ultimate.org/rss.xml" blog)
- ("http://danluu.com/atom.xml" blog)
- ("http://ferd.ca/feed.rss" blog)
- ("http://blog.fsck.com/atom.xml" blog)
- ("http://jvns.ca/atom.xml" blog)
- ("http://newartisans.com/rss.xml" blog emacs)
- ("http://bling.github.io/index.xml" blog emacs)
- ("https://rachelbythebay.com/w/atom.xml" blog)
- ("http://blog.nullspace.io/feed.xml" blog)
- ("http://www.mcfunley.com/feed/atom" blog)
- ("https://codewords.recurse.com/feed.xml" blog)
- ("http://akaptur.com/atom.xml" blog)
- ("http://davidad.github.io/atom.xml" blog)
- ("http://www.evanjones.ca/index.rss" blog)
- ("http://neverworkintheory.org/feed.xml" blog)
- ("http://blog.jessitron.com/feeds/posts/default" blog)
- ("http://feeds.feedburner.com/GustavoDuarte?format=xml" blog)
- ("http://blog.regehr.org/feed" blog)
- ("https://www.snellman.net/blog/rss-index.xml" blog)
- ("http://eli.thegreenplace.net/feeds/all.atom.xml" blog)
- ("https://idea.popcount.org/rss.xml" blog)
- ("https://aphyr.com/posts.atom" blog)
- ("http://kamalmarhubi.com/blog/feed.xml" blog)
- ("http://maryrosecook.com/blog/feed" blog)
- ("http://www.tedunangst.com/flak/rss" blog)
- ("http://yosefk.com/blog/feed" blog)
- ("http://www.benkuhn.net/rss/" blog)
- ("https://emacsgifs.github.io/feed.xml" blog emacs)

** Package: flx - fuzzy matching with good sorting
This package is used for fuzzy search in ivy.
#+BEGIN_SRC emacs-lisp
(use-package flx
  :straight t
  :if (not noninteractive)
  :defer t
)
#+END_SRC
** Package: hydra - interactive stateful keybindings
#+BEGIN_SRC emacs-lisp
(use-package hydra
  :straight t
  :if (not noninteractive)
  :commands (defhydra hydra-default-pre hydra-keyboard-quit
			 hydra-set-transient-map
			 hydra--call-interactively-remap-maybe
			 hydra-show-hint) )
(use-package lv
  :defer t
  :commands (lv-message)
)
#+END_SRC

** Package: ispell
http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html

#+BEGIN_SRC emacs-lisp
(use-package ispell
  :defer t
  :config
  ; Standard location of personal dictionary
  (setq ispell-personal-dictionary "~/.flydict")
  ;; set the default to english, files can switch locally to german
  (setq ispell-dictionary "en_US")
  (setq ispell-program-name (executable-find "aspell"))
  (setq ispell-extra-args
        '("--sug-mode=fast" ;; ultra|fast|normal|bad-spellers
		  "--ignore=3"))    ;; ignore words less than 3 characters long

  ;; Ignore org-mode properties
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  ;; Ignore source code blocks and examples
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))
)
#+END_SRC

The following change allows me to have some files flychecked in
german-new8 and others in english (the default). I just need to put
this line into the file:

#+BEGIN_EXAMPLE
# -*- ispell-local-dictionary: "german-new8" -*-
#+END_EXAMPLE

into the file.

#+BEGIN_SRC emacs-lisp
(add-to-list 'safe-local-variable-values '(ispell-dictionary . german-new8))
#+END_SRC

** Package: pdf-tools
- Home page: https://github.com/politza/pdf-tools
- Configuration based on https://github.com/abo-abo/hydra/wiki/PDF-Tools, but modified to lazy-load and to bind to *.pdf files

#+BEGIN_SRC emacs-lisp
(use-package pdf-tools
  :straight t
  :if (not noninteractive)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :commands (pdf-tools-install
			 hydra-pdftools/body)
  :config
  (pdf-tools-install)

  ;; silence "the following functions might not be defined at runtime"
  (declare-function hydra-master/body "config.el" (a b &c))
  (declare-function pdf-annot-list-annotations "pdf-annot.el" ())
  (declare-function pdf-annot-delete "pdf-annot.el" (a))
  (declare-function pdf-annot-attachment-dired "pdf-annot.el" (&regenerate-p))
  (declare-function pdf-annot-add-markup-annotation "pdf-annot.el" (list-of-edges type &color property-alist))
  (declare-function pdf-annot-add-text-annotation "pdf-annot.el" (pos &icon property-alist))
  (declare-function pdf-links-action-perform "pdf-annot.el" (link))
  (declare-function pdf-links-isearch-link "pdf-annot.el" ())
  (declare-function pdf-history-backward "pdf-annot.el" (n))
  (declare-function pdf-history-forward "pdf-annot.el" (n))

  (defhydra hydra-pdftools (:color blue :hint nil)
  	"
                                                                      ╭───────────┐
       Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
   ╭──────────────────────────────────────────────────────────────────┴───────────╯
         ^^_g_^^      _B_    ^↧^    _+_    ^ ^     [_al_] list    [_s_] search    [_u_] revert buffer
         ^^^↑^^^      ^↑^    _H_    ^↑^   ↦ _W_ ↤  [_am_] markup  [_o_] outline   [_i_] info
         ^^_p_^^      ^ ^    ^↥^    _0_    ^ ^     [_at_] text    [_F_] link      [_d_] dark mode
         ^^^↑^^^      ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link
    _h_ ←pag_e_→ _l_  _N_  │ _P_ │  _-_    _b_     [_aa_] dired
         ^^^↓^^^      ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   [_y_]  yank
         ^^_n_^^      ^ ^  _r_eset slice box
         ^^^↓^^^
         ^^_G_^^
   --------------------------------------------------------------------------------
        "
  	("\\" hydra-master/body "back")
  	("<ESC>" nil "quit")
  	("al" pdf-annot-list-annotations)
  	("ad" pdf-annot-delete)
  	("aa" pdf-annot-attachment-dired)
  	("am" pdf-annot-add-markup-annotation)
  	("at" pdf-annot-add-text-annotation)
  	("y"  pdf-view-kill-ring-save)
  	("+" pdf-view-enlarge :color red)
  	("-" pdf-view-shrink :color red)
  	("0" pdf-view-scale-reset)
  	("H" pdf-view-fit-height-to-window)
  	("W" pdf-view-fit-width-to-window)
  	("P" pdf-view-fit-page-to-window)
  	("n" pdf-view-next-page-command :color red)
  	("p" pdf-view-previous-page-command :color red)
  	("d" pdf-view-dark-minor-mode)
  	("b" pdf-view-set-slice-from-bounding-box)
  	("r" pdf-view-reset-slice)
  	("g" pdf-view-first-page)
  	("G" pdf-view-last-page)
  	("e" pdf-view-goto-page)
  	("o" pdf-outline)
  	("s" pdf-occur)
  	("i" pdf-misc-display-metadata)
  	("u" pdf-view-revert-buffer)
  	("F" pdf-links-action-perform)
  	("f" pdf-links-isearch-link)
  	("B" pdf-history-backward :color red)
  	("N" pdf-history-forward :color red)
  	("l" image-forward-hscroll :color red)
  	("h" image-backward-hscroll :color red))

  (bind-keys :map pdf-view-mode-map
  			 ("?"  . hydra-pdftools/body)
  			 ("g"  . pdf-view-first-page)
  			 ("G"  . pdf-view-last-page)
  			 ("l"  . image-forward-hscroll)
  			 ("h"  . image-backward-hscroll)
  			 ("j"  . pdf-view-next-page)
  			 ("k"  . pdf-view-previous-page)
  			 ("e"  . pdf-view-goto-page)
  			 ("u"  . pdf-view-revert-buffer)
  			 ("al" . pdf-annot-list-annotations)
  			 ("ad" . pdf-annot-delete)
  			 ("aa" . pdf-annot-attachment-dired)
  			 ("am" . pdf-annot-add-markup-annotation)
  			 ("at" . pdf-annot-add-text-annotation)
  			 ("y"  . pdf-view-kill-ring-save)
  			 ("i"  . pdf-misc-display-metadata)
  			 ("s"  . pdf-occur)
  			 ("b"  . pdf-view-set-slice-from-bounding-box)
  			 ("r"  . pdf-view-reset-slice))

)
#+END_SRC

Now define various autoload helpers:

#+BEGIN_SRC emacs-lisp
(use-package image-mode
  :commands (image-forward-hscroll image-backward-hscroll)
)
(use-package pdf-annot
  :commands (pdf-annot-minor-mode)
  :defines (pdf-annot-activate-created-annotations)
  :config
  (setq pdf-annot-activate-created-annotations t)
)
(use-package pdf-history
  :commands (pdf-history-minor-mode)
)
(use-package pdf-links
  :commands (pdf-links-minor-mode)
)
(use-package pdf-misc
  :commands (pdf-misc-display-metadata)
)
(use-package pdf-outline
  :commands (pdf-outline-minor-mode)
)
(use-package pdf-sync
  :commands (pdf-sync-minor-mode)
)
(use-package pdf-view
  :commands (pdf-view-dark-minor-mode
			 pdf-view-enlarge
			 pdf-view-first-page
			 pdf-view-fit-height-to-window
			 pdf-view-fit-page-to-window
			 pdf-view-fit-width-to-window
			 pdf-view-goto-page
			 pdf-view-last-page
			 pdf-view-next-page
			 pdf-view-next-page-command
			 pdf-view-previous-page
			 pdf-view-previous-page-command
			 pdf-view-reset-slice
			 pdf-view-revert-buffer
			 pdf-view-scale-reset
			 pdf-view-set-slice-from-bounding-box
			 pdf-view-shrink
             pdf-view-kill-ring-save)
  :config
  (setq-default pdf-view-display-size 'fit-width)
)
#+END_SRC

** Package: pdf-occur
#+BEGIN_SRC emacs-lisp
(use-package pdf-occur
  :commands (pdf-occur pdf-occur-global-minor-mode)
)
#+END_SRC emacs-lisp

** Package: pdf-outline
#+BEGIN_SRC emacs-lisp
(use-package pdf-outline
  :commands (pdf-outline)
)
#+END_SRC emacs-lisp


* Package: ivy
See: http://oremacs.com/swiper/
** Package: ivy
See http://oremacs.com/swiper/

#+BEGIN_SRC emacs-lisp
(use-package ivy
  :straight t
  :if (not noninteractive)
  :diminish ivy-mode
  :bind (("C-x C-b"  . ivy-switch-buffer) ;; was: list-buffers
		 ("C-c C-r"  . ivy-resume)
		 ;; http://oremacs.com/swiper/#minibuffer-key-bindings
		 ;; http://oremacs.com/swiper/#key-bindings-for-single-selection-action-then-exit-minibuffer
		 :map ivy-minibuffer-map
              ;; C-m, RET    ivy-done
		      ;; C-j         ivy-alt-done
		      ;; M-o         ivy-dispatching-done:    present several actions
		      ;; M-d, C-M-i  ivy-immediate-done:      use current input, not the candidate
			  ("M-d"   . ivy-immediate-done)
			  ;;
			  ;; http://oremacs.com/swiper/#key-bindings-for-multiple-selections-and-actions-keep-minibuffer-open
			  ;; C-M-m       ivy-call:                do action without being done
			  ;; C-M-n       ivy-next-line-and-call:  same, but go to next line
			  ;; C-M-p       ivy-prev-line-and-call:  same, but go to next line
			  ;; C-M-o       ivy-dispatching-call
              ;;
			  ;; http://oremacs.com/swiper/#key-bindings-that-alter-the-minibuffer-input
			  ("C-n"   . ivy-previous-history-element) ;; was ivy-next-line
			  ("C-p"   . ivy-next-history-element)     ;; was ify-previous-line
		)
  :commands (ivy-mode ivy--format-function-generic ivy--add-face ivy-read)
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)       ; extend searching to bookmarks and
  (setq ivy-height 20)                   ; set height of the ivy window
  (setq ivy-count-format "(%d/%d) ")     ; count format, from the ivy help page
  (setq ivy-use-selectable-prompt t)     ; allow partial candidates be selected

  (setq ivy-format-function 'ivy-format-function-arrow)
  (set-face-attribute 'ivy-current-match   nil :foreground nil :background nil)
  (set-face-attribute 'ivy-minibuffer-match-face-2 nil :foreground "gold1" :background nil)
  (set-face-attribute 'ivy-minibuffer-match-face-1 nil :foreground nil :background nil)
  (set-face-attribute 'ivy-minibuffer-match-face-3 nil :foreground "gold1" :background nil)
  (set-face-attribute 'ivy-minibuffer-match-face-4 nil :foreground "gold1" :background nil)
)
#+END_SRC
** Package: counsel
#+BEGIN_SRC emacs-lisp
(use-package s
  :straight t
  :commands (s-trim-right s-collapse-whitespace)
)

(use-package counsel
  :straight t
  :if (not noninteractive)
  :defer t
  :bind (("C-x C-f"   . counsel-find-file)
		 ("C-x g"     . counsel-git)  ;; find file in current git tree
		 ;; Help related
		 ("C-h f"     . counsel-describe-function)
		 ("C-h v"     . counsel-describe-variable)
		 ("C-h S"     . counsel-info-lookup-symbol)
		 ;; special characters
		 ("C-x 8 RET" . counsel-unicode-char)
		 ;; searching
		 ("M-s a"     . counsel-ag)
		 ("M-s g"     . counsel-git-grep)
		 ;; goto
		 ("M-g i"     . counsel-imenu)
		 ; yank/pop, see http://pragmaticemacs.com/emacs/counsel-yank-pop-with-a-tweak/
		 ("M-y"       . my-counsel-yank-pop)
		 )
  :config
  ;; counsel-describe-function use this to highlight interactive functions
  ;; counsel-describe-variable use this to highlight interactive macros
  (set-face-attribute 'highlight   nil :foreground "orange" :background nil)

  (require 's)
  (defun my-yank-pop-format-str (txt)
	(s-trim-right (s-collapse-whitespace (substring-no-properties txt))))

  (defun my-yank-pop-format (cands)
	"Transform CANDS into a string for minibuffer."
	(ivy--format-function-generic
	 (lambda (str)
	   (concat "> " (ivy--add-face (my-yank-pop-format-str str) 'ivy-current-match)))
	 (lambda (str)
	   (concat "  " (my-yank-pop-format-str str)))
	 cands
	 "\n"))

  (defun my-counsel-yank-pop ()
	"Ivy replacement for `yank-pop'."
	(interactive)
	(if (eq last-command 'yank)
		(progn
		  (setq ivy-completion-end (point))
		  (setq ivy-completion-beg
				(save-excursion
				  (search-backward (car kill-ring))
				  (point))))
	  (setq ivy-completion-beg (point))
	  (setq ivy-completion-end (point)))
	(let ((ivy-format-function #'my-yank-pop-format))
	  (ivy-read "kill-ring: " (cl-loop for kill in (delete-dups kill-ring)
									   unless (or (< (length kill) 3)
												  (string-match "\\`[\n[:blank:]]+\\'" kill))
									   collect kill)
				:action 'counsel-yank-pop-action
				:caller 'my-counsel-yank-pop)))

)
#+END_SRC
** Package: swiper
#+BEGIN_SRC emacs-lisp
(use-package swiper
  :straight t
  :if (not noninteractive)
  :defer t
  :bind ("M-s o" . swiper)
)
#+END_SRC
** Package: amx
#+BEGIN_SRC emacs-lisp
(use-package amx
  :straight t
  :if (not noninteractive)
  :bind ("M-x" . amx)
  :config
  (setq amx-save-file (locate-user-emacs-file "tmp/smex.el"))
)
#+END_SRC
** CANC Package: smex
#+BEGIN_SRC emacs-lisp
(use-package smex
  :bind ("M-x" . smex)
  :config
  (setq smex-save-file (locate-user-emacs-file "tmp/smex.el"))
  ;;(smex-mode)
)
#+END_SRC
** CANC Package: ivy-hydra
#+BEGIN_SRC emacs-lisp
(use-package ivy-hydra
  :defer t
)
#+END_SRC


* ERC - an IRC client
#+BEGIN_SRC emacs-lisp
(use-package my-erc
:commands (freenode oftc)
:bind ("M-g e" . freenode)
)
#+END_SRC emacs-lisp


* Mail & News

** Builtin package: sendmail
#+BEGIN_SRC emacs-lisp
(use-package sendmail
  :defer t
  :commands (mail-mode mail-text)
  :defines (send-mail-function)
  :config

  (setq send-mail-function 'sendmail-send-it
		sendmail-program "/usr/bin/msmtp"
		mail-specify-envelope-from t)
)
#+END_SRC

** Builtin package: message
#+BEGIN_SRC emacs-lisp
(use-package message
  :commands (message-mode message-cite-original-without-signature)
  :config

  ;; When composing a mail, start the auto-fill-mode.
  (add-hook 'message-mode-hook 'turn-on-auto-fill)
  ;; (add-hook 'message-setup-hook 'bbdb-define-all-aliases)

  ;; Generate the mail headers before you edit your message.
  (setq message-generate-headers-first t)

  ;; The message buffer will be killed after sending a message.
  (setq message-kill-buffer-on-exit t)

  ;; When I reply, I don't want to have me in To or Cc
  (setq message-dont-reply-to-names (concat "\\("
											user-mail-address
											;; Nor the Debian BTS
											;; "\\|^submit@bugs.debian\\.org$"
											"\\)"))

  ;; based on http://mbork.pl/2016-02-06_An_attachment_reminder_in_mu4e
  (defun my-message-attachment-present-p ()
	"Return t if an attachment is found in the current message."
	(save-excursion
	  (save-restriction
		(widen)
		(goto-char (point-min))
		(when (search-forward "<#part" nil t) t))))

  (defvar my-message-attachment-intent-re
	(regexp-opt '("I attach"
				  "I have attached"
				  "I've attached"
				  "I have included"
				  "I've included"
				  "see the attached"
				  "see the attachment"
				  "attached file"))
	"A regex which - if found in the message, and if there is no
attachment - should launch the no-attachment warning.")

  (defvar my-message-attachment-reminder
	"Are you sure you want to send this message without any attachment? "
	"The default question asked when trying to send a message
containing `my-message-attachment-intent-re' without an
actual attachment.")

  (defun my-message-warn-if-no-attachments ()
	"Ask the user if s?he wants to send the message even though
there are no attachments."
	(when (and (save-excursion
				 (save-restriction
				   (widen)
				   (goto-char (point-min))
				   (re-search-forward my-message-attachment-intent-re nil t)))
			   (not (my-message-attachment-present-p)))
	  (unless (y-or-n-p my-message-attachment-reminder)
		(keyboard-quit))))

  (add-hook 'message-send-hook #'my-message-warn-if-no-attachments)
)
#+END_SRC

** Builtin package: mm-decode
#+BEGIN_SRC emacs-lisp
(use-package mm-decode
  :defer t
  :config
  ;; Displaying zip/tar inline is a really, really stupid default!
  (setq mm-inlined-types
		(cl-remove-if (apply-partially #'string-match-p "\\(x-g?tar\\|zip\\)")
					  mm-inlined-types))
)
#+END_SRC

** Package: notmuch - full-text indexing e-mail client
*** notmuch
#+BEGIN_SRC emacs-lisp
(use-package my-notmuch
  :bind ("M-g n" . my-notmuch-hello)
)
#+END_SRC

* Programming

** Source code version control

*** Disable vc backends

Remove all back-ends from vc-mode, no need to check all these things, I use
=magit= for everything anyway:

#+BEGIN_SRC emacs-lisp
(setq vc-handled-backends '())
#+END_SRC

*** Package: magit
#+BEGIN_SRC emacs-lisp
(straight-use-package 'magit)
:straight t
(use-package magit-mode
  :commands (magit-display-buffer-fullframe-status-v1)
)
(use-package magit
  :diminish magit-auto-revert-mode  ;; disable "MRev" in the status line
  :defines (magit-rigid-key-bindings)
  :commands (magit-toplevel)
  :bind ("M-g m" . magit-status)
  :bind ("M-g M" . magit-list-repositories)
  :config
  ;; Open magit window full-screen
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  ;; When calling magit-status, save all buffers without further ado
  (setq magit-save-repository-buffers 'dontask)

  ;; Anything longer will be highlighted
  (setq git-commit-summary-max-length 70)

  ;; show a diff when committing
  (setq magit-commit-show-diff t)

  (setq magit-status-sections-hook '(
			;;magit-insert-status-headers
			magit-insert-unstaged-changes
			magit-insert-staged-changes
			magit-insert-untracked-files
			magit-insert-merge-log
			magit-insert-rebase-sequence magit-insert-am-sequence
			magit-insert-sequencer-sequence magit-insert-bisect-output
			magit-insert-bisect-rest magit-insert-bisect-log
			magit-insert-stashes
			magit-insert-unpulled-from-upstream
			magit-insert-unpulled-from-pushremote
			magit-insert-unpushed-to-upstream
			magit-insert-unpushed-to-pushremote))

  ;; make [MASTER] appear at the end of the summary line
  (setq magit-log-show-refname-after-summary t)

  ;; Bind M-g in magit mode to a function that visits the project on github
  (defun my-magit-browse-github ()
    "Browse to the project's github URL, if available"
    (interactive)
    (let ((url (with-temp-buffer
                 (unless (zerop (call-process-shell-command
                                 "git remote -v" nil t))
                   (error "Failed: 'git remote -v'"))
                 (goto-char (point-min))
                 (when (re-search-forward
                        "github\\.com[:/]\\(.+?\\)\\.git" nil t)
                   (format "https://github.com/%s" (match-string 1))))))
      (unless url
        (error "Can't find repository URL"))
      (browse-url url))
	  (with-temp-buffer
		(insert "bind_raise_or_run_web()")
		(call-process-region (point-min) (point-max) "awesome-client" t)))
  (bind-key "M-g g" 'my-magit-browse-github magit-mode-map)

  ;; Make "q" kill the window, not just bury it
  (bind-key "q" 'my--kill-buffer-and-window magit-mode-map)

  ;; Switch repositories with magit-list-repositories
  (setq magit-repository-directories
		'(
		  ("~"        . 1)
		  ("~/d"      . 1)
		  ("/usr/src" . 1)
		)
		magit-repolist-columns
		'(
		  ("Name"    25 magit-repolist-column-ident                  nil)
		  ("Version" 25 magit-repolist-column-version                nil)
		  ("Push"    4 magit-repolist-column-unpushed-to-upstream   (:right-align t))
		  ;; ("L<U"      3 magit-repolist-column-unpulled-from-upstream (:right-align t))
		  ("Path"    99 magit-repolist-column-path)))
)
#+END_SRC

*** Package: git-timemachine
#+BEGIN_SRC emacs-lisp
(use-package git-timemachine
  :straight t
  :commands git-timemachine
  )
#+END_SRC




** Tabs and indentation

*** Tab width 4
#+BEGIN_SRC emacs-lisp
(setq-default tab-width 4)
#+END_SRC

*** Deleting past a tab
Deleting past a tab normally changes tab into spaces. Don't do that,
kill the tab instead.

#+BEGIN_SRC emacs-lisp
(setq backward-delete-char-untabify-method nil)
#+END_SRC

*** RET is newline-and-indent
And if we ever need it, =C-j= is now newline.

#+BEGIN_SRC emacs-lisp
(bind-key "RET" 'newline-and-indent)
(bind-key "C-j" 'newline)
#+END_SRC

*** Builtin package: tabify - only tabify leading whitespace
#+BEGIN_SRC emacs-lisp
(use-package tabify
  :defer t
  :commands (tabify untabify)
  :config
  ;; only initial whitespace
  (setq tabify-regexp "^\t* [ \t]+"))
#+END_SRC

*** Package: dtrt-indent - guess indent mode
#+BEGIN_SRC emacs-lisp
(use-package dtrt-indent
  :straight t
  :if (not noninteractive)
  :defer nil
  :commands (dtrt-indent-mode)
  :config
  (dtrt-indent-mode 1)
  (setq dtrt-indent-verbosity 0)
  ;; dtrt-indent plays games with global-mode-string, urgh
  (delq 'dtrt-indent-mode-line-info global-mode-string)
)
#+END_SRC

*** Package: clean-aindent-mode - clean up wrong indentation
Nice tip from tuhdo, see https://www.emacswiki.org/emacs/CleanAutoIndent
#+BEGIN_SRC emacs-lisp
(use-package clean-aindent-mode
  :straight t
  :commands (clean-aindent-mode)
  :config
  (add-hook 'prog-mode-hook #'clean-aindent-mode)
)
#+END_SRC



** Expansion
*** Package: company-c-headers
#+BEGIN_SRC emacs-lisp
(use-package company-c-headers
  :straight t
  :if (not noninteractive)
  :config
  (add-to-list 'company-backends 'company-c-headers)
  (add-to-list 'company-c-headers-path-system "/usr/include/qt4/qt/")
  (add-to-list 'company-c-headers-path-system "/usr/include/qt4/QtCore/")
  (add-to-list 'company-c-headers-path-system "/usr/include/qt4/QtGui/")
)
#+END_SRC

*** CANC Package: yasnippet
Now try yasnippet again now that it's faster.

#+BEGIN_SRC emacs-lisp
(use-package yasnippet
  :if (not noninteractive)
  :defer t
  :bind (("C-c C-s" . yas-insert-snippet)  ;; used in org-mode for org-schedule
		 ("C-c s"   . yas-insert-snippet))
  ;; :diminish yas-minor-mode

  :config
  (setq yas-indent-line nil)

  ;; No dropdowns please.
  (setq yas-prompt-functions '(yas-completing-prompt))

  ;; No need to be so verbose
  (setq yas-verbosity 1)

  ;; Wrap around region
  (setq yas-wrap-around-region t)

  ;; Set my own
  ;; (setq yas-snippet-dirs '((locate-user-emacs-file "yasnippet")))

  ;; This creates errors when I open ansi-term
  ;; (yas-global-mode 1)
)
#+END_SRC



** Commenting

#+BEGIN_SRC emacs-lisp
(bind-key "C-c c" 'comment-dwim)
#+END_SRC
** Error navigation
#+BEGIN_SRC emacs-lisp
(bind-key "<f8>" 'next-error)
(bind-key "S-<f8>" 'previous-error)
#+END_SRC

** Make files with shebang executable
#+BEGIN_SRC emacs-lisp
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
#+END_SRC

** Builtin package: cc-mode - mayor mode for C and C++
#+BEGIN_SRC emacs-lisp
(use-package cc-mode
  ;; open *.h files normally in c++ mode
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
		 ("\\.inl\\'"                 . c++-mode))

  :config

  ;; This makes things like super_function_for_you a word
  (modify-syntax-entry ?_ "w")

  (setq fill-column 78)

  ;; Let RET break and continue a comment
  ;; C doesn't start functions with a ( in the first column
  (setq open-paren-in-column-0-is-defun-start nil)

  ;; Tab behavior
  (setq c-tab-always-indent nil  ;; insert real tab
		c-insert-tab-function 'indent-for-tab-command)

  ;; for C and C++ files
  (defun my-c-mode-common-setup ()
	(turn-off-auto-fill)
	)
  (add-hook 'c-mode-common-hook 'my-c-mode-common-setup)

  ;; only for C files
  (defun my-c-mode-setup ()
	(when (and buffer-file-name
			   (string-match "linux" buffer-file-name))
	  (progn (c-set-style "linux-tabs-only")
			 (setq tab-width 8
				   c-basic-offset 8))))
  (add-hook 'c-mode-hook 'my-c-mode-setup)
)
#+END_SRC

*** Builtin package: cc-engine
#+BEGIN_SRC emacs-lisp
(use-package cc-engine
  :defer t
  :config
  ;; Tell cc-mode not to check for old-style (K&R) function
  ;; declarations. This speeds up indenting a lot (I hear).
  (setq c-recognize-knr-p nil)
)
#+END_SRC

*** Builtin package: cc-vars
#+BEGIN_SRC emacs-lisp
(use-package cc-vars
  :defer t
  :config
  (setq c-default-style '((java-mode . "java")
						  (awk-mode . "awk")
						  (other . "linux")))
)
#+END_SRC

*** Builtin package: cc-styles
#+BEGIN_SRC emacs-lisp

(use-package cc-styles
  :commands (c-add-style)
  :config
  ;; Default style
  (c-add-style "linux-tabs-only"
			   '("linux" (c-offsets-alist (arglist-cont-nonempty
										   c-lineup-gcc-asm-reg
										   c-lineup-arglist-tabs-only))))
)
#+END_SRC

*** Helper for Linux
From linux/Documentation/CodingStyle, used in coding style "linux-tabs-only"

#+BEGIN_SRC emacs-lisp
(defvar c-syntactic-element)
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
		 (column (c-langelem-2nd-pos c-syntactic-element))
		 (offset (- (1+ column) anchor))
		 (steps (floor offset c-basic-offset)))
	(* (max steps 1)
	   c-basic-offset)))
#+END_SRC
** Builtin package: diff-mode: mayor mode for diff files

The following let the commits from =git diff >foo.diff= stand out more:

#+BEGIN_SRC emacs-lisp
(use-package diff-mode
  :defer t
  :config
  (defun my-diff-mode-setup ()
	(hi-lock-line-face-buffer "^commit")
	)
  (add-hook 'diff-mode-hook 'my-diff-mode-setup)
)
#+END_SRC

** Builtin package: ediff - interactive diff and merge
http://oremacs.com/2015/01/17/setting-up-ediff/

#+BEGIN_SRC emacs-lisp
(use-package ediff
  :commands (ediff-setup-keymap)
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-diff-options "-w")
  (defun my--ediff-hook ()
	(ediff-setup-keymap)
	(bind-key "j" 'ediff-next-difference ediff-mode-map)
	(bind-key "k" 'ediff-previous-difference ediff-mode-map))
  (add-hook 'ediff-mode-hook 'my--ediff-hook)
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo))
#+END_SRC

** Builtin package: elisp-mode - mayor mode for editing emacs lisp
#+BEGIN_SRC emacs-lisp
(defun my--elisp-setup ()
  ;; Setup imenu TODO
  (add-to-list 'imenu-generic-expression
			   '("Used Packages"
				 "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2))
  ;; automatically give help about function syntax
  (eldoc-mode t)
  ;; "-" is almost always part of a function- or variable-name
  (modify-syntax-entry ?- "w")
  ;; TODO
  ;; (unless (string= (buffer-name) "*scratch*")
  ;;   (auto-compile-mode 1))
  )
(use-package elisp-mode
  :defer t
  :config
  (add-hook 'emacs-lisp-mode-hook 'my--elisp-setup)
)
#+END_SRC

** Builtin package: nxml-mode: mayor mode for XML files
#+BEGIN_SRC emacs-lisp
(use-package nxml-mode
  :mode ("\\.xml$" . nxml-mode)
  :commands (indent-xml-region)
  :defer t
  :config
  (setq nxml-child-indent 4)
  (setq nxml-slash-auto-complete-flag t)
  ;; (add-hook 'nxml-mode-hook (lambda () (emmet-mode t)))))

 (defun indent-xml-region (begin end)
   "Pretty format XML markup in region. The function inserts
 linebreaks to separate tags that have nothing but whitespace
 between them. It then indents the markup by using nxml's
 indentation rules."
   (interactive "r")
   (save-excursion
	 (nxml-mode)
	 (goto-char begin)
	 (while (search-forward-regexp "\>[ \\t]*\<" nil t)
	   (backward-char) (insert "\n"))
	 (indent-region begin end)))
)
#+END_SRC

** Builtin package: python - mayor mode for Python
#+BEGIN_SRC emacs-lisp
(use-package python
  :config
  (defun my-python-setup ()
	(interactive)
	(setq indent-tabs-mode t
		  python-indent-offset 4
		  tab-width 4
		  ;; this fixes the weird indentation when entering a colon
		  ;; from http://emacs.stackexchange.com/questions/3322/python-auto-indent-problem
		  electric-indent-chars (delq ?: electric-indent-chars)))
  (add-hook 'python-mode-hook 'my-python-setup)
)
#+END_SRC

** Builtin package: sh-script - mayor mode for editing shell scripts
#+BEGIN_SRC emacs-lisp
(defun my-shell-tab-setup ()
  (interactive)
  (setq indent-tabs-mode t)
  (setq tab-width 4)
  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84))
  (bind-key "C-i" 'self-insert-command text-mode-map))
(use-package sh-script
  :defer t
  :config
  (add-hook 'sh-mode-hook    'my-shell-tab-setup)
)
#+END_SRC
** Builtin package: time
#+BEGIN_SRC emacs-lisp
(use-package time
  :defer t
  :config
  (setq display-time-world-time-format "%d.%m %R %Z"
		display-time-world-list '(("Europe/Berlin" "Frankfurt")
								  ("US/Arizona"    "Tucson")
								  ("Asia/Taipei"   "Taiwan")))
)
#+END_SRC

** Local package: compile
[[http://www.emacswiki.org/emacs/ModeCompile]]

#+BEGIN_SRC emacs-lisp
(defun my-compile-autoclose (buffer string)
  "Auto close compile log if there are no errors"
  (when (string-match "finished" string)
	(delete-window (get-buffer-window buffer t))
	(bury-buffer-internal buffer)))

(use-package compile
  :defer
  :diminish compilation-in-progress
  :config
  (defun my-colorize-compilation-buffer ()
	(read-only-mode 'toggle)
	(ansi-color-apply-on-region compilation-filter-start (point))
	(read-only-mode 'toggle))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer)
  (add-to-list 'compilation-finish-functions #'my-compile-autoclose)

  (setq compilation-always-kill t)
  (setq compilation-ask-about-save nil)
  (setq compilation-scroll-output 'first-error)

  ;; the next-error function weirdly stops at "In file included from
  ;; config.cpp:14:0:". Stop that:
  ;; http://stackoverflow.com/questions/15489319/how-can-i-skip-in-file-included-from-in-emacs-c-compilation-mode
  (setcar (nthcdr 5 (assoc 'gcc-include compilation-error-regexp-alist-alist)) 0)
)
#+END_SRC
** Local package: ivy-compile - interactive interface to select compile commands
Interactively select compile commands.

#+BEGIN_SRC emacs-lisp
(use-package ivy-compile
  :if (not noninteractive)
  :defer t
  :bind (("S-<f7>" . ivy-select-compile-command)
		 ("<f7>"   . ivy-compile))
)
#+END_SRC

** Local package: qt-pro - mayor mode for Qt project files
From  https://raw.githubusercontent.com/chriskonstad/emacs/master/elisp/qt-pro.el
#+BEGIN_SRC emacs-lisp
(use-package qt-pro
  :defer t
  :commands (qt-pro-mode)
  :mode ("\\.pr[io]\\'" . qt-pro-mode)
)
#+END_SRC

** Package: column-marker
#+BEGIN_SRC emacs-lisp
(use-package column-marker
  :straight t
  :commands (column-marker-1 column-marker-2)
  :init
  (defun my--column-marker-at-80 ()
	(interactive)
	(column-marker-2 80))
  (add-hook 'c-mode-hook 'my--column-marker-at-80)
  )
#+END_SRC
** Package: d-mode - mayor mode for D
#+BEGIN_SRC emacs-lisp
(use-package d-mode
  :straight t
  :defer t
  :mode ("\\.d\\'" . d-mode)
)
#+END_SRC

** Package: dump-jump (goto definition)
With this package you can goto a definition without any setup (e.g. no
TAGS file is needed).

See https://github.com/jacktasia/dumb-jump.

| C-M g | goto definition                    |
| C-M p | back (can be called multiple times |

#+BEGIN_SRC emacs-lisp
(use-package dumb-jump
  :straight t
  :bind (("C-M-g" . dumb-jump-go)
		 ("C-M-p" . dumb-jump-back)
		 ("C-M-q" . dumb-jump-quick-look) ;; this one is often used for other stuff
		 )
)
#+END_SRC

** Package: eros - show results of elisp evaluations

Eros-mode will show you the result of evaluating an elisp command
as an overlay in your elisp buffer. Try it out with =C-x C-e= now!

- https://www.reddit.com/r/emacs/comments/5iw5ml/eros_evaluation_result_overlays_for_emacs_lisp/
- https://github.com/xiongtx/eros

#+BEGIN_SRC emacs-lisp
(use-package eros
  :straight t
  :commands eros-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'eros-mode)
)
#+END_SRC

** Package: flycheck - compile-checking on-the-fly

| Key     | Function        |
|---------+-----------------|
| C-c ! l | list all issues |
| C-c ! n | next issue      |
| C-c ! p | previous issue  |

#+BEGIN_SRC emacs-lisp
(use-package flycheck
  :straight t
  :commands (flycheck-mode)
)
#+END_SRC

** Package: go-mode - mayor mode for Go
#+BEGIN_SRC emacs-lisp
(use-package go-mode
  :straight t
  :defer t
)
#+END_SRC

** Package: js2-mode - mayor mode for JavaScript
If i ever work more in JavaScript, I might add more from [[https://emacs.cafe/emacs/javascript/setup/2017/04/23/emacs-setup-javascript.html][this article]].

#+BEGIN_SRC emacs-lisp
(use-package js2-mode
  :straight t
  :mode ("\\.js\\'" . js2-mode)
  :interpreter ("node" . js2-mode)
  :config
  (setq js2-basic-offset 2
	js2-highlight-level 3)
  ;; we can run a nodejs REPL locally or over TRAMP, and it works out-of-the-box!
  (defalias 'run-node 'nodejs-repl)
)
(use-package js2-mode-extras
  :commands (js2-imenu-extras-mode)
  :config
  ;; better imenu
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
)
#+END_SRC

** Package: kurecolor - a hydra to modify color definitions
https://github.com/emacsfodder/kurecolor

This package allows interactive modification of color values.

#+BEGIN_SRC emacs-lisp
(use-package kurecolor
  :straight t
  :commands (kurecolor-decrease-brightness-by-step
			 kurecolor-increase-brightness-by-step
			 kurecolor-decrease-saturation-by-step
			 kurecolor-increase-saturation-by-step
			 kurecolor-decrease-hue-by-step
			 kurecolor-increase-hue-by-step
			 kurecolor-set-brightness
			 kurecolor-set-saturation
			 kurecolor-set-hue
			 kurecolor-hex-val-group
			 kurecolor-hex-sat-group
			 kurecolor-hex-hue-group
			 kurecolor-cssrgb-at-point-or-region-to-hex
			 kurecolor-hexcolor-at-point-or-region-to-css-rgb
			 kurecolor-hexcolor-at-point-or-region-to-css-rgba)
  :bind ("M-g k" . my-kurecolor)
  :config
  (defun my-kurecolor ()
	(interactive)
	(rainbow-mode t)
	(hydra-kurecolor/body)
	)
  (defhydra hydra-kurecolor (:color pink :hint  nil)
      "
Dec/Inc      _j_/_J_ brightness      _k_/_K_ saturation      _l_/_L_ hue
Set          _sj_ ^^ brightness      _sk_ ^^ saturation      _sl_ ^^ hue
Get          _gj_ ^^ brightness      _gk_ ^^ saturation      _gl_ ^^ hue

Convert      _ch_ ^^ RGB → Hex       _cr_ ^^ Hex → RGB       _cR_ ^^ Hex → RGBA
"
      ("j"  kurecolor-decrease-brightness-by-step)
      ("J"  kurecolor-increase-brightness-by-step)
      ("k"  kurecolor-decrease-saturation-by-step)
      ("K"  kurecolor-increase-saturation-by-step)
      ("l"  kurecolor-decrease-hue-by-step)
      ("L"  kurecolor-increase-hue-by-step)
      ("sj" kurecolor-set-brightness :color blue)
      ("sk" kurecolor-set-saturation :color blue)
      ("sl" kurecolor-set-hue :color blue)
      ("gj" kurecolor-hex-val-group :color blue)
      ("gk" kurecolor-hex-sat-group :color blue)
      ("gl" kurecolor-hex-hue-group :color blue)
      ("ch" kurecolor-cssrgb-at-point-or-region-to-hex :color blue)
      ("cr" kurecolor-hexcolor-at-point-or-region-to-css-rgb :color blue)
      ("cR" kurecolor-hexcolor-at-point-or-region-to-css-rgba :color blue)
      ("q"  nil "cancel" :color blue))
)
#+END_SRC

** Package: lua-mode: mayor mode for Lua
#+BEGIN_SRC emacs-lisp
(use-package lua-mode
  :straight t
  :mode (("\\.lua\\'" . lua-mode))
  :init
  ;; normally long lines get the same face as comments, which is quite irritating
  (defun my-lua-hook ()
	(setq-local whitespace-line-column 132))
  (add-hook 'lua-mode-hook #'my-lua-hook)
)
#+END_SRC
** Package: markdown-mode: mayor-mode for Markdown

#+BEGIN_SRC emacs-lisp
(use-package markdown-mode
  :straight t
  :mode (("\\.md\\'"       . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)))
#+END_SRC

** Package: pug-mode: mayor mode for vibe.d templates
#+BEGIN_SRC emacs-lisp
(use-package pug-mode
  :straight t
  :defer t
  :mode ("\\.dt\\'" . pug-mode)
)
#+END_SRC

** Package: rust-mode - mayor mode for Rust
#+BEGIN_SRC emacs-lisp
(use-package rust-mode
  :straight t
  :mode (("\\.rs\\'" . rust-mode))
)
#+END_SRC

** Package: toml-mode - mayor mode for Hugo's configuration
This is used for Hugo's config files:

#+BEGIN_SRC emacs-lisp
(use-package toml-mode
  :straight t
  :defer t
  :mode ("\\.toml\\'" . toml-mode)
)
#+END_SRC

** Package: rainbow-mode - show colors directly in the editor
Rainbow-mode turns color codes inside the editor into real colors.
Nice for elisp faces or CSS files.

#+BEGIN_SRC emacs-lisp
(use-package rainbow-mode
  :straight t
  :defer t
  :commands (rainbow-mode)
)
#+END_SRC
** Package: web-mode - mayor mode for HTML, CSS, JSON
Home page: http://web-mode.org/

#+BEGIN_SRC emacs-lisp
(use-package web-mode
  :straight t
  :commands (web-mode web-mode-guess-engine-and-content-type)
  :mode (("\\.html\\'" . web-mode)
		 ("\\.css\\'"  . web-mode)
		 ("\\.scss\\'" . web-mode)
		 ("\\.json\\'" . web-mode)
		 )
  :defines (web-mode-engines-alist)
  :config
  ;; remove the   (nil ("<!-" . "- | -->"))  data set:
  (unless (car (car (last web-mode-engines-auto-pairs)))
   	(setq web-mode-engines-auto-pairs (butlast web-mode-engines-auto-pairs)))

  (defun my-web-mode-hook ()
	(visual-line-mode 1)
	(setq web-mode-markup-indent-offset 2
		  web-mode-css-indent-offset 4
		  web-mode-code-indent-offset 2
		  web-mode-indent-style 2
		  web-mode-style-padding 1
		  web-mode-script-padding 1
		  web-mode-block-padding 0
		  indent-tabs-mode t
		  tab-width 4
		  web-mode-engines-alist '(("go" . "\\.html\\'")))
	(web-mode-guess-engine-and-content-type)
	)
  (add-hook 'web-mode-hook 'my-web-mode-hook)
)
#+END_SRC
** Package: zeal-at-point - documentation browser
I got the idea from this [[https://www.reddit.com/r/emacs/comments/4xa253/browser_for_api_documentation_inside_emacs/][reddit]] post: [[http://zealdocs.org/][Zeal]] and [[https://github.com/jinzhu/zeal-at-point][zeal-at-point]] are nice
documentation browsers.
#+BEGIN_SRC emacs-lisp
(use-package zeal-at-point
  :straight t
  :bind ("C-c d" . zeal-at-point)
  :config
  (add-to-list 'zeal-at-point-mode-alist '(c++-mode . "qt,c++"))
  ;; (add-hook 'rinari-minor-mode-hook (lambda () (setq zeal-at-point-docset "rails")))
)
#+END_SRC

** CANC Package: auto-compile

#+BEGIN_SRC emacs-lisp
(use-package auto-compile
  :config
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1)
  )
#+END_SRC
** CANC Package: semantic
#+BEGIN_SRC emacs-lisp
(use-package semantic
  :defer t
  :config

  ;; maybe set semantic-default-submodes
)
#+END_SRC
** CANC Package: smart-parens
Cancelled till I find out how to make it do it's magic only if there
rest of the line is empty.

#+BEGIN_SRC emacs-lisp
(use-package smartparens
  :defer t
  :diminish smartparens-mode
  :commands (show-smartparens-global-mode smartparens-global-mode sp-with-modes sp-local-pair)
)
(defun my-smart-parens-setup ()
  (show-smartparens-mode +1)
  (smartparens-mode 1)

  ;; when you press RET, the curly braces automatically
  ;; add another newline
  (sp-with-modes '(c-mode c++-mode)
	(sp-local-pair "{" nil
				   ;; :when '(sp-point-before-eol-p)
				   :post-handlers '(("||\n[i]"     "RET")))
	(sp-local-pair "/*" "*/"
				   ;; :when '(sp-point-before-eol-p)
				   :post-handlers '((" | "       "SPC")
									("* ||\n[i]" "RET"))))
  ;; (dolist (old-pair (cdr (assq t sp-pairs)))
  ;; 	(sp-pair (plist-get old-pair :open) nil :when '(sp-point-before-eol-p)))
  )
(add-hook 'c-mode-common-hook 'my-smart-parens-setup)
;; (add-hook 'emacs-lisp-mode-hook 'my-smart-parens-setup)
#+END_SRC


* Determine automatically loaded packages

First we create this =empty.el= file:

#+BEGIN_SRC emacs-lisp :tangle no
(message "FEATURES: %s" features)
#+END_SRC

and then we run emacs on this file:

#+BEGIN_SRC shell :tangle no
emacs --batch -Q -l empty.el --kill
#+END_SRC

The result will be assigned to =features-when-run-emacs-Q=. And if we
then find the intersection of the loaded packages between this batch
mode invocation and a normal invocation, we know what packages got
loaded due to our =init.el= file.

#+BEGIN_SRC emacs-lisp :tangle no
(setq features-when-run-emacs-Q '(mule-util tooltip eldoc
electric uniquify ediff-hook vc-hooks lisp-float-type mwheel
x-win term/common-win x-dnd tool-bar dnd fontset image regexp-opt
fringe tabulated-list newcomment elisp-mode lisp-mode prog-mode
register page menu-bar rfn-eshadow timer select scroll-bar mouse
jit-lock font-lock syntax facemenu font-core frame cl-generic
cham georgian utf-8-lang misc-lang vietnamese tibetan thai
tai-viet lao korean japanese eucjp-ms cp51932 hebrew greek
romanian slovak czech european ethiopic indian cyrillic chinese
charscript case-table epa-hook jka-cmpr-hook help simple abbrev
minibuffer cl-preloaded nadvice loaddefs button faces cus-face
macroexp files text-properties overlay sha1 md5 base64 format env
code-pages mule custom widget hashtable-print-readable backquote
dbusbind inotify dynamic-setting system-font-setting
font-render-setting move-toolbar gtk x-toolkit x multi-tty
make-network-process emacs))

(message "FEATURES: %s" (cl-set-difference features features-when-run-emacs-Q))
#+END_SRC

And finally, to find out which modules are loaded by a batch emacs, run

#+BEGIN_SRC shell :tangle no
emacs --batch -Q -l init.el --kill | fmt
#+END_SRC

Tata!
