;; A folding-editor-like minor mode.

;; Copyright (C) 1992, 1993, Jamie Lokier.  All rights reserved.

;; This file is intended to be used with GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; ---------------------------------------------------------------------

;; This is version 1.6 of Folding mode, under development.

;; This file has been edited with a folding editor (itself! :-).

;; Send suggestions and/or bug fixes to "u90jl@ecs.ox.ac.uk".

;; If you can, please check the most recent version of Folding mode
;; before reporting bugs.  If you can't, don't be afraid of reporting
;; bugs anyway.

;;{{{ Information

;; ----------------------- Archive information --------------------------

;; LCD Archive Entry:
;; folding|Jamie Lokier|u90jl@ecs.ox.ac.uk|
;; A folding-editor-like minor mode.|
;; 93-06-8|1.6|~/modes/folding.el.gz|

;; -------------------------- Installation ------------------------------

;; To install Folding mode, put this file (folding.el) on you Emacs-Lisp
;; load path, and put the following in your .emacs:
;;
;; (autoload 'folding-mode "folding"
;;  "Minor mode that simulates a folding editor" t)
;;
;; To have Folding mode start automatically when opening folded files,
;; add the following to your .emacs as well:
;;
;; (defun folding-mode-find-file-hook ()
;;   "One of the hooks called whenever a `find-file' is successful."
;;   (and (assq 'folded-file (buffer-local-variables))
;;        folded-file
;;        (folding-mode 1)
;;        (kill-local-variable 'folded-file)))
;;
;; (or (memq 'folding-mode-find-file-hook find-file-hooks)
;;     (setq find-file-hooks (append find-file-hooks
;;                                   '(folding-mode-find-file-hook))))
;;
;; If you load folding.el all the time during startup, none of the above
;; is necessary; it can be replaced with this after loading folding.el:
;;
;; (folding-mode-add-find-file-hook)
;;
;; Brief documentation for Folding mode (what it is, how you use it) is
;; provided with the definition of the function `folding-mode'.
;;
;; The best way to learn how to use Folding mode after installing it is
;; to find-file the source, M-x eval-current-buffer, M-x folding-mode,
;; and move in and out of the folds.  Keys are documented under the
;; function `folding-mode', though you might want to customize them.
;; Keys in folding mode are bound in the keymap `folding-mode-map'.

;; --------------------------- And the rest -----------------------------

;; There are is no real documentation yet; I haven't had time.  I intend
;; to write some one day, but I will refrain from predicting when.  Read
;; the documentation for the function `folding-mode' for the most useful
;; tips.

;; Emacs 18:
;; Folding mode has been tested with versions 18.55 and 18.58 of Emacs.

;; Epoch:
;; Folding mode has been tested on Epoch 4.0p2.

;; Lucid Emacs:
;; There is code in here to handle some aspects of Lucid Emacs.
;; However, up to version 19.6, there appears to be no way to display
;; folds.  Selective-display does not work, and neither do invisible
;; extents, so Folding mode has no chance of working.  This is likely to
;; change in future versions of Lucid Emacs.

;; Emacs 19: 
;; Tested on version 19.8, appears to be fine.
;; Minor bug: display the buffer in several different frames, then move
;; in and out of folds in the buffer.  The frames are automatically
;; moved to the top of the stacking order.

;; Some of the code is quite horrible, generally in order to avoid some
;; Emacs display "features".  Some of it is specific to certain versions
;; of Emacs.  By the time Emacs 19 is around and everyone is using it,
;; hopefully most of it won't be necessary.

;; ------------------------ More known bugs -----------------------------

;; *** Needs fold-fold-region to be more intelligent about
;; finding a good region.  Check folding a whole current fold.

;; *** Now works with 19!  But check out what happens when you exit a
;; fold with the file displayed in two frames.  Both windows get
;; fronted.  Better fix that sometime.

;; ------------------------- Future features ----------------------------

;; *** I will add a `fold-next-error' sometime.  It will only work with
;; Emacs versions later than 18.58, because compile.el in earlier
;; versions does not count line-numbers in the right way, when selective
;; display is active.

;; *** Fold titles should be optionally allowed on the closing fold
;; marks, and `fold-tidy-inside' should check that the opening title
;; matches the closing title.

;; *** `folded-file' set in the local variables at the end of a file
;; could encode the type of fold marks used in that file, and other
;; things, like the margins inside folds.

;; *** I can see a lot of use for the newer features of Emacs 19:
;;
;;   Using invisible text-properties (I hope they are intended to
;;   make text invisible; it isn't implemented like that yet), it
;;   will be possible to hide folded text without affecting the
;;   text of the buffer.  At the moment, Folding mode uses
;;   selective display to hide text, which involves substituting
;;   carriage-returns for line-feeds in the buffer.  This isn't
;;   such a good way.  It may also be possible to display
;;   different folds in different windows in Emacs 19.
;;
;;   Using even more text-properties, it may be possible to track
;;   pointer movements in and out of folds, and have Folding mode
;;   automatically enter or exit folds as necessary to maintain a
;;   sensible display.  Because the text itself is not modified
;;   (if overlays are used to hide text), this is quite safe.  It
;;   would make it unnecessary to provide functions like
;;   `fold-forward-char', `fold-goto-line' or `fold-next-error',
;;   and things like I-search would automatically move in and out
;;   of folds as necessary.
;;
;;   Yet more text-properties/overlays might make it possible to
;;   avoid using narrowing.  This might allow some major modes to
;;   indent text properly, e.g., C++ mode.

;;}}}
;;{{{ Declare `folding' as a feature

(provide 'folding)

;;}}}
;;{{{ Check Emacs version and set some constants.

;; Sets `fold-emacs-version' to `epoch, `lucid, or the numbers 18 or 19,
;; as appropriate, and sets a few related variables.

(setq fold-epoch-screens-p nil
      fold-lucid-screens-p nil
      fold-lucid-keymaps-p nil
      fold-emacs-frames-p nil)

(let ((case-fold-search t))
  (cond ((boundp 'epoch::version)		;; Epoch
	 (setq fold-epoch-screens-p t))
	((string-match "lucid" emacs-version)	;; Lucid Emacs
	 (setq fold-lucid-screens-p t
	       fold-lucid-keymaps-p t))
	((string< emacs-version "19"))		;; Emacs 18.x (or less)
	(t					;; Emacs 19+
	 (setq fold-emacs-frames-p t))))

;;}}}
;;{{{ Start Folding mode, and related items.  Documentation is here

;;{{{ folding-mode the variable

(defvar folding-mode nil
  "Non-nil means Folding mode is active in the current buffer.")

(make-variable-buffer-local 'folding-mode)
(set-default 'folding-mode nil)

;;}}}
;;{{{ folding-mode the function

(defun folding-mode (&optional arg inter)
  "Turns Folding mode (a minor mode) on and off.

These are the basic commands that Folding mode provides:
\\<folding-mode-map>
fold-enter:	    `\\[fold-enter]'
     Enters the fold that the point is on.

fold-exit:	    `\\[fold-exit]'
     Exits the current fold.

fold-fold-region:   `\\[fold-fold-region]'
     Surrounds the region with a new fold.

fold-top-level:	    `\\[fold-top-level]'
     Exits all folds.

fold-show:	    `\\[fold-show]'
     Opens the fold that the point is on, but does not enter it.

fold-hide:	    `\\[fold-hide]'
     Closes the fold that the point is in, exiting it if necessary.

fold-whole-buffer:  `\\[fold-whole-buffer]'
     Folds the whole buffer.

fold-open-buffer:   `\\[fold-open-buffer]'
     Unfolds the whole buffer; good to do just before a search.

fold-remove-folds:  `\\[fold-remove-folds]'
     Makes a ready-to-print, formatted, unfolded copy in another buffer.

Read the documentation for the above functions for more information.

Folds are a way of hierarchically organising the text in a file, so that
the text can be viewed and edited at different levels.  It is similar to
Outline mode in that parts of the text can be hidden from view.  A fold
is a region of text, surrounded by special \"fold marks\", which act
like brackets, grouping the text.  Fold mark pairs can be nested, and
they can have titles.  When a fold is folded, the text is hidden from
view, except for the first line, which acts like a title for the fold.

Folding mode is a minor mode, designed to cooperate with many other
major modes, so that many types of text can be folded while they are
being edited (eg., plain text, program source code, Texinfo, etc.).

For most types of folded file, lines representing folds have \"{{{\"
near the beginning.  To enter a fold, move the point to the folded line
and type `\\[fold-enter]'.  You should no longer be able to see the rest
of the file, just the contents of the fold, which you couldn't see
before.  You can use `\\[fold-exit]' to leave a fold, and you can enter
and exit folds to move around the structure of the file.

All of the text is present in a folded file all of the time.  It is just
hidden.  Folded text shows up as a line (the top fold mark) with \"...\"
at the end.  If you are in a fold, the mode line displays \"inside n
folds Narrow\", and because the buffer is narrowed you can't see outside
of the current fold's text.

By arranging sections of a large file in folds, and maybe subsections in
sub-folds, you can move around a file quickly and easily, and only have
to scroll through a couple of pages at a time.  If you pick the titles
for the folds carefully, they can be a useful form of documentation, and
make moving though the file a lot easier.  In general, searching through
a folded file for a particular item is much easier than without folds.

To make a new fold, set the mark at one end of the text you want in the
new fold, and move the point to the other end.  Then type
`\\[fold-fold-region]'.  The text you selected will be made into a fold,
and the fold will be entered.  If you just want a new, empty fold, set
the mark where you want the fold, and then create a new fold there
without moving the point.  Don't worry if the point is in the middle of
a line of text, `fold-fold-region' will not break text in the middle of
a line.  After making a fold, the fold is entered and the point is
positioned ready to enter a title for the fold.  Do not delete the fold
marks, which are usually something like \"{{{\" and \"}}}\".  There may
also be a bit of fold mark which goes after the fold title.

If the fold markers get messed up, or you just want to see the whole
unfolded file, use `\\[fold-open-buffer]' to unfolded the whole file, so
you can see all the text and all the marks.  This is useful for
checking/correcting unbalanced fold markers, and for searching for
things.  Use `\\[fold-whole-file]' to fold the buffer again.

`fold-exit' will attempt to tidy the current fold just before exiting
it.  It will remove any extra blank lines at the top and bottom,
\(outside the fold marks).  It will then ensure that fold marks exists,
and if they are not, will add them (after asking).  Finally, the number
of blank lines between the fold marks and the contents of the fold is
set to 1 (by default).

You can make folded files start Folding mode automatically when they are
visited by setting `folded-file' to t in the file's local variables.
For example, having the following at the end of an Emacs-Lisp file
causes it to be folded when visited:

;; Local variables:
;; folded-file: t
;; end:

This only works if you have the appropriate hook set up.  Look up the
function `folding-mode-add-find-file-hook' for details.

If the fold marks are not set on entry to Folding mode, they are set to
a default for current major mode, as defined by `fold-mode-marks-alist'
or to \"{{{ \" and \"}}}\" if none are specified.

To bind different commands to keys in Folding mode, set the bindings in
the keymap `folding-mode-map'.

The hooks `folding-mode-hook' and `<major-mode-name>-folding-hook' are
called before folding the buffer and applying the key bindings in
`folding-mode-map'.  This is a good hook to set extra or different key
bindings in `folding-mode-map'.  Note that key bindings in
`folding-mode-map' are only examined just after calling these hooks; new
bindings in those maps only take effect when Folding mode is being
started.

If Folding mode is not called interactively (`(interactive-p)' is nil),
and it is called with two or less arguments, all of which are nil, then
the point will not be altered if `fold-fold-on-startup' is set and
`fold-whole-buffer' is called.  This is generally not a good thing, as
it can leave the point inside a hidden region of a fold, but it is
required if the local variables set \"mode: folding\" when the file is
first read (see `hack-local-variables').

Not that you should ever want to, but to call Folding mode from a
program with the default behaviour (toggling the mode), call it with
something like `(folding-mode nil t)'.

Here is the full list of keys bound in Folding mode:
\\{folding-mode-map}"
  (interactive)
  (let ((new-folding-mode
	 (if (not arg) (not folding-mode)
	   (> (prefix-numeric-value arg) 0))))
    (or (eq new-folding-mode
	    folding-mode)
	(if folding-mode
	    (progn
	      (setq selective-display nil)
	      (fold-clear-stack)
	      (widen)
	      (fold-subst-regions (list 1 (point-max)) ?\r ?\n)
	      (and (boundp 'fold-saved-local-keymap)
		   (progn
		     (use-local-map fold-saved-local-keymap)
		     (kill-local-variable 'fold-saved-local-keymap)
		     (makunbound 'fold-saved-local-keymap))))
	  (make-local-variable 'fold-saved-local-keymap)
	  (setq fold-saved-local-keymap (current-local-map))
	  (setq selective-display t)
	  (setq selective-display-ellipses t)
	  (widen)
	  (set (make-local-variable 'fold-stack) nil)
	  (make-local-variable 'fold-top-mark)
	  (make-local-variable 'fold-secondary-top-mark)
	  (make-local-variable 'fold-top-regexp)
	  (make-local-variable 'fold-bottom-mark)
	  (make-local-variable 'fold-bottom-regexp)
	  (make-local-variable 'fold-regexp)
	  (or (and (boundp 'fold-top-regexp)
		   fold-top-regexp
		   (boundp 'fold-bottom-regexp)
		   fold-bottom-regexp)
	      (let ((fold-marks (assq major-mode
				      fold-mode-marks-alist)))
		(if fold-marks
		    (setq fold-marks (cdr fold-marks))
		  (setq fold-marks '("{{{ " "}}}")))
		(apply 'fold-set-marks fold-marks)))
	  (unwind-protect
	      (let ((hook-symbol (intern-soft
				  (concat
				   (symbol-name major-mode)
				   "-folding-hook"))))
		(run-hooks 'folding-mode-hook)
		(and hook-symbol
		     (run-hooks hook-symbol)))
	    (fold-set-mode-line)
	    (use-local-map
	     (fold-merge-keymaps (current-local-map) folding-mode-map)))
	  (and fold-fold-on-startup
	       (if (or (interactive-p)
		       arg
		       inter)
		   (fold-whole-buffer)
		 (save-excursion
		   (fold-whole-buffer))))
	  (fold-narrow-to-region nil nil t)))
    (setq folding-mode new-folding-mode)))

;;}}}
;;{{{ folding-mode-map

(defvar folding-mode-map nil
  "Keymap used in Folding mode (a minor mode).")

(and fold-lucid-keymaps-p
     (set-keymap-name folding-mode-map 'folding-mode-map))

(if folding-mode-map
    nil
  (setq folding-mode-map (make-sparse-keymap))
  (define-key folding-mode-map "\M-g" 'fold-goto-line)
  (define-key folding-mode-map "\C-c\\" 'fold-enter)
  (define-key folding-mode-map "\C-c/" 'fold-exit)
  (define-key folding-mode-map "\C-c\C-j" 'fold-top-level)
  (define-key folding-mode-map "\C-c\C-f" 'fold-fold-region)
  (define-key folding-mode-map "\C-c\C-s" 'fold-show)
  (define-key folding-mode-map "\C-c\C-h" 'fold-hide)
  (define-key folding-mode-map "\C-c\C-o" 'fold-open-buffer)
  (define-key folding-mode-map "\C-c\C-w" 'fold-whole-buffer)
  (define-key folding-mode-map "\C-c\C-r" 'fold-remove-folds)
  (define-key folding-mode-map "\C-f" 'fold-forward-char)
  (define-key folding-mode-map "\C-b" 'fold-backward-char)
  (define-key folding-mode-map "\C-e" 'fold-end-of-line))

;;}}}
;;{{{ fold-stack

;; This is a list of structures which keep track of folds being entered
;; and exited. It is a list of (MARKER . MARKER) pairs, followed by the
;; symbol `folded'.  The first of these represents the fold containing
;; the current one.  If the view is currently outside all folds, this
;; variable has value nil.

(defvar fold-stack nil
  "A list of marker pairs representing folds entered so far.")

;;}}}
;;{{{ fold-clear-stack

;; Clear the fold stack, and release all the markers it refers to.

(defun fold-clear-stack ()
  (let ((stack fold-stack))
    (setq fold-stack nil)
    (while (and stack (not (eq 'folded (car stack))))
      (set-marker (car (car stack)) nil)
      (set-marker (cdr (car stack)) nil)
      (setq stack (cdr stack)))))

;;}}}
;;{{{ fold-mode-string

(defvar fold-mode-string nil
  "Buffer-local variable that holds the fold depth description.")

(set-default 'fold-mode-string " Folding")

;;}}}
;;{{{ fold-set-mode-line

;; Sets `fold-mode-string' appropriately.  This allows the Folding mode
;; description in the mode line to reflect the current fold depth."

(defun fold-set-mode-line ()
  (if (null fold-stack)
      (kill-local-variable 'fold-mode-string)
    (make-local-variable 'fold-mode-string)
    (setq fold-mode-string (if (eq 'folded (car fold-stack))
				  " inside 1 fold"
				(concat " inside "
					(length fold-stack)
					" folds")))))

;;}}}
;;{{{ Update minor-mode-alist

(or (assq 'folding-mode minor-mode-alist)
    (setq minor-mode-alist
		(cons '(folding-mode fold-mode-string)
		      minor-mode-alist)))

;;}}}

;;}}}
;;{{{ Hooks and variables

;;{{{ folding-mode-hook

(defvar folding-mode-hook nil
  "Hook called when Folding mode is entered.

A hook named `<major-mode>-folding-hook' is also called, if it
exists.  Eg., `c-mode-folding-hook' is called whenever Folding mode is
started in C mode.")

;;}}}
;;{{{ fold-fold-on-startup

(defvar fold-fold-on-startup t
  "*If non-nil, buffers are folded when starting Folding mode.")

;;}}}
;;{{{ fold-internal-margins

(defvar fold-internal-margins 1
  "*Number of blank lines left next to fold marks when tidying folds.

This variable is local to each buffer.  To set the default value for all
buffers, use `set-default'.

When exiting a fold, and at other times, `fold-tidy-inside' is invoked
to ensure that the fold is in the correct form before leaving it.  This
variable specifies the number of blank lines to leave between the
enclosing fold marks and the enclosed text.

If this value is nil or negative, no blank lines are added or removed
inside the fold marks.  A value of 0 (zero) is valid, meaning leave no
blank lines.

See also `fold-tidy-inside'.")

(make-variable-buffer-local 'fold-internal-margins)

;;}}}
;;{{{ fold-mode-marks-alist

(defvar fold-mode-marks-alist nil
  "List of (major-mode . fold marks) default combinations to use.
When Folding mode is started, the major mode is checked, and if there
are fold marks for that major mode stored in `fold-mode-marks-alist',
those marks are used by default.  If none are found, the default values
of \"{{{ \" and \"}}}\" are used.")

;;}}}

;;}}}
;;{{{ Regular expressions for matching fold marks

;;{{{ fold-set-marks

;; You think those "\\(\\)" pairs are peculiar?  Me too.  Emacs regexp
;; stuff has a bug; sometimes "\\(.*\\)" fails when ".*" succeeds, but
;; only in a folded file!  Strange bug!  Must check it out sometime.

(defun fold-set-marks (top bottom &optional secondary)
  "Sets the folding top and bottom marks for the current buffer.

The fold top mark is set to TOP, and the fold bottom mark is set to
BOTTOM.  And optional SECONDARY top mark can also be specified -- this
is inserted by `fold-fold-region' after the fold top mark, and is
presumed to be put after the title of the fold.  This is not necessary
with the bottom mark because it has no title.

Various regular expressions are set with this function, so don't set the
mark variables directly."
  (set (make-local-variable 'fold-top-mark)
       top)
  (set (make-local-variable 'fold-bottom-mark)
       bottom)
  (set (make-local-variable 'fold-secondary-top-mark)
       secondary)
  (set (make-local-variable 'fold-top-regexp)
       (concat "\\(^\\|\r+\\)[ \t]*"
	       (regexp-quote fold-top-mark)))
  (set (make-local-variable 'fold-bottom-regexp)
       (concat "\\(^\\|\r+\\)[ \t]*"
	       (regexp-quote fold-bottom-mark)))
  (set (make-local-variable 'fold-regexp)
       (concat "\\(^\\|\r\\)\\([ \t]*\\)\\(\\("
	       (regexp-quote fold-top-mark)
	       "\\)\\|\\("
	       (regexp-quote fold-bottom-mark)
	       "[ \t]*\\(\\)\\($\\|\r\\)\\)\\)")))

;;}}}

;;}}}
;;{{{ Cursor movement that skips folded regions

;;{{{ fold-forward-char

(defun fold-forward-char (&optional arg)
  "Move point right ARG characters, skipping hidden folded regions.
Moves left if ARG is negative.  On reaching end of buffer, stop and
signal error."
  (interactive "p")
  (if (eq arg 1)
      ;; Do it a faster way for arg = 1.
      (if (eq (following-char) ?\r)
	  (let ((saved (point))
		(inhibit-quit t))
	    (end-of-line)
	    (if (not (eobp))
		(forward-char)
	      (goto-char saved)
	      (error "End of buffer")))
	;; `forward-char' here will do its own error if (eobp).
	(forward-char))
    (if (> 0 (or arg (setq arg 1)))
	(fold-backward-char (- arg))
      (let (goal saved)
	(while (< 0 arg)
	  (skip-chars-forward "^\r" (setq goal (+ (point) arg)))
	  (if (eq goal (point))
	      (setq arg 0)
	    (if (eobp)
		(error "End of buffer")
	      (setq arg (- goal 1 (point))
		    saved (point))
	      (let ((inhibit-quit t))
		(end-of-line)
		(if (not (eobp))
		    (forward-char)
		  (goto-char saved)
		  (error "End of buffer"))))))))))

;;}}}
;;{{{ fold-backward-char

(defun fold-backward-char (&optional arg)
  "Move point left ARG characters, skipping hidden folded regions.
Moves right if ARG is negative.  On reaching beginning of buffer, stop
and signal error."
  (interactive "p")
  (if (eq arg 1)
      ;; Do it a faster way for arg = 1.
      ;; Catch the case where we are in a hidden region, and bump into a \r.
      (if (or (eq (preceding-char) ?\n)
	      (eq (preceding-char) ?\r))
	  (let ((pos (1- (point)))
		(inhibit-quit t))
	    (forward-char -1)
	    (beginning-of-line)
	    (skip-chars-forward "^\r" pos))
	(forward-char -1))
    (if (> 0 (or arg (setq arg 1)))
	(fold-forward-char (- arg))
      (let (goal)
	(while (< 0 arg)
	  (skip-chars-backward "^\r\n" (max (point-min)
					    (setq goal (- (point) arg))))
	  (if (eq goal (point))
	      (setq arg 0)
	    (if (bobp)
		(error "Beginning of buffer")
	      (setq arg (- (point) 1 goal)
		    goal (point))
	      (let ((inhibit-quit t))
		(forward-char -1)
		(beginning-of-line)
		(skip-chars-forward "^\r" goal)))))))))

;;}}}
;;{{{ fold-end-of-line

(defun fold-end-of-line (&optional arg)
  "Move point to end of current line, but before hidden folded region.

Has the same behavior as `end-of-line', except that if the current line
ends with some hidden folded text (represented by an ellipsis), the
point is positioned just before it.  This prevents the point from being
placed inside the folded text, which is not normally useful."
  (interactive "p")
  (if (or (eq arg 1)
	  (not arg))
      (beginning-of-line)
    ;; `forward-line' also moves point to beginning of line.
    (forward-line (1- arg)))
  (skip-chars-forward "^\r\n"))

;;}}}
;;{{{ fold-skip-ellipsis-backward

(defun fold-skip-ellipsis-backward ()
  "Moves the point backwards out of folded text.

If the point is inside a folded region, the cursor is displayed at the
end of the ellipsis representing the folded part.  This function checks
to see if this is the case, and if so, moves the point backwards until
it is just outside the hidden region, and just before the ellipsis.

Returns t if the point was moved, nil otherwise."
  (interactive)
  (let ((pos (point))
	result)
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward "^\r" pos)
      (or (eq pos (point))
	  (setq pos (point)
		result t)))
    (goto-char pos)
    result))

;;}}}

;;}}}
;;{{{ Moving in and out of folds

;;{{{ fold-enter

(defun fold-enter (&optional noerror)
  "Open and enter the fold at or around the point.

Enters the fold that the point is inside, wherever the point is inside
the fold, provided it is a valid fold with balanced top and bottom
marks.  Returns nil if the fold entered contains no sub-folds, t
otherwise.  If an optional argument NOERROR is non-nil, returns nil if
there are no folds to enter, instead of causing an error.

If the point is inside a folded, hidden region (as represented by an
ellipsis), the position of the point in the buffer is preserved, and as
many folds as necessary are entered to make the surrounding text
visible.  This is useful after some commands eg., search commands."
  (interactive)
  (let ((goal (point)))
    (if (fold-skip-ellipsis-backward)
	(while (prog2 (beginning-of-line)
		      (fold-enter t)
		      (goto-char goal)))
      (let ((data (fold-show noerror t)))
	(and data
	     (progn
	       (setq fold-stack
		     (if fold-stack
			 (cons (cons (point-min-marker) (point-max-marker))
			       fold-stack)
		       '(folded)))
	       (fold-set-mode-line)
	       (fold-narrow-to-region (car data) (nth 1 data))
	       (nth 2 data)))))))

;;}}}
;;{{{ fold-exit

(defun fold-exit ()
  "Exits the current fold."
  (interactive)
  (if fold-stack
      (progn
	(fold-tidy-inside)
	(fold-subst-regions (list (point-min) (point-max)) ?\n ?\r)
	(goto-char (point-min))	       ;; So point is correct in other windows.
	(if (eq (car fold-stack) 'folded)
	    (fold-narrow-to-region nil nil t)
	  (fold-narrow-to-region (marker-position (car (car fold-stack)))
				 (marker-position (cdr (car fold-stack))) t))
	(and (consp (car fold-stack))
	     (set-marker (car (car fold-stack)) nil)
	     (set-marker (cdr (car fold-stack)) nil))
	(setq fold-stack (cdr fold-stack)))
    (error "Outside all folds"))
  (fold-set-mode-line))

;;}}}
;;{{{ fold-show

(defun fold-show (&optional noerror noskip)
  "Opens the fold that the point is on, but does not enter it.
Optional arg NOERROR means don't signal an error if there is no fold,
just return nil.  NOSKIP means don't jump out of a hidden region first.

Returns ((START END SUBFOLDS-P).  START and END indicate the extents of
the fold that was shown.  If SUBFOLDS-P is non-nil, the fold contains
subfolds."
  (interactive "p")
  (or noskip
      (fold-skip-ellipsis-backward))
  (let ((point (point))
	backward forward start end subfolds-not-p)
    (unwind-protect
	(or (and (integerp (car-safe (setq backward (fold-skip-folds t))))
		 (integerp (car-safe (setq forward (fold-skip-folds nil))))
		 (progn
		   (goto-char (car forward))
		   (skip-chars-forward "^\r\n")
		   (setq end (point))
		   (skip-chars-forward "\r\n")
		   (not (and fold-stack (eobp))))
		 (progn
		   (goto-char (car backward))
		   (skip-chars-backward "^\r\n")
		   (setq start (point))
		   (skip-chars-backward "\r\n")
		   (not (and fold-stack (bobp))))
		 (progn
		   (setq point start)
		   (setq subfolds-not-p	; Avoid holding the list through a GC.
			 (not (or (cdr backward) (cdr forward))))
		   (fold-subst-regions (append backward (nreverse forward))
				       ?\r ?\n)
		   (list start end (not subfolds-not-p))))
	    (if noerror
		nil
	      (error "Not on a fold")))
      (goto-char point))))

;;}}}
;;{{{ fold-hide

(defun fold-hide ()
  "Close the fold around the point, undoes effect of `fold-show'."
  (interactive)
  (fold-skip-ellipsis-backward)
  (if (and (integerp (setq start (car-safe (fold-skip-folds t))))
	   (integerp (setq end (car-safe (fold-skip-folds nil)))))
      (if (and fold-stack
	       (or (eq start (point-min))
		   (eq end (point-max))))
	  (error "Cannot hide current fold")
	(goto-char start)
	(skip-chars-backward "^\r\n")
	(fold-subst-regions (list start end) ?\n ?\r))
    (error "Not on a fold")))

;;}}}
;;{{{ fold-top-level

(defun fold-top-level ()
  "Exits all folds, to the top level."
  (interactive)
  (while fold-stack
    (fold-exit)))

;;}}}
;;{{{ fold-goto-line

(defun fold-goto-line (line)
  "Go to line ARG, entering as many folds as possible."
  (interactive "nGoto line: ")
  (fold-top-level)
  (goto-char 1)
  (and (< 1 line)
       (re-search-forward "[\n\C-m]" nil 0 (1- line)))
  (let ((goal (point)))
    (while (prog2 (beginning-of-line)
		  (fold-enter t)
		  (goto-char goal))))
  (fold-narrow-to-region (point-min) (point-max) t))

;;}}}

;;}}}
;;{{{ Searching for fold boundaries

;;{{{ fold-skip-folds

;; Skips forward through the buffer (backward if BACKWARD is non-nil)
;; until it finds a closing fold mark or the end of the buffer.  The
;; point is not moved.  Jumps over balanced fold-mark pairs on the way.
;; Returns t if the end of buffer was found in an unmatched fold-mark
;; pair, otherwise a list.

;; If the point is actually on an fold start mark, the mark is ignored;
;; if it is on an end mark, the mark is noted.  This decision is
;; reversed if BACKWARD is non-nil.  If optional OUTSIDE is non-nil and
;; BACKWARD is nil, either mark is noted.

;; The first element of the list is a position in the end of the closing
;; fold mark if one was found, or nil.  It is followed by (END START)
;; pairs (flattened, not a list of pairs).  The pairs indicating the
;; positions of folds skipped over; they are positions in the fold
;; marks, not necessarily at the ends of the fold marks.  They are in
;; the opposite order to that in which they were skipped.  The point is
;; left in a meaningless place.  If going backwards, the pairs are
;; (START END) pairs, as the fold marks are scanned in the opposite
;; order.

;; Works by maintaining the position of the top and bottom marks found
;; so far.  They are found separately using a normal string search for
;; the fixed part of a fold mark (because it is faster than a regexp
;; search if the string does not occur often outside of fold marks),
;; checking that it really is a proper fold mark, then considering the
;; earliest one found.  The position of the other (if found) is
;; maintained to avoid an unnecessary search at the next iteration.

(defun fold-skip-folds (backward &optional outside)
  (save-excursion
    (let ((depth 0) pairs point temp start first last
	  (first-mark (if backward fold-bottom-mark fold-top-mark))
	  (last-mark (if backward fold-top-mark fold-bottom-mark))
	  (search (if backward 'search-backward 'search-forward)))
      (skip-chars-backward "^\r\n")
      (if outside
	  nil
	(and (eq (preceding-char) ?\r)
	     (forward-char -1))
	(if (looking-at fold-top-regexp)
	    (if backward
		(setq last (match-end 1))
	      (skip-chars-forward "^\r\n"))))
      (while (progn 
	       ;; Find last first, prevents unnecessary searching for first.
	       (setq point (point))
	       (or last
		   (while (and (funcall search last-mark first t)
			       (progn
				 (setq temp (point))
				 (goto-char (match-beginning 0))
				 (skip-chars-backward " \t")
				 (and (not (setq last
						 (if (eq (preceding-char) ?\r)
						     temp
						   (and (bolp) temp))))
				      (goto-char temp)))))
		   (goto-char point))
	       (or first
		   (while (and (funcall search first-mark last t)
			       (progn
				 (setq temp (point))
				 (goto-char (match-beginning 0))
				 (skip-chars-backward " \t")
				 (and (not (setq first
						 (if (eq (preceding-char) ?\r)
						     temp
						   (and (bolp) temp))))
				      (goto-char temp))))))
	       ;; Return value of conditional says whether to iterate again.
	       (if (not last)
		   ;; Return from this with the result.
		   (not (setq pairs (if first t (cons nil pairs))))
		 (if (and first (if backward (> first last) (< first last)))
		     (progn
		       (goto-char first)
		       (if (eq 0 depth)
			   (setq start first
				 first nil
				 depth 1) ;; non-nil value, loop again.
			 (setq first nil
			       depth (1+ depth)))) ;; non-nil value, loop again
		   (goto-char last)
		   (if (eq 0 depth)
		       (not (setq pairs (cons last pairs)))
		     (or (< 0 (setq depth (1- depth)))
			 (setq pairs (cons last (cons start pairs))))
		     (setq last nil)
		     t)))))
      pairs)))

;;}}}

;;}}}
;;{{{ Functions that actually modify the buffer

;;{{{ fold-fold-region

(defun fold-fold-region (start end)
  "Places fold marks at the beginning and end of a specified region.
The region is specified by two arguments START and END.  The point is
left at a suitable place ready to insert the title of the fold."
  (interactive "r")
  (and (< end start)
       (setq start (prog1 end
		     (setq end start))))
  (setq end (set-marker (make-marker) end))
  (goto-char start)
  (beginning-of-line)
  (setq start (point))
  (insert-before-markers fold-top-mark)
  (let ((saved-point (point)))
    (and fold-secondary-top-mark
	 (insert-before-markers fold-secondary-top-mark))
    (insert-before-markers ?\n)
    (goto-char (marker-position end))
    (set-marker end nil)
    (and (not (bolp))
	 (eq 0 (forward-line))
	 (eobp)
	 (insert ?\n))
    (insert fold-bottom-mark)
    (insert ?\n)
    (setq fold-stack (if fold-stack
			    (cons (cons (point-min-marker)
					(point-max-marker))
				  fold-stack)
			  '(folded)))
    (fold-narrow-to-region start (1- (point)))
    (goto-char saved-point)
    (fold-set-mode-line))
  (save-excursion (fold-tidy-inside)))

;;}}}
;;{{{ fold-tidy-inside

(defun fold-tidy-inside ()
  "Adds or removes blank lines at the top and bottom of the current fold.
Also adds fold marks at the top and bottom (after asking), if they are not
there already.  The amount of space left depends on the variable
`fold-internal-margins', which is one by default."
  (interactive)
  (if buffer-read-only nil
    (goto-char (point-min))
    (and (eolp)
	 (progn (skip-chars-forward "\n")
		(delete-region (point-min) (point))))
    (and (if (looking-at fold-top-regexp)
	     (progn (forward-line 1)
		    (and (eobp) (insert ?\n))
		    t)
	   (and (y-or-n-p "Insert missing fold-top-mark? ")
		(progn (insert (concat fold-top-mark
				       "<Replaced missing fold top mark>"
				       (or fold-secondary-top-mark "")
				       "\n"))
		       t)))
	 fold-internal-margins
	 (<= 0 fold-internal-margins)
	 (let ((temp (point)))
	   (skip-chars-forward "\n")
	   (if (< 0 (setq temp (+ (- temp (point)) fold-internal-margins)))
	       (while (<= 0 (setq temp (1- temp))) (insert ?\n))
	     (or (eq 0 temp)
		 (delete-region (+ (point) temp) (point))))))
    (goto-char (point-max))
    (and (bolp)
	 (progn (skip-chars-backward "\n")
		(delete-region (point) (point-max))))
    (beginning-of-line)
    (and (or (looking-at fold-bottom-regexp)
	     (progn (goto-char (point-max)) nil)
	     (and (y-or-n-p "Insert missing fold-bottom-mark? ")
		  (progn
		    (insert (concat "\n" fold-bottom-mark))
		    (beginning-of-line)
		    t)))
	 fold-internal-margins
	 (<= 0 fold-internal-margins)
	 (let ((temp (point)))
	   (skip-chars-backward "\n")
	   (if (<= 0 (setq temp (+ (- (point) temp) fold-internal-margins)))
	       (while (<= 0 temp)
		 (setq temp (1- temp))
		 (insert ?\n))
	     (or (eq -1 temp)
		 (delete-region (point) (- (point) (1+ temp)))))))))

;;}}}

;;}}}
;;{{{ Operations on the whole buffer

;;{{{ fold-whole-buffer

(defun fold-whole-buffer ()
  "Folds every fold in the current buffer.
Fails if the fold markers are not balanced correctly.

If the buffer is being viewed in a fold, folds are repeatedly exited to
get to the top level first (this allows the folds to be tidied on the
way out).  The buffer modification flag is not affected, and this
function will work on read-only buffers."

  (interactive)
  (message "Folding buffer...")
  (let ((narrow-min (point-min))
	(narrow-max (point-max))
	fold-list fold)
    (save-excursion
      (widen)
      (goto-char 1)
      (setq fold-list (fold-skip-folds nil t))
      (narrow-to-region narrow-min narrow-max)
      (and (eq t fold-list)
	   (error "Cannot fold whole buffer -- unmatched begin-fold mark"))
      (and (integerp (car fold-list))
	   (error "Cannot fold whole buffer -- extraneous end-fold mark"))
      (fold-top-level)
      (widen)
      (goto-char 1)
      ;; Do the modifications forwards.
      (fold-subst-regions (nreverse (cdr fold-list)) ?\n ?\r))
    (beginning-of-line)
    (fold-narrow-to-region nil nil t)
    (message "Folding buffer... done")))

;;}}}
;;{{{ fold-open-buffer

(defun fold-open-buffer ()
  "Unfolds the entire buffer, leaving the point where it is.
Does not affect the buffer-modified flag, and can be used on read-only
buffers."
  (interactive)
  (message "Unfolding buffer...")
  (fold-clear-stack)
  (fold-set-mode-line)
  (unwind-protect
      (progn
	(widen)
	(fold-subst-regions (list 1 (point-max)) ?\r ?\n))
    (fold-narrow-to-region nil nil t))
  (message "Unfolding buffer... done"))

;;}}}
;;{{{ fold-remove-folds

(defun fold-remove-folds (&optional buffer pre-title post-title pad)
  "Removes folds from a buffer, for printing.

It copies the contents of the (hopefully) folded buffer BUFFER into a
buffer called `*Unfolded: <Original-name>*', removing all of the fold
marks.  It keeps the titles of the folds, however, and numbers them.
Subfolds are numbered in the form 5.1, 5.2, 5.3 etc., and the titles are
indented to eleven characters.

It accepts four arguments.  BUFFER is the name of the buffer to be
operated on, or a buffer.  nil means use the current buffer.  PRE-TITLE
is the text to go before the replacement fold titles, POST-TITLE is the
text to go afterwards.  Finally, if PAD is non-nil, the titles are all
indented to the same column, which is eleven plus the length of
PRE-TITLE.  Otherwise just one space is placed between the number and
the title."
  (interactive (list (read-buffer "Remove folds from buffer: "
				  (buffer-name)
				  t)
		     (read-string "String to go before enumerated titles: ")
		     (read-string "String to go after enumerated titles: ")
		     (y-or-n-p "Pad section numbers with spaces? ")))
  (set-buffer (setq buffer (get-buffer buffer)))
  (setq pre-title (or pre-title "")
	post-title (or post-title ""))
  (or folding-mode
      (error "Must be in Folding mode before removing folds"))
  (let ((new-buffer (get-buffer-create (concat "*Unfolded: "
					       (buffer-name buffer)
					       "*")))
	(section-list '(1))
	(section-prefix-list '(""))
	title
	(secondary-mark-length (length fold-secondary-top-mark))
	(regexp fold-regexp)
	(secondary-mark fold-secondary-top-mark)
	prefix
	(mode major-mode))
    (buffer-flush-undo new-buffer)
    (save-excursion
      (set-buffer new-buffer)
      (delete-region (point-min)
		     (point-max)))
    (save-restriction
      (widen)
      (copy-to-buffer new-buffer (point-min) (point-max)))
    (display-buffer new-buffer t)
    (set-buffer new-buffer)
    (subst-char-in-region (point-min) (point-max) ?\r ?\n)
    (funcall mode)
    (while (re-search-forward regexp nil t)
      (if (match-beginning 4)
	  (progn
	    (goto-char (match-end 4))
	    (setq title
		  (buffer-substring (point)
				    (progn (end-of-line)
					   (point))))
	    (delete-region (save-excursion
			     (goto-char (match-beginning 4))
			     (skip-chars-backward "\n\r")
			     (point))
			   (progn
			     (skip-chars-forward "\n\r")
			     (point)))
	    (and (<= secondary-mark-length
		     (length title))
		 (string-equal secondary-mark
			       (substring title
					  (- secondary-mark-length)))
		 (setq title (substring title
					0
					(- secondary-mark-length))))
	    (setq section-prefix-list
		  (cons (setq prefix (concat (car section-prefix-list)
					     (int-to-string (car section-list))
					     "."))
			section-prefix-list))
	    (or (cdr section-list)
		(insert ?\n))
	    (setq section-list
		  (cons 1
			(cons (1+ (car section-list))
			      (cdr section-list))))
	    (setq title (concat prefix
				(if pad
				    (make-string
				     (max 2 (- 8 (length prefix))) ? )
				  " ")
				title))
	    (message "Reformatting: %s%s%s"
		     pre-title
		     title
		     post-title)
	    (insert "\n\n"
		    pre-title
		    title
		    post-title
		    "\n\n"))
	(goto-char (match-beginning 5))
	(or (setq section-list (cdr section-list))
	    (error "Too many bottom-of-fold marks"))
	(setq section-prefix-list (cdr section-prefix-list))
	(delete-region (point)
		       (progn
			 (forward-line 1)
			 (point)))))
    (and (cdr section-list)
	 (error
	  "Too many top-of-fold marks -- reached end of file prematurely"))
    (goto-char (point-min))
    (buffer-enable-undo)
    (set-buffer-modified-p nil)
    (message "All folds reformatted.")))

;;}}}

;;}}}
;;{{{ Standard fold marks for various major modes

;;{{{ A function to set default marks, `fold-add-to-marks-list'

(defun fold-add-to-marks-list (mode top bottom
				    &optional secondary noforce message)
  "Add/set fold marks for a particular major mode.
When called interactively, asks for a major-mode name, and for
fold marks to be used in that mode.  It adds the new set to
`fold-mode-marks-alist', and if the mode name is the same as the current
major mode for the current buffer, the marks in use are also changed.

If called non-interactively, arguments are MODE, TOP, BOTTOM and
SECONDARY.  MODE is the symbol for the major mode for which marks are
being set.  TOP, BOTTOM and SECONDARY are strings, the three fold marks
to be used.  SECONDARY may be nil (as opposed to the empty string), but
the other two must be non-empty strings, and is an optional argument.

Two other optional arguments are NOFORCE, meaning do not change the
marks if marks are already set for the specified mode if non-nil, and
MESSAGE, which causes a message to be displayed if it is non-nil.  This
is also the message displayed if the function is called interactively.

To set default fold marks for a particular mode, put something like the
following in your .emacs:

\(fold-add-to-marks-list 'major-mode \"(** {{{ \" \"(** }}} **)\" \" **)\")

Look at the variable `fold-mode-marks-alist' to see what default settings
already apply.

`fold-set-marks' can be used to set the fold marks in use in the current
buffer without affecting the default value for a particular mode."
  (interactive
   (let* ((mode (completing-read
		 (concat "Add fold marks for major mode ("
			 (symbol-name major-mode)
			 "): ")
		 obarray
		 (function
		  (lambda (arg)
		    (and (commandp arg)
			 (string-match "-mode\\'"
				       (symbol-name arg)))))
		 t))
	  (mode (if (equal mode "")
		    major-mode
		  (intern mode)))
	  (object (assq mode fold-mode-marks-alist))
	  (old-top (and object
		   (nth 1 object)))
	  top
	  (old-bottom (and object
		      (nth 2 object)))
	  bottom
	  (secondary (and object
			 (nth 3 object)))
	  (prompt "Top fold marker: "))
     (and (equal secondary "")
	  (setq secondary nil))
     (while (not top)
       (setq top (read-string prompt (or old-top "{{{ ")))
       (and (equal top "")
	    (setq top nil)))
     (setq prompt (concat prompt
			  top
			  ", Bottom marker: "))
     (while (not bottom)
       (setq bottom (read-string prompt (or old-bottom "}}}")))
       (and (equal bottom "")
	    (setq bottom nil)))
     (setq prompt (concat prompt
			  bottom
			  (if secondary
			      ", Secondary marker: "
			    ", Secondary marker (none): "))
	   secondary (read-string prompt secondary))
     (and (equal secondary "")
	  (setq secondary nil))
     (list mode top bottom secondary nil t)))
  (let ((object (assq mode fold-mode-marks-alist)))
    (if (and object
	     noforce
	     message)
	(message "Fold markers for `%s' are already set."
		 (symbol-name mode))
      (if object
	  (or noforce
	      (setcdr object (if secondary
				 (list top bottom secondary)
			       (list top bottom))))
	(setq fold-mode-marks-alist
	      (cons (if secondary
			(list mode top bottom secondary)
		      (list mode top bottom))
		    fold-mode-marks-alist)))
      (and message
	     (message "Set fold marks for `%s' to \"%s\" and \"%s\"."
		      (symbol-name mode)
		      (if secondary
			  (concat top "name" secondary)
			(concat top "name"))
		      bottom)
	     (and (eq major-mode mode)
		  (fold-set-marks top bottom secondary))))))

;;}}}
;;{{{ Set some useful default fold marks

(fold-add-to-marks-list 'c-mode "/* {{{ " "/* }}} */" " */" t)
(fold-add-to-marks-list 'emacs-lisp-mode ";;{{{ " ";;}}}" nil t)
(fold-add-to-marks-list 'lisp-interaction-mode ";;{{{ " ";;}}}" nil t)
(fold-add-to-marks-list 'plain-tex-mode "%{{{ " "%}}}" nil t)
(fold-add-to-marks-list 'plain-TeX-mode "%{{{ " "%}}}" nil t)
(fold-add-to-marks-list 'latex-mode "%{{{ " "%}}}" nil t)
(fold-add-to-marks-list 'LaTeX-mode "%{{{ " "%}}}" nil t)
(fold-add-to-marks-list 'orwell-mode "{{{ " "}}}" nil t)
(fold-add-to-marks-list 'fundamental-mode "{{{ " "}}}" nil t)
(fold-add-to-marks-list 'modula-2-mode "(* {{{ " "(* }}} *)" " *)" t)
(fold-add-to-marks-list 'shellscript-mode "# {{{ " "# }}}" nil t)
(fold-add-to-marks-list 'perl-mode "# {{{ " "# }}}" nil t)
(fold-add-to-marks-list 'texinfo-mode "@c {{{ " "@c {{{endfold}}}" " }}}" t)
(fold-add-to-marks-list 'occam-mode "-- {{{ " "-- }}}" nil t)
(fold-add-to-marks-list 'lisp-mode ";;{{{ " ";;}}}" nil t)
(fold-add-to-marks-list 'tex-mode "%{{{ " "%}}}" nil t)
(fold-add-to-marks-list 'TeX-mode "%{{{ " "%}}}" nil t)
(fold-add-to-marks-list 'c++-mode "// {{{ " "// }}}" nil t)
(fold-add-to-marks-list 'bison-mode "/* {{{ " "/* }}} */" " */" t)
(fold-add-to-marks-list 'Bison-mode "/* {{{ " "/* }}} */" " */" t)
(fold-add-to-marks-list 'gofer-mode "-- {{{ " "-- }}}" nil t)

;;}}}

;;}}}
;;{{{ Start Folding mode automatically for folded files

;;{{{ folding-mode-find-file-hook

(defun folding-mode-find-file-hook ()
  "One of the hooks called whenever a `find-file' is successful.
It checks to see if `folded-file' has been set as a buffer-local
variable, and automatically starts Folding mode if it has.

This allows folded files to be automatically folded when opened.

To make this hook effective, the symbol `folding-mode-find-file-hook'
should be placed at the end of `find-file-hooks'.  If you have
some other hook in the list, for example a hook to automatically
uncompress or decrypt a buffer, it should go earlier on in the list.

See also `folding-mode-add-find-file-hook'."
  (and (assq 'folded-file (buffer-local-variables))
       folded-file
       (folding-mode 1)
       (kill-local-variable 'folded-file)))

;;}}}
;;{{{ folding-mode-add-find-file-hook

(defun folding-mode-add-find-file-hook ()
  "Appends `folding-mode-find-file-hook' to the list `find-file-hooks'.

This has the effect that afterwards, when a folded file is visited, if
appropriate Emacs local variable entries are recognised at the end of
the file, Folding mode is started automatically.

If `inhibit-local-variables' is non-nil, this will not happen regardless
of the setting of `find-file-hooks'.

To declare a file to be folded, put `folded-file: t' in the file's
local variables.  eg., at the end of a C source file, put:

/*
Local variables:
folded-file: t
*/

The local variables can be inside a fold."
  (interactive)
  (or (memq 'folding-mode-find-file-hook find-file-hooks)
      (setq find-file-hooks (append find-file-hooks
				    '(folding-mode-find-file-hook)))))

;;}}}

;;}}}
;;{{{ Gross, crufty hacks that seem necessary

;; The functions here have been tested with Emacs 18.55, Emacs 18.58,
;; Epoch 4.0p2 (based on Emacs 18.58) and Lucid Emacs 19.6.

;; Note that Lucid Emacs 19.6 can't do selective-display, and its
;; "invisible extents" don't work either, so Folding mode just won't
;; work with that version.

;; They shouldn't do the wrong thing with later versions of Emacs, but
;; they might not have the special effects either.  They may appear to
;; be excessive; that is not the case.  All of the peculiar things these
;; functions do is done to avoid some side-effect of Emacs' internal
;; logic that I have met.  Some of them work around bugs or unfortunate
;; (lack of) features in Emacs.  In most cases, it would be better to
;; move this into the Emacs C code.

;; Folding mode is designed to be simple to cooperate with as many
;; things as possible.  These functions go against that principle at the
;; coding level, but make life for the user bearable.

;;{{{ fold-merge-keymaps

;; Merge keymaps, because miner-mode keymaps aren't available in Emacs
;; 18.  In Lucid Emacs, keymaps can have parent keymaps, so that
;; mechanism is used instead and MAP isn't copied.

;; Takes two keymaps, MAP and EXTRA.  Merges each binding in EXTRA into
;; a copy of MAP, and returns the new keymap (bindings in EXTRA override
;; those in MAP).  MAP or EXTRA may be nil, indicating an empty keymap.
;; If they are both nil, nil is returned.  Sub-keymaps and even cons
;; cells containing bindings are not copied unnecessarily (well,
;; sometimes they are).  This means that if you modify the local map
;; when Folding mode is active, the effects are unpredictable: you may
;; also affect the keymap that was active before Folding mdoe was
;; started, and you may affect folding-mode-map.

(defun fold-merge-keymaps (map extra)
  (or map (setq map extra extra nil))
  (if (null extra)
      (and map (copy-keymap map))
    (if fold-lucid-keymaps-p
	(let ((new (copy-keymap extra)))
	  (set-keymap-parent new map)
	  new)
      (or (keymapp extra)
	  (signal 'wrong-type-argument (list 'keymapp extra)))
      (or (keymapp map)
	  (signal 'wrong-type-argument (list 'keymapp map)))
      (and (vectorp extra)
	   (let ((key (length extra))
		 (oldextra extra))
	     (setq extra nil)
	     (while (<= 0 (setq key (1- key)))
	       (and (aref oldextra key)
		    (setq extra (cons (cons key (aref oldextra key)) extra))))
	     (setq extra (cons 'keymap extra))))
      (and (cdr extra)
	   (let (key keycode cons-binding realdef def submap)

	     ;; Note that this copy-sequence will copy the spine of the
	     ;; sparse keymap, but it will not copy the cons cell used
	     ;; for each binding.  This is important; define-key does a
	     ;; setcdr to rebind a key, if that key was bound already,
	     ;; so define-key can't be used to change a binding.  Using
	     ;; copy-keymap instead would be excessive and slow, because
	     ;; it would be repeatedly invoked, as this function is
	     ;; called recursively.

	     (setq map (copy-sequence map))
	     (while (setq extra (cdr extra))
	       (setq keycode (car (car extra))
		     key (char-to-string keycode)
		     def (cdr (car extra))
		     realdef def)
	       (while (and def (if (symbolp def)
				   (setq def (symbol-function def))
				 (and (consp def)
				      (integerp (cdr def))
				      (keymapp (car def))
				      (setq def (lookup-key (car def)
							    (char-to-string
							     (cdr def))))))))
	       (if (and (keymapp def)
			(setq submap (lookup-key map key)))
		   (progn
		     (while (and submap
				 (if (symbolp submap)
				     (setq submap (symbol-function submap))
				   (and (consp submap)
					(integerp (cdr submap))
					(keymapp (car submap))
					(setq submap (lookup-key
						      (car submap)
						      (char-to-string
						       (cdr submap))))))))
		     (if (keymapp submap)
			 (if (vectorp map)
			     (aset map keycode
				   (fold-merge-keymaps submap def))
			   (setcdr (setq map (delq (assq keycode map) map))
				   (cons (cons keycode
					       (fold-merge-keymaps submap def))
					 (cdr map))))
		       (if (vectorp map)
			   (aset map keycode realdef)
			 (setcdr (setq map (delq (assq keycode map) map))
				 (cons (cons keycode realdef) (cdr map))))))
		 (and def
		      (if (vectorp map)
			  (aset map keycode realdef)
			(and (setq cons-binding (assq keycode map))
			     (setq map (delq cons-binding map)))
			(setcdr map (cons (cons keycode realdef)
					  (cdr map)))))))))
      map)))

;;}}}
;;{{{ fold-subst-regions

;; Substitute newlines for carriage returns or vice versa.
;; Avoid excessive file locking.

;; Substitutes characters in the buffer, even in a read-only buffer.
;; Takes LIST, a list of regions specified as sequence in the form
;; (START1 END1 START2 END2 ...).  In every region specified by each
;; pair, substitutes each occurence of character FIND by REPLACE.

;; The buffer-modified flag is not affected, undo information is not
;; kept for the change, and the function works on read-only files.  This
;; function is much more efficient called with a long sequence than
;; called for each region in the sequence.

;; If the buffer is not modified when the function is called, the
;; modified-flag is set before performing all the substitutions, and
;; locking is temporarily disabled.  This prevents Emacs from trying to
;; make then delete a lock file for *every* substitution, which slows
;; folding considerably, especially on a slow networked filesystem.
;; Without this, on my system, folding files on startup (and reading
;; other peoples' folded files) takes about five times longer.  Emacs
;; still locks the file once for this call under those circumstances; I
;; can't think of a way around that, but it isn't really a problem.

;; I consider these problems to be a bug in `subst-char-in-region'.

(defun fold-subst-regions (list find replace)
  (let ((buffer-read-only buffer-read-only) ;; Protect read-only flag.
	(modified (buffer-modified-p))
	(ask1 (symbol-function 'ask-user-about-supersession-threat))
	(ask2 (symbol-function 'ask-user-about-lock)))
    (unwind-protect
	(progn
	  (setq buffer-read-only nil)
	  (or modified
	      (progn
		(fset 'ask-user-about-supersession-threat
		      '(lambda (&rest x) nil))
		(fset 'ask-user-about-lock
		      '(lambda (&rest x) nil))
		(set-buffer-modified-p t))) ; Prevent file locking in the loop
	  (while list
	    (subst-char-in-region (car list) (nth 1 list) find replace t)
	    (setq list (cdr (cdr list)))))
      ;; buffer-read-only is restored by the let.
      ;; Don't want to change MODIFF time if it was modified before.
      (or modified
	  (unwind-protect
	      (set-buffer-modified-p nil)
	    (fset 'ask-user-about-supersession-threat ask1)
	    (fset 'ask-user-about-lock ask2))))))

;;}}}
;;{{{ fold-narrow-to-region

;; Narrow to region, without surprising displays.

;; Similar to `narrow-to-region', but also adjusts window-start to be
;; the start of the narrowed region.  If an optional argument CENTRE is
;; non-nil, the window-start is positioned to leave the point at the
;; centre of the window, like `recenter'.  START may be nil, in which
;; case the function acts more like `widen'.

;; Actually, all the window-starts for every window displaying the
;; buffer, as well as the last_window_start for the buffer are set.  The
;; points in every window are set to the point in the current buffer.
;; All this logic is necessary to prevent the display getting really
;; weird occasionally, even if there is only one window.  Try making
;; this function like normal `narrow-to-region' with a touch of
;; `recenter', then moving around lots of folds in a buffer displayed in
;; several windows.  You'll see what I mean.

;; last_window_start is set by making sure that the selected window is
;; displaying the current buffer, then setting the window-start, then
;; making the selected window display another buffer (which sets
;; last_window_start), then setting the selected window to redisplay the
;; buffer it displayed originally.

;; Note that whenever window-start is set, the point cannot be moved
;; outside the displayed area until after a proper redisplay.  If this
;; is possible, centre the display on the point.

;; In Emacs 19; Epoch or Lucid Emacs, searches all screens for all
;; windows.  In Emacs 19, they are called "frames".

(defun fold-narrow-to-region (&optional start end centre)
  (let* ((the-window (selected-window))
	 (the-screen (and fold-epoch-screens-p (epoch::current-screen)))
	 (screens (and fold-epoch-screens-p (epoch::screens-of-buffer)))
	 (selected-buffer (window-buffer the-window))
	 (window-ring the-window)
	 (window the-window)
	 (point (point))
	 (buffer (current-buffer))
	 temp)
    (unwind-protect
	(progn
	  (unwind-protect
	      (progn
		(if start
		    (narrow-to-region start end)
		  (widen))
		(setq point (point))
		(set-window-buffer window buffer)
		(while (progn
			 (and (eq buffer (window-buffer window))
			      (if centre
				  (progn
				    (select-window window)
				    (goto-char point)
				    (vertical-motion
				     (- (lsh (window-height window) -1)))
				    (set-window-start window (point))
				    (set-window-point window point))
				(set-window-start window (or start 1))
				(set-window-point window point)))
			 (or (not (eq (setq window
					    (if fold-emacs-frames-p
						(next-window window nil t)
					      (if fold-lucid-screens-p
						  (next-window window nil t t)
						(next-window window))))
				      window-ring))
			     (and (setq screens (cdr screens))
				  (setq window (epoch::first-window screen)
					window-ring window))))))
	    (and the-screen (epoch::select-screen the-screen))
	    (select-window the-window))
	  ;; Set last_window_start.
	  (unwind-protect
	      (if (not (eq buffer selected-buffer))
		  (set-window-buffer the-window selected-buffer)
		(if (get-buffer "*scratch*")
		    (set-window-buffer the-window (get-buffer "*scratch*"))
		  (set-window-buffer
		   the-window (setq temp (generate-new-buffer " *temp*"))))
		(set-window-buffer the-window buffer))
	    (and temp
		 (kill-buffer temp))))
      ;; Undo this side-effect of set-window-buffer.
      (set-buffer buffer)
      (goto-char (point)))))

;;}}}

;;}}}
;;{{{ Miscellaneous

;;{{{ kill-all-local-variables-hooks

;; This does not normally have any effect in Emacs.  In my setup,
;; this hook is called when the major mode changes, and it gives
;; Folding mode a chance to clear up first.

(and (boundp 'kill-all-local-variables-hooks)
     (or (memq 'fold-end-mode-quickly
	       kill-all-local-variables-hooks)
	 (setq kill-all-local-variables-hooks
	       (cons 'fold-end-mode-quickly
		     kill-all-local-variables-hooks))))

;;}}}
;;{{{ list-buffers-mode-alist

;; Also has no effect in standard Emacs.  With this variable set,
;; my setup shows "Folding" in the mode name part of the buffer list,
;; which looks nice :-).

(and (boundp 'list-buffers-mode-alist)
     (or (assq 'folding-mode list-buffers-mode-alist)
	 (setq list-buffers-mode-alist
	       (cons '(folding-mode "Folding")
		     list-buffers-mode-alist))))

;;}}}
;;{{{ fold-end-mode-quickly

(defun fold-end-mode-quickly ()
  "Replaces all ^M's with linefeeds and widen a folded buffer.
Only has any effect if Folding mode is active.

This should not in general be used for anything.  It is used when changing
major modes, by being placed in kill-mode-tidy-alist, to tidy the buffer
slightly.  It is similar to `(folding-mode 0)', except that it does not
restore saved keymaps etc.  Repeat: Do not use this function.  Its
behaviour is liable to change."
  (and (boundp 'folding-mode)
       (assq 'folding-mode
	     (buffer-local-variables))
       folding-mode
       (progn
	 (widen)
	 (fold-clear-stack)
	 (fold-subst-regions (list 1 (point-max)) ?\r ?\n))))

;;}}}
;;{{{ eval-current-buffer-open-folds

(defun eval-current-buffer-open-folds (&optional printflag)
  "Evaluate all of a folded buffer as Lisp code.
Unlike `eval-current-buffer', this function will evaluate all of a
buffer, even if it is folded.  It will also work correctly on non-folded
buffers, so is a good candidate for being bound to a key if you program
in Emacs-Lisp.

It works by making a copy of the current buffer in another buffer,
unfolding it and evaluating it.  It then deletes the copy.

Programs can pass argument PRINTFLAG which controls printing of output:
nil means discard it; anything else is stream for print."
  (interactive)
  (if (or (and (boundp 'folding-mode-flag)
	       folding-mode-flag)
	  (and (boundp 'folding-mode)
	       folding-mode))
      (let ((temp-buffer
	     (generate-new-buffer (buffer-name))))
	(message "Evaluating unfolded buffer...")
	(save-restriction
	  (widen)
	  (copy-to-buffer temp-buffer 1 (point-max)))
	(set-buffer temp-buffer)
	(subst-char-in-region 1 (point-max) ?\r ?\n)
	(let ((real-message-def (symbol-function 'message))
	      (suppress-eval-message))
	  (fset 'message
		(function
		 (lambda (&rest args)
		   (setq suppress-eval-message t)
		   (fset 'message real-message-def)
		   (apply 'message args))))
	  (unwind-protect
	      (eval-current-buffer printflag)
	    (fset 'message real-message-def)
	    (kill-buffer temp-buffer))
	  (or suppress-eval-message
	      (message "Evaluating unfolded buffer... Done"))))
    (eval-current-buffer printflag)))

;;}}}

;;}}}

;;{{{ Emacs local variables

;; Local variables:
;; folded-file: t
;; end:

;;}}}


