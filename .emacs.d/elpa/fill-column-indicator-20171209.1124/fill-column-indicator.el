;;; fill-column-indicator.el --- Graphically indicate the fill column

;; Copyright (c) 2011-2017 Alp Aker

;; Author: Alp Aker <alp.tekin.aker@gmail.com>
;; Version: 1.90
;; Package-Version: 20171209.1124
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; A copy of the GNU General Public License can be obtained from the
;; Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;; Many modern editors and IDEs can graphically indicate the location of the
;; fill column by drawing a thin line (in design parlance, a `rule') down the
;; length of the editing window.  Fill-column-indicator implements this
;; facility in Emacs.

;; Installation and Usage
;; ======================

;; Put this file in your load path and put:
;;
;;   (require 'fill-column-indicator)
;;
;; in your init file.

;; To toggle graphical indication of the fill column in a buffer, use the
;; command `fci-mode'.

;; Configuration
;; =============

;; By default, fci-mode draws its vertical indicator at the fill column.  If
;; you'd like it to be drawn at another column, set `fci-rule-column' to the
;; column number.  (A case in which this might be useful is when you want to
;; fill comments at, for example, column 70, but want a vertical rule at
;; column 80 or 100 to indicate the maximum line length for code.)  The
;; default behavior (showing the indicator at the fill column) is specified
;; by setting fci-rule-column to nil.

;; On graphical displays the fill-column rule is drawn using a bitmap
;; image.  Its color is controlled by the variable `fci-rule-color', whose
;; value can be any valid color name.  The rule's width in pixels is
;; determined by the variable `fci-rule-width'; the default value is 1.

;; The rule can be drawn as a solid or dashed line, controlled by the
;; variable `fci-rule-use-dashes'; the default is nil.  The dash appearance is
;; controlled by `fci-dash-pattern', which is the ratio of dash length to
;; line height; the default is 0.75. (The value should be a number between 0
;; and 1; values outside that interval are coerced to the nearest endpoint.)

;; The image formats fci-mode can use are XPM and PBM.  If Emacs has been
;; compiled with the appropriate library it uses XPM images by default; if
;; not it uses PBM images, which are natively supported.  You can specify a
;; particular choice of format by setting `fci-rule-image-format' explicitly
;; to xpm or pbm.

;; On character terminals the rule is drawn using the character specified by
;; `fci-rule-character'; the default is `|' (ascii 124).  If
;; `fci-rule-character-color' is nil, then it is drawn using fci-rule-color
;; (or the closest approximation thereto that the terminal is capable of); if
;; it is a color name, then that color is used instead.

;; If you'd like the rule to be drawn using fci-rule-character even on
;; graphical displays, set `fci-always-use-textual-rule' to a non-nil value.

;; These variables (as well as those described in the next section) can be
;; given buffer-local bindings.

;; Other Options
;; =============

;; When `truncate-lines' is nil, the effect of drawing a fill-column rule is
;; very odd looking. Indeed, it makes little sense to use a rule to indicate
;; the position of the fill column in that case (the positions at which the
;; fill column falls in the visual display space won't, in general, be
;; collinear).  For this reason, fci-mode sets truncate-lines to t in buffers
;; in which it is enabled and restores it to its previous value when
;; disabled.  You can turn this feature off by setting
;; `fci-handle-truncate-lines' to nil.

;; If `line-move-visual' is t, then vertical navigation can behave oddly in
;; several edge cases while fci-mode is enabled (this is due to a bug in
;; Emacs's C code).  Accordingly, fci-mode sets line-move-visual to nil in
;; buffers in which it is enabled and restores it to its previous value when
;; disabled.  This can be suppressed by setting `fci-handle-line-move-visual'
;; to nil.  (But you shouldn't want to do this.  There's no reason to use
;; line-move-visual if truncate-lines is t, and it doesn't make sense to use
;; something like fci-mode when truncate-lines is nil.)

;; Fci-mode needs free use of two characters (specifically, it needs the use
;; of two characters whose display table entries it can change
;; arbitrarily).  Its defualt is to use the first two characters of the
;; Private Use Area of the Unicode BMP, viz. U+E000 and U+E001.  If you need
;; to use those characters for some other purpose, set `fci-eol-char' and
;; `fci-blank-char' to different values.

;; Troubleshooting
;; ===============

;; o Fci-mode is intended to be used with monospaced fonts.  If you're using
;;   a monospaced font and the fill-column rule is missing or misaligned on a
;;   few lines but otherwise appears normal, then most likely (a) there are
;;   non-ascii characters on those lines that are being displayed using a
;;   non-monospaced font, or (b) your font-lock settings use bold or italics
;;   and those font variants aren't monospaced.

;; o Fci-mode in not currently compatible with Emacs's
;;   `show-trailing-whitespace' feature (given the way the latter is
;;   implemented, such compatibility is going to be hard to achieve).  A
;;   workaround is to configure `whitespace-mode' to replicate the
;;   functionality of show-trailing-whitespace.  This can be done with the
;;   following setting:
;;
;;     (setq whitespace-style '(face trailing))
;;
;;  With this, whitespace-mode produces the same basic effect as a non-nil
;;  value of show-trailing-whitespace, and compatibility with fci-mode is not
;;  a problem.

;; Known Issues
;; ============

;; o The indicator extends only to end of the buffer contents (as opposed to
;;   running the full length of the editing window).

;; o When portions of a buffer are invisible, such as when outline-mode is
;;   used to hide certain lines, the fill-column rule is hidden as well.

;; o Fci-mode should work smoothly when simultaneously displaying the same
;;   buffer on both a graphical display and on a character terminal.  It does
;;   not currently support simultaneous display of the same buffer on window
;;   frames with different default font sizes. (It would be feasible to
;;   support this use case, but thus far there seems to be no demand for
;;   it.)

;; Todo
;; ====

;; o Accommodate non-nil values of `hl-line-sticky-flag' and similar cases.

;; o Accommodate linum-mode more robustly.

;; o Compatibility with non-nil `show-trailing-whitespace.'

;; Acknowledgements
;; ================

;; Thanks to Ami Fischman, Christopher Genovese, Michael Hoffman, José
;; Alfredo Romero L., R. Lange, Joe Lisee, José Lombera, Frank Meffert,
;; Mitchell Peabody, sheijk, and an anonymous BT subscriber for bug reports
;; and suggestions.  Special thanks for code contributions: lomew, John Lamp,
;; Sean Perry, David Röthlisberger, Pär Wieslander.

;;; Code:

(unless (version<= "25" emacs-version)
  (error "Fill-column-indicator requires version 25 or later"))

;;; ---------------------------------------------------------------------
;;; User Options
;;; ---------------------------------------------------------------------

(defgroup fill-column-indicator nil
  "Graphically indicate the fill-column."
  :tag "Fill-Column Indicator"
  :group 'convenience
  :group 'fill)

;; We should be using :validate instead of :match, but that seems not to
;; work with defcustom widgets.
(defcustom fci-rule-column nil
  "Controls where fci-mode displays a vertical line (rule).

If nil, the rule is drawn at the fill column.  Otherwise, it is
drawn at the column given by this variable.

Changes to this variable do not take effect until the mode
function `fci-mode' is run."
  :group 'fill-column-indicator
  :tag "Fill-Column rule column"
  :type '(choice (const :tag "Use the fill column" nil)
                 (integer :tag "Use a custom column"
                          :match (lambda (w val) (fci-posint-p val)))))

(defcustom fci-rule-color "#cccccc"
  "Color used to draw the fill-column rule.

Changes to this variable do not take effect until the mode
function `fci-mode' is run."
  :group 'fill-column-indicator
  :tag "Fill-column rule color"
  :type 'color)

(defcustom fci-rule-width 1
  "Width in pixels of the fill-column rule on graphical displays.
Note that a value greater than the default character width is
treated as equivalent to the default character width.

Changes to this variable do not take effect until the mode
function `fci-mode' is run."
  :tag "Fill-Column Rule Width"
  :group 'fill-column-indicator
  :type  '(integer :match (lambda (w val) (fci-posint-p val))))

(defcustom fci-rule-image-format
  (if (image-type-available-p 'xpm) 'xpm 'pbm)
  "Image format used for the fill-column rule on graphical displays.

Changes to this variable do not take effect until the mode
function `fci-mode' is run."
  :tag "Fill-Column Rule Image Format"
  :group 'fill-column-indicator
  :type '(choice (symbol :tag "XPM" 'xpm)
                 (symbol :tag "PBM" 'pbm)))

(defcustom fci-rule-use-dashes nil
  "Whether to show the fill-column rule as dashes or as a solid line.
This has no effect on non-graphical displays.

Changes to this variable do not take effect until the mode
function `fci-mode' is run."
  :tag "Fill-Column Rule Use Dashes"
  :group 'fill-column-indicator
  :type 'boolean)

(defcustom fci-dash-pattern 0.75
  "When using a dashed rule, ratio of dash length to line height.
Values less than 0 or greather than 1 are coerced to the nearest
endpoint of that interval.

Changes to this variable do not take effect until the mode
function `fci-mode' is run."
  :tag "Fill-Column Rule Use Dashes"
  :group 'fill-column-indicator
  :type 'float)

(defcustom fci-rule-character ?|
  "Character used to draw the fill-column rule on character terminals.

Changes to this variable do not take effect until the mode
function `fci-mode' is run."
  :tag "Fill-Column Rule Character"
  :group 'fill-column-indicator
  :type 'character)

(defcustom fci-rule-character-color nil
  "Color used to draw the fill-column rule on character terminals.
If nil, the same color is used as for the graphical rule.

Changes to this variable do not take effect until the mode
function `fci-mode' is run."
  :group 'fill-column-indicator
  :tag "Fill-column rule color"
  :type '(choice (const :tag "Use same color as graphical rule" nil)
                 (color :tag "Specify a color")))

(defcustom fci-always-use-textual-rule nil
  "When non-nil, the rule is always drawn using textual characters.
Specifically, fci-mode will use `fci-rule-character' instead of
bitmap images to draw the rule on graphical displays.

Changes to this variable do not take effect until the mode
function `fci-mode' is run."
  :tag "Don't Use Image for Fill-Column Rule"
  :group 'fill-column-indicator
  :type 'boolean)

(defcustom fci-handle-truncate-lines t
  "Whether fci-mode should set truncate-lines to t while enabled.
If non-nil, fci-mode will set truncate-lines to t in buffers in
which it is enabled, and restore it to its previous value when
disabled.

Leaving this option set to the default value is recommended."
  :group 'fill-column-indicator
  :tag "Locally set truncate-lines to t during fci-mode"
  :type 'boolean)

(defcustom fci-handle-line-move-visual t
  "Whether fci-mode should set line-move-visual to nil while enabled.
If non-nil, fci-mode will set line-move-visual to nil in buffers
in which it is enabled, and restore t to its previous value when
disabled.

Leaving this option set to the default value is recommended."
  :group 'fill-column-indicator
  :tag "Locally set line-move-visual to nil during fci-mode"
  :type 'boolean)

(defcustom fci-eol-char ?\uE000
  "Character used for internal purposes by fci-mode.
If you need to use this character, set this variable's value to a
character you do not care about (a good choice is a character
from the Private Use Area of the Unicode BMP, i.e., the range
U+E000-U+F8FF, inclusive)."
  :group 'fill-column-indicator
  :type 'character)

(defcustom fci-blank-char ?\uE001
  "Character used for internal purposes by fci-mode.
If you need to use this character, set this variable's value to a
character you do not care about (a good choice is a character
from the Private Use Area of the Unicode BMP, i.e., the the range
U+E000-U+F8FF, inclusive)."
  :group 'fill-column-indicator
  :type 'character)

;;; ---------------------------------------------------------------------
;;; Internal Variables and Constants
;;; ---------------------------------------------------------------------

;; Record prior state of buffer.
(defvar fci-saved-line-move-visual)
(defvar fci-line-move-visual-was-buffer-local)
(defvar fci-saved-truncate-lines)
(defvar fci-saved-eol)
(defvar fci-made-display-table)

;; Record state of fci initialization in this buffer.
(defvar fci-display-table-processed)
(defvar fci-local-vars-set)

;; Record current state of some quantities, so we can detect changes to them.
(defvar fci-column)
(defvar fci-newline)
(defvar fci-tab-width)
(defvar fci-char-width)
(defvar fci-char-height)

;; Data used in setting the fill-column rule that only need to be
;; occasionally updated in a given buffer.
(defvar fci-limit)
(defvar fci-pre-limit-string)
(defvar fci-at-limit-string)
(defvar fci-post-limit-string)

;; The preceding internal variables need to be buffer local and reset when
;; the mode is disabled.
(defconst fci-internal-vars '(fci-saved-line-move-visual
                              fci-line-move-visual-was-buffer-local
                              fci-saved-truncate-lines
                              fci-saved-eol
                              fci-made-display-table
                              fci-display-table-processed
                              fci-local-vars-set
                              fci-column
                              fci-newline
                              fci-tab-width
                              fci-char-width
                              fci-char-height
                              fci-limit
                              fci-pre-limit-string
                              fci-at-limit-string
                              fci-post-limit-string))

(dolist (var fci-internal-vars)
  (make-variable-buffer-local var))

;; Hooks we use.
(defconst fci-hook-assignments
  '((after-change-functions fci-redraw-region t t)
    (before-change-functions fci-extend-rule-for-deletion nil t)
    (window-scroll-functions fci-update-window-for-scroll nil t)
    (window-configuration-change-hook  fci-redraw-frame)
    (post-command-hook  fci-post-command-check nil t)
    (change-major-mode-hook turn-off-fci-mode nil t)
    (longlines-mode-hook fci-update-all-windows nil t)))

;;; ---------------------------------------------------------------------
;;; Miscellany
;;; ---------------------------------------------------------------------

(defun fci-get-buffer-windows (&optional all-frames)
  "Return a list of windows displaying the current buffer."
  (get-buffer-window-list (current-buffer) 'no-minibuf all-frames))

(defun fci-posint-p (x)
  "Return true if X is an integer greater than zero."
  (and (wholenump x)
       (/= 0 x)))

;;; ---------------------------------------------------------------------
;;; Mode Definition
;;; ---------------------------------------------------------------------

;;;###autoload
(define-minor-mode fci-mode
  "Toggle fci-mode on and off.
Fci-mode indicates the location of the fill column by drawing a
thin line (a `rule') at the fill column.

With prefix ARG, turn fci-mode on if and only if ARG is positive.

The following options control the appearance of the fill-column
rule: `fci-rule-column', `fci-rule-width', `fci-rule-color',
`fci-rule-use-dashes', `fci-dash-pattern', `fci-rule-character',
and `fci-rule-character-color'.  For further options, see the
Customization menu or the package file.  (See the latter for tips
on troubleshooting.)"

  nil nil nil

  (if fci-mode
      ;; Enabling.
      (condition-case error
          (progn
            (fci-check-user-options)
            (fci-process-display-table)
            (fci-set-local-vars)
            (fci-get-frame-dimens)
            (dolist (hook fci-hook-assignments)
              (apply 'add-hook hook))
            (setq fci-column (or fci-rule-column fill-column)
                  fci-tab-width tab-width
                  fci-limit (if fci-newline
                                (1+ (- fci-column (length fci-saved-eol)))
                              fci-column))
            (fci-make-overlay-strings)
            (fci-update-all-windows t))
        (error
         (fci-mode 0)
         (signal (car error) (cdr error))))
    ;; Disabling.
    (fci-restore-display-table)
    (fci-restore-local-vars)
    (dolist (hook fci-hook-assignments)
      (remove-hook (car hook) (nth 1 hook) (nth 3 hook)))
    (fci-delete-overlays-buffer)
    (dolist (var fci-internal-vars)
      (set var nil))))

;;;###autoload
(defun turn-on-fci-mode ()
  "Turn on fci-mode unconditionally."
  (interactive)
  (fci-mode 1))

(defun turn-off-fci-mode ()
  "Turn off fci-mode unconditionally."
  (interactive)
  (fci-mode 0))

;;; ---------------------------------------------------------------------
;;; Display Property Specs
;;; ---------------------------------------------------------------------

(defun fci-overlay-fills-background-p (olay)
  "Return true if OLAY specifies a background color."
  (let ((olay-face (overlay-get olay 'face)))
    (when olay-face
      (if (facep olay-face)
          (not (eq (face-attribute olay-face :background nil t) 'unspecified))
        (if (consp olay-face)
            (if (listp (cdr olay-face))
                (if (facep (car olay-face))
                    (not (memq t (mapcar #'(lambda (f) (eq (face-attribute f :background nil t) 'unspecified))
                                         olay-face)))
                  (plist-member olay-face :background))
              (eq (car olay-face) 'background-color)))))))

(defun fci-competing-overlay-p (posn)
  "Return true if there is an overlay at POSN that fills the background."
  (memq t (mapcar #'fci-overlay-fills-background-p (overlays-at posn))))

;; The display spec used in overlay before strings to pad out the rule to the
;; fill-column.
(defconst fci-padding-display
  '((when (not (fci-competing-overlay-p buffer-position))
      . (space :align-to fci-column))
    (space :width 0)))

;; Generate the display spec for the rule.  Basic idea is to use a "cascading
;; display property" to display the textual rule if the display doesn't
;; support images and the graphical rule if it does, but in either case only
;; display a rule if no other overlay wants to fill the background at the
;; relevant buffer position.
(defun fci-rule-display (blank rule-img rule-str for-pre-string)
  "Generate a display specification for a fill-column rule overlay string."
  (let* ((cursor-prop (if (and (not for-pre-string) (not fci-newline)) t))
         (propertized-rule-str (propertize rule-str 'cursor cursor-prop))
         (display-prop (if rule-img
                           `((when (not (or (display-images-p)
                                            (fci-competing-overlay-p buffer-position)))
                               . ,propertized-rule-str)
                             (when (not (fci-competing-overlay-p buffer-position))
                               . ,rule-img)
                             (space :width 0))
                         `((when (not (fci-competing-overlay-p buffer-position))
                             . ,propertized-rule-str)
                           (space :width 0)))))
    (propertize blank 'cursor cursor-prop 'display display-prop)))

;;; ---------------------------------------------------------------------
;;; Enabling
;;; ---------------------------------------------------------------------

(defun fci-check-user-options ()
  "Check that all user options for fci-mode have valid values."
  (unless (memq fci-rule-image-format '(xpm pbm))
    (error "Unrecognized value of `fci-rule-image-format'"))
  ;; If the third element of a binding form is t, then nil is an acceptable
  ;; value for the variable; otherwise, the variable value must satisfy the
  ;; given predicate.
  (let ((checks '((fci-rule-color color-defined-p)
                  (fci-rule-column fci-posint-p t)
                  (fci-rule-width fci-posint-p t)
                  (fci-rule-character-color color-defined-p t)
                  (fci-rule-character characterp)
                  (fci-blank-char characterp)
                  (fci-dash-pattern floatp)
                  (fci-eol-char characterp))))
    (dolist (check checks)
      (let ((value (symbol-value (nth 0 check)))
            (pred (nth 1 check))
            (nil-is-ok (nth 2 check)))
        (unless (or (and nil-is-ok (null value))
                    (funcall pred value))
          (signal 'wrong-type-argument (list pred value)))))))

(defun fci-process-display-table ()
  "Set up a buffer-local display table for fci-mode."
  (unless fci-display-table-processed
    (unless buffer-display-table
      (setq buffer-display-table (make-display-table)
            fci-made-display-table t))
    (aset buffer-display-table fci-blank-char [32])
    (setq fci-saved-eol (aref buffer-display-table 10))
    ;; Assumption: the display-table entry for character 10 is either nil or
    ;; a vector whose last element is the newline glyph.
    (let ((glyphs (butlast (append fci-saved-eol nil)))
          eol)
      (if glyphs
          (setq fci-newline [10]
                eol (vconcat glyphs))
        (setq fci-newline nil
              eol [32]))
      (aset buffer-display-table 10 fci-newline)
      (aset buffer-display-table fci-eol-char eol))
    (setq fci-display-table-processed t)))

(defun fci-set-local-vars ()
  "Set miscellaneous local variables when fci-mode is enabled."
  (unless fci-local-vars-set
    (when (and fci-handle-line-move-visual
               (boundp 'line-move-visual))
      (if (local-variable-p 'line-move-visual)
          (setq fci-line-move-visual-was-buffer-local t
                fci-saved-line-move-visual line-move-visual
                line-move-visual nil)
        (set (make-local-variable 'line-move-visual) nil)))
    (when fci-handle-truncate-lines
      (setq fci-saved-truncate-lines truncate-lines
            truncate-lines t))
    (setq fci-local-vars-set t)))

(defun fci-make-rule-string ()
  "Return a string for drawing the fill-column rule."
  (let ((color (or fci-rule-character-color
                   fci-rule-color)))
    ;; Make sure we don't inherit weight or slant from font-lock.
    (propertize (char-to-string fci-rule-character)
                'face `(:foreground ,color :weight normal :slant normal))))

(defun fci-make-img-descriptor ()
  "Make an image descriptor for the fill-column rule."
  (unless (or (= 0 fci-char-width)
              fci-always-use-textual-rule)
    ;; No point passing width, height, color etc. directly to the image
    ;; functions: those variables have either global or buffer-local
    ;; scope, so the image-generating functions can access them directly.
    (if (eq fci-rule-image-format 'xpm)
        (fci-make-xpm-img)
      (fci-make-pbm-img))))

(defun fci-get-frame-dimens ()
  "Determine the frame character height and width.

If the selected frame cannot display images, use the character
height and width of the first graphic frame in the frame list
displaying the current buffer.  (This fallback behavior is just a
rough heuristic.)"
  (let ((frame (catch 'found-graphic
                 (if (display-images-p)
                     (selected-frame)
                   (dolist (win (fci-get-buffer-windows t))
                     (when (display-images-p (window-frame win))
                       (throw 'found-graphic (window-frame win))))))))
    (setq fci-char-width (frame-char-width frame)
          fci-char-height (frame-char-height frame))))

(defmacro fci-with-rule-parameters (&rest body)
  "Define various quantites used in generating rule image descriptors."
  (declare (indent defun))
  `(let* ((height-str (number-to-string fci-char-height))
          (width-str (number-to-string fci-char-width))
          (rule-width (min fci-rule-width fci-char-width))
          (hmargin (/ (- fci-char-width rule-width) 2.0))
          (left-margin (floor hmargin))
          (right-margin (ceiling hmargin))
          (segment-ratio (if fci-rule-use-dashes fci-dash-pattern 1))
          (segment-ratio-coerced (min 1 (max 0 segment-ratio)))
          (segment-length (round (* segment-ratio-coerced fci-char-height)))
          (vmargin (/ (- fci-char-height segment-length) 2.0))
          (top-margin (floor vmargin))
          (bottom-margin (ceiling vmargin)))
     ,@body))

(defun fci-mapconcat (sep &rest lists)
  "Concatenate the strings in LISTS, using SEP as separator."
  (mapconcat #'identity (apply 'nconc lists) sep))

(defun fci-make-pbm-img ()
  "Return an image descriptor for the fill-column rule in PBM format."
  (fci-with-rule-parameters
    (let* ((magic-number "P1\n")
           (dimens (concat width-str " " height-str "\n"))
           (on-pixels (fci-mapconcat " "
                                     (make-list left-margin "0")
                                     (make-list rule-width "1")
                                     (make-list right-margin "0")))
           (off-pixels (fci-mapconcat " " (make-list fci-char-width "0")))
           (raster (fci-mapconcat "\n"
                                  (make-list top-margin off-pixels)
                                  (make-list segment-length on-pixels)
                                  (make-list bottom-margin off-pixels)))
           (data (concat magic-number dimens raster)))
      `(image :type pbm
              :data ,data
              :mask heuristic
              :foreground ,fci-rule-color
              :ascent center))))

(defun fci-make-xpm-img ()
  "Return an image descriptor for the fill-column rule in XPM format."
  (fci-with-rule-parameters
    (let* ((identifier "/* XPM */\nstatic char *rule[] = {")
           (dimens (concat "\"" width-str " " height-str " 2 1\","))
           (color-spec (concat "\"1 c " fci-rule-color "\",\"0 c None\","))
           (on-pixels (concat "\""
                              (make-string left-margin ?0)
                              (make-string rule-width ?1)
                              (make-string right-margin ?0)
                              "\","))
           (off-pixels (concat "\"" (make-string fci-char-width ?0) "\","))
           (raster (fci-mapconcat ""
                                  (make-list top-margin off-pixels)
                                  (make-list segment-length on-pixels)
                                  (make-list bottom-margin off-pixels)))
           (end "};")
           (data (concat identifier dimens color-spec raster end)))
      `(image :type xpm
              :data ,data
              :mask heuristic
              :ascent center))))

(defun fci-make-overlay-strings ()
  "Generate the overlay strings used to display the fill-column rule."
  (let* ((str (fci-make-rule-string))
         (img (fci-make-img-descriptor))
         (blank-str (char-to-string fci-blank-char))
         (eol-str (char-to-string fci-eol-char))
         (end-cap (propertize blank-str 'display '(space :width 0)))
         (pre-or-post-eol (propertize eol-str
                                      'cursor t
                                      'display (propertize eol-str 'cursor t)))
         (pre-padding (propertize blank-str 'display fci-padding-display))
         (pre-rule (fci-rule-display blank-str img str t))
         (at-rule (fci-rule-display blank-str img str fci-newline))
         (at-eol (if fci-newline pre-or-post-eol "")))
    (setq fci-pre-limit-string (concat pre-or-post-eol pre-padding pre-rule)
          fci-at-limit-string (concat at-eol at-rule)
          fci-post-limit-string (concat pre-or-post-eol end-cap))))

;;; ---------------------------------------------------------------------
;;; Disabling
;;; ---------------------------------------------------------------------

(defun fci-restore-local-vars ()
  "Restore miscellaneous local variables when fci-mode is disabled."
  (when fci-local-vars-set
    (when (and fci-handle-line-move-visual
               (boundp 'line-move-visual))
      (if fci-line-move-visual-was-buffer-local
          (setq line-move-visual fci-saved-line-move-visual)
        (kill-local-variable 'line-move-visual)))
    (when fci-handle-truncate-lines
      (setq truncate-lines fci-saved-truncate-lines))))

(defun fci-restore-display-table ()
  "Restore the buffer display table when fci-mode is disabled."
  (when (and buffer-display-table
             fci-display-table-processed)
    (aset buffer-display-table 10 fci-saved-eol)
    ;; Don't set buffer-display-table to nil even if we created the display
    ;; table; only do so if nothing else has changed it.
    (when (and fci-made-display-table
               (equal buffer-display-table (make-display-table)))
      (setq buffer-display-table nil))))

;;; ---------------------------------------------------------------------
;;; Drawing and Erasing
;;; ---------------------------------------------------------------------

(defun fci-get-overlays-region (start end)
  "Return all overlays between START and END displaying the fill-column rule."
  (delq nil (mapcar #'(lambda (o) (if (overlay-get o 'fci) o))
                    (overlays-in start end))))

(defun fci-delete-overlays-region (start end)
  "Delete overlays displaying the fill-column rule between START and END."
  (mapc #'(lambda (o) (if (overlay-get o 'fci) (delete-overlay o)))
        (overlays-in start end)))

(defun fci-delete-overlays-buffer ()
  "Delete all overlays displaying the fill-column rule in the current buffer."
  (save-restriction
    (widen)
    (fci-delete-overlays-region (point-min) (point-max))))

(defsubst fci-posn-visible-p (posn ranges)
  "Return true if POSN falls within an interval in RANGES."
  (memq t (mapcar #'(lambda (range) (and (<= (car range) posn)
                                         (< posn (cdr range))))
                  ranges)))

(defsubst fci-get-visible-ranges ()
  "Return the window start and end for each window on the current buffer."
  (mapcar #'(lambda (w) (cons (window-start w) (window-end w 'updated)))
          (fci-get-buffer-windows t)))

(defun fci-delete-unneeded ()
  "Erase the fill-column rule at buffer positions not visible in any window."
  (let ((olays (fci-get-overlays-region (point-min) (point-max)))
        (ranges (fci-get-visible-ranges)))
    (dolist (o olays)
      (unless (fci-posn-visible-p (overlay-start o) ranges)
        (delete-overlay o)))))

;; It would be slightly faster to run this backwards from END to START, but
;; only if we maintained the overlay center at an early position in the
;; buffer.  Since other packages that use overlays typically place them while
;; traversing the buffer in a forward direction, that would be a bad idea.
(defun fci-put-overlays-region (start end)
  "Place overlays displaying the fill-column rule between START and END."
  (goto-char start)
  (let (o cc)
    (while (search-forward "\n" end t)
      (goto-char (match-beginning 0))
      (setq cc (current-column)
            o (make-overlay (match-beginning 0) (match-beginning 0)))
      (overlay-put o 'fci t)
      (cond
       ((< cc fci-limit)
        (overlay-put o 'after-string fci-pre-limit-string))
       ((> cc fci-limit)
        (overlay-put o 'after-string fci-post-limit-string))
       (t
        (overlay-put o 'after-string fci-at-limit-string)))
      (goto-char (match-end 0)))))

(defun fci-redraw-region (start end _ignored)
  "Erase and redraw the fill-column rule between START and END."
  (save-match-data
    (save-excursion
      (let ((inhibit-point-motion-hooks t))
        (goto-char end)
        (setq end (line-beginning-position 2))
        (fci-delete-overlays-region start end)
        (fci-put-overlays-region start end)))))

(defun fci-redraw-window (win &optional start)
  "Redraw the fill-column rule in WIN starting from START."
  (fci-redraw-region (or start (window-start win)) (window-end win t) 'ignored))

;; This doesn't determine the strictly minimum amount by which the rule needs
;; to be extended, but the amount used is always sufficient, and determining
;; the genuine minimum is more expensive than doing the extra drawing.
(defun fci-extend-rule-for-deletion (start end)
  "Extend the fill-column rule after a deletion that spans newlines."
  (unless (= start end)
    (let ((delenda (fci-get-overlays-region start end)))
      (when delenda
        (let ((lossage (1+ (length delenda)))
              (max-end 0)
              win-end)
          (mapc #'delete-overlay delenda)
          (dolist (win (fci-get-buffer-windows t))
            ;; Do not ask for an updated value of window-end.
            (setq win-end (window-end win))
            (when (and (< 0 (- (min win-end end)
                               (max (window-start win) start)))
                       (< max-end win-end))
              (setq max-end win-end)))
          (unless (= max-end (point-max))
            (fci-redraw-region max-end
                               (save-excursion
                                 (goto-char max-end)
                                 (line-beginning-position lossage))
                               nil)))))))

(defun fci-update-window-for-scroll (win start)
  "Redraw the fill-column rule in WIN after it has been been scrolled."
   (fci-delete-unneeded)
   (fci-redraw-window win start))

(defun fci-update-all-windows (&optional all-frames)
  "Redraw the fill-column rule in all windows showing the current buffer."
  (dolist (win (fci-get-buffer-windows all-frames))
    (fci-redraw-window win)))

(defun fci-redraw-frame ()
  "Redraw the fill-column rule in all windows on the selected frame."
  (let* ((wins (window-list (selected-frame) 'no-minibuf))
         (bufs (delete-dups (mapcar #'window-buffer wins))))
    (dolist (buf bufs)
      (with-current-buffer buf
        (when fci-mode
          (fci-delete-unneeded)
          (fci-update-all-windows))))))

;;; ---------------------------------------------------------------------
;;; Workarounds
;;; ---------------------------------------------------------------------

;; This in placed in post-command-hook and does four things:
;; 1. If the display table has been deleted or something has changed the
;;    display table for newline chars, we regenerate overlay strings after
;;    reprocessing the display table.
;; 2. If the default char width or height has changed, we regenerate the rule
;;    image.  (This handles both font changes and also cases where we
;;    activate the mode while displaying on a char terminal then subsequently
;;    display the buffer on a window frame.)
;; 3. If the value of `tab-width' or `fill-column' has changed, we reset the
;;    rule.
;; 4. Cursor properties are ignored when they're out of sight because of
;;    horizontal scrolling.  We detect such situations and force a return
;;    from hscrolling to bring our requested cursor position back into view.
;; These are all fast tests, so despite the large remit this function
;; shouldn't noticeably affect editing speed.
(defun fci-post-command-check ()
  "This function is a gross hack."
  (cond
   ((not (and buffer-display-table
              (equal (aref buffer-display-table 10) fci-newline)))
    (setq fci-display-table-processed nil)
    (fci-mode 1))
   ((and (< 1 (frame-char-width))
         (not fci-always-use-textual-rule)
         (not (and (= (frame-char-width) fci-char-width)
                   (= (frame-char-height) fci-char-height))))
    (fci-mode 1))
   ((not (and (= (or fci-rule-column fill-column) fci-column)
              (= tab-width fci-tab-width)))
    (fci-mode 1))
   ((and (< 0 (window-hscroll))
         auto-hscroll-mode
         (<= (current-column) (window-hscroll)))
    ;; Fix me:  Rather than setting hscroll to 0, this should reproduce the
    ;; relevant part of the auto-hscrolling algorithm.  Most people won't
    ;; notice the difference in behavior, though.
    (set-window-hscroll (selected-window) 0))))

(provide 'fill-column-indicator)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; fill-column-indicator.el ends here
