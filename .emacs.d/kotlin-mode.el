;;; kotlin-mode.el --- Major mode for kotlin -*- lexical-binding: t; -*-

;; Copyright © 2015  Shodai Yokoyama

;; Author: Shodai Yokoyama (quantumcars@gmail.com)
;; Keywords: languages
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'rx)
(require 'cc-cmds)

(defgroup kotlin nil
  "A Kotlin major mode."
  :group 'languages)

(defcustom kotlin-tab-width default-tab-width
  "The tab width to use for indentation."
  :type 'integer
  :group 'kotlin-mode
  :safe 'integerp)

(defvar kotlin-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "<tab>") 'c-indent-line-or-region)
    map)
  "Keymap for kotlin-mode")

(defvar kotlin-mode-syntax-table
  (let ((st (make-syntax-table)))

    ;; Strings
    (modify-syntax-entry ?\" "\"" st)

    ;; `_' as being a valid part of a word
    (modify-syntax-entry ?_ "w" st)

    ;; b-style comment
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    st))


;;; Font Lock

(defconst kotlin-mode--misc-keywords
  '("package" "import"))

(defconst kotlin-mode--type-decl-keywords
  '("nested" "inner" "data" "class" "interface" "trait" "typealias" "enum" "object"))

(defconst kotlin-mode--fun-decl-keywords
  '("fun"))

(defconst kotlin-mode--val-decl-keywords
  '("val" "var"))

(defconst kotlin-mode--statement-keywords
  '(;; Branching
    "if" "else"
    ;; Exceptions
    "try" "catch" "finally" "throw"
    ;; Loops
    "while" "for" "do" "continue" "break"
    ;; Miscellaneous
    "when" "is" "in" "as" "return"))

(defconst kotlin-mode--context-variables-keywords
  '("this" "super"))

(defvar kotlin-mode--keywords
  (append kotlin-mode--misc-keywords
          kotlin-mode--type-decl-keywords
          kotlin-mode--fun-decl-keywords
          kotlin-mode--val-decl-keywords
          kotlin-mode--statement-keywords
          kotlin-mode--context-variables-keywords)
  "Keywords used in Kotlin language.")

(defconst kotlin-mode--constants-keywords
  '("null" "true" "false"))

(defconst kotlin-mode--modifier-keywords
  '("open" "private" "protected" "public" "lateinit"
    "override" "abstract" "final" "companion"
    "annotation" "internal" "const" "in" "out")) ;; "in" "out"

(defconst kotlin-mode--property-keywords
  '("by")) ;; "by" "get" "set"

(defconst kotlin-mode--initializer-keywords
  '("init" "constructor"))

(defvar kotlin-mode--font-lock-keywords
  `(;; Keywords
    (,(rx-to-string
     `(and bow (group (or ,@kotlin-mode--keywords)) eow)
     t)
     1 font-lock-keyword-face)

    ;; Package names
    (,(rx-to-string
       `(and (or ,@kotlin-mode--misc-keywords) (+ space)
             (group (+ (any word ?.))))
       t)
     1 font-lock-string-face)

    ;; Types
    (,(rx-to-string
      `(and bow upper (group (* (or word "<" ">" "." "?" "!"))))
      t)
     0 font-lock-type-face)

    ;; Classes/Enums
    (,(rx-to-string
      `(and bow (or ,@kotlin-mode--type-decl-keywords) eow (+ space)
            (group (+ word)) eow)
      t)
     1 font-lock-type-face)

    ;; Constants
    (,(rx-to-string
       `(and bow (group (or ,@kotlin-mode--constants-keywords)) eow)
       t)
     0 font-lock-constant-face)

    ;; Value bindings
    (,(rx-to-string
       `(and bow (or ,@kotlin-mode--val-decl-keywords) eow
             (+ space)
             (group (+ word)) (* space)  (\? ":"))
       t)
     1 font-lock-variable-name-face t)

    ;; Function names
    (,(rx-to-string
       `(and (or ,@kotlin-mode--fun-decl-keywords)
             (+ space) bow (group (+ (any alnum word))) eow)
       t)
     1 font-lock-function-name-face)

    ;; Access modifier
    ;; Access modifier is valid identifier being used as variable
    ;; TODO: Highlight only modifiers in the front of class/fun
    (,(rx-to-string
       `(and bow (group (or ,@kotlin-mode--modifier-keywords))
             eow)
       t)
     1 font-lock-keyword-face)

    ;; Properties
    ;; by/get/set are valid identifiers being used as variable
    ;; TODO: Highlight keywords in the property declaration statement
    ;; (,(rx-to-string
    ;;    `(and bow (group (or ,@kotlin-mode--property-keywords)) eow)
    ;;    t)
    ;;  1 font-lock-keyword-face)

    ;; Constructor/Initializer blocks
    (,(rx-to-string
       `(and bow (group (or ,@kotlin-mode--initializer-keywords)) eow)
       t)
     1 font-lock-keyword-face)

    ;; String interpolation
    (kotlin-mode--match-interpolation 0 font-lock-variable-name-face t))
  "Default highlighting expression for `kotlin-mode'")

(defun kotlin-mode--new-font-lock-keywords ()
  '(
    ("package\\|import" . font-lock-keyword-face)
    ))

(defun kotlin-mode--syntax-propertize-interpolation ()
  (let* ((pos (match-beginning 0))
         (context (save-excursion
                    (save-match-data (syntax-ppss pos)))))
    (when (nth 3 context)
      (put-text-property pos
                         (1+ pos)
                         'kotlin-property--interpolation
                         (match-data)))))

(defun kotlin-mode--syntax-propertize-function (start end)
  (let ((case-fold-search))
    (goto-char start)
    (remove-text-properties start end '(kotlin-property--interpolation))
    (funcall
     (syntax-propertize-rules
      ((let ((identifier '(any alnum " !%&()*+-./:<>?[]^_|~")))
         (rx-to-string
          `(or (group "${" (* ,identifier) "}")
               (group "$" (+ ,identifier)))))
       (0 (ignore (kotlin-mode--syntax-propertize-interpolation)))))
     start end)))

(defun kotlin-mode--match-interpolation (limit)
  (let ((pos (next-single-char-property-change (point)
                                               'kotlin-property--interpolation
                                               nil
                                               limit)))
    (when (and pos (> pos (point)))
      (goto-char pos)
      (let ((value (get-text-property pos 'kotlin-property--interpolation)))
        (if value
            (progn (set-match-data value)
                   t)
          (kotlin-mode--match-interpolation limit))))))

(defun kotlin-mode--prev-line ()
  "Moves up to the nearest non-empty line"
  (if (not (bobp))
      (progn
        (forward-line -1)
        (while (and (looking-at "^[ \t]*$") (not (bobp)))
          (forward-line -1)))))

(defun kotlin-mode--indent-line ()
  "Indent current line as kotlin code"
  (interactive)
  (beginning-of-line)
  (if (bobp) ; 1.)
      (progn
        (kotlin-mode--beginning-of-buffer-indent))
    (let ((not-indented t) cur-indent)
      (cond ((looking-at "^[ \t]*\\.") ; line starts with .
             (save-excursion
               (kotlin-mode--prev-line)
               (cond ((looking-at "^[ \t]*\\.")
                      (setq cur-indent (current-indentation)))

                     (t
                      (setq cur-indent (+ (current-indentation) (* 2 kotlin-tab-width)))))
               (if (< cur-indent 0)
                   (setq cur-indent 0))))

            ((looking-at "^[ \t]*}") ; line starts with }
             (save-excursion
               (kotlin-mode--prev-line)
               (while (and (or (looking-at "^[ \t]*$") (looking-at "^[ \t]*\\.")) (not (bobp)))
                 (kotlin-mode--prev-line))
               (cond ((or (looking-at ".*{[ \t]*$") (looking-at ".*{.*->[ \t]*$"))
                      (setq cur-indent (current-indentation)))
                     (t
                      (setq cur-indent (- (current-indentation) kotlin-tab-width)))))
             (if (< cur-indent 0)
                 (setq cur-indent 0)))

            ((looking-at "^[ \t]*)") ; line starts with )
             (save-excursion
               (kotlin-mode--prev-line)
               (setq cur-indent (- (current-indentation) (* 2 kotlin-tab-width))))
             (if (< cur-indent 0)
                 (setq cur-indent 0)))

            (t
             (save-excursion
               (while not-indented
                 (kotlin-mode--prev-line)
                 (cond ((looking-at ".*{[ \t]*$") ; line ends with {
                        (setq cur-indent (+ (current-indentation) kotlin-tab-width))
                        (setq not-indented nil))

                       ((looking-at "^[ \t]*}") ; line starts with }
                        (setq cur-indent (current-indentation))
                        (setq not-indented nil))

                       ((looking-at ".*{.*->[ \t]*$") ; line ends with ->
                        (setq cur-indent (+ (current-indentation) kotlin-tab-width))
                        (setq not-indented nil))

                       ((looking-at ".*([ \t]*$") ; line ends with (
                        (setq cur-indent (+ (current-indentation) (* 2 kotlin-tab-width)))
                        (setq not-indented nil))

                       ((looking-at "^[ \t]*).*$") ; line starts with )
                        (setq cur-indent (current-indentation))
                        (setq not-indented nil))

                       ((bobp) ; 5.)
                        (setq not-indented nil)))))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))


(defun kotlin-mode--beginning-of-buffer-indent ()
  (indent-line-to 0))

;;;###autoload
(define-derived-mode kotlin-mode prog-mode "Kotlin"
  "Major mode for editing Kotlin."

  (setq font-lock-defaults '((kotlin-mode--font-lock-keywords) nil nil))
  (setq-local syntax-propertize-function #'kotlin-mode--syntax-propertize-function)
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-padding) 1)
  (set (make-local-variable 'comment-start-skip) "\\(//+\\|/\\*+\\)\\s *")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'indent-line-function) 'kotlin-mode--indent-line)

  :group 'kotlin
  :syntax-table kotlin-mode-syntax-table)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.kt\\'" . kotlin-mode) t)

(provide 'kotlin-mode)
;;; kotlin-mode.el ends here
