;;; synosaurus-wordnet.el --- Wordnet backend for synosaurus  -*- lexical-binding: t -*-

;; Copyright (C) 2019  Hans-Peter Deifel

;; Author: Hans-Peter Deifel <hpd@hpdeifel.de>
;; Keywords: wp

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

;;; An English thesaurus
;;;
;;; You will need to have the wn programm installed

;;; Code:

(require 'synosaurus)

(defvar synosaurus-wordnet--command "wn")
(defvar synosaurus-wordnet--options '("-synsv" "-synsn" "-synsa" "-synsr"))

(defun synosaurus-wordnet--chomp (str)
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(defun synosaurus-wordnet--collect-list ()
  (let ((p (point)))
    (end-of-line)
    (let* ((str (buffer-substring p (point)))
           (list (split-string str "," t))
           (stripped (mapcar 'synosaurus-wordnet--chomp list)))
      stripped)))

(defun synosaurus-wordnet--parse-buffer ()
  (let ((words))
    (goto-char (point-min))
    (while (search-forward-regexp "^Sense" nil t)
      (forward-line 1)
      (beginning-of-line)
      (push (synosaurus-wordnet--collect-list) words))
    words))

;;;###autoload
(defun synosaurus-backend-wordnet (word)
  (let ((buf (get-buffer-create "*Wordnet*")))
    (with-current-buffer buf
      (erase-buffer)
      (apply 'call-process synosaurus-wordnet--command nil buf nil word synosaurus-wordnet--options)
      (synosaurus-wordnet--parse-buffer))))


(provide 'synosaurus-wordnet)
;;; synosaurus-wordnet.el ends here
