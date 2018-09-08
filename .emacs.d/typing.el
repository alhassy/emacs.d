;;; typing.el --- The Typing Of Emacs

;; Copyright (C)  2000, 2001  Alex Schroeder <alex@gnu.org>
;; Zombie mode and Q&A game (c) 2011 Sacha Chua <sacha@sachachua.com>

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Version: 1.1.4
;; Keywords: games
;; URL: http://www.emacswiki.org/emacs/TypingOfEmacs

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;; This is a game inspired by The Typing Of The Dead on the Dreamcast.
;; That game itself is a parody of The House Of The Dead.  In the
;; latter, players use a light gun to shoot zombies and other undead.
;; In the former, players have to *type* the names of the undead in
;; order to shoot them.  The Typing Of Emacs is the parody of the
;; parody, since there are no undead (except possibly for the user
;; staring at Emacs).
;;
;; In order to play, choose any buffer and type M-x typing-of-emacs RET
;; to start the game.  Traditionally used files are the source code of
;; the package, the Emacs NEWS file, and words file.
;;
;; In order to install, put this file somewhere on your `load-path' and
;; add (autoload 'typing-of-emacs "The Typing Of Emacs, a game." t) to
;; your ~/.emacs file.
;;
;; Note that this package will create a highscore list for you.  The
;; name of the file is stored in `toe-highscore-file'.  Set it to nil if
;; you don't want yet another dot file to clutter your home directory.

;;; Thanks:

;; Hrvoje Niksic <hniksic@iskon.hr> for the cl stuff.  I've decided that
;; maybe CL isn't such a bad thing after all.  :)

 

;;; Code:

;; The CL stuff is required in order to speed up the parsing function
;; tremendously.  Hrvoje Niksic <hniksic@iskon.hr> says:
;;
;;   "Before applying this fix, GNU Emacs 20.5.1 takes 73 seconds to
;;   parse the `words' file on a very fast machine.  After applying it,
;;   it takes 3 seconds.  Under XEmacs, the improvement is even more
;;   dramatic -- from 92 seconds to 1.2 seconds.
;;
;;   (If you plan to time the results yourself, don't forget to compile
;;   the code -- it uses macros heavily, which make it cons in
;;   interpreted mode.)"

(require 'cl)

(defgroup typing-of-emacs nil
  "Typing of Emacs."
  :version "20.6"
  :group 'games)

(defcustom toe-words-per-level 5
  "*Number of words you have to type to complete a level."
  :type 'number
  :group 'typing-of-emacs)

(defcustom toe-starting-time-per-word 3
  "*Number of seconds to type a word at the beginning of the game."
  :type 'number
  :group 'typing-of-emacs)

(defvar toe-time-per-word nil
  "Current time per word.")

(defcustom toe-starting-time-per-question 20
  "Number of seconds to type the answer to a question."
  :type 'number
  :group 'typing-of-emacs)

(defcustom toe-starting-length 3
  "*The length of the words with which the game starts."
  :type 'number
  :group 'typing-of-emacs)

(defvar toe-length nil
  "Current length of words.")

(defcustom toe-max-length 30
  "*The maximum length of words used in the game."
  :type 'number
  :group 'typing-of-emacs)

(defcustom toe-starting-lives 3
  "*The number of lives a player has at game start."
  :type 'number
  :group 'typing-of-emacs)

(defvar toe-lives nil
  "Current count of lives.")

(defcustom toe-treat-words 'nil
  "Upcase, downcase or do nothing with words found.
You may also specify your own favorite function.  The function
must accept a string as a parameter and return a string for the
game to use."
  :type '(choice (const :tag "do nothing" nil)
		 (const upcase)
		 (const downcase)
		 (function))
  :group 'typing-of-emacs)

(defcustom toe-success-messages 
  '("yes!" "YES!" "OK!" "Done!" "Hit!" "Wow!" "Wonderful!" "Excellent."
    "Amazing!" "Great!" "Go ahead!" "That's it!" "You can do it!")
  "Messages printed after a correct answer."
  :type '(repeat string)
  :group 'typing-of-emacs)

(defcustom toe-failure-messages 
  '("Ouch!" "No!" "Nooooo!" "Argh!" "Go away!" "Desist!" "Stop now!"
    "Miserable!" "Shame on you!" "Loser!" "You'll never get it.")
  "Messages printed after a wrong answer."
  :type '(repeat string)
  :group 'typing-of-emacs)

(defcustom toe-buffer-name "*The Typing Of Emacs*"
  "Name of the game buffer.
Note that if the name starts and ends with `*', the buffer
can be killed even when modified.  This is a good thing."
  :type 'string
  :group 'typing-of-emacs)

(defcustom toe-highscore-file "~/.toe-scores"
  "Name of the highscore-file.
If non-nil, save highscores in the file given."
  :type '(choice (const :tag "No persistent highscores" nil)
		 file)
  :group 'typing-of-emacs)

;; Parsing the buffer

(defun toe-parse-region-words (start end)
  "Return words from the region as an alist.
The alist is made of elements (LENGTH WORD ...) where LENGTH is an
integer describing the length of the words in the list.  Each WORD in
the list is treated according to `toe-treat-words'.  The minimum length
for the words is `toe-starting-length'."
  (save-excursion
    (goto-char start)
    (let (wordhash-alist); alist of (LENGTH HASH) elements
      (while (< (point) end)
	(skip-syntax-forward "^w" end)
	(let* ((pos (point))
	       (l (skip-syntax-forward "w" end)))
	  (if (and (>= l toe-starting-length)
		   (<= l toe-max-length))
	      (let ((word (buffer-substring-no-properties pos (point))))
		(if toe-treat-words
		    (setq word (funcall toe-treat-words word)))
		(let ((wordhash (cdr (assq l wordhash-alist))))
		  (if wordhash
		      (setf (gethash word wordhash) t)
		    (setq wordhash (make-hash-table :test 'equal))
		    (setf (gethash word wordhash) t)
		    (push (cons l wordhash) wordhash-alist)))))))
      ;; now transform (LENGTH HASH) elements into (LENGTH WORD ...) elements
      (mapcar (lambda (pair)
		(cons (car pair)
		      (let (words)
			(maphash (lambda (word ignored)
				   (push word words))
				 (cdr pair))
			words)))
	      wordhash-alist))))

(defun toe-parse-region-questions (start end)
  "Return questions and answers from the region as an alist.
The alist is made of elements (LENGTH (question answer) ...)
where LENGTH is an integer describing the length of the answer.
Each ANSWER in the list is treated accourding to `toe-treat-words'.
The minimum length for the words is `toe-starting-length'."
  (save-excursion
    (goto-char start)
    (let (results)
      (while (re-search-forward "^\\(.*\\?\\)[ \t]*\r?\n\\(.*\\)$" end t)
	(let* ((question (match-string-no-properties 1))
	       (answer
		(if toe-treat-words
		    (funcall toe-treat-words (match-string-no-properties 2))
		  (match-string-no-properties 2)))
	       (qhash (cdr (assq (length answer) results))))
	  (if qhash
	      (setf (gethash question qhash) answer)
	    (setq qhash (make-hash-table :test 'equal))
	    (setf (gethash question qhash) answer)
	    (push (cons (length answer) qhash) results))))
      ;; now transform (LENGTH HASH) elements into (LENGTH
      ;; (question answer) ...) elements
      (mapcar (lambda (pair)
		(cons (car pair)
		      (let (questions)
			(maphash
			 (lambda (question answer)
			   (push (cons question answer) questions))
			 (cdr pair))
			questions)))
	      results))))
	
(defun toe-parse-region (start end)
  "Parse region and return words, questions, and answers.
Words are defined as strings consisting of word characters per syntax
table of the buffer.  Questions are sentences ending in a questionmark.
Answers are the sentences following a question."
  (list (toe-parse-region-words start end)
	(toe-parse-region-questions start end)))

;; Setting up the game buffer

(defvar toe-level 1
  "Current level of the game.")

(defun toe-status ()
  "Print status line."
  (if (not (bolp))
      (newline))
  (insert (format "\nLEVEL %d: %d SECONDS PER WORD!  YOU HAVE %d LIVES LEFT.\n" 
		  toe-level toe-time-per-word toe-lives)))

(defun toe-setup-buffer ()
  "Create and switch to new buffer."
  (switch-to-buffer (get-buffer-create toe-buffer-name))
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert "T h e   T y p i n g   O f   E m a c s . . .\n")
  (toe-status))

;; Asking for stuff and giving feedback

(defun toe-ask-for (question &optional answer)
  "Ask for a word or ask a QUESTION and expect an ANSWER."
  (or answer (setq answer question))
  (if (not (bolp))
      (newline))
  (when (featurep 'emacspeak)
    (dtk-speak question))
  (insert question)
  (with-timeout
      (toe-time-per-word
       (insert " - Timeout!")
       (toe-failure))
    (let ((str (read-string "Go ahead, type: ")))
      (if (string-equal str answer)
	  (toe-success)
	(insert " - " str "?")
	(toe-failure)))))

(defun toe-success ()
  "Give success feedback."
  (toe-feedback toe-success-messages)
  'success)

(defun toe-failure ()
  "Give failure feedback."
  (toe-feedback toe-failure-messages)
  ;; dynamic binding!
  (setq toe-lives (1- toe-lives))
  (insert (format " - %d LIVES LEFT!" toe-lives))
  (if (> toe-lives 0) 'success 'failure))

(defun toe-feedback (stuff)
  "Give feedback by choosing a random phrase from STUFF."
  (let ((feedback (elt stuff (random (length stuff)))))
    (when (featurep 'emacspeak)
      (dtk-speak feedback))
    (insert " - " feedback)))

;; Only way to get out of the game

(defun toe-score (letters words start end)
  "Calculate characters per minutes typed and print it.
LETTERS is the number of characters typed.
START and END are times such as returned by
`current-time'."
  (if (not (bolp))
      (newline))
  (let (highscores)
    (let* ((time (+ (* (expt 2 16)
		       (- (nth 0 end) (nth 0 start)))
		    (- (nth 1 end) (nth 1 start))))
	   (score (if (> time 0) (/ (* 60 letters) time) 0)))
      (insert (format "You have reached %d characters per minute in %d seconds by typing %d words."
		      score time words))
      (message "Updating highscores...")
      (if (and toe-highscore-file (file-readable-p toe-highscore-file))
	  (with-temp-buffer
	    (insert-file-contents toe-highscore-file)
	    (setq highscores (read (current-buffer)))
	    (add-to-list 'highscores (list (or (user-full-name) (user-login-name)) 
					   score time words end))))
      (if (and toe-highscore-file (file-writable-p toe-highscore-file))
	  (with-temp-buffer
	    (print highscores (current-buffer))
	    (write-file toe-highscore-file)))
      (message "Updating highscores...done"))
    (setq highscores (sort highscores (lambda (a b) (> (nth 3 a) (nth 3 b)))))
    (insert "\n\nHall Of Fame\n\n")
    (let ((count 0))
      (while (< count 20)
	(let ((x (elt highscores count)))
	  (if x
	      (insert (format "%3d. %.20s %5d cpm  %5d sec  %5d words  %s\n"
			      (1+ count) (nth 0 x) (nth 1 x) (nth 2 x) (nth 3 x) 
			      (format-time-string "%Y-%m-%d %H:%M" (nth 4 x)))))
	  (setq count (1+ count))))))
  (if (get-buffer toe-buffer-name)
      (view-buffer toe-buffer-name 'kill-buffer)))

;; Helper functions - they use some variables to communicate with the
;; rest of the program.

(defun toe-new-words (alist)
  "Return a list of words with length `toe-length' from ALIST.

If `toe-length' is greater than `toe-max-length', `toe-length' is
reduced to `toe-starting-length' and `toe-time' is reduced by one
second."
  (let (words)
    (while (and (null (setq words (cdr (assoc toe-length alist))))
		(<= toe-length toe-max-length))
      (setq toe-length (1+ toe-length)))
    ;; If word length increased beyond the limit, restart faster
    ;; game.
    (when (> toe-length toe-max-length)
      (setq toe-time-per-word (1- toe-time-per-word)
	    toe-length toe-starting-length
	    toe-level (1+ toe-level))
      (toe-status))
    ;; If we bailed out because toe-length was too large, find words now.
    (while (and (null (setq words (cdr (assoc toe-length alist))))
		(<= toe-length toe-max-length))
      (setq toe-length (1+ toe-length)))
    ;; If nothing found, alist must be corrupt.
    (if (> toe-length toe-max-length)
	(error "Internal error in the typing game"))
    (copy-sequence words)))

(defun typing-of-emacs-questions (&optional zombie)
  "Play the game The Typing of Emacs, Q&A version."
  (interactive "P")
  (if (string-equal (buffer-name) toe-buffer-name)
      (progn
	(set-buffer-modified-p nil)
	(kill-buffer (current-buffer))))
  (let* ((toe-level 1)
	 (toe-length toe-starting-length)
	 (toe-time-per-word toe-starting-time-per-word)
	 (toe-lives toe-starting-lives)
	 (question-alist (cadr (toe-parse-region (point-min) (point-max))))
	 (game-start (current-time))
	 (game-in-progress 'success) 
	 (letter-count 0) (total-word-count 0) (level-word-count 0)
	 questions)
    (if (null question-alist)
	(error "No usable questions found in this buffer"))
    (toe-setup-buffer)
    (while (if zombie game-in-progress (eq game-in-progress 'success))
      ;; Choose new and longer word list if is exhausted
      (or questions (setq toe-length (1+ toe-length)
			  questions (toe-new-words question-alist)))
      ;; Choose a word from the list and require user to type it.
      (let ((pair (elt questions (random (length questions)))))
	(if (eq (setq game-in-progress (toe-ask-for (car pair) (cdr pair))) 'success)
	    (setq letter-count (+ letter-count toe-length)
		  total-word-count (1+ total-word-count)
		  level-word-count (1+ level-word-count)
		  questions (delete pair questions)))
	;; If next level is reached, force finding of new words.
	(if (>= level-word-count toe-words-per-level)
	    (setq level-word-count 0
		  questions nil))))
    (toe-score letter-count total-word-count game-start (current-time))))
  
;; Main entry point
;;;###autoload
(defun typing-of-emacs (&optional zombie)
  "Play the game The Typing Of Emacs.
The game builds a list of words from the current buffer.
In the buffer *The Typing Of Emacs* you will be asked to
type the words.  Should you take more than a certain
number of seconds to do the typing, you loose.  As you
continue playing the words will get longer and longer."
  (interactive "P")
  (if (string-equal (buffer-name) toe-buffer-name)
      (progn
	(set-buffer-modified-p nil)
	(kill-buffer (current-buffer))))
  (let* ((toe-level 1)
	 (toe-length toe-starting-length)
	 (toe-time-per-word toe-starting-time-per-word)
	 (toe-lives toe-starting-lives)
	 (word-alist (car (toe-parse-region (point-min) (point-max))))
	 (words (toe-new-words word-alist))
	 (game-start (current-time)) (game-in-progress 'success) 
	 (letter-count 0) (total-word-count 0) (level-word-count 0))
    (if (null word-alist)
	(error "No usable words found in this buffer"))
    (toe-setup-buffer)
    (while (if zombie game-in-progress (eq game-in-progress 'success))
      ;; Choose new and longer word list if is exhausted
      (or words (setq toe-length (1+ toe-length)
		      words (toe-new-words word-alist)))
      ;; Choose a word from the list and require user to type it.
      (let ((word (elt words (random (length words)))))
	(if (eq (setq game-in-progress (toe-ask-for word)) 'success)
	    (setq letter-count (+ letter-count toe-length)
		  total-word-count (1+ total-word-count)
		  level-word-count (1+ level-word-count)
		  words (delete word words)))
	;; If next level is reached, force finding of new words.
	(if (>= level-word-count toe-words-per-level)
	    (setq level-word-count 0
		  words nil))))
    (toe-score letter-count total-word-count game-start (current-time))))

(provide 'typing)
;;; typing.el ends here
