;; [[file:init.org::*Why Emacs? Because of Org-agenda: /“Write fragmentarily, read collectively”/][Why Emacs? Because of Org-agenda: /“Write fragmentarily, read collectively”/:1]]
;; I like to write everything in one massive file, and the agenda should consult it.
(setq org-agenda-files (list (f-expand "~/Dropbox/my-life.org")))
;; Why Emacs? Because of Org-agenda: /“Write fragmentarily, read collectively”/:1 ends here

;; [[file:init.org::*Why Emacs? Because of Org-agenda: /“Write fragmentarily, read collectively”/][Why Emacs? Because of Org-agenda: /“Write fragmentarily, read collectively”/:2]]
;; `org-ql' is a Lispy query language for Org files.  It allows you to find Org
;; entries matching certain criteria and return a list of them or perform
;; actions on them.
(use-package org-ql)
;; Why Emacs? Because of Org-agenda: /“Write fragmentarily, read collectively”/:2 ends here

;; [[file:init.org::*Timestamps and their uses][Timestamps and their uses:2]]
(defun my/agenda-for-day ()
  "Call this method, then enter say “-fri” to see tasks timestamped for last Friday."
  (interactive)
  (let* ((date (org-read-date))
         (org-agenda-buffer-tmp-name (format "*Org Agenda(a:%s)*" date))
         (org-agenda-sticky nil))
    (org-agenda-list nil date nil)
    ;; Putting the agenda in log mode, allows to see the tasks marked as DONE
    ;; at the corresponding time of closing. If, like me, you clock all your
    ;; working time, the task will appear also every time it was worked on.
    ;; This is great to get a sens of what was accomplished.
    (org-agenda-log-mode)))
;; Timestamps and their uses:2 ends here

;; [[file:init.org::*My default ~org-agenda-custom-commands~][My default ~org-agenda-custom-commands~:1]]
;; For each block in my Agenda, only show appointments, and tasks, occurring
;; today. For this week, month, etc, press “v w” or “v m”.
(setq org-agenda-span 'day)

(setq org-agenda-sticky nil)

(setq org-agenda-files (list (f-expand "~/Dropbox/my-life.org")))
;; My default ~org-agenda-custom-commands~:1 ends here

;; [[file:init.org::*My default ~org-agenda-custom-commands~][My default ~org-agenda-custom-commands~:2]]
(setq org-agenda-custom-commands
      '(("t" "My list of all TODO entries" tags-todo "-recurring-someday+LEVEL=3"
         ((org-agenda-overriding-header "\nTODOs sorted by state, priority, effort")
          (org-agenda-sorting-strategy '(todo-state-down priority-down effort-up))
          (org-super-agenda-groups (progn
                                     (org-super-agenda-mode t)
                                     '((:name "Important" :and (:priority "A" :not (:todo ("DONE" "CANCELLED"))))
                                       (:name "Process your Inbox" :tag "inbox")
                                       (:name "Approved" :todo "APPROVED")
                                       (:name "Started" :todo "STARTED")
                                       (:name "Waiting" :todo "WAITING")
                                       (:name "Low Priority" :priority "C" :tag "maybe"))))))
	("a" "Daily Agenda;    Productivity  ≈   ♯DONE / ♯TASKS"
         ((org-ql-block
	   '(tags "inbox")
	   ((org-ql-block-header "\n📩 Process Inbox: “m” to mark then “B r” to refile marked items 📥\n")))

	  ;; `M-x org-copy` is your friend. Archive an entry as is, but copy the useful parts.
	  ;; Archiving is useful for clocking reports from the past.
	  (org-ql-block
	   `(and (done) (not (tags "Top")) (closed :to ,(- (calendar-day-of-week (calendar-current-date))))) ;; i.e.;  :to ,(org-read-date nil nil "-1d")
	   ((org-ql-block-header (propertize "\n📜 Items to review: Mine for useful info then archieve or delete. ☑️\n"
					     'help-echo "Press E to toggle seeing ~5 lines of each entry."
					     ;; Reduce the number of DONE and archived headlines so agenda operations that skip over these can finish faster.
					     ))))
	  
	  (org-ql-block
	   '(tags-local "Top")
	   ((org-ql-block-header "\n⚡ Top goals for the month ⚡\n")
	    (org-agenda-remove-tags t)))


	  (org-ql-block
	   '(and (tags-local "Happy") (or (scheduled -7) (deadline -7) (not (done))))
	   ((org-ql-block-header "\n🤗 I'd be happy if I got the following done this week 🥰\n")
	    (org-agenda-remove-tags t)))


	  ;; What “I've done so far” is all tasks closed this week.
	  (org-ql-block
	   `(and (not (tags "Recurring")) (or (todo "WAITING" "APPROVED") (and (done) (closed :from ,(- (calendar-day-of-week (calendar-current-date))) :to today)))) ;; “start-of-week” /from today/
	   ((org-ql-block-header (propertize "\n✅ What I've done so far this week 💯\n" 'help-echo "Press E to toggle seeing ~5 lines of each entry. \n If DONE, mine for useful info then archive or delete."))))
	  
	  ;; NOTE: I don't want to use predicate (not (done)) since I want to see the DONE items
	  ;; as a reminder to myself to actually archive these tasks.
	  (org-ql-block
	   '(deadline auto)
	   ((org-ql-block-header "\n🎯 Deadlines\n")))


	  (org-ql-block
	   '(and (not (habit)) (not (done)) (scheduled :to today) (not (scheduled :on today)))
	   ((org-ql-block-header "\n📆 Overdue\n")))

	  
	  (org-ql-block
	   '(and
	     (todo "STARTED")
	     (level '> 1)
	     (not (tags-local "Someday" "Top" "SocialCredit"))
	     (not (scheduled :from today)))
	   ((org-ql-block-header "\n🤡 Please 𝒓𝒆𝒅𝒖𝒄𝒆 the number of (unscheduled) open loops\n")))

	  (org-ql-block
	   '(and (scheduled :on today) (ts :with-time nil))
	   ((org-ql-block-header "\n😵‍💫 ﴾ Any time ≈ No time﴿ Scheduled today, but not time-blocked\n")))

	  ;; TODO: Use “agenda*” ?
	  ;; The agenda* view is the same as agenda except that it only considers appointments, i.e., scheduled and deadline items that have a time specification ‘[h]h:mm’ in their timestamps.
	  ;; https://orgmode.org/manual/Special-Agenda-Views.html#FOOT172
	  (agenda ""
		  ((org-agenda-overriding-header
		    "\n🔎 Please focus on 𝒪𝓃𝓁𝓎 these tasks for the day!")
		   (org-agenda-format-date "")
		   (org-agenda-skip-function
		    (lambda nil (org-back-to-heading t)
		      (cl-letf*
			  (((symbol-function 'day)
			    (lambda (org-date-string)
			      (cl-fourth (org-parse-time-string org-date-string))))
			   ((symbol-function 'month)
			    (lambda (org-date-string)
			      (cl-fifth (org-parse-time-string org-date-string))))
			   ((symbol-function 'is-repeating)
			    (lambda (org-date-string)
			      (s-matches? "<[^ ]* [^ ]* [^ ]* [^ ]*\\(d\\|m\\)>"
					  org-date-string)))
			   ((symbol-function 'before)
			    (lambda (x y)
			      (or (< (month x) (month y))
				  (and (= (month x) (month y)) (<= (day x) (day y))))))
			   ((symbol-function 'at-least)
			    (lambda (x y) (or (equal y x) (before y x))))
			   ((symbol-function 'skip)
			    (lambda nil (save-excursion (org-end-of-subtree t) (point))))
			   (scheduled (org-entry-get (point) "SCHEDULED"))
			   (today (org-read-date nil nil "+0d"))
			   (non-habit?
			    (not
			     (equal "habit"
				    (ignore-errors
				      (downcase (org-entry-get (point) "STYLE"))))))
			   (deadline? (org-entry-get (point) "DEADLINE"))
			   (overdue? (not (or (not scheduled) (at-least scheduled today))))
			   (scheduled-without-time
			    (and scheduled (not (s-matches? "<.* .* .*:.*>" scheduled)))))
			(when
			    (and non-habit?
				 (or deadline? overdue? scheduled-without-time
				     (and scheduled
					  (or (s-matches? "<[^ ]* [^ ]*>" scheduled)
					      (s-matches? "<[^ ]* [^ ]* [^ ]*\\(d\\|m\\)>"
							  scheduled)))))
			  (skip)))))
		   (org-agenda-current-time-string "⏰ ⮜┈┈┈┈┈┈┈ now")
		   ;; :org-agenda-remove-tags t
		   ;; :org-agenda-time-grid nil
		   ;; :org-use-tag-inheritance nil
		   (org-agenda-span 'day)
		   (org-agenda-prefix-format " ○ %t %s")
		   (org-agenda-time-grid
		    '((daily today require-timed) (800 1000 1200 1400 1600 1800 2000)
		      " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))))
	  
	  ;; What I've left to do is all incomplete tasks scheduled within the next 5-𝓃 days, where 𝓃 is the numeral of the current week day.
	  ;; Mon=1, ⋯, Thu=4, ⋯
	  ;;
	  ;; NOTE: org-ql and org-agenda are two implementations of essentially the same idea.
	  ;; As such, org-ql doesn't honour all of org-agenda's configurations.
	  ;; E.g., org-agenda-sorting-strategy seems to be honoured when set to todo-state-up, but otherwise ignored.
	  ;; See https://github.com/alphapapa/org-ql/issues/79, “Sort entries by due date when using org-ql-block #79”.
	  ;; See also https://github.com/alphapapa/org-ql/issues/370, “Agenda entries are missing properties necessary for view filtering #370”.
	  (org-ql-block
	   (cl-letf (((symbol-function 'org-ql-view--add-todo-face) (lambda (ignored_todo_state))))
	     `(and (not (tags "Recurring" "Happy")) (not (done)) (scheduled :from 1 :to ,(- 7 (calendar-day-of-week (calendar-current-date)))))) ;; “end-of-week” /from today/
	   ((org-agenda-sorting-strategy '(timestamp-up)) ;; Sort by any timestamp, early first. ;; ⟵- Not yet honoured, see #79.
	    (org-agenda-todo-keyword-format "")  ;; ⟵- Not yet honoured, see #79.
	    (org-ql-block-header (propertize
				  "\n🗯️ Incomplete tasks scheduled later this week\n"
				  'help-echo "Be aware!"))))

	  ;; TODO: When I enter the WAITING state, add a property WAITING_SINCE with a timestamp.
	  ;; Then this query here can inspect that timestamp and see if it's been over a week.
	  (org-ql-block
	   '(todo "WAITING")
	   ((org-ql-block-header "\n💢 I've been waiting on these [for over a week?], send reminder!\n")))

	  ;; More items here
	  ))))


;; NOTE: I find these queries by playing with:  (org-ql-search org-agenda-files '(tags-local "Top"))


;; All entries with a timestamp that looks like “<2025-04-16 Wed 18:00-18:30 .+1w>”, the important part is “.+”.
;; (org-ql-search org-agenda-files '(and (ts) (regexp "<[[:digit:]]\\{4\\}-[[:digit:]]+-[[:digit:]]+ .*\\.\\+.*>")) :title "All of my recurring events")


;; From: https://orgmode.org/manual/Speeding-Up-Your-Agendas.html
(setq org-agenda-ignore-properties '(stats)) ;; This will disable parsing and updating statistic cookies.


;; org-ql-block is mostly a "nice to have" feature. It isn't a high priority to
;; make it 100% compatible with every Org Agenda feature (doing so would
;; practically go against the purpose of org-ql, which began as a
;; reimplementation). [Src:
;; https://github.com/alphapapa/org-ql/issues/79#issuecomment-2360241077]
;;
;;
(setq my/special-org-ql-header "\n🗯️ Incomplete tasks scheduled later this week\n")
;; Hide TODO keyword
(advice-add
 'org-ql-view--add-todo-face
 :around
 (defun my/org-ql-omit-todo-keyword-for-specific-header-block (orig-fn keyword)
   (unless (string-equal org-ql-block-header my/special-org-ql-header)
     (funcall orig-fn keyword))))
;; Hide PRIORITY
(advice-add
 'org-ql-view--add-priority-face
 :around
 (defun my/org-ql-omit-priority-for-specific-header-block (orig-fn keyword)
   (unless (string-equal org-ql-block-header my/special-org-ql-header)
     (funcall orig-fn keyword))))
;; Sort by DATE
(advice-add
 'org-ql-select :around 
 (defun my/org-ql-select--sort-if-needed (orig-fn from query &rest args)
   "Advice around `org-ql-select` to inject :sort '(date) under special block header."
   (apply orig-fn from query
	  (if (string-equal org-ql-block-header my/special-org-ql-header)
	      (apply #'plist-put args :sort '(date))
	    args))))
;; My default ~org-agenda-custom-commands~:2 ends here

;; [[file:init.org::*My default ~org-agenda-custom-commands~][My default ~org-agenda-custom-commands~:3]]
;; ≡ Enable folding via indentation in normal agenda buffer ≡
;; So that I can easily “TAB” to toggle folding sections 😉
(use-package origami
  :bind (:map org-agenda-mode-map ("<tab>" . origami-toggle-node))
  :hook org-agenda-mode)
;;
;; Alternatives: my/auto-set-selective-display-mode or (set-selective-display 1)
;; My default ~org-agenda-custom-commands~:3 ends here

;; [[file:init.org::*Getting Started with org-agenda][Getting Started with org-agenda:1]]
;; Jump straight to my 𝓪genda dashboard ---no dispatch menu please!
(bind-key
 "C-c a"
 (defun my/org-agenda ()
   (interactive)
   (org-agenda nil "a")
   (when org-super-agenda-mode
     (org-super-agenda-mode -1)
     (org-agenda-redo))
   (beginning-of-buffer)))

;; I want the following to happen whenever I do “g” or “/” in the agenda.
(add-hook 'org-agenda-finalize-hook
          (defun my/agenda-look-nice ()
            (-let [fill-column 120]
              (olivetti-mode))
            (message "Press “/ -Work” to hide all :Work: entries.")))


;; Reduce unhelpful visual clutter.
;;
;; Everything in my agenda is something I need to do, so no need to show me the actual todo state.
;; The TODO state is useful for filters, but after the filter it's not useful to see.
;; (To hide priorities as well, maybe not advisble, see https://emacs.stackexchange.com/a/61451)
(setq org-agenda-todo-keyword-format "")
;; Getting Started with org-agenda:1 ends here

;; [[file:init.org::*Agenda Dashboard: Buttons & Random Quote][Agenda Dashboard: Buttons & Random Quote:1]]
;; For testing:
;; (aql "hola" :query [(= TODO "I don't care, just show me the dashboard please")] :interactive t)
;;
(cl-defun my/insert-button (label action &key (foreground "white") (background "blue") (echo "This is a custom button"))
  "Insert a styled button at point"
  ;; Example Use:
  ;; (my/insert-button "Hello" (lambda (pos) (message (format "Hello at %s" pos))) :foreground "cyan" :background nil :echo "Press me!!")
  (interactive)
  ;; TODO: Use lf-define instead
  (cl-assert (stringp label) t "my/agenda-button: First arg should be a string")
  (cl-assert (functionp action) t "my/agenda-button: Second arg should be a lambda")
  (let ((start (point)))
    (insert-text-button
     label
     'action action
     'follow-link t
     :type (define-button-type 'custom-button
             ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html
             'face (list :foreground foreground
                         :background background
                         :slant 'italic
                         :weight 'bold
                         :box '(:line-width 2 :style released-button))
             'help-echo echo))))

(add-to-list
 'org-agenda-finalize-hook
 (cl-defun my/extra-agenda-prose ()

   (goto-char 1) ;; Sometimes this is not honoured by org-agenda, e.g., when changing state of a task
   (when (= (point) 1) ;; i.e., only do this once, when the buffer was created.
     (my/insert-button "New Journal Entry"
                       (lambda (pos) (my/capture-journal-entry))
                       :foreground "cyan"
                       :background nil
                       :echo "I enjoy rereading my journal and reliving nice memories ᕦ( ᴼ ڡ ᴼ )ᕤ")
     ;; TODO:? “See all `:Work:` open loops” button?
     (insert "\t")
     (my/insert-button "Consume Content"
                       (lambda (pos) (my/consume-content))
                       :foreground "cyan" :background nil
                       :echo "Get a random subtree from “Consume Content”")
     (insert "\t")
     (my/insert-button "Tell me a funny!"
                       (lambda (pos) (message (dad-joke-get)))
                       :foreground "cyan"
                       :background nil
                       :echo "Smile, it's a form of charity!")
     (insert "\t")
     (my/insert-button "Random WikiShia"
                       (lambda (pos) (browse-url "https://en.wikishia.net/view/Special:Random"))
                       :foreground "cyan"
                       :background nil
                       :echo "Learn, grow!")
     (insert "\t")
     (my/insert-button "Search" ;; TODO: As I learn more agenda search features, I'll make this into a completing-read menu?
                       (lambda (pos) (org-agenda nil "t"))
                       :foreground "pink"
                       :background nil
                       :echo "All TODOs I'd like to actually work on, so as to have a meaningful life")
     ;;
     ;; NOTE: ‘someday’ things sometimes go into my quote system so that I run into them sometime; lol likewise for things I want to remember
     ;;
     (insert "\n")
     (setq my/quote-start (point))
     (setq my/quote-end nil)
     (insert-text-button
      " " ;; Populated via “ display ”; but must be non-empty.
      'action (lambda (&rest args)
                (read-only-mode -1)
                (put-text-property my/quote-start my/quote-end 'display (my/string-fill-column-and-center 70 (my/random-quote)))
                (read-only-mode +1))
      'display (format "\n%s\n" (my/string-fill-column-and-center 70 (my/random-quote)))
      'follow-link t
      :type (define-button-type 'custom-button
              ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html
              'face (list :foreground  "forest green" :slant 'italic)
              'help-echo "Click to see another random quote!"))
     (setq my/quote-end (point))
     (insert "\n\n"))))

(defun my/string-fill-column-and-center (width str)
  (with-temp-buffer
    (insert str)
    (-let [fill-column width] (fill-paragraph))
    (center-region (point-min) (point-max))
    (buffer-string)))
;;
;; Example usage:
(my/string-fill-column-and-center 27 "“We don't think about sinning as you don't think about eating rotten food.” ---Imam As-Sadiq")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MY RANDOM QUOTE                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: When :use_body: is present, get the body but also split the body by “\n\s*-----*”, a line of at least 5 dashes.
(defun my/org-get-title-or-body ()
  "Get the title of an Org heading, or its body if tagged with :use_body:."
  (interactive)
  (save-excursion
    (when (org-at-heading-p)
      (let ((tags (org-get-tags))
            (title (substring-no-properties (org-get-heading t t t t))) ;; Get clean title without keywords or tags
            (body (progn
                    (org-end-of-meta-data)
                    (buffer-substring-no-properties
                     (point)
                     (org-end-of-subtree t t)))))
        (if (member "use_body" tags)
            (string-trim body)
          title)))))


(defun org-get-random-leaf-headline-or-body ()
  "
+ Org headlines that have no nested items are called `leaf nodes'.
+ Nesting of sections does not matter.
+ Items marked :use_body: have their body returned instead of title.
+ Sections with children have only their children consulted, as such we can nest arbitrarily without any issues.
+ COMMENTED-out headlines will not be considered
"
  (interactive)
  (let ((headlines '()))
    (org-map-entries
     (lambda ()
       (unless (org-goto-first-child) ;; Check if the heading has no children
         (push (my/org-get-title-or-body) headlines)))
     nil       ; no tag search, default is nil
     nil       ; use entire buffer for search, default is nil
     'comment) ; skip commented headlines
    (seq-random-elt headlines)))


(defun my/random-quote ()
  (interactive)
  (save-excursion
    (save-restriction
      ;;
      (find-file "~/.emacs.d/quotes.org")
      (widen)
      (-let [quote (org-get-random-leaf-headline-or-body)]
        (kill-buffer)
        quote))))
;; Agenda Dashboard: Buttons & Random Quote:1 ends here

;; [[file:init.org::*Agenda Variables][Agenda Variables:1]]
;; When I clock into a tasg, a “:LOGBOOK:” drawer is created to hold the timing meta-data for the task.
;; When I make document something I've learned with “C-c C-z”, the resulting note should also go into “:LOGBOOK:”.
(setq org-log-into-drawer t)

(setq org-agenda-span 'day)

(setq  org-fold-catch-invisible-edits 'show-and-error ;; Avoid accidental edits to folded sections
       org-special-ctrl-a/e t ;; C-a/C-e know about leading “*” and ending :tags:
       ;; Agenda styling
       org-agenda-tags-column -80
       org-agenda-time-grid '((daily today require-timed)
                              (800 1000 1200 1400 1600 1800 2000)
                              " ───── " "───────────────")
       org-agenda-current-time-string "◀── now ─────────────────────────────────────────────────")
;; Agenda Variables:1 ends here

;; [[file:init.org::*How tasks look in org agenda][How tasks look in org agenda:1]]
;; Start each agenda item with ‘○’, then show me it's %timestamp and how many
;; times it's been re-%scheduled.
(setq org-agenda-prefix-format " ○ %?-12t%-6e%s ")

(setq org-agenda-deadline-leaders '("DUE:       " "In %3d d.: " "%2d d. ago:  "))

(setq org-agenda-scheduled-leaders
      '(""                ;; Don't say “Scheduled ⟨Task⟩”, just show “⟨Task⟩”.
        "Overdue%2dx "))  ;; If something's overdue, say “Overdue 𝓃× ⟨Task⟩”.
;; How tasks look in org agenda:1 ends here

;; [[file:init.org::*Show me the agenda when I've been idle for 10 minutes][Show me the agenda when I've been idle for 10 minutes:1]]
;; Stop this with:  (cancel-function-timers 'my/pop-up-agenda-timer)
(setq my/pop-up-agenda-timer (run-with-idle-timer (* 60 30) t 'my/org-agenda))
;; Show me the agenda when I've been idle for 10 minutes:1 ends here
