;; [[file:init.org::*Eval-and-run: ~C-x C-e~ on test forms][Eval-and-run: ~C-x C-e~ on test forms:1]]
;; ═══════════════════════════════════════════════════════════════════
;; Load snap.el — shared snapshot testing infrastructure
;; ═══════════════════════════════════════════════════════════════════
;; Provides: `deftestfixture', `deftest', `define-relation',
;; `define-relation--show-diff', the `deftestfixture--registry',
;; and the `eval-last-sexp' advice for eval-and-run / C-u update.
(require 'snap)

;; ── Domain-specific fixtures (not in snap.el) ──────────────────────

;;  Switches to a temp org-mode buffer, runs the body, kills the buffer even on failure.
(deftestfixture deforgtest
  "Switch to a temporary org-mode buffer, run BODY, then kill the buffer.
Most config-smoke tests need an org buffer with mode hooks fired —
this fixture axes that boilerplate.  The buffer object is bound to
`*original-test-buffer*' so tests can reference it after switching away."
  (let ((*original-test-buffer* (generate-new-buffer "*test-org*")))
    (switch-to-buffer *original-test-buffer*)
    (org-mode)
    (unwind-protect
        (progn &body)
      ;; Even if some assertion fails, we should clock-out! No dangling clocks!
      (ignore-errors (org-clock-out))
      ;; Kill the buffer even when a `should' signals failure —
      ;; without unwind-protect, a failing assertion would leak it.
      (when (get-buffer *original-test-buffer*)
        (kill-buffer *original-test-buffer*)))))
;; Eval-and-run: ~C-x C-e~ on test forms:1 ends here

;; [[file:init.org::*Eval-and-run: ~C-x C-e~ on test forms][Eval-and-run: ~C-x C-e~ on test forms:2]]
;; The 😴 macro defers code via `run-with-idle-timer'.  In batch mode
;; those timers never fire, so hooks, mode setups, etc. are absent.
;; Force every pending timer to fire now, making the deferred init eager.
(mapc (lambda (timer) (ignore-errors (timer-event-handler timer))) timer-idle-list)

;; Several use-package declarations register global modes via
;; :hook (after-init . global-*-mode).  In CI, after-init-hook fires
;; before load-file "init.el", so these registrations miss the boat.
;; Re-run the hook so they activate.
(run-hooks 'after-init-hook)

;; Workflow states are declared lazily via `:before' advice on `org-todo'
;; — the advice fires on the first state transition and then self-removes.
;; Tests that assert on `org-todo-keywords' directly, or that reference
;; custom keywords like `INVESTIGATED', would otherwise observe the stock
;; Emacs defaults.  Force eager declaration so tests see the real config.
(my/ensure-workflow-states)

(advice-add 'org-resolve-clocks :override #'ignore)
;; Eval-and-run: ~C-x C-e~ on test forms:2 ends here

(ert-deftest company-works-as-expected-in-text-mode ()
  :tags '(company)
  (skip-unless nil) ;; Company-mode not in use; revisit when re-enabled.
  (switch-to-buffer "*TESTING COMPANY MODE ~ Text*")

  ;; Ensure we have a clear buffer, and enter some text. (Namely, Python code).
  (erase-buffer)
  (insert "\n def first(x): pass")
  (insert "\n def fierce(a, b): pass")

  ;; Completion anything mode is enabled by default.
  (should company-mode)

  ;; There are 2 completion candidates: The two we wrote.
  (insert "\n fi")
  (company-manual-begin) ;; i.e., C-/
  (should (equal company-candidates '("fierce" "first")))

  ;; [fi ↦ fierce] Default option is the most recently matching written word.
  (insert "\n fi")
  (execute-kbd-macro (kbd "C-/ <return>"))
  (should (looking-back "fierce"))

  ;; [fi ↦ first] We can use M-2 to select the candidate “first”
  (insert "\n fi")
  (execute-kbd-macro (kbd "C-/ M-2"))
  (should (looking-back "first"))

  (kill-buffer))

;; Let's enter Python mode and see how things change

(ert-deftest company-shows-keywords-alongside-completions-alphabetically ()
  :tags '(company)
  (skip-unless nil) ;; Company-mode not in use; revisit when re-enabled.
  (switch-to-buffer "*TESTING COMPANY MODE ~ Python*")
  (python-mode)

  ;; Ensure we have a clear buffer, and enter some text. (Namely, Python code).
  (erase-buffer)
  (insert "\n def first(x): pass")
  (insert "\n def fierce(a, b): pass")

  ;; There are 3 completion candidates: The two we wrote, & the third being a Python keyword.
  (insert "\n fi")
  (company-manual-begin)
  (should (equal company-candidates '("fierce" "first" #("finally" 0 7 (company-backend company-keywords)))))

  ;; Candidates are shown alphabetically: M-2 yields “finally”.
  (execute-kbd-macro (kbd "C-g C-/ M-2")) ;; Quit and restart the completion, to get to starting position, then M-2.
  (should (looking-back "finally"))

  (kill-buffer))

;; [[file:init.org::#Coding-with-a-Fruit-Salad-Semantic-Highlighting][Coding with a Fruit Salad: Semantic Highlighting:2]]
(deftest "prog-mode activates indentation and completion" [config-smoke]
  (skip-unless (fboundp 'aggressive-indent-mode))
  (switch-to-buffer "*test-prog-mode*")
  (unwind-protect
      (progn
        (emacs-lisp-mode)
        (should aggressive-indent-mode)
        ;; corfu's globalized mode skips noninteractive sessions (no
        ;; child frames in batch), so verify the global config instead.
        (should (bound-and-true-p global-corfu-mode))
        (should eldoc-mode)
        (should color-identifiers-mode))
    (kill-buffer "*test-prog-mode*")))
;; Coding with a Fruit Salad: Semantic Highlighting:2 ends here

;; [[file:init.org::*E2E Test][E2E Test:1]]
(require 'lf) ;; For `lf-unindent'; installed via use-package but deferred.

(defun my/test--name-to-username (full-name)
  "Derive a Gerrit-style username from FULL-NAME.
Lowercases the first initial and surname: \"Grace Hopper\" → \"ghopper\"."
  (let ((parts (s-split " " (s-trim full-name))))
    (downcase (concat (substring (car parts) 0 1)
                      (-1 parts)))))

(defun my/as-gerrit-patch (spec)
  "Parse a human-readable SPEC string into a Gerrit change alist.
SPEC is a terse commit-message-like block.  Leading indentation is
stripped via `lf-unindent', so callers can indent the string to
match surrounding code:

  (my/as-gerrit-patch \"
      [core] Fix the frobnicate

      BUG-42 #progress

      Owner: Grace Hopper
      Reviewer: Alan Turing
      Added-Reviewer: Ada Lovelace
      Reviewer-Negative: Skynet\")

The first non-blank line becomes `subject'.  Everything before the
metadata lines (Change-Id, Owner, Reviewer, Added-Reviewer,
Reviewer-Negative) forms `commitMessage'.  `Change-Id' is optional —
when present it sets `number' (parsed via `string-to-number'); when
absent, `my/as-gerrit-stack' auto-assigns from the position index.
`Owner' populates the `owner' alist.

Reviewer keywords:
  Reviewer:          voted Code-Review (positive) — appears in both
                     `allReviewers' and `currentPatchSet.approvals'.
  Added-Reviewer:    added but hasn't voted — appears in
                     `allReviewers' only (no approval entry).
  Reviewer-Negative: voted Code-Review -1 — appears in both
                     `allReviewers' and `approvals' with value \"-1\".
  Patchset:          integer, sets `currentPatchSet.number'.
                     Omit for default (no number in the alist).
  Comments:          integer, generates that many stub `comments' entries.
                     Omit for default (empty comments list).
  Size:              \"NxM\" where N = insertions, M = deletions.
                     Sets `sizeInsertions' and `sizeDeletions' on
                     `currentPatchSet'.  E.g., \"200x50\" → 200 ins, 50 del.
  Verified:          \"+1\" or \"-1\".  Adds a Verified approval from Jenkins.
  Last-Updated:      relative age like \"5 days ago\" or \"3 months ago\".
                     Converted to a `lastUpdated' epoch for staleness.
  WIP:               presence sets `(wip . t)' on the change.  The
                     relation body infers kind = `wip' when owner is \"Me\".
  Attention:         presence sets `(attention . t)'.  The relation body
                     infers kind = `my-needing-action' when owner is \"Me\".
  Urgent:            presence sets `(urgent . t)' on the change.
                     The relation body propagates this to the work-item's
                     `urgent' slot after construction."
  (let* ((lines (s-lines (s-trim (lf-unindent spec))))
         (subject (car lines))
         ;; Partition into commit-message vs. metadata lines.
         (meta-re (concat "^\\(Change-Id\\|Owner\\|Reviewer"
                          "\\|Added-Reviewer\\|Reviewer-Negative"
                          "\\|Last-Updated\\|Patchset\\|Comments"
                          "\\|Size\\|Verified\\|WIP\\|Attention"
                          "\\|Urgent\\):"))
         (msg-lines (-take-while
                     (lambda (l) (not (s-matches-p meta-re l)))
                     lines))
         (meta-lines (-drop (length msg-lines) lines))
         (commit-msg (s-trim (s-join "\n" msg-lines)))
         ;; Parse metadata.
         (number nil) (owner-name nil) (last-updated nil)
         (patchset-num nil) (comment-count nil)
         (size-insertions nil) (size-deletions nil)
         (verified-value nil) (wip-p nil) (attention-p nil) (urgent-p nil)
         (reviewers nil) (added-reviewers nil) (neg-reviewers nil))
    (dolist (l meta-lines)
      (cond
       ((s-prefix-p "Change-Id:" l)
        (setq number (string-to-number (s-trim (substring l 10)))))
       ((s-prefix-p "Owner:" l)
        (setq owner-name (s-trim (substring l 6))))
       ((s-prefix-p "Last-Updated:" l)
        (let ((val (s-trim (substring l 13))))
          (setq last-updated
                (if (string-match "\\([0-9]+\\) \\(day\\|week\\|month\\|year\\)s? ago" val)
                    (let* ((n (string-to-number (match-string 1 val)))
                           (unit (match-string 2 val))
                           (secs (* n (pcase unit
                                        ("day" 86400)
                                        ("week" 604800)
                                        ("month" 2592000)
                                        ("year" 31536000)))))
                      (truncate (- (float-time) secs)))
                  (string-to-number val)))))
       ((s-prefix-p "Reviewer-Negative:" l)
        (push (s-trim (substring l 18)) neg-reviewers))
       ((s-prefix-p "Added-Reviewer:" l)
        (push (s-trim (substring l 15)) added-reviewers))
       ((s-prefix-p "Reviewer:" l)
        (push (s-trim (substring l 9)) reviewers))
       ((s-prefix-p "Patchset:" l)
        (setq patchset-num (string-to-number (s-trim (substring l 9)))))
       ((s-prefix-p "Comments:" l)
        (setq comment-count (string-to-number (s-trim (substring l 9)))))
       ((s-prefix-p "Size:" l)
        (let ((val (s-trim (substring l 5))))
          (when (string-match "\\([0-9]+\\)x\\([0-9]+\\)" val)
            (setq size-insertions (string-to-number (match-string 1 val))
                  size-deletions  (string-to-number (match-string 2 val))))))
       ((s-prefix-p "Verified:" l)
        (setq verified-value (s-trim (substring l 9))))
       ((s-prefix-p "WIP:" l)
        (setq wip-p t))
       ((s-prefix-p "Attention:" l)
        (setq attention-p t))
       ((s-prefix-p "Urgent:" l)
        (setq urgent-p t))))
    (setq reviewers (nreverse reviewers)
          added-reviewers (nreverse added-reviewers)
          neg-reviewers (nreverse neg-reviewers))
    (let ((all-rv (-map (lambda (r)
                          `((name . ,r)
                            (username . ,(my/test--name-to-username r))))
                        (append reviewers added-reviewers neg-reviewers)))
          (pos-approvals
           (-map (lambda (r)
                   `((type . "Code-Review")
                     (by (name . ,r)
                         (username . ,(my/test--name-to-username r)))))
                 reviewers))
          (neg-approvals
           (-map (lambda (r)
                   `((type . "Code-Review") (value . "-1")
                     (by (name . ,r)
                         (username . ,(my/test--name-to-username r)))))
                 neg-reviewers))
          (verified-approvals
           (when verified-value
             `(((type . "Verified") (value . ,verified-value)
                (by (name . "Jenkins")
                    (username . "jenkins")))))))
      `((number . ,number)
        (subject . ,subject)
        (commitMessage . ,commit-msg)
        ,@(when last-updated `((lastUpdated . ,last-updated)))
        ,@(when wip-p `((wip . t)))
        ,@(when attention-p `((attention . t)))
        ,@(when urgent-p `((urgent . t)))
        (owner (name . ,owner-name)
               (username . ,(my/test--name-to-username owner-name)))
        ,@(when all-rv `((allReviewers . ,all-rv)))
        (currentPatchSet
         ,@(when patchset-num `((number . ,patchset-num)))
         (approvals . ,(append pos-approvals neg-approvals verified-approvals))
         ,@(when size-insertions `((sizeInsertions . ,size-insertions)))
         ,@(when size-deletions `((sizeDeletions . ,size-deletions))))
        ,@(when comment-count
            `((comments . ,(-map (lambda (i)
                                   `((timestamp . ,(truncate (float-time)))
                                     (message . ,(format "stub comment %d" i))))
                                 (number-sequence 1 comment-count)))))))))

(defun my/as-gerrit-stack (&rest specs)
  "Map `my/as-gerrit-patch' over SPECS, returning a list of change alists.
Each element of SPECS is a human-readable patch string (see
`my/as-gerrit-patch' for the format).  A single-element list is
a valid stack — useful for the lone-change happy path.

The change `number' is auto-assigned from the position index
(0, 1, 2, ...) when the spec omits a `Change-Id:' line —
removing boilerplate from tests."
  (--map-indexed (let ((c (my/as-gerrit-patch it)))
                   (unless ('number c)
                     (setf ('number c) it-index))
                   c)
                 specs))

(defun my/gerrit-stack-from-string (s)
  "Split S on sequences of 5+ dashes, pass each segment to `my/as-gerrit-stack'.
By way of example:
  (my/gerrit-stack-from-string
   \"[core] Fix frobnicate
    BUG-42 #progress
    Owner: Grace Hopper
    ----------
    [core] Add validation
    BUG-42 #related
    Owner: Grace Hopper\")
returns a two-element stack — identical to calling `my/as-gerrit-stack'
with two separate string arguments."
  (let ((segments (split-string s "-\\{5,\\}" t "[ \t\n]*")))
    (apply #'my/as-gerrit-stack segments)))

(defun defworkitemtest--produce-items (stack kind-override)
  "Produce a list of work-items from STACK, mirroring production classification.
When KIND-OVERRIDE is non-nil, use it for all items (bypasses inference).
Otherwise we partition by the \"Me\" sentinel in `Owner:' lines and
inspect `WIP:' / `Attention:' / `Urgent:' metadata — the same axes
the production coordinator uses:
  Owner = Me + WIP:             →  `wip'
  Owner = Me + Attention:       →  `my-needing-action'
  Owner = Me (no flags)         →  `please-review'
  Owner ≠ Me                    →  `reviews-needed'  (via `work-items-from-review-stack')
Changes carrying `(urgent . t)' propagate the flag to their work-item."
  (let* ((items
          (if kind-override
              (work-items-from-stack stack kind-override)
            (let* ((me-username (my/test--name-to-username "Me"))
                   (mine (-filter (lambda (c)
                                    (equal "Me" ('owner 'name c)))
                                  stack))
                   (others (-remove (lambda (c)
                                      (equal "Me" ('owner 'name c)))
                                    stack))
                   (my-kind (when mine
                              (let ((tip (-1 mine)))
                                (cond
                                 (('wip tip)       'wip)
                                 (('attention tip)  'my-needing-action)
                                 (t                           'please-review))))))
              (append
               (when mine (work-items-from-stack mine my-kind))
               (when others (work-items-from-review-stack others me-username))))))
         ;; Propagate Urgent: from changes to work-items.
         (urgent-tickets
          (let ((ht (make-hash-table :test 'equal)))
            (dolist (c stack)
              (when ('urgent c)
                (dolist (tid (my/gerrit--extract-jira-tickets c))
                  (puthash tid t ht))))
            ht)))
    (dolist (it items)
      (when (and (:jira it)
                 (gethash (:jira it) urgent-tickets))
        (setf (:urgent it) t)))
    items))

;; ──────────────────────────────────────────────────────────────────
;; Work-item test fixture: define-work-item-test via define-relation.
;;
;; Every test is pure data — input stack(s) + expected rendition.
;; The "Me" sentinel in `Owner:' lines drives kind inference,
;; mirroring the production coordinator.  `C-u C-x C-e' auto-fills
;; or refreshes output keys.
;;
;; Use `:stack' for a single (possibly multi-change) stack, or
;; `:stacks' for a list of independent stacks merged before sorting.
;; Optional `:hover' checks help-echo strings (one per sorted item).
;; ──────────────────────────────────────────────────────────────────
(define-relation work-item (stack stacks kind results-in hover)
  "Verify that STACK (or STACKS) renders to RESULTS-IN.
STACK is a single multi-change stack string (changes separated by
5+ dashes).  STACKS is a list of independent stack strings — each
parsed separately and their work-items merged before sorting.
Provide one or the other, never both.

When HOVER is non-nil it must be a list of help-echo strings, one per
sorted work-item.  Omit HOVER to skip hover checks entirely.

KIND overrides status inference for single-stack mode.  For STACKS
mode, kind is always inferred from the \"Me\" / metadata convention.

The \"Me\" sentinel in `Owner:' lines drives kind inference:
  Owner = Me              →  `please-review'
  Owner = Me + WIP:       →  `wip'
  Owner = Me + Attention: →  `my-needing-action'
  Owner ≠ Me              →  `reviews-needed'"
  (let ((my\jira-ticket-regex "\\(BUG-[0-9]+\\)")
        (my\gerrit-base-url "https://gerrit.example.com")
        (my\gerrit-project-path "/c/repo/+/")
        (my\jira-base-url "https://jira.example.com/browse"))
    (let* ((all-items
            (if stacks
                ;; Multiple independent stacks — parse each, merge items.
                (-mapcat (lambda (s)
                           (defworkitemtest--produce-items
                            (my/gerrit-stack-from-string s) nil))
                         stacks)
              ;; Single stack (possibly multi-change).
              (defworkitemtest--produce-items
               (my/gerrit-stack-from-string stack) kind)))
           (sorted   (work-item--sort all-items))
           (rendered (mapconcat #'work-item-to-string sorted "\n"))
           (hovers   (mapcar (lambda (it)
                               (let ((raw (substring-no-properties
                                           (work-item-help-echo it))))
                                 (s-join "\n" (mapcar #'s-trim (s-lines raw)))))
                             sorted)))
      (should (equal results-in rendered))
      (when hover (should (equal hover hovers)))
      (list :results-in rendered :hover hovers))))


(define-work-item-test "work-item-to-string shows the tip's Jira ticket when the stack intersection is empty" [work-item]
  :stack "[core] Extract sermon on piety from Nahj al-Balagha

          Owner: Grace Hopper
          --------
          [core] Add khutbah validation for Friday prayers

          BUG-42 #progress

          Owner: Grace Hopper"
  :results-in "[[https://jira.example.com/browse/BUG-42][BUG-42]] Add khutbah validation for Friday prayers ∷ Review Grace Hopper's [[https://gerrit.example.com/c/repo/+/1][latest efforts]]")

(define-work-item-test "work-item-to-string shows the shared Jira ticket when every change references it" [work-item]
  :stack "[lang] Model the event of Ghadir Khumm

          BUG-7 #progress

          Owner: Ada Lovelace
          --------
          [lang] Parse the Prophet's declaration at Ghadir

          BUG-7 #progress

          Owner: Ada Lovelace"
  :results-in "[[https://jira.example.com/browse/BUG-7][BUG-7]] Parse the Prophet's declaration at Ghadir ∷ Review Ada Lovelace's [[https://gerrit.example.com/c/repo/+/1][latest efforts]]")

(define-work-item-test "work-item-to-string shows Jira ticket for a single-change stack" [work-item]
  :stack "[ui] Render Ali's counsel to Malik al-Ashtar

          BUG-101 #progress

          Owner: Alan Turing"
  :results-in "[[https://jira.example.com/browse/BUG-101][BUG-101]] Render Ali's counsel to Malik al-Ashtar ∷ Review Alan Turing's [[https://gerrit.example.com/c/repo/+/0][latest efforts]]")

(define-work-item-test "reviews-needed splits multi-author stack into per-author items" [work-item]
  :stack "[core] Document Ali's role at the Battle of Badr

          BUG-52 #progress

          Owner: Ada Lovelace
          --------
          [core] Record Ali's duel with Amr ibn Abd Wudd

          BUG-52 #progress

          Owner: Ada Lovelace
          --------
          [core] Chronicle the Battle of Uhud

          BUG-61 #progress

          Owner: Grace Hopper
          --------
          [core] Model Ali's defence of the Prophet at Uhud

          BUG-61 #progress

          Owner: Grace Hopper"
  :results-in "[[https://jira.example.com/browse/BUG-61][BUG-61]] Model Ali's defence of the Prophet at Uhud ∷ Review Grace Hopper's [[https://gerrit.example.com/c/repo/+/3][latest efforts]]
[[https://jira.example.com/browse/BUG-52][BUG-52]] Record Ali's duel with Amr ibn Abd Wudd ∷ Review Ada Lovelace's [[https://gerrit.example.com/c/repo/+/1][latest efforts]]")

(define-work-item-test "reviews-needed splits same-author multi-ticket stack into per-ticket items" [work-item]
  :stack "[lang] Index Ali's letters in Nahj al-Balagha

          BUG-48 #resolve

          Owner: Alan Turing
          --------
          [lang] Parse Ali's aphorisms on justice and governance

          BUG-51 #progress

          Owner: Alan Turing"
  :results-in "[[https://jira.example.com/browse/BUG-51][BUG-51]] Parse Ali's aphorisms on justice and governance ∷ Review Alan Turing's [[https://gerrit.example.com/c/repo/+/1][latest efforts]]
[[https://jira.example.com/browse/BUG-48][BUG-48]] Index Ali's letters in Nahj al-Balagha ∷ Review Alan Turing's [[https://gerrit.example.com/c/repo/+/0][latest efforts]]")

(define-work-item-test "reviews-needed produces one item for single-author single-ticket stack" [work-item]
  :stack "[ui] Typeset Ali's sermon on monotheism (Khutbat al-Tawhid)

          BUG-99 #progress

          Owner: Alan Turing"
  :results-in "[[https://jira.example.com/browse/BUG-99][BUG-99]] Typeset Ali's sermon on monotheism (Khutbat al-Tawhid) ∷ Review Alan Turing's [[https://gerrit.example.com/c/repo/+/0][latest efforts]]")

(define-work-item-test "extract-jira-tickets uses last reference as primary ticket" [work-item]
  :stack "[lang] Parse Ali's treaty with Muawiya at Siffin

          BUG-30 #progress
          BUG-49 #progress

          Owner: Grace Hopper"
  :results-in "[[https://jira.example.com/browse/BUG-49][BUG-49]] Parse Ali's treaty with Muawiya at Siffin ∷ Review Grace Hopper's [[https://gerrit.example.com/c/repo/+/0][latest efforts]]")

(define-work-item-test "reviews-needed drops jira-less changes from multi-ticket stack" [work-item]
  :stack "[predict, refactor] Migrate Siffin arbitration records

          BUG-77 #progress

          Owner: Alan Turing
          --------
          [predict, refactor] Extract Kharijite dissent timeline

          Owner: Alan Turing
          --------
          [predict] Reject fabricated hadith about Nahrawan

          BUG-88 #resolve

          Owner: Alan Turing"
  :results-in "[[https://jira.example.com/browse/BUG-88][BUG-88]] Reject fabricated hadith about Nahrawan ∷ Review Alan Turing's [[https://gerrit.example.com/c/repo/+/2][latest efforts]]
[[https://jira.example.com/browse/BUG-77][BUG-77]] Migrate Siffin arbitration records ∷ Review Alan Turing's [[https://gerrit.example.com/c/repo/+/0][latest efforts]]")

(define-work-item-test "reviews-needed still shows item when no change has a jira ticket" [work-item]
  :stack "[predict] Catalogue Ali's judges in Kufa

          Owner: Alan Turing
          --------
          [predict] Record Kumayl ibn Ziyad's governorship

          Owner: Alan Turing"
  :results-in "Sidequest{ Record Kumayl ibn Ziyad's governorship } ∷ Review Alan Turing's [[https://gerrit.example.com/c/repo/+/1][latest efforts]]")

(define-work-item-test "please-review shows non-voting reviewer by name" [work-item]
  :stack "[core, refactor] Simplify Ali's migration from Mecca to Medina

          BUG-123 #progress

          Owner: Me
          Added-Reviewer: Alan Turing
          --------
          [core] Model Ali sleeping in the Prophet's bed on Laylat al-Mabit

          BUG-123 #related

          Owner: Me"
  :results-in "[[https://jira.example.com/browse/BUG-123][BUG-123]] Model Ali sleeping in the Prophet's bed on Laylat al-Mabit ∷ Alan Turing please review this [[https://gerrit.example.com/c/repo/+/1][work]]")

(define-work-item-test "please-review surfaces bot with negative Code-Review" [work-item]
  :stack "[core] Record Ali's conquest of Khaybar

          BUG-42 #progress

          Owner: Me
          Added-Reviewer: Alan Turing
          Reviewer-Negative: Skynet"
  :results-in "[[https://jira.example.com/browse/BUG-42][BUG-42]] Record Ali's conquest of Khaybar ∷ Alan Turing and Skynet please review this [[https://gerrit.example.com/c/repo/+/0][work]]")

(define-work-item-test "please-review filters out bot with positive Code-Review" [work-item]
  :stack "[core] Document Ali lifting the gate of Khaybar

          BUG-42 #progress

          Owner: Me
          Added-Reviewer: Alan Turing
          Reviewer: Skynet"
  :results-in "[[https://jira.example.com/browse/BUG-42][BUG-42]] Document Ali lifting the gate of Khaybar ∷ Alan Turing please review this [[https://gerrit.example.com/c/repo/+/0][work]]")

(define-work-item-test "multi-ticket stack produces one work-item per ticket" [work-item]
  :stack "[lang] Encode Ali's arbitration at Dumat al-Jandal

          BUG-49 #resolve

          Owner: Me
          --------
          [lang] Model the Kharijite revolt after Siffin

          BUG-48 #resolve

          Owner: Me"
  :results-in "[[https://jira.example.com/browse/BUG-48][BUG-48]] Model the Kharijite revolt after Siffin ∷ Reviewers please review this [[https://gerrit.example.com/c/repo/+/1][work]]
[[https://jira.example.com/browse/BUG-49][BUG-49]] Encode Ali's arbitration at Dumat al-Jandal ∷ Reviewers please review this [[https://gerrit.example.com/c/repo/+/0][work]]")

(define-work-item-test "jira-less item renders with Sidequest prefix" [work-item]
  :stack "[lang] Transcribe Ali's du'a known as Du'a Kumayl

          !NO_JIRA

          Owner: Me
          Added-Reviewer: Grace Hopper
          Attention: yes"
  :results-in "Sidequest{ Transcribe Ali's du'a known as Du'a Kumayl } ∷ Address [[https://gerrit.example.com/c/repo/+/0][latest feedback]] from Grace Hopper")

(define-work-item-test "age prefix appears in rendered work-item" [work-item]
  :stack "[base] Summarise Ali's caliphate in Kufa (36-40 AH)

          BUG-77 #progress

          Owner: Me
          Added-Reviewer: Alan Turing
          Last-Updated: 5 months ago"
  :results-in "[5 months] [[https://jira.example.com/browse/BUG-77][BUG-77]] Summarise Ali's caliphate in Kufa (36-40 AH) ∷ Alan Turing please review this [[https://gerrit.example.com/c/repo/+/0][work]]")

(define-work-item-test "age is nil when no lastUpdated on change" [work-item]
  :stack "[base] Draft Ali's letter to the people of Egypt

          BUG-88 #progress

          Owner: Me
          WIP: yes"
  :results-in "[[https://jira.example.com/browse/BUG-88][BUG-88]] Draft Ali's letter to the people of Egypt ∷ Resume or abandon this [[https://gerrit.example.com/c/repo/+/0][work]]?")

(define-work-item-test "sort: urgent first, stalest-first everywhere, sidequests last" [work-item]
  :stacks ("[base] Record the Battle of the Camel (Jamal)

            BUG-10 #progress

            Owner: Grace Hopper
            Last-Updated: 6 months ago"

           "[base] Document Ali's sermon after Siffin

            BUG-20 #progress

            Owner: Alan Turing
            Last-Updated: 2 days ago"

           "[base] Fix Ali's appointment of Malik al-Ashtar

            BUG-30 #progress

            Owner: Ada Lovelace
            Last-Updated: 1 days ago
            Urgent: yes"

           "[base] Catalogue Ali's sayings on patience

            !NO_JIRA

            Owner: Grace Hopper
            Last-Updated: 1 years ago"

           "[base] Index Ali's rulings on the Bayt al-Mal

            !NO_JIRA

            Owner: Alan Turing
            Last-Updated: 3 days ago")
  :results-in "🔴 [1 day] [[https://jira.example.com/browse/BUG-30][BUG-30]] Fix Ali's appointment of Malik al-Ashtar ∷ Review Ada Lovelace's [[https://gerrit.example.com/c/repo/+/0][latest efforts]]
[6 months] [[https://jira.example.com/browse/BUG-10][BUG-10]] Record the Battle of the Camel (Jamal) ∷ Review Grace Hopper's [[https://gerrit.example.com/c/repo/+/0][latest efforts]]
[2 days] [[https://jira.example.com/browse/BUG-20][BUG-20]] Document Ali's sermon after Siffin ∷ Review Alan Turing's [[https://gerrit.example.com/c/repo/+/0][latest efforts]]
[1 year] Sidequest{ Catalogue Ali's sayings on patience } ∷ Review Grace Hopper's [[https://gerrit.example.com/c/repo/+/0][latest efforts]]
[3 days] Sidequest{ Index Ali's rulings on the Bayt al-Mal } ∷ Review Alan Turing's [[https://gerrit.example.com/c/repo/+/0][latest efforts]]")

(define-work-item-test "help-echo surfaces metadata from rich fixture" [work-item]
  :stack "[core] Compile Ali's guidance on distributing the treasury

          BUG-99 #progress

          Owner: Ali ibn Abi Talib
          Reviewer: Hassan ibn Ali
          Reviewer: Hussain ibn Ali
          Last-Updated: 3 days ago
          Patchset: 7
          Comments: 12
          Size: 300x150
          Verified: -1"
  :results-in "[3 days] [[https://jira.example.com/browse/BUG-99][BUG-99]] Compile Ali's guidance on distributing the treasury ∷ Review Ali ibn Abi Talib's [[https://gerrit.example.com/c/repo/+/0][latest efforts]]"
  :hover ("Open their change and leave a Code-Review vote.\n[size L] [ps 7] [12 comments] [CI ✗]\n🔴 CI is red — fix the build before anything else."))
;; E2E Test:1 ends here

;; [[file:init.org::*Also, logging][Also, logging:2]]
(deftest "org workflow states are configured" [config-smoke]
  (should (equal org-todo-keywords
                 '((sequence "TODO(t)" "INVESTIGATED(i)" "STARTED(s)"
                             "|" "PAUSED(p@/!)" "WAITING(w)" "APPROVED(a)"
                             "REFERENCE(r)" "DONE(d)" "CANCELLED(c@)"))))
  (should (equal org-log-done 'time)))

(deforgtest "org speed keys transition TODO state" [config-smoke]
  (should org-use-speed-commands)
  (insert "* TODO Test task\n")
  (goto-char (point-min))
  (execute-kbd-macro (kbd "t i"))
  (should (equal (org-entry-get (point) "TODO") "INVESTIGATED")))

(deforgtest "DONE state adds CLOSED timestamp" [config-smoke]
  (insert "* TODO Wrap it up\n")
  (goto-char (point-min))
  (org-todo "DONE")
  (should (org-entry-get (point) "CLOSED")))
;; Also, logging:2 ends here

;; [[file:init.org::*Adding New *Tasks/Notes* Quickly Without Disturbing The Current Task Content][Adding New *Tasks/Notes* Quickly Without Disturbing The Current Task Content:2]]
(deforgtest "C-RET creates org heading with CREATED property" [config-smoke]
  (execute-kbd-macro (kbd "C-<return>"))
  (insert "My neato notes")
  (should (s-matches? (format-time-string "\\[%Y-%m-%d")
                      (or (org-entry-get (point) "CREATED") ""))))

(deforgtest "org heading gets SCHEDULED and DEADLINE" [config-smoke]
  (execute-kbd-macro (kbd "C-<return>"))
  (insert "Planned task")
  (execute-kbd-macro (kbd "C-c C-s <return>"))
  (execute-kbd-macro (kbd "C-c C-d <return>"))
  (should (org-entry-get (point) "SCHEDULED"))
  (should (org-entry-get (point) "DEADLINE")))
;; Adding New *Tasks/Notes* Quickly Without Disturbing The Current Task Content:2 ends here

;; [[file:init.org::#drag-and-drop-images-into-org-mode][“Smart Paste”: Drag and Drop Images/(Any File!) into Org-Mode:2]]
(deforgtest "smart paste inserts plain text as-is" [config-smoke smart-paste]
  (kill-new "Plain text is pasted as is")
  (my/dwim-paste)
  (should (equal (s-trim (buffer-string))
                 "Plain text is pasted as is")))

(deforgtest "smart paste transforms Gerrit link to Org link" [config-smoke smart-paste]
  ;; Simulate what yank-media does: call the STRING handler directly
  ;; with the clipboard text.  In a GUI, s-v -> my/dwim-paste ->
  ;; yank-media -> STRING handler.  We bypass the clipboard read.
  (my/yank-plaintext-media
   "STRING"
   "12345: [foo, bar] OCaml Syntax: Fix foo bar baz | https://gerrit.example.com/c/abc/+/12345")
  (should (equal (s-trim (buffer-string))
                 "[[https://gerrit.example.com/c/abc/+/12345][OCaml Syntax: Fix foo bar baz]]")))

(deforgtest "C-u prefix does raw paste even for URLs" [config-smoke smart-paste]
  (kill-new "https://github.com/alphapapa/org-ql/tree/master")
  (let ((current-prefix-arg '(4)))
    (my/dwim-paste))
  (should (equal (s-trim (buffer-string))
                 "https://github.com/alphapapa/org-ql/tree/master")))

(deforgtest "smart paste converts URL to titled Org link" [config-smoke smart-paste]
  ;; Fetches the page title over the network --- skip in batch.
  (skip-unless (not noninteractive))
  (my/yank-plaintext-media
   "STRING" "https://github.com/alphapapa/org-ql/tree/master")
  (should (s-starts-with?
           "[[https://github.com/alphapapa/org-ql/tree/master]["
           (s-trim (buffer-string)))))
;; “Smart Paste”: Drag and Drop Images/(Any File!) into Org-Mode:2 ends here

(deforgtest "C-c SPC jumps to clocked task" [config-smoke]
  (require 'org-capture) ;; my/say-bismillah-on-clock-in checks org-capture-mode
  (insert "* TODO Clocked task\n")
  (goto-char (point-min))
  (org-clock-in)
  (switch-to-buffer "*scratch*")
  (should (equal (buffer-name) "*scratch*"))
  (execute-kbd-macro (kbd "C-c SPC"))
  (should (equal (current-buffer) *original-test-buffer*))
  (should (equal (org-get-heading) "TODO Clocked task")))

(deforgtest "org-mode activates completion and eldoc" [config-smoke]
  (skip-unless (fboundp 'eldoc-box-hover-mode))
  (should (bound-and-true-p global-corfu-mode))
  (should eldoc-mode)
  (should eldoc-box-hover-mode))

;; [[file:init.org::*Implementation][Implementation:2]]
(ert-deftest hierarchical-archive-merges-duplicate-headings ()
  "Archiving a child, then its parent, produces a single merged heading."
  :tags '(archive)
  ;; `org-archive-finalize-hook' was introduced in Org 9.8; the hook
  ;; that drives hierarchical archiving simply doesn't fire on older Org.
  (skip-unless (version<= "9.8" org-version))
  (let* ((src-file (make-temp-file "archive-src" nil ".org"
                                   "* A\nSome useful context\n** B\nMore Info\n** C\nBye!\n"))
         (archive-file (concat (file-name-sans-extension src-file) "_archive.org"))
         (org-archive-location (concat archive-file "::"))
         ;; Suppress prompts and messages during the test.
         (inhibit-message t))
    (unwind-protect
        (progn
          ;; Archive ** B
          (find-file src-file)
          (goto-char (point-min))
          (search-forward "** B")
          (my/org-archive-subtree-hierarchically)

          ;; Archive * A (which still carries ** C)
          (goto-char (point-min))
          (search-forward "* A")
          (my/org-archive-subtree-hierarchically)

          ;; Verify: the archive should contain a single * A with
          ;; body text, ** B, and ** C --- no duplicates.
          (with-current-buffer (find-file-noselect archive-file)
            (let ((contents (buffer-string)))
              ;; Exactly one "* A" heading.
              (should (= 1 (s-count-matches "^\\* A$" contents)))
              ;; Body text from the parent is present.
              (should (s-contains-p "Some useful context" contents))
              ;; Both children are present.
              (should (s-contains-p "** B" contents))
              (should (s-contains-p "More Info" contents))
              (should (s-contains-p "** C" contents))
              (should (s-contains-p "Bye!" contents)))))

      ;; Cleanup: silence kill-buffer prompts for modified buffers.
      (--each (list src-file archive-file)
        (-when-let (buf (get-file-buffer it))
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf))
        (when (file-exists-p it) (delete-file it))))))
;; Implementation:2 ends here

(ert-deftest lsp-hover-shows-type-signature ()
  (skip-unless (fboundp 'lsp-workspace-folders-add))
  ;; Make a temporary scratch.js file with the given contents.
  (-let [scratch.js (make-temp-file "scratch" nil ".js" "const first = (x, y) => 3")]
    (find-file scratch.js)
    (lsp-workspace-folders-add (f-parent scratch.js))
    (lsp)

    ;; lsp-hover uses lsp--eldoc-message, so let's save the hover info.
    (advice-add #'lsp--eldoc-message :before (lambda (&rest msg) (setq my/lsp-hover-message (substring-no-properties (car msg)))))

    (end-of-buffer)
    (insert "\n first")
    (lsp-hover)  ;; Alternatively: (lsp-describe-thing-at-point)
    (should (equal "const first: (x: any, y: any) => number" my/lsp-hover-message))

    (save-buffer)
    (kill-buffer)))
