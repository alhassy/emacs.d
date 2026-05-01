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
