(ert-deftest company-works-as-expected-in-text-mode ()
  :tags '(company)
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

;; [[file:init.org::*Implementation][Implementation:2]]
(ert-deftest hierarchical-archive-merges-duplicate-headings ()
  "Archiving a child, then its parent, produces a single merged heading."
  :tags '(archive)
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
