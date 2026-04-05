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
