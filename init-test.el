;; [[file:init.org::#Word-Completion][Word Completion:2]]
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

  )
;; Word Completion:2 ends here

;; [[file:init.org::*E2E Test][E2E Test:1]]
(ert-deftest hideshow-is-enabled-and-folds-by-default ()
  :tags '(hideshow)
  (-let [contents "function fierce(name) { \n return \\`\\${name}: ROAR\\` \n }"]
    (shell-command (format "echo \"%s\" > ~/.emacs.d/scratch.js" contents))

    ;; Hideshow is enabled whenever we open a code file
    (find-file "~/.emacs.d/scratch.js")
    (should hs-minor-mode)

    ;; Function definition is a hidden block
    (end-of-line)
    (backward-char 2)
    (should (hs-already-hidden-p))

    ;; Indeed, the hidden block starts at the first line break and ends just before the second.
    (-let [ov (hs-already-hidden-p)]
      (-let [(first\n second\n) (-elem-indices "\n" (s-split "" contents))]
        (should (equal (overlay-start ov) first\n)) ;; ≈ 25
        (should (equal (overlay-end ov) (1- second\n))))) ;; ≈ 52

    (kill-buffer)))
;; E2E Test:1 ends here

;; [[file:init.org::#LSP-Making-Emacs-into-a-generic-full-featured-programming-IDE][LSP: Making Emacs into a generic full-featured programming IDE:6]]
(ert-deftest lsp-hover-shows-type-signature ()
  (shell-command "touch ~/.emacs.d/scratch.js")
  (find-file "~/.emacs.d/scratch.js")
  (erase-buffer)

  (lsp)
  ;; lsp-hover uses lsp--eldoc-message, so let's save the hover info.
  (advice-add #'lsp--eldoc-message :before (lambda (&rest msg) (setq my/lsp-hover-message (car msg))))

  (insert "\n const first = (x, y) => 3")

  (insert "\n first")
  (lsp-hover) ;; Alternatively: (lsp-describe-thing-at-point)

  (should (equal "const first: (x: any, y: any) => number"
                 (substring-no-properties (car my/lsp-hover-message))))

  (save-buffer)
  (kill-buffer))
;; LSP: Making Emacs into a generic full-featured programming IDE:6 ends here
