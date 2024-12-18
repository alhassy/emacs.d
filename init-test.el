;; [[file:init.org::#E2E-Test][E2E Test:1]]
(ert-deftest hideshow-is-enabled-and-folds-by-default ()
  :tags '(hideshow)
  ;; Make a temporary scratch.js file with the given contents.
  (let* ((contents "function fierce(name) { \n return `${name}: ROAR` \n }")
         (scratch.js (make-temp-file "scratch" nil ".js" contents)))

    ;; Hideshow is enabled whenever we open a code file
    (find-file scratch.js)
    (should hs-minor-mode)

    ;; Function definition is a hidden block
    (end-of-line)
    (backward-char 2)
    (should (hs-already-hidden-p))

    ;; Indeed, the hidden block starts at the first line break and ends just after the second.
    (-let [ov (hs-already-hidden-p)]
      (-let [(first\n second\n) (-elem-indices "\n" (s-split "" contents))]
        (should (equal (overlay-start ov) first\n)) ;; ≈ 25
        (should (equal (overlay-end ov) (+ second\n 2))))) ;; ≈ 52

    (kill-buffer)))
;; E2E Test:1 ends here
