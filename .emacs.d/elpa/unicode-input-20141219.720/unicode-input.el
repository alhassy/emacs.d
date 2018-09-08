;;; unicode-input.el --- Support for unicode character input
;;
;; Copyright(C) 2014-2015 m00nlight
;;
;; This file is NOT part of GNU Emacs.
;;
;; Author: m00nlight <dot_wangyushi@yeah.net>
;; Maintainer: m00nlight <dot_wangyushi@yeah.net>
;; Created: 13 Dec 2014
;; Keywords: unicode, input,
;; Package-Version: 20141219.720
;; Bitbucket: https://bitbucket.org/m00nlight/unicode-input/

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.



;;
;;; Commentary:
;; The unicode-input.el is for use to input unicode character while editing.
;; It is mainly an solution to the problem haskell-unicode-input-method can
;; not work simultaneously with the "company-ghc" mode.
;;
;; For convenient usage, the package define some abbrevations for the Greek
;; letters. One can easily enable "abbrev-mode" in certain programming mode
;; for convenient Greek letters replacement.  So you need to enable the
;; "abbrev-mode" in the mode hook. For this part, see the installation part
;; for details.
;;
;; The haskell unicode symbol mainly contains the unicode symbol defined in
;; "Prelude.Unicode" in the base-unicode-symbol package, except the "÷", it
;; seems to be an keyboard macro of Emacs. Need *TODO* solve the future. See
;; <http://hackage.haskell.org/package/base-unicode-symbols-0.2.2.4/docs/\
;; Prelude-Unicode.html> for details.


;;; Installation:
;;
;; To use "unicode-input" in "haskell-mode", you need to do the following:
;;
;; 1. You need first to install and configure the haskell-mode for haskell
;; programming either from the "elpa" packages or from the source. You can
;; see the instruction <https://github.com/haskell/haskell-mode>. Notice not
;; to enable the "haskell-unicode-input-method".
;;
;; 2. Place unicode-input.el on you emacs load-path
;;
;; 3. Compile the file (for speed)
;; M-x byte-compile-file <location of unicode-input.el>
;;
;; 4. Add the following to your .emacs/init file
;; (require 'unicode-input)
;;
;; Usage:
;;
;; (require 'haskell-mode)
;; (require 'unicode-input)

;; (add-hook 'haskell-mode-hook
;; 	  (lambda ()
;; 	    (abbrev-mode 1)
;; 	    (unicode-input-mode 1)))

;;; Usage:
;;
;; For Greek letters, just type their name and it will be replaced by their
;; unicode counter-part.
;;
;; For other unicode characters, just press <f9> and then press their ansi
;; plain counter-part, then unicode character will be inserted at the cursor.

;; To enable base unicode syntax for you haskell program, you need to put 
;; {-# LANGUAGE UnicodeSyntax #-} at the beginning of you program, and
;; "import Prelude.Unicode" in you program. For the set related unicode symbols
;; you may also need to "import Data.List.Unicode" into you program. A sample
;; haskell program may look like the following

;; {-# LANGUAGE UnicodeSyntax #-}
;; import Prelude.Unicode
;; import Data.List.Unicode

;; myId ∷ a → a
;; myId x = x

;; dummyId ∷ a → a
;; dummyId = myId ∘ myId

;; dummyConcat ∷ [a] → [a] → [a]
;; dummyConcat xs ys = xs ⧺ ys



;;; Code:

(defvar unicode-input-prefix "<f9>"
  "Default keymap prefix.")

(defvar *unicode-input-greek-letters*
  '(("alpha"		"α")
    ("Alpha"		"Α")
    ("beta"		"β")
    ("Beta"		"Β")
    ("gamma"		"γ")
    ("Gamma"		"Γ")
    ("delta"		"δ")
    ("Delta"		"Δ")
    ("epsilon"		"ε")
    ("Epsilon"		"Ε")
    ("zeta"		"ζ")
    ("Zeta"		"Ζ")
    ("eta"		"η")
    ("Eta"		"Η")
    ("theta"		"θ")
    ("Theta"		"Θ")
    ("iota"		"ι")
    ("Iota"		"Ι")
    ("kappa"		"κ")
    ("Kappa"		"Κ")
    ("lambda"		"λ")
    ("Lambda"		"Λ")
    ("lamda"		"λ")
    ("Lamda"		"Λ")
    ("mu"		"μ")
    ("Mu"		"Μ")
    ("nu"		"ν")
    ("Nu"		"Ν")
    ("xi"		"ξ")
    ("Xi"		"Ξ")
    ("omicron"		"ο")
    ("Omicron"		"Ο")
    ("pi"		"π")
    ("Pi"		"Π")
    ("rho"		"ρ")
    ("Rho"		"Ρ")
    ("sigma"		"σ")
    ("Sigma"		"Σ")
    ("tau"		"τ")
    ("Tau"		"Τ")
    ("upsilon"		"υ")
    ("Upsilon"		"Υ")
    ("phi"		"φ")
    ("Phi"		"Φ")
    ("chi"		"χ")
    ("Chi"		"Χ")
    ("psi"		"ψ")
    ("Psi"		"Ψ")
    ("omega"		"ω")
    ("Omega"		"Ω")
    ("digamma"		"ϝ")
    ("Digamma"		"Ϝ")
    ("san"		"ϻ")
    ("San"		"Ϻ")
    ("qoppa"		"ϙ")
    ("Qoppa"		"Ϙ")
    ("sampi"		"ϡ")
    ("Sampi"		"Ϡ")
    ("stigma"		"ϛ")
    ("Stigma"		"Ϛ")
    ("heta"		"ͱ")
    ("Heta"		"Ͱ")
    ("sho"		"ϸ")
    ("Sho"		"Ϸ")))

(define-abbrev-table 'global-abbrev-table
  *unicode-input-greek-letters*)

(defvar *unicode-input-haskell-characters*
  '(("A"		"𝔸")
    ("B"		"𝔹")
    ("C"		"ℂ")
    ("D"		"𝔻")
    ("E"		"𝔼")
    ("F"		"𝔽")
    ("G"		"𝔾")
    ("H"		"ℍ")
    ("I"		"𝕀")
    ("J"		"𝕁")
    ("K"		"𝕂")
    ("L"		"𝕃")
    ("M"		"𝕄")
    ("N"		"ℕ")
    ("O"		"𝕆")
    ("P"		"ℙ")
    ("Q"		"ℚ")
    ("R"		"ℝ")
    ("S"		"𝕊")
    ("T"		"𝕋")
    ("U"		"𝕌")
    ("V"		"𝕍")
    ("W"		"𝕎")
    ("X"		"𝕏")
    ("Y"		"𝕐")
    ("Z"		"ℤ")
    ("|gamma|"		"ℽ")
    ("|Gamma|"		"ℾ")
    ("|pi|"		"ℼ")
    ("|Pi|"		"ℿ")
    ;; Types
    ("::"		"∷")
    ;; Quantifiers
    ("forall"		"∀")
    ("exists"		"∃")
    ;; Arrows
    ("->"		"→")
    ("<-"		"←")
    ("=>"		"⇒")
    ;; logic operator
    ("&&"		"∧")
    ("||"		"∨")
    ;; Comparisons operators
    ("=="		"≡")
    ("/="		"≢")
    ("<="		"≤")
    (">="		"≥")
    ("/<"		"≮")
    ("/>"		"≯")
    ("++"		"⧺")
    ;; Arithmetic
    ;;("(/)"			"÷")  ;; comment due to not work
    ("*"			"⋅")
    ("elem"			"∈")
    ("notElem"			"∉")
    ("member"			"∈")
    ("notMember"		"∉")
    ("union"			"∪")
    ("intersection"		"∩")
    ("isSubsetOf"		"⊆")
    ("isProperSubsetOf"		"⊂")
    ;; function
    ("."			"∘")
    ;; 
    ("empty"			"∅")
    ("undefined"		"⊥")
    ))


(defvar *unicode-input-subscript*
  '(("_0"	"₀")
    ("_1"	"₁")
    ("_2"	"₂")
    ("_3"	"₃")
    ("_4"	"₄")
    ("_5"	"₅")
    ("_6"	"₆")
    ("_7"	"₇")
    ("_8"	"₈")
    ("_9"	"₉")
    ("_+"	"₊")
    ("_-"	"₋")
    ("_("	"₍")
    ("_)"	"₎")
    ("_="	"₌")
    ("_a"	"ₐ")
    ("_e"	"ₑ")
    ("_o"	"ₒ")
    ("_x"	"ₓ")
    ("_h"	"ₕ")
    ("_k"	"ₖ")
    ("_l"	"ₗ")
    ("_m"	"ₘ")
    ("_n"	"ₙ")
    ("_p"	"ₚ")
    ("_s"	"ₛ")
    ("_t"	"ₜ")))

(defvar *unicode-input-superscript*
  '(("^0"	"⁰")
    ("^1"	"¹")
    ("^2"	"²")
    ("^3"	"³")
    ("^4"	"⁴")
    ("^5"	"⁵")
    ("^6"	"⁶")
    ("^7"	"⁷")
    ("^8"	"⁸")
    ("^9"	"⁹")
    ("^i"	"ⁱ")
    ("^+"	"⁺")
    ("^-"	"⁻")
    ("^="	"⁼")
    ("^("	"⁽")
    ("^)"	"⁾")
    ("^n"	"ⁿ")))


;;;###autoload
(define-minor-mode unicode-input-mode
  "For unicode characters input. "
  nil
  :keymap (let ((map (make-keymap))
		(uprefix (concat unicode-input-prefix " ")))
	    (progn
	      (dolist (x *unicode-input-haskell-characters*)
		(define-key map (kbd (concat unicode-input-prefix (car x)))
		  (cadr x)))
	      (dolist (x *unicode-input-subscript*)
		(define-key map (kbd (concat unicode-input-prefix (car x)))
		  (cadr x)))
	      (dolist (x *unicode-input-superscript*)
		(define-key map (kbd (concat unicode-input-prefix (car x)))
		  (cadr x))))
	    map))

;;;###autoload
(defun unicode-input-mode-enable ()
  (unicode-input-mode 1))

;;;###autoload
(defun unicode-input-mode-disable ()
  (unicode-input-mode 0))



(provide 'unicode-input)

;;; unicode-input.el ends here

