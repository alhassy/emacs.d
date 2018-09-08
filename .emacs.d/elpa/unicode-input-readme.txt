The unicode-input.el is for use to input unicode character while editing.
It is mainly an solution to the problem haskell-unicode-input-method can
not work simultaneously with the "company-ghc" mode.

For convenient usage, the package define some abbrevations for the Greek
letters. One can easily enable "abbrev-mode" in certain programming mode
for convenient Greek letters replacement.  So you need to enable the
"abbrev-mode" in the mode hook. For this part, see the installation part
for details.

The haskell unicode symbol mainly contains the unicode symbol defined in
"Prelude.Unicode" in the base-unicode-symbol package, except the "÷", it
seems to be an keyboard macro of Emacs. Need *TODO* solve the future. See
<http://hackage.haskell.org/package/base-unicode-symbols-0.2.2.4/docs/\
Prelude-Unicode.html> for details.


Installation:

To use "unicode-input" in "haskell-mode", you need to do the following:

1. You need first to install and configure the haskell-mode for haskell
programming either from the "elpa" packages or from the source. You can
see the instruction <https://github.com/haskell/haskell-mode>. Notice not
to enable the "haskell-unicode-input-method".

2. Place unicode-input.el on you emacs load-path

3. Compile the file (for speed)
M-x byte-compile-file <location of unicode-input.el>

4. Add the following to your .emacs/init file
(require 'unicode-input)

Usage:

(require 'haskell-mode)
(require 'unicode-input)

(add-hook 'haskell-mode-hook
	  (lambda ()
	    (abbrev-mode 1)
	    (unicode-input-mode 1)))
