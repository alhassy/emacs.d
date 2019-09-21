;; [[file:~/.emacs.d/init.org][No heading:1]]
(defun show-me ()
  "Evaluate a Lisp expression and insert its value
   as a comment at the end of the line.

   Useful for documenting values or checking values.
  "
  (interactive)
  (-let [it
         (thread-last (thing-at-point 'line)
           read-from-string
           car
           eval
           (format " ;; ⇒ %s"))]
    (end-of-line)
    (insert it)))
;; No heading:1 ends here

;; [[file:~/.emacs.d/init.org::*Booting%20Up][Booting Up:1]]
(setq enable-local-variables :safe)
;; Booting Up:1 ends here

;; [[file:~/.emacs.d/init.org::*~~/.emacs~%20vs.%20~init.org~][~~/.emacs~ vs. ~init.org~:1]]
(-let [custom "~/.emacs.d/custom.el"]
  (unless (file-exists-p custom)
    (eshell-command (format "touch %s" custom)))
  (setq custom-file custom)
  (load custom-file))
;; ~~/.emacs~ vs. ~init.org~:1 ends here

;; [[file:~/.emacs.d/init.org::enable%20making%20init%20and%20readme][enable making init and readme]]
  (defun my/make-init-el-and-README ()
    (interactive "P") ;; Places value of universal argument into: current-prefix-arg
    (when current-prefix-arg
      (let (org-export-use-babel)
        (save-excursion
          ;; Make init.el
          (org-babel-tangle)
          (byte-compile-file "~/.emacs.d/init.el")
          (load-file "~/.emacs.d/init.el")

          ;; Make README.md
          (org-babel-goto-named-src-block "make-readme")
          (org-babel-execute-src-block)

          (message "Tangled, compiled, and loaded init.el; and made README.md")))))

  (add-hook 'after-save-hook 'my/make-init-el-and-README nil 'local-to-this-file-please)
;; enable making init and readme ends here

;; [[file:~/.emacs.d/init.org::*~use-package~%20--The%20start%20of%20~init.el~][~use-package~ --The start of ~init.el~:1]]
;; In ~/.emacs
;;
;; (require 'package)
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
;; (package-initialize)
;; (require 'org-tempo)
;;
;; ~use-package~ --The start of ~init.el~:1 ends here

;; [[file:~/.emacs.d/init.org::*~use-package~%20--The%20start%20of%20~init.el~][~use-package~ --The start of ~init.el~:2]]
;; Make all commands of the “package” module present.
(require 'package)

;; Speef up start up by not loading any packages at startup.
;; (setq package-enable-at-startup nil)
;; Look at the *Messages* buffer before setting this to nil, then after.

;; (setq gnutls-algorithm-priority nil) "NORMAL:-VERS-TLS1.3")

;; Internet repositories for new packages.
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ;; Maintainer is AWOL.
                         ;; ("marmalade" . "https://marmalade-repo.org/packages/")
                         ))

;; Actually get “package” to work.
(package-initialize)

(package-refresh-contents)
;; ~use-package~ --The start of ~init.el~:2 ends here

;; [[file:~/.emacs.d/init.org::*~use-package~%20--The%20start%20of%20~init.el~][~use-package~ --The start of ~init.el~:3]]
;; Unless it's already installed, update the packages archives,
;; then install the most recent version of “use-package”.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
;; ~use-package~ --The start of ~init.el~:3 ends here

;; [[file:~/.emacs.d/init.org::*~use-package~%20--The%20start%20of%20~init.el~][~use-package~ --The start of ~init.el~:4]]
(setq use-package-always-ensure t)
;; ~use-package~ --The start of ~init.el~:4 ends here

;; [[file:~/.emacs.d/init.org::*~use-package~%20--The%20start%20of%20~init.el~][~use-package~ --The start of ~init.el~:5]]
;; Making it easier to discover Emacs key presses.
(use-package which-key
 :diminish which-key-mode
 :init (which-key-mode)
 :config (which-key-setup-side-window-bottom)
         (setq which-key-idle-delay 0.05)
)
;; ~use-package~ --The start of ~init.el~:5 ends here

;; [[file:~/.emacs.d/init.org::*~use-package~%20--The%20start%20of%20~init.el~][~use-package~ --The start of ~init.el~:6]]
(use-package diminish)

;; Let's hide some markers.
(diminish 'eldoc-mode)
(diminish 'org-indent-mode)
(diminish 'subword-mode)
;; ~use-package~ --The start of ~init.el~:6 ends here

;; [[file:~/.emacs.d/init.org::*~use-package~%20--The%20start%20of%20~init.el~][~use-package~ --The start of ~init.el~:7]]
;; Efficient version control.
(use-package magit
  :config (global-set-key (kbd "C-x g") 'magit-status)
)

(use-package htmlize)
;; Main use: Org produced htmls are coloured.
;; Can be used to export a file into a coloured html.

(use-package biblio)     ;; Quick BibTeX references, sometimes.

;; Get org-headers to look pretty! E.g., * → ⊙, ** ↦ ◯, *** ↦ ★
;; https://github.com/emacsorphanage/org-bullets
(use-package org-bullets)
(add-hook 'org-mode-hook 'org-bullets-mode)

(use-package haskell-mode)

(use-package dash)    ;; “A modern list library for Emacs”
(use-package s   )    ;; “The long lost Emacs string manipulation library”.
;; ~use-package~ --The start of ~init.el~:7 ends here

;; [[file:~/.emacs.d/init.org::*~use-package~%20--The%20start%20of%20~init.el~][~use-package~ --The start of ~init.el~:8]]
;; Don't ask for confirmation when opening symlinked files.
(setq vc-follow-symlinks t)
;; ~use-package~ --The start of ~init.el~:8 ends here

;; [[file:~/.emacs.d/init.org::*~magit~%20--Emacs'%20porcelain%20interface%20to%20git][~magit~ --Emacs' porcelain interface to git:1]]
;; See here for a short & useful tutorial:
;; https://alvinalexander.com/git/git-show-change-username-email-address
(when (equal ""
(shell-command-to-string "git config user.name"))
  (shell-command "git config --global user.name \"Musa Al-hassy\"")
  (shell-command "git config --global user.email \"alhassy@gmail.com\""))
;; ~magit~ --Emacs' porcelain interface to git:1 ends here

;; [[file:~/.emacs.d/init.org::*~magit~%20--Emacs'%20porcelain%20interface%20to%20git][~magit~ --Emacs' porcelain interface to git:2]]
(use-package magit)

;; Do not ask about this variable when cloning.
(setq magit-clone-set-remote.pushDefault t)

(cl-defun maybe-clone (remote &optional (local (concat "~/" (file-name-base remote))))
  "Clone a ‘remote’ repository if the ‘local’ directory does not exist.
    Yields ‘nil’ when no cloning transpires, otherwise yields “cloned-repo”.

    ‘local’ is optional and defaults to the base name; e.g.,
    if ‘remote’is ‘https://github.com/X/Y’ then ‘local’ becomes ‘~/Y’.
  "
  (if (file-directory-p local)

     'repo-already-exists

     (async-shell-command (concat "git clone " remote " " local))
     (add-to-list 'magit-repository-directories `(,local   . 0))
     'cloned-repo)
)

;; Set variable without asking.
(setq magit-clone-set-remote.pushDefault 't)

;; Public repos
(maybe-clone "https://github.com/alhassy/emacs.d" "~/.emacs.d")
(maybe-clone "https://github.com/alhassy/alhassy.github.io")
(maybe-clone "https://github.com/alhassy/ElispCheatSheet")
(maybe-clone "https://github.com/alhassy/RubyCheatSheet")
(maybe-clone "https://github.com/alhassy/FSharpCheatSheet")
(maybe-clone "https://github.com/alhassy/CatsCheatSheet")
(maybe-clone "https://github.com/alhassy/org-agda-mode")
(maybe-clone "https://github.com/JacquesCarette/TheoriesAndDataStructures")
(maybe-clone "https://github.com/alhassy/islam")
(maybe-clone "https://gitlab.cas.mcmaster.ca/armstmp/cs3mi3.git" "~/3mi3")
;; ~magit~ --Emacs' porcelain interface to git:2 ends here

;; [[file:~/.emacs.d/init.org::*~magit~%20--Emacs'%20porcelain%20interface%20to%20git][~magit~ --Emacs' porcelain interface to git:3]]
(maybe-clone "https://github.com/alhassy/CheatSheet")

(maybe-clone "https://github.com/alhassy/OCamlCheatSheet")
(maybe-clone "https://github.com/alhassy/PrologCheatSheet")

(maybe-clone "https://github.com/alhassy/MyUnicodeSymbols")
(maybe-clone "https://github.com/alhassy/interactive-way-to-c")
(maybe-clone "https://github.com/alhassy/next-700-module-systems-proposal.git" "~/thesis-proposal")
(maybe-clone "https://github.com/JacquesCarette/MathScheme")
(maybe-clone "https://github.com/alhassy/gentle-intro-to-reflection" "~/reflection/")

;; Private repos

(maybe-clone "https://gitlab.cas.mcmaster.ca/schaapal/metaocaml-kwic.git" "~/alex") ;; metaprogramming, ocaml, phd
(maybe-clone "https://gitlab.cas.mcmaster.ca/MathScheme/TheoryPresentations.git" "~/yasmine") ;; theory presentations, scala, phd
(maybe-clone "https://gitlab.cas.mcmaster.ca/MathScheme/Differentiating-Programs.git" "~/noel") ;; calculus for datatypes, phd

;;
(maybe-clone "https://gitlab.cas.mcmaster.ca/alhassm/CAS781" "~/cas781") ;; cat adventures
;;
;; (maybe-clone "https://gitlab.cas.mcmaster.ca/carette/cs3fp3.git" "~/3fp3")
;; (maybe-clone "https://gitlab.cas.mcmaster.ca/RATH/RATH-Agda"     "~/RATH-Agda")
(maybe-clone "https://gitlab.cas.mcmaster.ca/3ea3-winter2019/assignment-distribution.git" "~/3ea3/assignment-distribution")
(maybe-clone "https://gitlab.cas.mcmaster.ca/3ea3-winter2019/notes.git" "~/3ea3/notes")
(maybe-clone "https://gitlab.cas.mcmaster.ca/3ea3-winter2019/assignment-development.git" "~/3ea3/assignment-development")
(maybe-clone "https://gitlab.cas.mcmaster.ca/3ea3-winter2019/kandeeps.git" "~/3ea3/sujan")
(maybe-clone "https://gitlab.cas.mcmaster.ca/3ea3-winter2019/horsmane.git" "~/3ea3/emily")
(maybe-clone "https://gitlab.cas.mcmaster.ca/3ea3-winter2019/anderj12.git" "~/3ea3/jacob")
;; (maybe-clone "https://gitlab.cas.mcmaster.ca/alhassm/3EA3.git" "~/3ea3/_2018")
;; (maybe-clone "https://gitlab.cas.mcmaster.ca/2DM3/LectureNotes.git" "~/2dm3")

;; Likely want to put a hook when closing emacs, or at some given time,
;; to show me this buffer so that I can ‘push’ if I haven't already!
;
; (magit-list-repositories)
;; ~magit~ --Emacs' porcelain interface to git:3 ends here

;; [[file:~/.emacs.d/init.org::*~magit~%20--Emacs'%20porcelain%20interface%20to%20git][~magit~ --Emacs' porcelain interface to git:4]]
(require 'magit-git)

(defun my/magit-check-file-and-popup ()
  "If the file is version controlled with git
  and has uncommitted changes, open the magit status popup."
  (let ((file (buffer-file-name)))
    (when (and file (magit-anything-modified-p t file))
      (message "This file has uncommited changes!")
      (when nil ;; Became annyoying after some time.
      (split-window-below)
      (other-window 1)
      (magit-status)))))

;; I usually have local variables, so I want the message to show
;; after the locals have been loaded.
(add-hook 'find-file-hook
  '(lambda ()
      (add-hook 'hack-local-variables-hook 'my/magit-check-file-and-popup)
   ))
;; ~magit~ --Emacs' porcelain interface to git:4 ends here

;; [[file:~/.emacs.d/init.org::*~magit~%20--Emacs'%20porcelain%20interface%20to%20git][~magit~ --Emacs' porcelain interface to git:5]]
(use-package git-timemachine)
;; ~magit~ --Emacs' porcelain interface to git:5 ends here

;; [[file:~/.emacs.d/init.org::*Fix%20spelling%20as%20you%20type%20--thesaurus%20&%20dictionary%20too!][Fix spelling as you type --thesaurus & dictionary too!:1]]
(use-package flyspell
  :hook (
           (prog-mode . flyspell-prog-mode)
           (text-mode . flyspell-mode))
)
;; Fix spelling as you type --thesaurus & dictionary too!:1 ends here

;; [[file:~/.emacs.d/init.org::*Fix%20spelling%20as%20you%20type%20--thesaurus%20&%20dictionary%20too!][Fix spelling as you type --thesaurus & dictionary too!:2]]
(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-dictionary "en_GB") ;; set the default dictionary

(diminish 'flyspell-mode) ;; Don't show it in the modeline.
;; Fix spelling as you type --thesaurus & dictionary too!:2 ends here

;; [[file:~/.emacs.d/init.org::*Fix%20spelling%20as%20you%20type%20--thesaurus%20&%20dictionary%20too!][Fix spelling as you type --thesaurus & dictionary too!:4]]
(eval-after-load "flyspell"
  ' (progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))
;; Fix spelling as you type --thesaurus & dictionary too!:4 ends here

;; [[file:~/.emacs.d/init.org::*Fix%20spelling%20as%20you%20type%20--thesaurus%20&%20dictionary%20too!][Fix spelling as you type --thesaurus & dictionary too!:5]]
(global-font-lock-mode t)
(custom-set-faces '(flyspell-incorrect ((t (:inverse-video t)))))
;; Fix spelling as you type --thesaurus & dictionary too!:5 ends here

;; [[file:~/.emacs.d/init.org::*Fix%20spelling%20as%20you%20type%20--thesaurus%20&%20dictionary%20too!][Fix spelling as you type --thesaurus & dictionary too!:6]]
(setq ispell-silently-savep t)
;; Fix spelling as you type --thesaurus & dictionary too!:6 ends here

;; [[file:~/.emacs.d/init.org::*Fix%20spelling%20as%20you%20type%20--thesaurus%20&%20dictionary%20too!][Fix spelling as you type --thesaurus & dictionary too!:7]]
(setq ispell-personal-dictionary "~/.emacs.d/.aspell.en.pws")
;; Fix spelling as you type --thesaurus & dictionary too!:7 ends here

;; [[file:~/.emacs.d/init.org::*Fix%20spelling%20as%20you%20type%20--thesaurus%20&%20dictionary%20too!][Fix spelling as you type --thesaurus & dictionary too!:8]]
(add-hook          'c-mode-hook 'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
;; Fix spelling as you type --thesaurus & dictionary too!:8 ends here

;; [[file:~/.emacs.d/init.org::*Fix%20spelling%20as%20you%20type%20--thesaurus%20&%20dictionary%20too!][Fix spelling as you type --thesaurus & dictionary too!:9]]
(use-package synosaurus
  :diminish synosaurus-mode
  :init    (synosaurus-mode)
  :config  (setq synosaurus-choose-method 'popup) ;; 'ido is default.
           (global-set-key (kbd "M-#") 'synosaurus-choose-and-replace)
)
;; Fix spelling as you type --thesaurus & dictionary too!:9 ends here

;; [[file:~/.emacs.d/init.org::*Fix%20spelling%20as%20you%20type%20--thesaurus%20&%20dictionary%20too!][Fix spelling as you type --thesaurus & dictionary too!:10]]
;; (shell-command "brew cask install xquartz &") ;; Dependency
;; (shell-command "brew install wordnet &")
;; Fix spelling as you type --thesaurus & dictionary too!:10 ends here

;; [[file:~/.emacs.d/init.org::*Fix%20spelling%20as%20you%20type%20--thesaurus%20&%20dictionary%20too!][Fix spelling as you type --thesaurus & dictionary too!:11]]
(use-package wordnut
 :bind ("M-!" . wordnut-lookup-current-word))

;; Use M-& for async shell commands.
;; Fix spelling as you type --thesaurus & dictionary too!:11 ends here

;; [[file:~/.emacs.d/init.org::*Fix%20spelling%20as%20you%20type%20--thesaurus%20&%20dictionary%20too!][Fix spelling as you type --thesaurus & dictionary too!:13]]
(autoload 'typing-of-emacs "~/.emacs.d/typing.el" "The Typing Of Emacs, a game." t)
;; Fix spelling as you type --thesaurus & dictionary too!:13 ends here

;; [[file:~/.emacs.d/init.org::*Fix%20spelling%20as%20you%20type%20--thesaurus%20&%20dictionary%20too!][Fix spelling as you type --thesaurus & dictionary too!:14]]
(use-package speed-type)
;; Fix spelling as you type --thesaurus & dictionary too!:14 ends here

;; [[file:~/.emacs.d/init.org::*Fix%20spelling%20as%20you%20type%20--thesaurus%20&%20dictionary%20too!][Fix spelling as you type --thesaurus & dictionary too!:15]]
(use-package google-translate
 :config
   (global-set-key "\C-ct" 'google-translate-at-point)
)
;; Fix spelling as you type --thesaurus & dictionary too!:15 ends here

;; [[file:~/.emacs.d/init.org::*Using%20a%20Grammar%20&%20Style%20Checker][Using a Grammar & Style Checker:1]]
(use-package langtool
 :config
  (setq langtool-language-tool-jar
     "~/Applications/LanguageTool-4.5/languagetool-commandline.jar")
)
;; Using a Grammar & Style Checker:1 ends here

;; [[file:~/.emacs.d/init.org::*Using%20a%20Grammar%20&%20Style%20Checker][Using a Grammar & Style Checker:2]]
;; Quickly check, correct, then clean up /region/ with M-^

(add-hook 'langtool-error-exists-hook
  (lambda ()
    (langtool-correct-buffer)
    (langtool-check-done)
  ))

(global-set-key "\M-^" (lambda () (interactive) (message "Grammar checking begun ...") (langtool-check)))
;; Using a Grammar & Style Checker:2 ends here

;; [[file:~/.emacs.d/init.org::*Unicode%20Input%20via%20Agda%20Input][Unicode Input via Agda Input:1]]
; (load (shell-command-to-string "agda-mode locate"))
;;
;; Seeing: One way to avoid seeing this warning is to make sure that agda2-include-dirs is not bound.
; (makunbound 'agda2-include-dirs)
;; Unicode Input via Agda Input:1 ends here

;; [[file:~/.emacs.d/init.org::*Unicode%20Input%20via%20Agda%20Input][Unicode Input via Agda Input:2]]
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "/usr/local/bin/agda-mode locate")))
;; Unicode Input via Agda Input:2 ends here

;; [[file:~/.emacs.d/init.org::*Unicode%20Input%20via%20Agda%20Input][Unicode Input via Agda Input:3]]
(require 'agda-input)
(add-hook 'text-mode-hook (lambda () (set-input-method "Agda")))
(add-hook 'org-mode-hook (lambda () (set-input-method "Agda")))
;; Unicode Input via Agda Input:3 ends here

;; [[file:~/.emacs.d/init.org::*Unicode%20Input%20via%20Agda%20Input][Unicode Input via Agda Input:4]]
;;(setq agda2-program-args (quote ("RTS" "-M4G" "-H4G" "-A128M" "-RTS")))
;; Unicode Input via Agda Input:4 ends here

;; [[file:~/.emacs.d/init.org::*Unicode%20Input%20via%20Agda%20Input][Unicode Input via Agda Input:5]]
(add-to-list 'agda-input-user-translations '("set" "𝒮ℯ𝓉"))
;; Unicode Input via Agda Input:5 ends here

;; [[file:~/.emacs.d/init.org::*Unicode%20Input%20via%20Agda%20Input][Unicode Input via Agda Input:6]]
(loop for item in
      '(
        ;; categorial
        ("alg" "𝒜𝓁ℊ")
        ("split" "▵")
        ("join" "▿")
        ("adj" "⊣")
        (";;" "﹔")
        (";;" "⨾")
        (";;" "∘")
        ;; lattices
        ("meet" "⊓")
        ("join" "⊔")
        ;; residuals
        ("syq"  "╳")
        ("over" "╱")
        ("under" "╲")
        ;; Z-quantification range notation, e.g., “∀ x ❙ R • P”
        ("|" "❙")
        ("with" "❙")
        ;; adjunction isomorphism pair
        ("floor"  "⌊⌋")
        ("lower"  "⌊⌋")
        ("lad"    "⌊⌋")
        ("ceil"   "⌈⌉")
        ("raise"  "⌈⌉")
        ("rad"    "⌈⌉")
        ;; more (key value) pairs here
        )
      do (add-to-list 'agda-input-user-translations item))
;; Unicode Input via Agda Input:6 ends here

;; [[file:~/.emacs.d/init.org::*Unicode%20Input%20via%20Agda%20Input][Unicode Input via Agda Input:7]]
;; angry, cry, why-you-no
(add-to-list 'agda-input-user-translations
   '("whyme" "ლ(ಠ益ಠ)ლ" "ヽ༼ಢ_ಢ༽ﾉ☂" "щ(゜ロ゜щ)"))
;; confused, disapprove, dead, shrug
(add-to-list 'agda-input-user-translations
   '("what" "「(°ヘ°)" "(ಠ_ಠ)" "(✖╭╮✖)" "¯\\_(ツ)_/¯"))
;; dance, csi
(add-to-list 'agda-input-user-translations
   '("cool" "┏(-_-)┓┏(-_-)┛┗(-_-﻿ )┓" "•_•)
( •_•)>⌐■-■
(⌐■_■)
"))
;; love, pleased, success, yesss
(add-to-list 'agda-input-user-translations
   '("smile" "♥‿♥" "(─‿‿─)" "(•̀ᴗ•́)و" "(งಠ_ಠ)ง"))
;; Unicode Input via Agda Input:7 ends here

;; [[file:~/.emacs.d/init.org::*Unicode%20Input%20via%20Agda%20Input][Unicode Input via Agda Input:8]]
;; activate translations
(agda-input-setup)
;; Unicode Input via Agda Input:8 ends here

;; [[file:~/.emacs.d/init.org::*Syncing%20to%20the%20System's%20~$PATH~][Syncing to the System's ~$PATH~:1]]
(use-package exec-path-from-shell
  :init
    (when (memq window-system '(mac ns x))
     (exec-path-from-shell-initialize))
)
;; Syncing to the System's ~$PATH~:1 ends here

;; [[file:~/.emacs.d/init.org::*Keeping%20My%20System%20Up%20to%20Date][Keeping My System Up to Date:1]]
(defun my/stay-up-to-date ()

  "Ensure that OS and Emacs pacakges are up to date.

   Takes ~5 secons when everything is up to date.
  "

  (async-shell-command "brew update && brew upgrade")
  (other-window 1)
  (rename-buffer "Keeping-system-up-to-date")

  (package-refresh-contents)
  (insert "Emacs packages have been updated.")

  (other-window 1)
)

(add-hook 'after-init-hook 'my/stay-up-to-date)

;; For now, doing this since I'm also calling my/stay-up-to-date with
;; after-init-hook which hides the startup message.
(add-hook 'after-init-hook 'display-startup-echo-area-message)
;; Keeping My System Up to Date:1 ends here

;; [[file:~/.emacs.d/init.org::*Who%20am%20I?%20%E2%94%80Using%20Gnus%20for%20Gmail][Who am I? ─Using Gnus for Gmail:1]]
(setq user-full-name    "Musa Al-hassy"
      user-mail-address "alhassy@gmail.com")
;; Who am I? ─Using Gnus for Gmail:1 ends here

;; [[file:~/.emacs.d/init.org::*Who%20am%20I?%20%E2%94%80Using%20Gnus%20for%20Gmail][Who am I? ─Using Gnus for Gmail:2]]
     (setq message-send-mail-function 'smtpmail-send-it)
;; Who am I? ─Using Gnus for Gmail:2 ends here

;; [[file:~/.emacs.d/init.org::*Using%20Emacs%20in%20any%20text%20area%20on%20my%20OS][Using Emacs in any text area on my OS:1]]
(shell-command "curl -fsSL https://raw.github.com/zachcurry/emacs-anywhere/master/install | bash")

(server-start)
;; Using Emacs in any text area on my OS:1 ends here

;; [[file:~/.emacs.d/init.org::*Restarting%20Emacs][Restarting Emacs:1]]
;; Provides only the command “restart-emacs”.
(use-package restart-emacs
  :commands restart-emacs)
;; Restarting Emacs:1 ends here

;; [[file:~/.emacs.d/init.org::*Cosmetics][Cosmetics:2]]
;; Make it very easy to see the line with the cursor.
(global-hl-line-mode t)

;; Clean up any accidental trailing whitespace and in other places,
;; upon save.
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Keep self motivated!
(setq frame-title-format '("" "%b - Living The Dream (•̀ᴗ•́)و"))
;; Cosmetics:2 ends here

;; [[file:~/.emacs.d/init.org::*Themes][Themes:1]]
;; Treat all themes as safe; no query before use.
(setf custom-safe-themes t)

;; Nice looking themes ^_^
(use-package solarized-theme :demand t)
(use-package doom-themes  :demand t)
(use-package spacemacs-common
    :ensure spacemacs-theme
    :config (load-theme 'spacemacs-light t))

(defun my/disable-all-themes ()
  (dolist (th custom-enabled-themes)
          (disable-theme th))
)

(defun my/load-dark-theme ()
  ;;   (load-theme 'spacemacs-dark)   ;; orginally
  (my/disable-all-themes)
  (load-theme 'doom-vibrant)
)

(defun my/load-light-theme ()
  (load-theme 'spacemacs-light)   ;; orginally
  ;; Recently I'm liking this ordered mixture.
  ;; (load-theme 'solarized-light) (load-theme 'doom-solarized-light)
)

;; “C-x t” to toggle between light and dark themes.
(defun my/toggle-theme () "Toggle between dark and light themes."
  (interactive)
  ;; Load dark if light is top-most enabled theme, else load light.
  (if (equal (car custom-enabled-themes) 'doom-vibrant)
      (my/load-light-theme)
      (my/load-dark-theme)
  )

  ;; The dark theme's modeline separator is ugly.
  ;; Keep reading below regarding “powerline”.
  ;; (setq powerline-default-separator 'arrow)
  ;; (spaceline-spacemacs-theme)
)

(global-set-key "\C-x\ t" 'my/toggle-theme)

;; Initially begin with the light theme.
; (ignore-errors (load-theme 'spacemacs-light t))
(my/toggle-theme)
;; Themes:1 ends here

;; [[file:~/.emacs.d/init.org::*Startup%20message:%20Emacs%20&%20Org%20versions][Startup message: Emacs & Org versions:1]]
;; Silence the usual message: Get more info using the about page via C-h C-a.
(setq inhibit-startup-message t)

(defun display-startup-echo-area-message ()
  "The message that is shown after ‘user-init-file’ is loaded."
  (message
      (concat "Welcome "      user-full-name
              "! Emacs "      emacs-version
              "; Org-mode "   org-version
              "; System "    (system-name)
                  (format "; Time %.3fs"
                      (float-time (time-subtract (current-time)
                                    before-init-time)))
      )
  )
)
;; Startup message: Emacs & Org versions:1 ends here

;; [[file:~/.emacs.d/init.org::*Startup%20message:%20Emacs%20&%20Org%20versions][Startup message: Emacs & Org versions:2]]
;; Welcome Musa Al-hassy! Emacs 26.1; Org-mode 9.2.3; System alhassy-air.local
;; Startup message: Emacs & Org versions:2 ends here

;; [[file:~/.emacs.d/init.org::*Startup%20message:%20Emacs%20&%20Org%20versions][Startup message: Emacs & Org versions:4]]
(setq initial-major-mode 'org-mode)
;; Startup message: Emacs & Org versions:4 ends here

;; [[file:~/.emacs.d/init.org::*Persistent%20Scratch%20Buffer][Persistent Scratch Buffer:1]]
(setq initial-scratch-message (concat
  "#+Title: Persistent Scratch Buffer"
  "\n#\n # Welcome! This’ a place for trying things out. \n"))
;; Persistent Scratch Buffer:1 ends here

;; [[file:~/.emacs.d/init.org::*Persistent%20Scratch%20Buffer][Persistent Scratch Buffer:2]]
;; A very simple function to recreate the scratch buffer:
;; ( http://emacswiki.org/emacs/RecreateScratchBuffer )
(defun scratch ()
   "create a scratch buffer"
   (interactive)
   (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
   (insert initial-scratch-message)
   (org-mode))

;; This doubles as a quick way to avoid the common formula: C-x b RET *scratch*
;; Persistent Scratch Buffer:2 ends here

;; [[file:~/.emacs.d/init.org::*Persistent%20Scratch%20Buffer][Persistent Scratch Buffer:3]]
(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))
;; Persistent Scratch Buffer:3 ends here

;; [[file:~/.emacs.d/init.org::*Spaceline:%20A%20sleek%20mode%20line][Spaceline: A sleek mode line:1]]
(use-package spaceline
  :config
  (require 'spaceline-config)
  (setq spaceline-buffer-encoding-abbrev-p nil)
  (setq spaceline-line-column-p nil)
  (setq spaceline-line-p nil)
  (setq powerline-default-separator 'arrow)
  :init
 (spaceline-helm-mode) ;; When using helm, mode line looks prettier.
 ; (ignore-errors (spaceline-spacemacs-theme))
)
;; Spaceline: A sleek mode line:1 ends here

;; [[file:~/.emacs.d/init.org::*Flashing%20when%20something%20goes%20wrong%20%E2%94%80no%20blinking][Flashing when something goes wrong ─no blinking:1]]
(setq visible-bell 1)
;; Enable flashing mode-line on errors
;; On MacOS, this shows a caution symbol ^_^

;; Blinking cursor rushes me to type; let's slow down.
(blink-cursor-mode -1)
;; Flashing when something goes wrong ─no blinking:1 ends here

;; [[file:~/.emacs.d/init.org::*My%20to-do%20list:%20The%20initial%20buffer%20when%20Emacs%20opens%20up][My to-do list: The initial buffer when Emacs opens up:1]]
(find-file "~/Dropbox/todo.org")
;; (setq initial-buffer-choice "~/Dropbox/todo.org")

(split-window-right)			  ;; C-x 3
(other-window 1)                              ;; C-x 0
;; toggle enable-local-variables :all           ;; Load *all* locals.
    ;; toggle org-confirm-babel-evaluate nil    ;; Eval *all* blocks.
      (find-file "~/.emacs.d/init.org")
;; My to-do list: The initial buffer when Emacs opens up:1 ends here

;; [[file:~/.emacs.d/init.org::*Showing%20date,%20time,%20and%20battery%20life][Showing date, time, and battery life:1]]
(setq display-time-day-and-date t)
(display-time)

;; (display-battery-mode 1)
;; Nope; let's use a fancy indicator …

(use-package fancy-battery
  :diminish
  :config
    (setq fancy-battery-show-percentage t)
    (setq battery-update-interval 15)
    (fancy-battery-mode)
    (display-battery-mode)
)
;; Showing date, time, and battery life:1 ends here

;; [[file:~/.emacs.d/init.org::*Hiding%20Scrollbar,%20tool%20bar,%20and%20menu][Hiding Scrollbar, tool bar, and menu:1]]
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
;; Hiding Scrollbar, tool bar, and menu:1 ends here

;; [[file:~/.emacs.d/init.org::*Increase/decrease%20text%20size][Increase/decrease text size:1]]
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
  ;; C-x C-0 restores the default font size

(add-hook 'text-mode-hook
            '(lambda ()
               (visual-line-mode 1)
                   (diminish 'visual-line-mode)
               ))
;; Increase/decrease text size:1 ends here

;; [[file:~/.emacs.d/init.org::*Delete%20Selection%20mode][Delete Selection mode:1]]
  (delete-selection-mode 1)
;; Delete Selection mode:1 ends here

;; [[file:~/.emacs.d/init.org::*Highlight%20&%20complete%20parenthesis%20pair%20when%20cursor%20is%20near%20;-)][Highlight & complete parenthesis pair when cursor is near ;-):1]]
;; Highlight expression within matching parens when near one of them.
(setq show-paren-delay 0)
(setq blink-matching-paren nil)
(setq show-paren-style 'expression)
(show-paren-mode)

;; Colour parens, and other delimiters, depending on their depth.
;; Very useful for parens heavy languages like Lisp.
(use-package rainbow-delimiters)

(add-hook 'org-mode-hook
  '(lambda () (rainbow-delimiters-mode 1)))
(add-hook 'prog-mode-hook
  '(lambda () (rainbow-delimiters-mode 1)))
;; Highlight & complete parenthesis pair when cursor is near ;-):1 ends here

;; [[file:~/.emacs.d/init.org::*Highlight%20&%20complete%20parenthesis%20pair%20when%20cursor%20is%20near%20;-)][Highlight & complete parenthesis pair when cursor is near ;-):3]]
(electric-pair-mode 1)
;; Highlight & complete parenthesis pair when cursor is near ;-):3 ends here

;; [[file:~/.emacs.d/init.org::*Highlight%20&%20complete%20parenthesis%20pair%20when%20cursor%20is%20near%20;-)][Highlight & complete parenthesis pair when cursor is near ;-):4]]
(setq electric-pair-inhibit-predicate
      (lambda (c)
        (or (member c '(?< ?>)) (electric-pair-default-inhibit c))))

(when (< 1 2) 'bye)

;; Act as usual unless a ‘<’ or ‘>’ is encountered.
;; ( char-at is really “character at poisition”; C-h o! )
(setq rainbow-delimiters-pick-face-function
      (lambda (depth match loc)
        (unless (member (char-after loc) '(?< ?>))
          (rainbow-delimiters-default-pick-face depth match loc))))

;; Final piece.
(modify-syntax-entry ?< "(>")
(modify-syntax-entry ?> ")<")
;; Highlight & complete parenthesis pair when cursor is near ;-):4 ends here

;; [[file:~/.emacs.d/init.org::*Highlight%20&%20complete%20parenthesis%20pair%20when%20cursor%20is%20near%20;-)][Highlight & complete parenthesis pair when cursor is near ;-):6]]
(setq electric-pair-pairs
         '(
           (?~ . ?~)
           (?* . ?*)
           (?/ . ?/)
          ))
;; Highlight & complete parenthesis pair when cursor is near ;-):6 ends here

;; [[file:~/.emacs.d/init.org::*Highlight%20&%20complete%20parenthesis%20pair%20when%20cursor%20is%20near%20;-)][Highlight & complete parenthesis pair when cursor is near ;-):7]]
;; Disable pairs when entering minibuffer
(add-hook 'minibuffer-setup-hook (lambda () (electric-pair-mode 0)))

;; Renable pairs when existing minibuffer
(add-hook 'minibuffer-exit-hook (lambda () (electric-pair-mode 1)))
;; Highlight & complete parenthesis pair when cursor is near ;-):7 ends here

;; [[file:~/.emacs.d/init.org::*Minibuffer%20should%20display%20line%20and%20column%20numbers][Minibuffer should display line and column numbers:1]]
; (line-number-mode t)
(column-number-mode t)
;; Minibuffer should display line and column numbers:1 ends here

;; [[file:~/.emacs.d/init.org::*Minibuffer%20should%20display%20line%20and%20column%20numbers][Minibuffer should display line and column numbers:2]]
(global-display-line-numbers-mode t)

;; Have a uniform width for displaying line numbers,
;; rather than having the width grow as necessary.
(setq display-line-numbers-width-start t)
;; Minibuffer should display line and column numbers:2 ends here

;; [[file:~/.emacs.d/init.org::*Never%20lose%20the%20cursor][Never lose the cursor:1]]
(use-package beacon
  :ensure t
  :demand t
  :init
  (setq beacon-color "#666600")
  :config (beacon-mode))
;; Never lose the cursor:1 ends here

;; [[file:~/.emacs.d/init.org::*Neotree:%20Directory%20Tree%20Listing][Neotree: Directory Tree Listing:1]]
;; neotree --sidebar for project file navigation
(use-package neotree
  :config (global-set-key "\C-x\ d" 'neotree-toggle))

;; Only do this once:
(when nil
  (use-package all-the-icons)
  (all-the-icons-install-fonts 'install-without-asking))

(setq neo-theme 'icons)
(neotree-refresh)

;; Open it up upon startup.
(neotree-toggle)
;; Neotree: Directory Tree Listing:1 ends here

;; [[file:~/.emacs.d/init.org::*Life%20within%20Org-mode][Life within Org-mode:2]]
(use-package org
  :ensure org-plus-contrib
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))
;; Life within Org-mode:2 ends here
