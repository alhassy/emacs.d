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
(maybe-clone "https://github.com/alhassy/CatsCheatSheet")
(maybe-clone "https://github.com/alhassy/org-agda-mode")
(maybe-clone "https://github.com/JacquesCarette/TheoriesAndDataStructures")
(maybe-clone "https://github.com/alhassy/islam")
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

(global-set-key "\M-^" 'langtool-check)
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

;; [[file:~/.emacs.d/init.org::*Life%20within%20Org-mode][Life within Org-mode:3]]
(setq org-ellipsis " ⤵")
;; Life within Org-mode:3 ends here

;; [[file:~/.emacs.d/init.org::*Life%20within%20Org-mode][Life within Org-mode:4]]
;; Fold all source blocks on startup.
(setq org-hide-block-startup t)

;; Lists may be labelled with letters.
(setq org-list-allow-alphabetical t)

;; Avoid accidentally editing folded regions, say by adding text after an Org “⋯”.
(setq org-catch-invisible-edits 'show)

;; I use indentation-sensitive programming languages.
;; Tangling should preserve my indentation.
(setq org-src-preserve-indentation t)

;; Tab should do indent in code blocks
(setq org-src-tab-acts-natively t)

;; Give quote and verse blocks a nice look.
(setq org-fontify-quote-and-verse-blocks t)

;; Pressing ENTER on a link should follow it.
(setq org-return-follows-link t)
;; Life within Org-mode:4 ends here

;; [[file:~/.emacs.d/init.org::*Manipulating%20Sections][Manipulating Sections:1]]
(setq org-use-speed-commands t)
;; Manipulating Sections:1 ends here

;; [[file:~/.emacs.d/init.org::*Seamless%20Navigation%20Between%20Source%20Blocks][Seamless Navigation Between Source Blocks:1]]
;; Overriding keys for printing buffer, duplicating gui frame, and isearch-yank-kill.
;;
(define-key org-mode-map (kbd "s-p") #'org-babel-previous-src-block)
(define-key org-mode-map (kbd "s-n") #'org-babel-next-src-block)
(define-key org-mode-map (kbd "s-e") #'org-edit-src-code)
(define-key org-src-mode-map (kbd "s-e") #'org-edit-src-exit)
;; Seamless Navigation Between Source Blocks:1 ends here

;; [[file:~/.emacs.d/init.org::*Modifying%20~<return>~][Modifying ~<return>~:1]]
(add-hook 'org-mode-hook '(lambda ()
  (local-set-key (kbd "<return>") 'org-return-indent))
  (local-set-key (kbd "C-M-<return>") 'electric-indent-just-newline))
;; Modifying ~<return>~:1 ends here

;; [[file:~/.emacs.d/init.org::*~C-a,e,k~%20and%20Yanking%20of%20sections][~C-a,e,k~ and Yanking of sections:1]]
;; On an org-heading, C-a goes to after the star, heading markers.
;; To use speed keys, run C-a C-a to get to the star markers.
;;
;; C-e goes to the end of the heading, not including the tags.
;;
(setq org-special-ctrl-a/e t)

;; C-k no longer removes tags, if activated in the middle of a heading's name.
(setq org-special-ctrl-k t)

;; When you yank a subtree and paste it alongside a subtree of depth ‘d’,
;; then the yanked tree's depth is adjusted to become depth ‘d’ as well.
;; If you don't want this, then refile instead of copy pasting.
(setq org-yank-adjusted-subtrees t)
;; ~C-a,e,k~ and Yanking of sections:1 ends here

;; [[file:~/.emacs.d/init.org::*Using%20org-mode%20as%20a%20Day%20Planner][Using org-mode as a Day Planner:1]]
(setq org-default-notes-file "~/Dropbox/todo.org")
(define-key global-map "\C-cc" 'org-capture)
;; Using org-mode as a Day Planner:1 ends here

;; [[file:~/.emacs.d/init.org::*Using%20org-mode%20as%20a%20Day%20Planner][Using org-mode as a Day Planner:2]]
(cl-defun my/make/org-capture-template
   (shortcut heading &optional (no-todo nil) (description heading) (category heading))
  "Quickly produce an org-capture-template.

  After adding the result of this function to ‘org-capture-templates’,
  we will be able perform a capture with “C-c c ‘shortcut’”
  which will have description ‘description’.
  It will be added to the tasks file under heading ‘heading’
  and be marked with category  ‘category’.

  ‘no-todo’ omits the ‘TODO’ tag from the resulting item; e.g.,
  when it's merely an interesting note that needn't be acted upon.
  ─Probably a bad idea─

  Defaults for ‘description’ and ‘category’ are set to the same as
  the ‘heading’. Default for ‘no-todo’ is ‘nil’.
  "
  `(,shortcut ,description entry
      (file+headline org-default-notes-file
         ,(concat heading "\n#+CATEGORY: " category))
      , (concat "*" (unless no-todo " TODO") " %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n")
      :empty-lines 1)
)

(setq org-capture-templates
  `(
     ,(my/make/org-capture-template "t" "Tasks, Getting Things Done")
     ,(my/make/org-capture-template "r" "Research")
     ,(my/make/org-capture-template "m" "Email")
     ,(my/make/org-capture-template "e" "Emacs (•̀ᴗ•́)و")
     ,(my/make/org-capture-template "b" "Blog")
     ,(my/make/org-capture-template "a" "Arbitrary Reading and Learning")
     ,(my/make/org-capture-template "p" "Personal Matters")
))
;; Using org-mode as a Day Planner:2 ends here

;; [[file:~/.emacs.d/init.org::*Using%20org-mode%20as%20a%20Day%20Planner][Using org-mode as a Day Planner:3]]
  ;; Ensure notes are stored at the top of a tree.
  (setq org-reverse-note-order nil)
;; Using org-mode as a Day Planner:3 ends here

;; [[file:~/.emacs.d/init.org::*Using%20org-mode%20as%20a%20Day%20Planner][Using org-mode as a Day Planner:4]]
(define-key global-map "\C-ca" 'org-agenda)
;; Using org-mode as a Day Planner:4 ends here

;; [[file:~/.emacs.d/init.org::*Using%20org-mode%20as%20a%20Day%20Planner][Using org-mode as a Day Planner:5]]
;; Include agenda archive files when searching for things
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))
;; Using org-mode as a Day Planner:5 ends here

;; [[file:~/.emacs.d/init.org::*Using%20org-mode%20as%20a%20Day%20Planner][Using org-mode as a Day Planner:6]]
;; Invoing the agenda command shows the agenda and enables
;; the org-agenda variables.
(org-agenda "a" "a")
;; Using org-mode as a Day Planner:6 ends here

;; [[file:~/.emacs.d/init.org::*Using%20org-mode%20as%20a%20Day%20Planner][Using org-mode as a Day Planner:7]]
;; Pressing ‘c’ in the org-agenda view shows all completed tasks,
;; which should be archived.
(add-to-list 'org-agenda-custom-commands
  '("c" todo "DONE|ON_HOLD|CANCELLED" nil))
;; Using org-mode as a Day Planner:7 ends here

;; [[file:~/.emacs.d/init.org::*Using%20org-mode%20as%20a%20Day%20Planner][Using org-mode as a Day Planner:8]]
(add-to-list 'org-agenda-custom-commands
  '("u" alltodo ""
     ((org-agenda-skip-function
        (lambda ()
              (org-agenda-skip-entry-if 'scheduled 'deadline 'regexp  "\n]+>")))
              (org-agenda-overriding-header "Unscheduled TODO entries: "))))
;; Using org-mode as a Day Planner:8 ends here

;; [[file:~/.emacs.d/init.org::*Automating%20%5B%5Bhttps://en.wikipedia.org/wiki/Pomodoro_Technique%5D%5BPomodoro%5D%5D%20--Dealing%20with%20dreadful%20tasks][Automating [[https://en.wikipedia.org/wiki/Pomodoro_Technique][Pomodoro]] --Dealing with dreadful tasks:1]]
;; Tasks get a 25 minute count down timer
(setq org-timer-default-timer 25)

;; Use the timer we set when clocking in happens.
(add-hook 'org-clock-in-hook
  (lambda () (org-timer-set-timer '(16))))

;; unless we clocked-out with less than a minute left,
;; show disappointment message.
(add-hook 'org-clock-out-hook
  (lambda ()
  (unless (s-prefix? "0:00" (org-timer-value-string))
     (message-box "The basic 25 minutes on this dreadful task are not up; it's a shame to see you leave."))
     (org-timer-stop)
     ))
;; Automating [[https://en.wikipedia.org/wiki/Pomodoro_Technique][Pomodoro]] --Dealing with dreadful tasks:1 ends here

;; [[file:~/.emacs.d/init.org::*Journaling][Journaling:1]]
(use-package org-journal
  :bind (("C-c j" . org-journal-new-entry))
  :config
  (setq org-journal-dir "~/Dropbox/journal/")
  (setq org-journal-file-type 'yearly)
)
;; Journaling:1 ends here

;; [[file:~/.emacs.d/init.org::*Workflow%20States][Workflow States:1]]
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s@/!)" "|" "DONE(d/!)")
              (sequence "WAITING(w@/!)" "ON_HOLD(h@/!)" "|" "CANCELLED(c@/!)")
             )
      )
)
;; Workflow States:1 ends here

;; [[file:~/.emacs.d/init.org::*Workflow%20States][Workflow States:2]]
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("STARTED" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("ON_HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold))))
;; Workflow States:2 ends here

;; [[file:~/.emacs.d/init.org::*Workflow%20States][Workflow States:3]]
(setq org-use-fast-todo-selection t)
;; Workflow States:3 ends here

;; [[file:~/.emacs.d/init.org::*Workflow%20States][Workflow States:4]]
;; Install the tool
; (async-shell-command "brew cask install java") ;; Dependency
; (async-shell-command "brew install plantuml")

;; Tell emacs where it is.
;; E.g., (async-shell-command "find / -name plantuml.jar")
(setq org-plantuml-jar-path
      (expand-file-name "/usr/local/Cellar/plantuml/1.2019.5/libexec/plantuml.jar"))

;; Enable C-c C-c to generate diagrams from plantuml src blocks.
(add-to-list 'org-babel-load-languages '(plantuml . t) )
(require 'ob-plantuml)

; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))
;; Workflow States:4 ends here

;; [[file:~/.emacs.d/init.org::*Working%20with%20Citations][Working with Citations:1]]
(use-package org-ref :demand t)

;; Files to look at when no “╲bibliography{⋯}” is not present in a file.
;; Most useful for non-LaTeX files.
(setq reftex-default-bibliography '("~/thesis-proposal/References.bib"))

(use-package helm-bibtex :demand t)

(setq bibtex-completion-bibliography "~/thesis-proposal/References.bib")
;; Working with Citations:1 ends here

;; [[file:~/.emacs.d/init.org::*Show%20off-screen%20Heading%20at%20the%20top%20of%20the%20window][Show off-screen Heading at the top of the window:1]]
 (use-package org-sticky-header
  :config
  (setq-default
   org-sticky-header-full-path 'full
   ;; Child and parent headings are seperated by a /.
   org-sticky-header-outline-path-separator " / "))
(org-sticky-header-mode)
;; Show off-screen Heading at the top of the window:1 ends here

;; [[file:~/.emacs.d/init.org::*Clocking%20Work%20Time][Clocking Work Time:1]]
;; Record a note on what was acciomplished when clocking out of an item.
(setq org-log-note-clock-out t)
;; Clocking Work Time:1 ends here

;; [[file:~/.emacs.d/init.org::*Clocking%20Work%20Time][Clocking Work Time:2]]
;; List of all the files where todo items can be found. Only one for now.
(setq org-agenda-files '("~/Dropbox/todo.org"))

;; How many days ahead the default agenda view should look
(setq org-agenda-ndays 7)

;; How many days early a deadline item will begin showing up in your agenda list.
(setq org-deadline-warning-days 14)

;; In the agenda view, days that have no associated tasks will still have a line showing the date.
(setq org-agenda-show-all-dates t)

(setq org-agenda-skip-deadline-if-done t)

;; Scheduled items marked as complete will not show up in your agenda view.
(setq org-agenda-skip-scheduled-if-done t)

;; The agenda view – even in the 7-days-at-a-time view – will always begin on the current day.
;; This is important, since while using org-mode as a day planner, you never want to think of
;; days gone past. That’s something you do in other ways, such as when reviewing completed tasks.
(setq org-agenda-start-on-weekday nil)
;; Clocking Work Time:2 ends here

;; [[file:~/.emacs.d/init.org::*Clocking%20Work%20Time][Clocking Work Time:3]]
(setq confirm-kill-emacs 'yes-or-no-p)
;; Clocking Work Time:3 ends here

;; [[file:~/.emacs.d/init.org::*Clocking%20Work%20Time][Clocking Work Time:4]]
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)

;; Show lot of clocking history
(setq org-clock-history-length 23)

;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)

;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)

;; Do not prompt to resume an active clock
;; (setq org-clock-persist-query-resume nil)

;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)
;; Clocking Work Time:4 ends here

;; [[file:~/.emacs.d/init.org::*Clocking%20Work%20Time][Clocking Work Time:5]]
(setq org-clock-sound "~/.emacs.d/school-bell.wav")
;; Clocking Work Time:5 ends here

;; [[file:~/.emacs.d/init.org::*%5B%5Bhttps://revealjs.com/?transition=zoom#/%5D%5BReveal.JS%5D%5D%20--%20The%20HTML%20Presentation%20Framework][[[https://revealjs.com/?transition=zoom#/][Reveal.JS]] -- The HTML Presentation Framework:1]]
(use-package ox-reveal
 :config (setq org-reveal-root "https://cdn.jsdelivr.net/reveal.js/3.0.0/"))
;; [[https://revealjs.com/?transition=zoom#/][Reveal.JS]] -- The HTML Presentation Framework:1 ends here

;; [[file:~/.emacs.d/init.org::*Coloured%20LaTeX%20using%20Minted][Coloured LaTeX using Minted:1]]
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -output-directory %o %f"
        "biber %b"
        "pdflatex -shell-escape -output-directory %o %f"
        "pdflatex -shell-escape -output-directory %o %f")
)
;; Coloured LaTeX using Minted:1 ends here

;; [[file:~/.emacs.d/init.org::*Executing%20code%20from%20~src~%20blocks][Executing code from ~src~ blocks:1]]
; Seamless use of babel: No confirmation upon execution.
;; Downside: Could accidentally evaluate harmful code.
(setq org-confirm-babel-evaluate nil)
;; Executing code from ~src~ blocks:1 ends here

;; [[file:~/.emacs.d/init.org::*Executing%20code%20from%20~src~%20blocks][Executing code from ~src~ blocks:2]]
 (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (emacs-lisp . t)
     ;; (shell	 . t)
     (python . t)
     (haskell . t)
     (ruby	 . t)
     (ocaml	 . t)
     (C . t)  ;; Captial “C” gives access to C, C++, D
     (dot	 . t)
     (latex	 . t)
     (org	 . t)
     (makefile	 . t)
     ))

;; Preserve my indentation for source code during export.
(setq org-src-preserve-indentation t)

;; The export process hangs Emacs, let's avoid this.
;; MA: For one reason or another, this crashes more than I'd like.
;; (setq org-export-in-background t)
;; Executing code from ~src~ blocks:2 ends here

;; [[file:~/.emacs.d/init.org::*Hiding%20Emphasise%20Markers%20&%20Inlining%20Images][Hiding Emphasise Markers & Inlining Images:1]]
;; org-mode math is now highlighted ;-)
(setq org-highlight-latex-and-related '(latex))

;; Hide the *,=,/ markers
(setq org-hide-emphasis-markers t)

;; (setq org-pretty-entities t)
;; to have \alpha, \to and others display as utf8 http://orgmode.org/manual/Special-symbols.html
;; Hiding Emphasise Markers & Inlining Images:1 ends here

;; [[file:~/.emacs.d/init.org::*Jumping%20without%20hassle][Jumping without hassle:1]]
(defun my/org-goto-line (line)
  "Go to the indicated line, unfolding the parent Org header.

   Implementation: Go to the line, then look at the 1st previous
   org header, now we can unfold it whence we do so, then we go
   back to the line we want to be at.
  "
  (interactive "nEnter line: ")
  (goto-line line)
  (org-previous-visible-heading 1)
  (org-cycle)
  (goto-line line)
)
;; Jumping without hassle:1 ends here

;; [[file:~/.emacs.d/init.org::*Folding%20within%20a%20subtree][Folding within a subtree:1]]
(defun my/org-fold-current-subtree-anywhere-in-it ()
  "Hide the current heading, while being anywhere inside it."
  (interactive)
  (save-excursion
    (org-narrow-to-subtree)
    (org-shifttab)
    (widen))
)

(add-hook 'org-mode-hook '(lambda ()
  (local-set-key (kbd "C-c C-h") 'my/org-fold-current-subtree-anywhere-in-it)))
;; Folding within a subtree:1 ends here

;; [[file:~/.emacs.d/init.org::*Making%20then%20opening%20html's%20from%20org's][Making then opening html's from org's:1]]
(cl-defun my/org-html-export-to-html (&optional (filename (buffer-name)))
  "Produce an HTML from the given ‘filename’, or otherwise current buffer,
   then open it in my default brower.
  "
 (interactive)
 (org-html-export-to-html)
 (let ((it (concat (file-name-sans-extension buffer-file-name) ".html")))
   (browse-url it)
   (message (concat it " has been opened in Chromium."))
   'success ;; otherwise we obtain a "compiler error".
 )
)
;; Making then opening html's from org's:1 ends here

;; [[file:~/.emacs.d/init.org::*Making%20then%20opening%20pdf's%20from%20org's][Making then opening pdf's from org's:1]]
(cl-defun my/org-latex-export-to-pdf (&optional (filename (buffer-name)))
  "Produce a PDF from the given ‘filename’, or otherwise current buffer,
   then open it in my default viewer.
  "
 (interactive)
 (org-latex-export-to-pdf)
 (let ((it (concat (file-name-sans-extension filename) ".pdf")))
   (eshell-command (concat "open " it  " & ")))
   (message (concat it " has been opened in your PDF viewer."))
   'success ;; otherwise we obtain a "compiler error".
)
;; Making then opening pdf's from org's:1 ends here

;; [[file:~/.emacs.d/init.org::*Interpret%20the%20Haskell%20source%20blocks%20in%20a%20file][Interpret the Haskell source blocks in a file:1]]
(defvar *current-module* "NoModuleNameSpecified"
  "The name of the module, file, that source blocks are
   currently being tangled to.

   This technique is insipired by “Interactive Way to C”;
   see https://alhassy.github.io/InteractiveWayToC/.
  ")

(defun current-module ()
  "Returns the current module under focus."
  *current-module*)

(defun set-module (name)
   "Set the name of the module currently under focus.

    Usage: When a module is declared, i.e., a new file has begun,
    then that source blocks header should be “:tangle (set-module ”name-here”)”.
    succeeding source blocks now inherit this name and so are tangled
    to the same module file. How? By placing the following line at the top
    of your Org file: “‘#+PROPERTY: header-args :tangle (current-module))’.

    This technique structures “Interactive Way to C”.
   "
   (setq *current-module* name)
)

(cl-defun my/org-run-haskell (&optional target (filename (buffer-name)))
  "Tangle Haskell source blocks of given ‘filename’, or otherwise current buffer,
   and load the resulting ‘target’ file into a ghci buffer.

   If no name is provided for the ‘target’ file that is generated from the
   tangeling process, it is assumed to be the buffer's name with a ‘hs’ extension.

   Note that this only loads the blocks tangled to ‘target’.

   For example, file ‘X.org’ may have haskell blocks that tangle to files
   ‘X.hs’, ‘Y.hs’ and ‘Z.hs’. If no target name is supplied, we tangle all blocks
   but only load ‘X.hs’ into the ghci buffer. A helpful technique to load the
   last, bottom most, defined haskell module, is to have the module declaration's
   source block be ‘:tangle (setq CODE “Y.hs”)’, for example; then the following
   code blocks will inherit this location provided our Org file has at the top
   ‘#+PROPERTY: header-args :tangle (current-module))’.
   Finally, our ‘compile-command’ suffices to be ‘(my/org-run-haskell CODE)’.
   ─
   This technique structures “Interactive Way to C”.
  "
   (let* ((it  (if target target (concat (file-name-sans-extension filename) ".hs")))
         (buf (concat "*GHCI* " it)))

     (-let [kill-buffer-query-functions nil] (ignore-errors (kill-buffer buf)))
     (org-babel-tangle it "haskell")
     (async-shell-command (concat "ghci " it) buf)
     (switch-to-buffer-other-window buf)
     (end-of-buffer)
   )
)

;; Set this as the ‘compile-command’ in ‘Local Variables’, for example.
;; Interpret the Haskell source blocks in a file:1 ends here

;; [[file:~/.emacs.d/init.org::*Expected%20IDE%20Support][Expected IDE Support:1]]
;; Use 4 spaces in places of tabs when indenting.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; Expected IDE Support:1 ends here

;; [[file:~/.emacs.d/init.org::*Backups][Backups:1]]
;; New location for backups.
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Never silently delete old backups.
(setq delete-old-versions -1)

;; Use version numbers for backup files.
(setq version-control t)

;; Even version controlled files get to be backed up.
(setq vc-make-backup-files t)
;; Backups:1 ends here

;; [[file:~/.emacs.d/init.org::*Backups][Backups:2]]
(use-package backup-walker
  :commands backup-walker-start)
;; Backups:2 ends here

;; [[file:~/.emacs.d/init.org::*Highlighting%20TODO-s%20&%20Showing%20them%20in%20Magit][Highlighting TODO-s & Showing them in Magit:1]]
;; NOTE that the highlighting works even in comments.
(use-package hl-todo
  :config
  ;; Enable it for text-like locations
  (add-hook 'text-mode-hook (lambda () (hl-todo-mode t)))
  ;; Adding some new keywords: TEST, WK, MA, JC.
  (add-to-list 'hl-todo-keyword-faces '("TEST" . "#dc8cc3"))
  (add-to-list 'hl-todo-keyword-faces '("MA" . "#dc8cc3"))
  (add-to-list 'hl-todo-keyword-faces '("WK" . "#dc8cc3"))
  (add-to-list 'hl-todo-keyword-faces '("JC" . "#dc8cc3"))
)
;; Highlighting TODO-s & Showing them in Magit:1 ends here

;; [[file:~/.emacs.d/init.org::*Highlighting%20TODO-s%20&%20Showing%20them%20in%20Magit][Highlighting TODO-s & Showing them in Magit:2]]
;; MA: The todo keywords work in code too!
(use-package magit-todos
  :after magit
  :after hl-todo
  :config
  ;; For some reason cannot use :custom with this package.
  (custom-set-variables
    '(magit-todos-keywords (list "TODO" "FIXME" "MA" "WK" "JC")))
  (magit-todos-mode))
;; Highlighting TODO-s & Showing them in Magit:2 ends here

;; [[file:~/.emacs.d/init.org::*Taking%20a%20tour%20of%20one's%20edits][Taking a tour of one's edits:1]]
(use-package goto-chg
   ;; Give me a description of the change made at a particular stop.
  :init (setq glc-default-span 0)
  :bind (("C-c e ," . goto-last-change)
         ("C-c e ." . goto-last-change-reverse)))
;; Taking a tour of one's edits:1 ends here

;; [[file:~/.emacs.d/init.org::*Edit%20as%20Root][Edit as Root:1]]
(defun find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
root-privileges (using tramp/sudo), if the file is not writable by
user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))

(bind-key "C-x F" 'find-file-as-root)
;; Edit as Root:1 ends here

;; [[file:~/.emacs.d/init.org::*Enabling%20CamelCase%20Aware%20Editing%20Operations][Enabling CamelCase Aware Editing Operations:1]]
(global-subword-mode 1)
;; Enabling CamelCase Aware Editing Operations:1 ends here

;; [[file:~/.emacs.d/init.org::*Keep%20buffers%20open%20across%20sessions][Keep buffers open across sessions:1]]
;; Keep open files open across sessions.
(desktop-save-mode 1)
(setq desktop-restore-eager 10)
;; Keep buffers open across sessions:1 ends here

;; [[file:~/.emacs.d/init.org::*Mouse%20Editing%20Support][Mouse Editing Support:1]]
;; Text selected with the mouse is automatically copied to clipboard.
(setq mouse-drag-copy-region t)
;; Mouse Editing Support:1 ends here

;; [[file:~/.emacs.d/init.org::*Dimming%20Unused%20Windows][Dimming Unused Windows:1]]
(use-package dimmer
  :config (dimmer-mode))
;; Dimming Unused Windows:1 ends here

;; [[file:~/.emacs.d/init.org::*Having%20a%20workspace%20manager%20in%20Emacs][Having a workspace manager in Emacs:1]]
(use-package perspective)

;; Activate it.
(persp-mode)

;; In the modeline, tell me which workspace I'm in.
(persp-turn-on-modestring)
;; Having a workspace manager in Emacs:1 ends here

;; [[file:~/.emacs.d/init.org::*Jump%20between%20windows%20using%20Cmd+Arrow%20&%20between%20recent%20buffers%20with%20Meta-Tab][Jump between windows using Cmd+Arrow & between recent buffers with Meta-Tab:1]]
(use-package windmove
  :config
  ;; use command key on Mac
  (windmove-default-keybindings 'super)
  ;; wrap around at edges
  (setq windmove-wrap-around t))
;; Jump between windows using Cmd+Arrow & between recent buffers with Meta-Tab:1 ends here

;; [[file:~/.emacs.d/init.org::*Jump%20between%20windows%20using%20Cmd+Arrow%20&%20between%20recent%20buffers%20with%20Meta-Tab][Jump between windows using Cmd+Arrow & between recent buffers with Meta-Tab:2]]
(use-package buffer-flip
  :bind
   (:map buffer-flip-map
    ("M-<tab>" . buffer-flip-forward)
    ("M-S-<tab>" . buffer-flip-backward)
    ("C-g" . buffer-flip-abort))
  :config
    (setq buffer-flip-skip-patterns
        '("^\\*helm\\b"))
)
;; key to begin cycling buffers.
(global-set-key (kbd "M-<tab>") 'buffer-flip)
;; Jump between windows using Cmd+Arrow & between recent buffers with Meta-Tab:2 ends here

;; [[file:~/.emacs.d/init.org::*Completion%20Frameworks][Completion Frameworks:1]]
(use-package helm
 :diminish
 :init (helm-mode t)
 :bind
  ("C-x C-r" . helm-recentf)      ; search for recently edited

  ;; Helm provides generic functions for completions to replace
  ;; tab-completion in Emacs with no loss of functionality.
  ("M-x" . 'helm-M-x)
  ;; ("C-x b". 'helm-buffers-list) ;; Avoid seeing all those *helm⋯* mini buffers!
  ("C-x b". 'helm-mini) ;; see buffers & recent files; more useful.
  ("C-x r b" .'helm-filtered-bookmarks)
  ("C-x C-f" . 'helm-find-files)

  ;; A menu of all “top-level items” in a file; e.g.,
  ;; functions and constants in source code or headers in an org-mode file.
  ;;
  ;; Nifty way to familarise yourself with a new code base, or one from a while ago.
  ;;
  ("C-c i" . 'helm-imenu)

   ;; Show all meaningful Lisp symbols whose names match a given pattern.
   ;; Helpful for looking up commands.
   ("C-h a" . helm-apropos)

   ;; Look at what was cut recently & paste it in.
   ("M-y" . helm-show-kill-ring)
)
;; (global-set-key (kbd "M-x") 'execute-extended-command) ;; Default “M-x”

;; Yet, let's keep tab-completetion anyhow.
(define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
(define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
;; We can list ‘actions’ on the currently selected item by C-z.
(define-key helm-map (kbd "C-z")  'helm-select-action)
;; Completion Frameworks:1 ends here

;; [[file:~/.emacs.d/init.org::*Completion%20Frameworks][Completion Frameworks:2]]
(setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf
                                    helm-source-bookmarks
                                    helm-source-bookmark-set
                                    helm-source-buffer-not-found))
;; Completion Frameworks:2 ends here

;; [[file:~/.emacs.d/init.org::*Completion%20Frameworks][Completion Frameworks:3]]
;; (shell-command "brew install surfraw &")
;;
;; Invoke helm-surfraw
;; Completion Frameworks:3 ends here

;; [[file:~/.emacs.d/init.org::*Completion%20Frameworks][Completion Frameworks:4]]
(use-package helm-swoop
  :bind
  (
   ("C-s"     . 'helm-swoop)           ;; search current buffer
   ("C-M-s"   . 'helm-multi-swoop-all) ;; Search all buffer
   ;; Go back to last position where ‘helm-swoop’ was called
   ("C-S-s" . 'helm-swoop-back-to-last-point)
  )
 :config ;; Following from helm-swoop's github page.
   ;; Give up colour for speed.
  (setq helm-swoop-speed-or-color nil)
  ;; If this value is t, split window inside the current window
  (setq helm-swoop-split-with-multiple-windows nil)

)
;; Completion Frameworks:4 ends here

;; [[file:~/.emacs.d/init.org::*Completion%20Frameworks][Completion Frameworks:7]]
(use-package company
  :diminish
  :config
    (setq company-dabbrev-other-buffers t
          company-dabbrev-code-other-buffers t

          ;; Allow (lengthy) numbers to be eligible for completion.
          company-complete-number t

          ;; M-⟪num⟫ to select an option according to its number.
          company-show-numbers t

          ;; Only 2 letters required for completion to activate.
          company-minimum-prefix-length 2

          ;; Do not downcase completions by default.
          company-dabbrev-downcase nil

          ;; Even if I write something with the ‘wrong’ case,
          ;; provide the ‘correct’ casing.
          company-dabbrev-ignore-case t

          ;; Immediately activate completion.
          company-idle-delay 0
          )

    (global-company-mode 1)
)
;; So fast that we don't need this.
;; (global-set-key (kbd "C-c h") 'company-complete)
;; Completion Frameworks:7 ends here

;; [[file:~/.emacs.d/init.org::*Completion%20Frameworks][Completion Frameworks:8]]
(use-package company-emoji)
(add-to-list 'company-backends 'company-emoji)
;; Completion Frameworks:8 ends here

;; [[file:~/.emacs.d/init.org::*Completion%20Frameworks][Completion Frameworks:9]]
(use-package emojify
 :config (setq emojify-display-style 'image)
 :init (global-emojify-mode 1) ;; Will install missing images, if need be.
)
;; Completion Frameworks:9 ends here

;; [[file:~/.emacs.d/init.org::*Completion%20Frameworks][Completion Frameworks:11]]
(use-package company-quickhelp
 :config
   (setq company-quickhelp-delay 0.1)
   (company-quickhelp-mode)
)
;; Completion Frameworks:11 ends here

;; [[file:~/.emacs.d/init.org::*Helpful%20Utilities%20&%20Shortcuts][Helpful Utilities & Shortcuts:1]]
;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable ‘possibly confusing commands’
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
;; Helpful Utilities & Shortcuts:1 ends here

;; [[file:~/.emacs.d/init.org::*Bind%20~recompile~%20to%20~C-c%20C-m~%20--%20%E2%80%9Cm%E2%80%9D%20for%20%E2%80%9Cm%E2%80%9Dake][Bind ~recompile~ to ~C-c C-m~ -- “m” for “m”ake:1]]
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-m") 'recompile)
    map)
  "my-keys-minor-mode keymap.")

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " my-keys")

(my-keys-minor-mode)

(diminish 'my-keys-minor-mode) ;; Don't show it in the modeline.
;; Bind ~recompile~ to ~C-c C-m~ -- “m” for “m”ake:1 ends here

;; [[file:~/.emacs.d/init.org::*Reload%20buffer%20with%20~f5~][Reload buffer with ~f5~:1]]
(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t nil)))
;; Reload buffer with ~f5~:1 ends here

;; [[file:~/.emacs.d/init.org::*Reload%20buffer%20with%20~f5~][Reload buffer with ~f5~:2]]
;; Auto update buffers that change on disk.
;; Will be prompted if there are changes that could be lost.
(global-auto-revert-mode 1)
;; Reload buffer with ~f5~:2 ends here

;; [[file:~/.emacs.d/init.org::*Kill%20to%20start%20of%20line][Kill to start of line:1]]
;; M-k kills to the left
(global-set-key "\M-k" '(lambda () (interactive) (kill-line 0)) )
;; Kill to start of line:1 ends here

;; [[file:~/.emacs.d/init.org::*~file-as-list~%20and%20~file-as-string~][~file-as-list~ and ~file-as-string~:1]]
(defun file-as-list (filename)
  "Return the contents of FILENAME as a list of lines"
  (with-temp-buffer
    (insert-file-contents filename)
    (split-string (buffer-string))))

(defun file-as-string (filename)
  "Return the contents of FILENAME as a list of lines"
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))
;; ~file-as-list~ and ~file-as-string~:1 ends here

;; [[file:~/.emacs.d/init.org::*~kill-other-buffers~][~kill-other-buffers~:1]]
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
;; ~kill-other-buffers~:1 ends here

;; [[file:~/.emacs.d/init.org::*Switching%20from%202%20horizontal%20windows%20to%202%20vertical%20windows][Switching from 2 horizontal windows to 2 vertical windows:1]]
(defun ensure-two-vertical-windows ()
  "I used this method often when programming in Coq."
 (interactive)
 (other-window 1)			;; C-x 0
 (let ((otherBuffer (buffer-name)))
   (delete-window)			;; C-x 0
   (split-window-right)			;; C-x 3
   (other-window 1)			;; C-x 0
   (switch-to-buffer otherBuffer)	;; C-x b RET
 )
 (other-window 1)
)
(global-set-key (kbd "C-|") 'ensure-two-vertical-windows)
;; Switching from 2 horizontal windows to 2 vertical windows:1 ends here

;; [[file:~/.emacs.d/init.org::*~re-replace-in-file~][~re-replace-in-file~:1]]
(defun re-replace-in-file (file regex whatDo)
   "Find and replace a regular expression in-place in a file.

   Terrible function … before I took the time to learn any Elisp!
   "

    (find-file file)
    (goto-char 0)
    (let ((altered (replace-regexp-in-string regex whatDo (buffer-string))))
      (erase-buffer)
      (insert altered)
      (save-buffer)
      (kill-buffer)
   )
)
;; ~re-replace-in-file~:1 ends here

;; [[file:~/.emacs.d/init.org::*~mapsto~:%20Simple%20rewriting%20for%20current%20buffer][~mapsto~: Simple rewriting for current buffer:1]]
(defun mapsto (this that)
  "In the current buffer make the regular expression rewrite: this ↦ that."
  (let* ((current-location (point))
       ;; Do not alter the case of the <replacement text>.
       (altered (replace-regexp-in-string this (lambda (x) that) (buffer-string) 'no-fixed-case))
       )
      (erase-buffer)
      (insert altered)
      (save-buffer)
      (goto-char current-location)
  )
)
;; ~mapsto~: Simple rewriting for current buffer:1 ends here

;; [[file:~/.emacs.d/init.org::*Obtaining%20Values%20of%20~#+KEYWORD~%20Annotations][Obtaining Values of ~#+KEYWORD~ Annotations:1]]
;; Src: http://kitchingroup.cheme.cmu.edu/blog/2013/05/05/Getting-keyword-options-in-org-files/
(defun org-keywords ()
  "Parse the buffer and return a cons list of (property . value) from lines like: #+PROPERTY: value"
  (org-element-map (org-element-parse-buffer 'element) 'keyword
                   (lambda (keyword) (cons (org-element-property :key keyword)
                                           (org-element-property :value keyword)))))

(defun org-keyword (KEYWORD)
  "Get the value of a KEYWORD in the form of #+KEYWORD: value"
  (cdr (assoc KEYWORD (org-keywords))))
;; Obtaining Values of ~#+KEYWORD~ Annotations:1 ends here

;; [[file:~/.emacs.d/init.org::*Quickly%20pop-up%20a%20terminal,%20run%20a%20command,%20close%20it][Quickly pop-up a terminal, run a command, close it:1]]
(cl-defun toggle-terminal (&optional (name "*eshell-pop-up*"))
   "Pop up a terminal, do some work, then close it using the same command.

   The toggle behaviour is tied into the existence of the pop-up buffer.
   If the buffer exists, kill it; else create it.
   "
   (interactive)
   (cond
     ;; when the terminal buffer is alive, kill it.
     ((get-buffer name)  (kill-buffer name)
                         (ignore-errors (delete-window)))
     ;; otherwise, set value to refer to a new eshell buffer.
     (t                  (split-window-right)
                         (other-window 1)
                         (eshell)
                         (rename-buffer name))
   )
)

(global-set-key "\C-t" 'toggle-terminal)
;; Quickly pop-up a terminal, run a command, close it:1 ends here

;; [[file:~/.emacs.d/init.org::*~C-x%20k~%20kills%20current%20buffer][~C-x k~ kills current buffer:1]]
;; Kill current buffer; prompt only if
;; there are unsaved changes.
(global-set-key (kbd "C-x k")
  '(lambda () (interactive) (kill-buffer (current-buffer))))
;; ~C-x k~ kills current buffer:1 ends here

;; [[file:~/.emacs.d/init.org::*Publishing%20articles%20to%20my%20personal%20blog][Publishing articles to my personal blog:1]]
(define-key global-map "\C-cb" 'my/publish-to-blog)

(cl-defun my/publish-to-blog (&optional (draft nil) (local nil))
  "
  Using ‘AlBasmala’ setup to publish current article to my blog.
  Details of AlBasmala can be found here:
  https://alhassy.github.io/AlBasmala/

  Locally: ~/alhassy.github.io/content/AlBasmala.org

  A ‘draft’ will be produced in about ~7 seconds, but does not re-produce
  a PDF and the article has a draft marker near the top. Otherwise,
  it will generally take ~30 seconds due to PDF production, which is normal.
  The default is not a draft and it takes ~20 seconds for the live
  github.io page to update.

  The ‘local’ optiona indicates whether the resulting article should be
  viewed using the local server or the live webpage. Live page is default.

  When ‘draft’ and ‘local’ are both set, the resulting page may momentarily
  show a page-not-found error, simply refresh.
  "

  (load-file "~/alhassy.github.io/content/AlBasmala.el")

  ;; --MOVE ME TO ALBASMALA--
  ;; Sometimes the file I'm working with is not a .org file, so:
  (setq file.org (buffer-name))

  (preview-article :draft draft)
  (unless draft (publish))
  (let ((server (if local "http://localhost:4000/" "https://alhassy.github.io/")))
    (async-shell-command (concat "open " server NAME "/") "*blog-post-in-browser*"))
)
;; Publishing articles to my personal blog:1 ends here

;; [[file:~/.emacs.d/init.org::*Excellent%20PDF%20Viewer][Excellent PDF Viewer:1]]
;; First: (async-shell-command "brew install --HEAD dunn/homebrew-emacs/pdf-tools")

;; Then:
(use-package pdf-tools
  :ensure t
  :config
  (custom-set-variables
    '(pdf-tools-handle-upgrades nil))
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo"))

;; Finally:
(pdf-tools-install)

;; Now PDFs opened in Emacs are in pdfview-mode.
;; Excellent PDF Viewer:1 ends here

;; [[file:~/.emacs.d/init.org::*Undo%20tree][Undo tree:1]]
;; Allow tree-semantics for undo operations.
(package-install 'undo-tree)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; Execute (undo-tree-visualize) then navigate along the tree to witness
;; changes being made to your file live!

;; Each node in the undo tree should have a timestamp.
(setq undo-tree-visualizer-timestamps t)

;; Show a diff window displaying changes between undo nodes.
(setq undo-tree-visualizer-diff t)
;; Undo tree:1 ends here

;; [[file:~/.emacs.d/init.org::*Using%20Emacs%20in%20any%20text%20area%20on%20my%20OS][Using Emacs in any text area on my OS:2]]
(add-hook 'ea-popup-hook
  (lambda (app-name window-title x y w h)
   (org-mode)
   (set-input-method "Agda")
  )
)
;; Using Emacs in any text area on my OS:2 ends here
