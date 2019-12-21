;; [[file:~/.emacs.d/init.org::*Support for ‘Custom’][Support for ‘Custom’:1]]
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;; Support for ‘Custom’:1 ends here

;; [[file:~/.emacs.d/init.org::*Support for ‘Custom’][Support for ‘Custom’:2]]
(setq enable-local-variables :safe)
;; Support for ‘Custom’:2 ends here

;; [[file:~/.emacs.d/init.org::*=use-package= ---The start of =init.el=][=use-package= ---The start of =init.el=:1]]
;; Make all commands of the “package” module present.
(require 'package)

;; Internet repositories for new packages.
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")))

;; Actually get “package” to work.
(package-initialize)
(package-refresh-contents)
;; =use-package= ---The start of =init.el=:1 ends here

;; [[file:~/.emacs.d/init.org::*=use-package= ---The start of =init.el=][=use-package= ---The start of =init.el=:2]]
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
;; =use-package= ---The start of =init.el=:2 ends here

;; [[file:~/.emacs.d/init.org::*=use-package= ---The start of =init.el=][=use-package= ---The start of =init.el=:3]]
(setq use-package-always-ensure t)
;; =use-package= ---The start of =init.el=:3 ends here

;; [[file:~/.emacs.d/init.org::*=use-package= ---The start of =init.el=][=use-package= ---The start of =init.el=:4]]
(use-package auto-package-update
  :config
  ;; Delete residual old versions
  (setq auto-package-update-delete-old-versions t)
  ;; Do not bother me when updates have taken place.
  (setq auto-package-update-hide-results t)
  ;; Update installed packages at startup if there is an update pending.
  (auto-package-update-maybe))
;; =use-package= ---The start of =init.el=:4 ends here

;; [[file:~/.emacs.d/init.org::*=use-package= ---The start of =init.el=][=use-package= ---The start of =init.el=:5]]
;; Making it easier to discover Emacs key presses.
(use-package which-key
  :diminish
  :config (which-key-mode)
          (which-key-setup-side-window-bottom)
          (setq which-key-idle-delay 0.05))
;; =use-package= ---The start of =init.el=:5 ends here

;; [[file:~/.emacs.d/init.org::*=use-package= ---The start of =init.el=][=use-package= ---The start of =init.el=:6]]
(use-package diminish
  :config ;; Let's hide some markers.
    (diminish 'eldoc-mode)
    (diminish 'org-indent-mode)
    (diminish 'subword-mode))
;; =use-package= ---The start of =init.el=:6 ends here

;; [[file:~/.emacs.d/init.org::*=use-package= ---The start of =init.el=][=use-package= ---The start of =init.el=:7]]
;; Efficient version control.
(use-package magit
  :config (global-set-key (kbd "C-x g") 'magit-status))

(use-package htmlize)
;; Main use: Org produced htmls are coloured.
;; Can be used to export a file into a coloured html.

;; Quick BibTeX references, sometimes.
(use-package biblio)

;; Get org-headers to look pretty! E.g., * → ⊙, ** ↦ ◯, *** ↦ ★
;; https://github.com/emacsorphanage/org-bullets
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; Haskell's cool
(use-package haskell-mode)

;; Lisp libraries with Haskell-like naming.
(use-package dash)    ;; “A modern list library for Emacs”
(use-package s   )    ;; “The long lost Emacs string manipulation library”.

;; Library for working with system files;
;; e.g., f-delete, f-mkdir, f-move, f-exists?, f-hidden?
(use-package f)
;; =use-package= ---The start of =init.el=:7 ends here

;; [[file:~/.emacs.d/init.org::enable making init and readme][enable making init and readme]]
(defun my/make-init-el-and-README ()
    (interactive "P") ;; Places value of universal argument into: current-prefix-arg
    (when current-prefix-arg
      (let* ((time      (current-time))
                 (_date     (format-time-string "_%Y-%m-%d"))
                 (.emacs    "~/.emacs")
                 (.emacs.el "~/.emacs.el"))

        (save-excursion
          ;; remove any other initialisation file candidates
          (ignore-errors
            (f-move .emacs    (concat .emacs _data))
            (f-move .emacs.el (concat .emacs.el _data)))

          ;; Make init.el
          (org-babel-tangle)
          ; (byte-compile-file "~/.emacs.d/init.el")
          (load-file "~/.emacs.d/init.el")

          ;; Make README.org
          (org-babel-goto-named-src-block "make-readme")
          (org-babel-execute-src-block)

          ;; Acknowledgement
          (message "Tangled, compiled, and loaded init.el; and made README.md … %.06f seconds"
                   (float-time (time-since time)))))))

  (add-hook 'after-save-hook 'my/make-init-el-and-README nil 'local-to-this-file-please)
;; enable making init and readme ends here

;; [[file:~/.emacs.d/init.org::*~README~ ---From ~init.org~ to ~init.el~][~README~ ---From ~init.org~ to ~init.el~:5]]
(use-package toc-org
  ;; Automatically update toc when saving an Org file.
  :hook (org-mode . toc-org-mode))

;; Make toc-org links appear to be the same as their visible text.
(defun toc-org-hrefify-org (str &optional hash)
  "Given a heading, transform it into a href using the org-mode rules."
  (toc-org-format-visible-link str))
;; ~README~ ---From ~init.org~ to ~init.el~:5 ends here

;; [[file:~/.emacs.d/init.org::*~README~ ---From ~init.org~ to ~init.el~][~README~ ---From ~init.org~ to ~init.el~:6]]
(cl-defun my/org-replace-tree-contents (heading &key (with "") (offset 0))
  "Replace the contents of org tree HEADING with WITH, starting at OFFSET.

Clear a subtree leaving first 3 lines untouched  ⇐  :offset 3
Deleting a tree & its contents                   ⇐  :offset -1, or any negative number.
Do nothing to a tree of 123456789 lines          ⇐  :offset 123456789

Precondition: offset < most-positive-fixnum; else we wrap to a negative number."
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (re-search-forward (format "^\\*+ %s" (regexp-quote heading)))
    ;; To avoid ‘forward-line’ from spilling onto other trees.
    (org-narrow-to-subtree)
    (org-mark-subtree)
    ;; The 1+ is to avoid the heading.
    (dotimes (_ (1+ offset)) (forward-line))
    (delete-region (region-beginning) (region-end))
    (insert with)
    (widen)))

;; Erase :TOC: body.
;; (my/org-replace-tree-contents "Table of Contents")
;; ~README~ ---From ~init.org~ to ~init.el~:6 ends here

;; [[file:~/.emacs.d/init.org::*Installing Emacs packages directly from source][Installing Emacs packages directly from source:1]]
(use-package quelpa-use-package)
;; Installing Emacs packages directly from source:1 ends here

;; [[file:~/.emacs.d/init.org::*Installing Emacs packages directly from source][Installing Emacs packages directly from source:2]]
(use-package info+
  :quelpa (info+ :fetcher wiki :url "https://www.emacswiki.org/emacs/info%2b.el"))
;; Installing Emacs packages directly from source:2 ends here

;; [[file:~/.emacs.d/init.org::*=magit= ---Emacs' porcelain interface to git][=magit= ---Emacs' porcelain interface to git:1]]
;; See here for a short & useful tutorial:
;; https://alvinalexander.com/git/git-show-change-username-email-address
(when (equal ""
(shell-command-to-string "git config user.name"))
  (shell-command "git config --global user.name \"Musa Al-hassy\"")
  (shell-command "git config --global user.email \"alhassy@gmail.com\""))
;; =magit= ---Emacs' porcelain interface to git:1 ends here

;; [[file:~/.emacs.d/init.org::*=magit= ---Emacs' porcelain interface to git][=magit= ---Emacs' porcelain interface to git:2]]
(use-package magit)

;; Do not ask about this variable when cloning.
(setq magit-clone-set-remote.pushDefault t)

(cl-defun maybe-clone (remote &optional (local (concat "~/" (file-name-base remote))))
  "Clone a REMOTE repository if the LOCAL directory does not exist.

Yields ‘repo-already-exists’ when no cloning transpires,
otherwise yields ‘cloned-repo’.

LOCAL is optional and defaults to the base name; e.g.,
if REMOTE is https://github.com/X/Y then LOCAL becomes ~/Y."
  (if (file-directory-p local)
      'repo-already-exists
    (async-shell-command (concat "git clone " remote " " local))
    (add-to-list 'magit-repository-directories `(,local   . 0))
    'cloned-repo))

(maybe-clone "https://github.com/alhassy/emacs.d" "~/.emacs.d")
(maybe-clone "https://github.com/alhassy/alhassy.github.io")
(maybe-clone "https://github.com/alhassy/CheatSheet")
(maybe-clone "https://github.com/alhassy/ElispCheatSheet")
(maybe-clone "https://github.com/alhassy/CatsCheatSheet")
(maybe-clone "https://github.com/alhassy/islam")

;; For brevity, many more ‘maybe-clone’ clauses are hidden in the source file.
;; =magit= ---Emacs' porcelain interface to git:2 ends here

;; [[file:~/.emacs.d/init.org::*=magit= ---Emacs' porcelain interface to git][=magit= ---Emacs' porcelain interface to git:3]]
(maybe-clone "https://github.com/alhassy/OCamlCheatSheet")
(maybe-clone "https://github.com/alhassy/AgdaCheatSheet")
(maybe-clone "https://github.com/alhassy/org-agda-mode")
(maybe-clone "https://github.com/JacquesCarette/TheoriesAndDataStructures")
(maybe-clone "https://github.com/alhassy/RubyCheatSheet")
(maybe-clone "https://github.com/alhassy/melpa")
(maybe-clone "https://github.com/alhassy/PrologCheatSheet")
(maybe-clone "https://github.com/alhassy/FSharpCheatSheet")

(maybe-clone "https://gitlab.cas.mcmaster.ca/armstmp/cs3mi3.git" "~/3mi3")
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
;; =magit= ---Emacs' porcelain interface to git:3 ends here

;; [[file:~/.emacs.d/init.org::*=magit= ---Emacs' porcelain interface to git][=magit= ---Emacs' porcelain interface to git:4]]
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
      (add-hook 'hack-local-variables-hook 'my/magit-check-file-and-popup)))
;; =magit= ---Emacs' porcelain interface to git:4 ends here

;; [[file:~/.emacs.d/init.org::*=magit= ---Emacs' porcelain interface to git][=magit= ---Emacs' porcelain interface to git:5]]
(use-package git-timemachine)
;; =magit= ---Emacs' porcelain interface to git:5 ends here

;; [[file:~/.emacs.d/init.org::*Syncing to the System's =$PATH=][Syncing to the System's =$PATH=:1]]
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))
;; Syncing to the System's =$PATH=:1 ends here

;; [[file:~/.emacs.d/init.org::*Keeping my system up to date][Keeping my system up to date:1]]
(defun my/stay-up-to-date ()
  "Ensure that OS and Emacs package listings are up to date.

   Takes ~5 seconds when everything is up to date."
  (async-shell-command "brew update && brew upgrade")
  (other-window 1)
  (rename-buffer "Keeping-system-up-to-date")

  (package-refresh-contents 'please-do-so-in-the-background)
  (message "Updated Emacs package manager.")
  (other-window 1))

(add-hook 'after-init-hook 'my/stay-up-to-date)

;; For now, doing this since I'm also calling my/stay-up-to-date with
;; after-init-hook which hides the startup message.
(add-hook 'after-init-hook 'display-startup-echo-area-message)
;; Keeping my system up to date:1 ends here

;; [[file:~/.emacs.d/init.org::*Installing OS packages from within Emacs ---Amethyst!][Installing OS packages from within Emacs ---Amethyst!:1]]
;; Auto installing OS system packages
(use-package use-package-ensure-system-package)
;; Installing OS packages from within Emacs ---Amethyst!:1 ends here

;; [[file:~/.emacs.d/init.org::*Installing OS packages from within Emacs ---Amethyst!][Installing OS packages from within Emacs ---Amethyst!:3]]
;; An Emacs-based interface to the package manager of your operating system.
(use-package helm-system-packages)
;; Installing OS packages from within Emacs ---Amethyst!:3 ends here

;; [[file:~/.emacs.d/init.org::*Installing OS packages from within Emacs ---Amethyst!][Installing OS packages from within Emacs ---Amethyst!:4]]
;; Unlike the Helm variant, we need to specify our OS pacman.
(setq system-packages-package-manager 'brew)

;; Use “brew cask install” instead of “brew install” for installing programs.
(setf (nth 2 (assoc 'brew system-packages-supported-package-managers))
      '(install . "brew cask install"))

;; If the given system package doesn't exist; install it.
;; (system-packages-ensure "amethyst")
;; Installing OS packages from within Emacs ---Amethyst!:4 ends here

;; [[file:~/.emacs.d/init.org::*Who am I? ---Using Gnus for Gmail][Who am I? ---Using Gnus for Gmail:1]]
(setq user-full-name    "Musa Al-hassy"
      user-mail-address "alhassy@gmail.com")
;; Who am I? ---Using Gnus for Gmail:1 ends here

;; [[file:~/.emacs.d/init.org::*Who am I? ---Using Gnus for Gmail][Who am I? ---Using Gnus for Gmail:2]]
     (setq message-send-mail-function 'smtpmail-send-it)
;; Who am I? ---Using Gnus for Gmail:2 ends here

;; [[file:~/.emacs.d/init.org::*Hydra: Supply a prefix only once][Hydra: Supply a prefix only once:1]]
;; Invoke all possible key extensions having a common prefix by
;; supplying the prefix only once.
(use-package hydra)

;; The standard syntax:
;; (defhydra hydra-example (global-map "C-c v") ;; Prefix
;;   ;; List of triples (extension method description) )
;; Hydra: Supply a prefix only once:1 ends here

;; [[file:~/.emacs.d/init.org::*Cosmetics][Cosmetics:1]]
;; Keep self motivated!
(setq frame-title-format '("" "%b - Living The Dream (•̀ᴗ•́)و"))
;; Cosmetics:1 ends here

;; [[file:~/.emacs.d/init.org::*Cosmetics][Cosmetics:2]]
;; Make it very easy to see the line with the cursor.
(global-hl-line-mode t)

;; Clean up any accidental trailing whitespace and in other places,
;; upon save.
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Nice soft yellow, pleasing ---try background colour for html export ;-)
(add-to-list 'default-frame-alist '(background-color . "#fcf4dc"))
;; "white"; or this darker yellow "#eae3cb"
;; Cosmetics:2 ends here

;; [[file:~/.emacs.d/init.org::*Startup message: Emacs & Org versions][Startup message: Emacs & Org versions:1]]
;; Silence the usual message: Get more info using the about page via C-h C-a.
(setq inhibit-startup-message t)

(defun display-startup-echo-area-message ()
  "The message that is shown after ‘user-init-file’ is loaded."
  (message
      (concat "Welcome "      user-full-name
              "! Emacs "      emacs-version
              "; Org-mode "   org-version
              "; System "     (system-name)
                  (format "; Time %.3fs"
                      (float-time (time-subtract (current-time)
                                    before-init-time))))))
;; Startup message: Emacs & Org versions:1 ends here

;; [[file:~/.emacs.d/init.org::*Startup message: Emacs & Org versions][Startup message: Emacs & Org versions:2]]
;; Welcome Musa Al-hassy! Emacs 26.1; Org-mode 9.2.3; System alhassy-air.local
;; Startup message: Emacs & Org versions:2 ends here

;; [[file:~/.emacs.d/init.org::*Themes][Themes:1]]
;; Treat all themes as safe; no query before use.
(setf custom-safe-themes t)

;; Nice looking themes ^_^
(use-package solarized-theme)
(use-package doom-themes)
(use-package spacemacs-common
  :ensure spacemacs-theme)

;; Infinite list my commonly used themes.
(setq my/themes '(doom-solarized-light doom-vibrant spacemacs-light))
(setcdr (last my/themes) my/themes)

(cl-defun my/disable-all-themes (&key (new-theme (pop my/themes)))
  "Disable all themes and load NEW-THEME, which defaults from ‘my/themes’."
  (interactive)
  (dolist (τ custom-enabled-themes)
    (disable-theme τ))
  (when new-theme (load-theme new-theme)))

;; The dark theme's modeline separator is ugly.
;; Keep reading below regarding “powerline”.
;; (setq powerline-default-separator 'arrow)
;; (spaceline-spacemacs-theme)

;; “C-x t” to toggle between personal themes.
(defalias 'my/toggle-theme #' my/disable-all-themes)
(global-set-key "\C-x\ t" 'my/toggle-theme)
(my/toggle-theme)
;; Themes:1 ends here

;; [[file:~/.emacs.d/init.org::*Persistent Scratch Buffer][Persistent Scratch Buffer:1]]
(use-package persistent-scratch
  ;; Enable both autosave and restore the last saved state of scratch
  ;; buffer, if any, on Emacs start.
  :config (persistent-scratch-setup-default))
;; Persistent Scratch Buffer:1 ends here

;; [[file:~/.emacs.d/init.org::*Persistent Scratch Buffer][Persistent Scratch Buffer:2]]
(defun scratch ()
   "Recreate the scratch buffer, loading any persistent state."
   (interactive)
   (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
   (insert initial-scratch-message)
   (ignore-errors (persistent-scratch-restore))
   (persistent-scratch-autosave-mode 1)
   (org-mode)
   (local-set-key (kbd "C-x C-s") 'persistent-scratch-save))

;; This doubles as a quick way to avoid the common formula: C-x b RET *scratch*
;; Persistent Scratch Buffer:2 ends here

;; [[file:~/.emacs.d/init.org::*Persistent Scratch Buffer][Persistent Scratch Buffer:3]]
(setq initial-scratch-message (concat
  "#+Title: Persistent Scratch Buffer"
  "\n#\n# Welcome! This’ a place for trying things out."
  "\n#\n# ⟨ ‘C-x C-s’ here saves to ~/.emacs.d/.persistent-scratch ⟩ \n\n"))
;; Persistent Scratch Buffer:3 ends here

;; [[file:~/.emacs.d/init.org::*Spaceline: A sleek mode line][Spaceline: A sleek mode line:1]]
;; When using helm & info & default, mode line looks prettier.
(use-package spaceline
  :custom (spaceline-buffer-encoding-abbrev-p nil)
          (spaceline-line-column-p nil)
          (spaceline-line-p nil)
          (powerline-default-separator 'arrow)
  :config (require 'spaceline-config)
          (spaceline-helm-mode)
          (spaceline-info-mode)
          (spaceline-emacs-theme))
;; Spaceline: A sleek mode line:1 ends here

;; [[file:~/.emacs.d/init.org::*Flashing when something goes wrong ---no blinking][Flashing when something goes wrong ---no blinking:1]]
(setq visible-bell 1)
;; Flashing when something goes wrong ---no blinking:1 ends here

;; [[file:~/.emacs.d/init.org::*Flashing when something goes wrong ---no blinking][Flashing when something goes wrong ---no blinking:2]]
(blink-cursor-mode 1)
;; Flashing when something goes wrong ---no blinking:2 ends here

;; [[file:~/.emacs.d/init.org::*My to-do list: The initial buffer when Emacs opens up][My to-do list: The initial buffer when Emacs opens up:1]]
(find-file "~/Dropbox/todo.org")
(split-window-right)			  ;; C-x 3
(other-window 1)                              ;; C-x 0
(let ((enable-local-variables :all)           ;; Load *all* locals.
      (org-confirm-babel-evaluate nil))       ;; Eval *all* blocks.
  (find-file "~/.emacs.d/init.org"))
;; My to-do list: The initial buffer when Emacs opens up:1 ends here

;; [[file:~/.emacs.d/init.org::*Showing date, time, and battery life][Showing date, time, and battery life:1]]
(setq display-time-day-and-date t)
(display-time)

;; (display-battery-mode -1)
;; Nope; let's use a fancy indicator …
;;
(use-package fancy-battery
  :diminish
  :custom (fancy-battery-show-percentage  t)
          (battery-update-interval       15)
  :config (fancy-battery-mode))
;; Showing date, time, and battery life:1 ends here

;; [[file:~/.emacs.d/init.org::*Hiding Scrollbar, tool bar, and menu][Hiding Scrollbar, tool bar, and menu:1]]
(tool-bar-mode   -1)  ;; No large icons please
(scroll-bar-mode -1)  ;; No visual indicator please
(menu-bar-mode   -1)  ;; The Mac OS top pane has menu options
;; Hiding Scrollbar, tool bar, and menu:1 ends here

;; [[file:~/.emacs.d/init.org::*Increase/decrease text size][Increase/decrease text size:1]]
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
;; C-x C-0 restores the default font size
;; Increase/decrease text size:1 ends here

;; [[file:~/.emacs.d/init.org::*Delete Selection Mode][Delete Selection Mode:1]]
(delete-selection-mode 1)
;; Delete Selection Mode:1 ends here

;; [[file:~/.emacs.d/init.org::*Highlight & complete parenthesis pair when cursor is near ;-)][Highlight & complete parenthesis pair when cursor is near ;-):1]]
(setq show-paren-delay 0)
(setq show-paren-style 'mixed)
(show-paren-mode)
;; Highlight & complete parenthesis pair when cursor is near ;-):1 ends here

;; [[file:~/.emacs.d/init.org::*Highlight & complete parenthesis pair when cursor is near ;-)][Highlight & complete parenthesis pair when cursor is near ;-):2]]
(use-package rainbow-delimiters
  :disabled
  :hook ((org-mode prog-mode text-mode) . rainbow-delimiters-mode))
;; Highlight & complete parenthesis pair when cursor is near ;-):2 ends here

;; [[file:~/.emacs.d/init.org::*Highlight & complete parenthesis pair when cursor is near ;-)][Highlight & complete parenthesis pair when cursor is near ;-):4]]
(electric-pair-mode 1)
;; Highlight & complete parenthesis pair when cursor is near ;-):4 ends here

;; [[file:~/.emacs.d/init.org::*Highlight & complete parenthesis pair when cursor is near ;-)][Highlight & complete parenthesis pair when cursor is near ;-):5]]
;; The ‘<’ and ‘>’ are not ‘parenthesis’, so give them no compleition.
(setq electric-pair-inhibit-predicate
      (lambda (c)
        (or (member c '(?< ?> ?~)) (electric-pair-default-inhibit c))))

;; Treat ‘<’ and ‘>’ as if they were words, instead of ‘parenthesis’.
(modify-syntax-entry ?< "w<")
(modify-syntax-entry ?> "w>")
;; Highlight & complete parenthesis pair when cursor is near ;-):5 ends here

;; [[file:~/.emacs.d/init.org::*Highlight & complete parenthesis pair when cursor is near ;-)][Highlight & complete parenthesis pair when cursor is near ;-):7]]
(setq electric-pair-pairs
         '(;; (?~ . ?~)
           (?* . ?*)
           (?/ . ?/)))
;; Highlight & complete parenthesis pair when cursor is near ;-):7 ends here

;; [[file:~/.emacs.d/init.org::*Highlight & complete parenthesis pair when cursor is near ;-)][Highlight & complete parenthesis pair when cursor is near ;-):8]]
;; Disable pairs when entering minibuffer
(add-hook 'minibuffer-setup-hook (lambda () (electric-pair-mode 0)))

;; Renable pairs when existing minibuffer
(add-hook 'minibuffer-exit-hook (lambda () (electric-pair-mode 1)))
;; Highlight & complete parenthesis pair when cursor is near ;-):8 ends here

;; [[file:~/.emacs.d/init.org::*Minibuffer should display line and column numbers][Minibuffer should display line and column numbers:1]]
; (line-number-mode t)
(column-number-mode t)
;; Minibuffer should display line and column numbers:1 ends here

;; [[file:~/.emacs.d/init.org::*Minibuffer should display line and column numbers][Minibuffer should display line and column numbers:2]]
(setq display-line-numbers-width-start t)
(global-display-line-numbers-mode      t)
;; Minibuffer should display line and column numbers:2 ends here

;; [[file:~/.emacs.d/init.org::*Never lose the cursor][Never lose the cursor:1]]
(use-package beacon
  :config (setq beacon-color "#666600")
  :hook   ((org-mode text-mode) . beacon-mode))
;; Never lose the cursor:1 ends here

;; [[file:~/.emacs.d/init.org::*Neotree: Directory Tree Listing][Neotree: Directory Tree Listing:1]]
;; Fancy icons for neotree
;; Only do this once:
(use-package all-the-icons
  :config (all-the-icons-install-fonts 'install-without-asking))

;; Sidebar for project file navigation
(use-package neotree
  :config (global-set-key "\C-x\ d" 'neotree-toggle)
          (setq neo-theme 'icons)
          (neotree-refresh))

;; Open it up upon startup.
;; (neotree-toggle)
;; Neotree: Directory Tree Listing:1 ends here

;; [[file:~/.emacs.d/init.org::*Tabs][Tabs:1]]
(use-package awesome-tab
  :disabled
  :quelpa (awesome-tab :fetcher git :url "https://github.com/manateelazycat/awesome-tab.git")
  :config (awesome-tab-mode t))

;; Show me /all/ the tabs at once, in one group.
(defun awesome-tab-buffer-groups ()
  (list (awesome-tab-get-group-name (current-buffer))))
;; Tabs:1 ends here

;; [[file:~/.emacs.d/init.org::*Window resizing using the golden ratio][Window resizing using the golden ratio:1]]
(use-package golden-ratio
  :disabled
  :diminish golden-ratio-mode
  :init (golden-ratio-mode 1))
;; Window resizing using the golden ratio:1 ends here

;; [[file:~/.emacs.d/init.org::*Fill-mode ---Word Wrapping][Fill-mode ---Word Wrapping:1]]
;; Let's avoid going over 80 columns
(setq fill-column 80)

;; Wrap long lines when editing text
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
;; Fill-mode ---Word Wrapping:1 ends here

;; [[file:~/.emacs.d/init.org::*Fix spelling as you type --thesaurus & dictionary too!][Fix spelling as you type --thesaurus & dictionary too!:1]]
(use-package flyspell
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)))
;; Fix spelling as you type --thesaurus & dictionary too!:1 ends here

;; [[file:~/.emacs.d/init.org::*Fix spelling as you type --thesaurus & dictionary too!][Fix spelling as you type --thesaurus & dictionary too!:2]]
(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-dictionary "en_GB") ;; set the default dictionary

(diminish 'flyspell-mode) ;; Don't show it in the modeline.
;; Fix spelling as you type --thesaurus & dictionary too!:2 ends here

;; [[file:~/.emacs.d/init.org::*Fix spelling as you type --thesaurus & dictionary too!][Fix spelling as you type --thesaurus & dictionary too!:4]]
(eval-after-load "flyspell"
  ' (progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))
;; Fix spelling as you type --thesaurus & dictionary too!:4 ends here

;; [[file:~/.emacs.d/init.org::*Fix spelling as you type --thesaurus & dictionary too!][Fix spelling as you type --thesaurus & dictionary too!:5]]
(global-font-lock-mode t)
(custom-set-faces '(flyspell-incorrect ((t (:inverse-video t)))))
;; Fix spelling as you type --thesaurus & dictionary too!:5 ends here

;; [[file:~/.emacs.d/init.org::*Fix spelling as you type --thesaurus & dictionary too!][Fix spelling as you type --thesaurus & dictionary too!:6]]
(setq ispell-silently-savep t)
;; Fix spelling as you type --thesaurus & dictionary too!:6 ends here

;; [[file:~/.emacs.d/init.org::*Fix spelling as you type --thesaurus & dictionary too!][Fix spelling as you type --thesaurus & dictionary too!:7]]
(setq ispell-personal-dictionary "~/.emacs.d/.aspell.en.pws")
;; Fix spelling as you type --thesaurus & dictionary too!:7 ends here

;; [[file:~/.emacs.d/init.org::*Fix spelling as you type --thesaurus & dictionary too!][Fix spelling as you type --thesaurus & dictionary too!:8]]
(add-hook          'c-mode-hook 'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
;; Fix spelling as you type --thesaurus & dictionary too!:8 ends here

;; [[file:~/.emacs.d/init.org::*Fix spelling as you type --thesaurus & dictionary too!][Fix spelling as you type --thesaurus & dictionary too!:9]]
(use-package synosaurus
  :diminish synosaurus-mode
  :init    (synosaurus-mode)
  :config  (setq synosaurus-choose-method 'popup) ;; 'ido is default.
           (global-set-key (kbd "M-#") 'synosaurus-choose-and-replace)
)
;; Fix spelling as you type --thesaurus & dictionary too!:9 ends here

;; [[file:~/.emacs.d/init.org::*Fix spelling as you type --thesaurus & dictionary too!][Fix spelling as you type --thesaurus & dictionary too!:10]]
;; (shell-command "brew cask install xquartz &") ;; Dependency
;; (shell-command "brew install wordnet &")
;; Fix spelling as you type --thesaurus & dictionary too!:10 ends here

;; [[file:~/.emacs.d/init.org::*Fix spelling as you type --thesaurus & dictionary too!][Fix spelling as you type --thesaurus & dictionary too!:11]]
(use-package wordnut
 :bind ("M-!" . wordnut-lookup-current-word))

;; Use M-& for async shell commands.
;; Fix spelling as you type --thesaurus & dictionary too!:11 ends here

;; [[file:~/.emacs.d/init.org::*Fix spelling as you type --thesaurus & dictionary too!][Fix spelling as you type --thesaurus & dictionary too!:13]]
(autoload 'typing-of-emacs "~/.emacs.d/typing.el" "The Typing Of Emacs, a game." t)
;; Fix spelling as you type --thesaurus & dictionary too!:13 ends here

;; [[file:~/.emacs.d/init.org::*Fix spelling as you type --thesaurus & dictionary too!][Fix spelling as you type --thesaurus & dictionary too!:14]]
(use-package speed-type)
;; Fix spelling as you type --thesaurus & dictionary too!:14 ends here

;; [[file:~/.emacs.d/init.org::*Fix spelling as you type --thesaurus & dictionary too!][Fix spelling as you type --thesaurus & dictionary too!:15]]
(use-package google-translate
 :config
   (global-set-key "\C-ct" 'google-translate-at-point)
)
;; Fix spelling as you type --thesaurus & dictionary too!:15 ends here

;; [[file:~/.emacs.d/init.org::*Using a Grammar & Style Checker][Using a Grammar & Style Checker:1]]
(use-package langtool
 :config
  (setq langtool-language-tool-jar
     "~/Applications/LanguageTool-4.5/languagetool-commandline.jar")
)
;; Using a Grammar & Style Checker:1 ends here

;; [[file:~/.emacs.d/init.org::*Using a Grammar & Style Checker][Using a Grammar & Style Checker:2]]
;; Quickly check, correct, then clean up /region/ with M-^

(add-hook 'langtool-error-exists-hook
  (lambda ()
     (langtool-correct-buffer)
     (langtool-check-done)))

(global-set-key "\M-^"
                (lambda ()
                  (interactive)
                  (message "Grammar checking begun ...")
                  (langtool-check)))
;; Using a Grammar & Style Checker:2 ends here

;; [[file:~/.emacs.d/init.org::*Lightweight Prose Proofchecking][Lightweight Prose Proofchecking:1]]
(use-package writegood-mode
  ;; Load this whenver I'm composing prose.
  :hook (text-mode org-mode)
  ;; Some additional weasel words.
  :config
  (--map (push it writegood-weasel-words)
         '("some" "simple" "simply" "easy" "often" "easily" "probably"
           "clearly"               ;; Is the premise undeniably true?
           "experience shows"      ;; Whose? What kind? How does it do so?
           "may have"              ;; It may also have not!
           "it turns out that")))  ;; How does it turn out so?
           ;; ↯ What is the evidence of highighted phrase? ↯
;; Lightweight Prose Proofchecking:1 ends here

;; [[file:~/.emacs.d/init.org::*Placeholder Text ---For Learning & Experimenting][Placeholder Text ---For Learning & Experimenting:1]]
(use-package lorem-ipsum)
;; Placeholder Text ---For Learning & Experimenting:1 ends here

;; [[file:~/.emacs.d/init.org::*Unicode Input via Agda Input][Unicode Input via Agda Input:1]]
; (load (shell-command-to-string "agda-mode locate"))
;;
;; Seeing: One way to avoid seeing this warning is to make sure that agda2-include-dirs is not bound.
; (makunbound 'agda2-include-dirs)
;; Unicode Input via Agda Input:1 ends here

;; [[file:~/.emacs.d/init.org::*Unicode Input via Agda Input][Unicode Input via Agda Input:2]]
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "/usr/local/bin/agda-mode locate")))
;; Unicode Input via Agda Input:2 ends here

;; [[file:~/.emacs.d/init.org::*Unicode Input via Agda Input][Unicode Input via Agda Input:3]]
(require 'agda-input)
(add-hook 'text-mode-hook (lambda () (set-input-method "Agda")))
(add-hook 'org-mode-hook  (lambda () (set-input-method "Agda")))
;; Unicode Input via Agda Input:3 ends here

;; [[file:~/.emacs.d/init.org::*Unicode Input via Agda Input][Unicode Input via Agda Input:4]]
;;(setq agda2-program-args (quote ("RTS" "-M4G" "-H4G" "-A128M" "-RTS")))
;; Unicode Input via Agda Input:4 ends here

;; [[file:~/.emacs.d/init.org::*Unicode Input via Agda Input][Unicode Input via Agda Input:5]]
(add-to-list 'agda-input-user-translations '("set" "𝒮ℯ𝓉"))
;; Unicode Input via Agda Input:5 ends here

;; [[file:~/.emacs.d/init.org::*Unicode Input via Agda Input][Unicode Input via Agda Input:6]]
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

;; [[file:~/.emacs.d/init.org::*Unicode Input via Agda Input][Unicode Input via Agda Input:7]]
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

;; [[file:~/.emacs.d/init.org::*Unicode Input via Agda Input][Unicode Input via Agda Input:8]]
;; activate translations
(agda-input-setup)
;; Unicode Input via Agda Input:8 ends here

;; [[file:~/.emacs.d/init.org::*Taking a tour of one's edits][Taking a tour of one's edits:1]]
;; Give me a description of the change made at a particular stop.
(use-package goto-chg
  :init (setq glc-default-span 0))

(defhydra hydra-edits (global-map "C-c e")
  ("," goto-last-change "Goto nᵗʰ last change")
  ("." goto-last-change-reverse "Goto more recent change"))
;; Taking a tour of one's edits:1 ends here

;; [[file:~/.emacs.d/init.org::*Moving Text Around][Moving Text Around:1]]
;; M-↑,↓ moves line, or marked region; prefix is how many lines.
(use-package move-text)
(move-text-default-bindings)
;; Moving Text Around:1 ends here

;; [[file:~/.emacs.d/init.org::*Enabling CamelCase Aware Editing Operations][Enabling CamelCase Aware Editing Operations:1]]
(global-subword-mode 1)
;; Enabling CamelCase Aware Editing Operations:1 ends here

;; [[file:~/.emacs.d/init.org::*Life within Org-mode][Life within Org-mode:2]]
(use-package org
  :ensure org-plus-contrib
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))
;; Life within Org-mode:2 ends here

;; [[file:~/.emacs.d/init.org::*Life within Org-mode][Life within Org-mode:3]]
(setq org-ellipsis " ⤵")
;; Life within Org-mode:3 ends here

;; [[file:~/.emacs.d/init.org::*Life within Org-mode][Life within Org-mode:4]]
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

;; [[file:~/.emacs.d/init.org::*Life within Org-mode][Life within Org-mode:5]]
(setq initial-major-mode 'org-mode)
;; Life within Org-mode:5 ends here

;; [[file:~/.emacs.d/init.org::*Manipulating Sections][Manipulating Sections:1]]
(setq org-use-speed-commands t)
;; Manipulating Sections:1 ends here

;; [[file:~/.emacs.d/init.org::*Seamless Navigation Between Source Blocks][Seamless Navigation Between Source Blocks:1]]
;; Overriding keys for printing buffer, duplicating gui frame, and isearch-yank-kill.
;;
(define-key org-mode-map (kbd "s-p") #'org-babel-previous-src-block)
(define-key org-mode-map (kbd "s-n") #'org-babel-next-src-block)
(define-key org-mode-map (kbd "s-e") #'org-edit-src-code)
(define-key org-src-mode-map (kbd "s-e") #'org-edit-src-exit)
;; Seamless Navigation Between Source Blocks:1 ends here

;; [[file:~/.emacs.d/init.org::*Modifying ~<return>~][Modifying ~<return>~:1]]
(add-hook 'org-mode-hook '(lambda ()
   (local-set-key (kbd "<return>") 'org-return-indent))
   (local-set-key (kbd "C-M-<return>") 'electric-indent-just-newline))
;; Modifying ~<return>~:1 ends here

;; [[file:~/.emacs.d/init.org::*~C-a,e,k~ and Yanking of sections][~C-a,e,k~ and Yanking of sections:1]]
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

;; [[file:~/.emacs.d/init.org::*Executing code from ~src~ blocks][Executing code from ~src~ blocks:1]]
; Seamless use of babel: No confirmation upon execution.
;; Downside: Could accidentally evaluate harmful code.
(setq org-confirm-babel-evaluate nil)
;; Executing code from ~src~ blocks:1 ends here

;; [[file:~/.emacs.d/init.org::*Executing code from ~src~ blocks][Executing code from ~src~ blocks:2]]
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

;; [[file:~/.emacs.d/init.org::*Hiding Emphasise Markers & Inlining Images][Hiding Emphasise Markers & Inlining Images:1]]
;; org-mode math is now highlighted ;-)
(setq org-highlight-latex-and-related '(latex))

;; Hide the *,=,/ markers
(setq org-hide-emphasis-markers t)

;; (setq org-pretty-entities t)
;; to have \alpha, \to and others display as utf8 http://orgmode.org/manual/Special-symbols.html
;; Hiding Emphasise Markers & Inlining Images:1 ends here

;; [[file:~/.emacs.d/init.org::*Working with Citations][Working with Citations:1]]
;; Files to look at when no “╲bibliography{⋯}” is not present in a file.
;; Most useful for non-LaTeX files.
(setq reftex-default-bibliography '("~/thesis-proposal/papers/References.bib"))
(setq bibtex-completion-bibliography (car reftex-default-bibliography))

(use-package org-ref
  :demand t
  :config (setq org-ref-default-bibliography reftex-default-bibliography))

(use-package helm-bibtex :demand t)
;; Working with Citations:1 ends here

;; [[file:~/.emacs.d/init.org::*Show off-screen Heading at the top of the window][Show off-screen Heading at the top of the window:1]]
 (use-package org-sticky-header
  :hook (org-mode . org-sticky-header-mode)
  :config
  (setq-default
   org-sticky-header-full-path 'full
   ;; Child and parent headings are seperated by a /.
   org-sticky-header-outline-path-separator " / "))
;; Show off-screen Heading at the top of the window:1 ends here

;; [[file:~/.emacs.d/init.org::*Clocking Work Time][Clocking Work Time:1]]
;; Record a note on what was accomplished when clocking out of an item.
(setq org-log-note-clock-out t)
;; Clocking Work Time:1 ends here

;; [[file:~/.emacs.d/init.org::*Clocking Work Time][Clocking Work Time:2]]
;; List of all the files & directories where todo items can be found. Only one for now.
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

;; [[file:~/.emacs.d/init.org::*Clocking Work Time][Clocking Work Time:3]]
(setq confirm-kill-emacs 'yes-or-no-p)
;; Clocking Work Time:3 ends here

;; [[file:~/.emacs.d/init.org::*Clocking Work Time][Clocking Work Time:4]]
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

;; [[file:~/.emacs.d/init.org::*Clocking Work Time][Clocking Work Time:5]]
(setq org-clock-sound "~/.emacs.d/school-bell.wav")
;; Clocking Work Time:5 ends here

;; [[file:~/.emacs.d/init.org::*[[https://revealjs.com/?transition=zoom#/\][Reveal.JS\]] -- The HTML Presentation Framework][[[https://revealjs.com/?transition=zoom#/][Reveal.JS]] -- The HTML Presentation Framework:1]]
(use-package ox-reveal
  :config (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))
;; [[https://revealjs.com/?transition=zoom#/][Reveal.JS]] -- The HTML Presentation Framework:1 ends here

;; [[file:~/.emacs.d/init.org::*[[https://revealjs.com/?transition=zoom#/\][Reveal.JS\]] -- The HTML Presentation Framework][[[https://revealjs.com/?transition=zoom#/][Reveal.JS]] -- The HTML Presentation Framework:3]]
(setq org-reveal-title-slide "<h1>%t</h1> <h3>%a</h3>
<font size=\"1\">
<a href=\"?print-pdf&showNotes=true\">
⟪ Flattened View ; Press <code>?</code> for Help ⟫
</a>
</font>")
;; [[https://revealjs.com/?transition=zoom#/][Reveal.JS]] -- The HTML Presentation Framework:3 ends here

;; [[file:~/.emacs.d/init.org::*Coloured LaTeX using Minted][Coloured LaTeX using Minted:1]]
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -output-directory %o %f"
        "biber %b"
        "pdflatex -shell-escape -output-directory %o %f"
        "pdflatex -shell-escape -output-directory %o %f")
)
;; Coloured LaTeX using Minted:1 ends here

;; [[file:~/.emacs.d/init.org::*Jumping without hassle][Jumping without hassle:1]]
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

;; [[file:~/.emacs.d/init.org::*Folding within a subtree][Folding within a subtree:1]]
(defun my/org-fold-current-subtree-anywhere-in-it ()
  "Hide the current heading, while being anywhere inside it."
  (interactive)
  (save-excursion
    (org-narrow-to-subtree)
    (org-shifttab)
    (widen)))

(add-hook 'org-mode-hook '(lambda ()
  (local-set-key (kbd "C-c C-h") 'my/org-fold-current-subtree-anywhere-in-it)))
;; Folding within a subtree:1 ends here

;; [[file:~/.emacs.d/init.org::*Ensuring Useful HTML Anchors][Ensuring Useful HTML Anchors:1]]
(defun my/ensure-headline-ids (&rest _)
  "Org trees without a :CUSTOM_ID: property have the property set to be their heading. All non-alphanumeric characters are replaced with ‘-’.

   If multiple trees end-up with the same id property, issue a message and undo
   any property insertion thus far.
  "
  (interactive)
  (let ((ids))
    (org-map-entries
     (lambda ()
       (org-with-point-at (point)
         (let ((id (org-entry-get nil "CUSTOM_ID")))
           (unless id
             (setq id (s-replace-regexp "[^[:alnum:]]" "-" (nth 4 (org-heading-components))))
             (if (not (member id ids))
                 (push id ids)
               (message-box "Oh no, a repeated id!\n\n\t%s" id)
               (undo)
               (setq quit-flag t))
             (org-entry-put nil "CUSTOM_ID" id))))))))

;; Whenever html & md export happens, ensure we have headline ids.
(advice-add 'org-html-export-to-html :before 'my/ensure-headline-ids)
(advice-add 'org-md-export-to-markdown :before 'my/ensure-headline-ids)
;; Ensuring Useful HTML Anchors:1 ends here

;; [[file:~/.emacs.d/init.org::*Interpret the Haskell source blocks in a file][Interpret the Haskell source blocks in a file:1]]
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

;; [[file:~/.emacs.d/init.org::*Expected IDE Support][Expected IDE Support:1]]
;; Use 4 spaces in places of tabs when indenting.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Always stay indented: Automatically have blocks reindented after every change.
(use-package aggressive-indent :demand t)
(global-aggressive-indent-mode t)
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

;; [[file:~/.emacs.d/init.org::*Editor Documentation with Contextual Information][Editor Documentation with Contextual Information:1]]
(use-package helpful)

(defun my/describe-symbol (symbol)
  "A “C-h o” replacement using “helpful”:
   If there's a thing at point, offer that as default search item.

   If a prefix is provided, i.e., “C-u C-h o” then the built-in
   “describe-symbol” command is used.

   ⇨ Pretty docstrings, with links and highlighting.
   ⇨ Source code of symbol.
   ⇨ Callers of function symbol.
   ⇨ Key bindings for function symbol.
   ⇨ Aliases.
   ⇨ Options to enable tracing, dissable, and forget/unbind the symbol!
  "
  (interactive "p")
  (let* ((thing (symbol-at-point))
         (val (completing-read
               (format "Describe symbol (default %s): " thing)
               (vconcat (list thing) obarray)
               (lambda (vv)
                (cl-some (lambda (x) (funcall (nth 1 x) vv))
                         describe-symbol-backends))
               t nil nil))
         (it (intern val)))

    (if current-prefix-arg
        (funcall #'describe-symbol it)
      (cond
       ((or (functionp it) (macrop it) (commandp it)) (helpful-callable it))
       (t (helpful-symbol it))))))

;; Keybindings.
(global-set-key (kbd "C-h o") #'my/describe-symbol)
(global-set-key (kbd "C-h k") #'helpful-key)
;; Editor Documentation with Contextual Information:1 ends here

;; [[file:~/.emacs.d/init.org::*~M-n,p~: Word-at-Point Navigation][~M-n,p~: Word-at-Point Navigation:1]]
(use-package smartscan
  :config
    (global-set-key (kbd "M-n") 'smartscan-symbol-go-forward)
    (global-set-key (kbd "M-p") 'smartscan-symbol-go-backward)
    (global-set-key (kbd "M-'") 'my/symbol-replace))
;; ~M-n,p~: Word-at-Point Navigation:1 ends here

;; [[file:~/.emacs.d/init.org::*~M-n,p~: Word-at-Point Navigation][~M-n,p~: Word-at-Point Navigation:2]]
(defun my/symbol-replace (replacement)
  "Replace all standalone symbols in the buffer matching the one at point."
  (interactive  (list (read-from-minibuffer "Replacement for thing at point: " nil)))
  (save-excursion
    (let ((symbol (or (thing-at-point 'symbol) (error "No symbol at point!"))))
      (beginning-of-buffer)
      ;; (query-replace-regexp symbol replacement)
      (replace-regexp (format "\\b%s\\b" (regexp-quote symbol)) replacement))))
;; ~M-n,p~: Word-at-Point Navigation:2 ends here

;; [[file:~/.emacs.d/init.org::*~M-n,p~: Word-at-Point Navigation][~M-n,p~: Word-at-Point Navigation:3]]
;; C-n, next line, inserts newlines when at the end of the buffer
(setq next-line-add-newlines t)
;; ~M-n,p~: Word-at-Point Navigation:3 ends here

;; [[file:~/.emacs.d/init.org::*Highlight Defined Emacs Lisp Symbols][Highlight Defined Emacs Lisp Symbols:1]]
(use-package highlight-defined)
(add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode)
;; Highlight Defined Emacs Lisp Symbols:1 ends here

;; [[file:~/.emacs.d/init.org::*Highlighting TODO-s & Showing them in Magit][Highlighting TODO-s & Showing them in Magit:1]]
;; NOTE that the highlighting works even in comments.
(use-package hl-todo
  ;; Enable it for text-like locations
  :hook ((text-mode org-mode prog-mode) . hl-todo-mode)
  :init
  ;; Adding new keywords
  (loop for kw in '("TEST" "MA" "WK" "JC")
        do (add-to-list 'hl-todo-keyword-faces (cons kw "#dc8cc3"))))
;; Highlighting TODO-s & Showing them in Magit:1 ends here

;; [[file:~/.emacs.d/init.org::*Highlighting TODO-s & Showing them in Magit][Highlighting TODO-s & Showing them in Magit:2]]
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

;; [[file:~/.emacs.d/init.org::*What's changed & who's to blame?][What's changed & who's to blame?:1]]
;; Hunk navigation and commiting.
(use-package git-gutter+
  :ensure t
  ;; Shucks, this is way to slow for large files.
  ;; :init (global-git-gutter+-mode)
  :diminish (git-gutter+-mode))
;; What's changed & who's to blame?:1 ends here

;; [[file:~/.emacs.d/init.org::*What's changed & who's to blame?][What's changed & who's to blame?:2]]
(defhydra hydra-version-control (git-gutter+-mode-map "C-x v")
  "Version control"
  ;; (extension method description)
  ("n" git-gutter+-next-hunk "Next hunk")
  ("p" git-gutter+-previous-hunk "Previous hunk")
  ("=" git-gutter+-show-hunk "Show hunk diff")
  ("r" git-gutter+-revert-hunks "Revert hunk\n")
  ("c" git-gutter+-stage-and-commit "Stage & commit hunk")
  ("C" git-gutter+-stage-and-commit-whole-buffer "Stage & commit entire buffer")
  ("U" git-gutter+-unstage-whole-buffer "Unstage whole buffer"))
;; What's changed & who's to blame?:2 ends here

;; [[file:~/.emacs.d/init.org::*What's changed & who's to blame?][What's changed & who's to blame?:3]]
;; Colour fringe to indicate alterations.
;; (use-package diff-hl)
;; (global-diff-hl-mode)
;; What's changed & who's to blame?:3 ends here

;; [[file:~/.emacs.d/init.org::*What's changed & who's to blame?][What's changed & who's to blame?:4]]
;; Popup for who's to blame for alterations.
(use-package git-messenger :demand t)
;;
;; Message menu let's us use magit diff to see the commit change.
(setq git-messenger:use-magit-popup t)
;;
;; Always show who authored the commit and when.
;; (setq git-messenger:show-detail t)

;; View current file in browser on github.
;; More generic is “browse-at-remote”.
(use-package github-browse-file)

;; Add these to the version control hydra.
;;
(defhydra hydra-version-control (git-gutter+-mode-map "C-x v")
  ("b" git-messenger:popup-message "Who's to blame?")
  ;; C-u C-x b ╱ u b ∷ Also show who authored the change and when.
  ("g" github-browse-file-blame "Show file in browser in github")
  ("s" magit-status "Git status of current buffer"))
;; What's changed & who's to blame?:4 ends here

;; [[file:~/.emacs.d/init.org::*What's changed & who's to blame?][What's changed & who's to blame?:5]]
(use-package git-link)

(defhydra hydra-version-control (git-gutter+-mode-map "C-x v")
  ("l" git-link "Git URL for current location"))
;; What's changed & who's to blame?:5 ends here

;; [[file:~/.emacs.d/init.org::*Edit as Root][Edit as Root:1]]
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

;; [[file:~/.emacs.d/init.org::*Restarting Emacs][Restarting Emacs:1]]
;; Provides only the command “restart-emacs”.
(use-package restart-emacs
  :commands restart-emacs)
;; Restarting Emacs:1 ends here

;; [[file:~/.emacs.d/init.org::*Keep buffers open across sessions][Keep buffers open across sessions:1]]
;; Keep open files open across sessions.
(desktop-save-mode 1)
(setq desktop-restore-eager 10)
;; Keep buffers open across sessions:1 ends here

;; [[file:~/.emacs.d/init.org::*Mouse Editing Support][Mouse Editing Support:1]]
;; Text selected with the mouse is automatically copied to clipboard.
(setq mouse-drag-copy-region t)
;; Mouse Editing Support:1 ends here

;; [[file:~/.emacs.d/init.org::*Dimming Unused Windows][Dimming Unused Windows:1]]
(use-package dimmer
  :config (dimmer-mode))
;; Dimming Unused Windows:1 ends here

;; [[file:~/.emacs.d/init.org::*Jump between windows using Cmd+Arrow & between recent buffers with Meta-Tab][Jump between windows using Cmd+Arrow & between recent buffers with Meta-Tab:1]]
(use-package windmove
  :config
  ;; use command key on Mac
  (windmove-default-keybindings 'super)
  ;; wrap around at edges
  (setq windmove-wrap-around t))
;; Jump between windows using Cmd+Arrow & between recent buffers with Meta-Tab:1 ends here

;; [[file:~/.emacs.d/init.org::*Jump between windows using Cmd+Arrow & between recent buffers with Meta-Tab][Jump between windows using Cmd+Arrow & between recent buffers with Meta-Tab:2]]
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

;; [[file:~/.emacs.d/init.org::*Coding with a Fruit Salad: Semantic Highlighting][Coding with a Fruit Salad: Semantic Highlighting:1]]
(use-package color-identifiers-mode)
(global-color-identifiers-mode)
;; Coding with a Fruit Salad: Semantic Highlighting:1 ends here

;; [[file:~/.emacs.d/init.org::*Completion Frameworks][Completion Frameworks:1]]
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

;; [[file:~/.emacs.d/init.org::*Completion Frameworks][Completion Frameworks:2]]
(setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf
                                    helm-source-bookmarks
                                    helm-source-bookmark-set
                                    helm-source-buffer-not-found))
;; Completion Frameworks:2 ends here

;; [[file:~/.emacs.d/init.org::*Completion Frameworks][Completion Frameworks:3]]
;; (shell-command "brew install surfraw &")
;;
;; Invoke helm-surfraw
;; Completion Frameworks:3 ends here

;; [[file:~/.emacs.d/init.org::*Completion Frameworks][Completion Frameworks:4]]
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

;; [[file:~/.emacs.d/init.org::*Completion Frameworks][Completion Frameworks:7]]
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

;; [[file:~/.emacs.d/init.org::*Completion Frameworks][Completion Frameworks:8]]
(use-package company-emoji)
(add-to-list 'company-backends 'company-emoji)
;; Completion Frameworks:8 ends here

;; [[file:~/.emacs.d/init.org::*Completion Frameworks][Completion Frameworks:9]]
(use-package emojify
 :config (setq emojify-display-style 'image)
 :init (global-emojify-mode 1) ;; Will install missing images, if need be.
)
;; Completion Frameworks:9 ends here

;; [[file:~/.emacs.d/init.org::*Completion Frameworks][Completion Frameworks:11]]
(use-package company-quickhelp
 :config
   (setq company-quickhelp-delay 0.1)
   (company-quickhelp-mode)
)
;; Completion Frameworks:11 ends here

;; [[file:~/.emacs.d/init.org::*Text Folding with [[https://github.com/gregsexton/origami.el\][Origami-mode]\]][Text Folding with [[https://github.com/gregsexton/origami.el][Origami-mode]]:1]]
(use-package origami)
;; Text Folding with [[https://github.com/gregsexton/origami.el][Origami-mode]]:1 ends here

;; [[file:~/.emacs.d/init.org::*Text Folding with [[https://github.com/gregsexton/origami.el\][Origami-mode]\]][Text Folding with [[https://github.com/gregsexton/origami.el][Origami-mode]]:2]]
(push (cons 'agda2-mode (origami-markers-parser "{-" "-}"))
      origami-parser-alist)
;; Text Folding with [[https://github.com/gregsexton/origami.el][Origami-mode]]:2 ends here

;; [[file:~/.emacs.d/init.org::*Text Folding with [[https://github.com/gregsexton/origami.el\][Origami-mode]\]][Text Folding with [[https://github.com/gregsexton/origami.el][Origami-mode]]:3]]
(defun my/search-hook-function ()
  (when origami-mode (origami-toggle-node (current-buffer) (point))))

;; Open folded nodes if a search stops there.
(add-hook 'helm-swoop-after-goto-line-action-hook #'my/search-hook-function)
;;
;; Likewise for incremental search, isearch, users.
;; (add-hook 'isearch-mode-end-hook #'my/search-hook-function)
;; Text Folding with [[https://github.com/gregsexton/origami.el][Origami-mode]]:3 ends here

;; [[file:~/.emacs.d/init.org::*Text Folding with [[https://github.com/gregsexton/origami.el\][Origami-mode]\]][Text Folding with [[https://github.com/gregsexton/origami.el][Origami-mode]]:4]]
(defhydra folding-with-origami-mode (global-map "C-c f")
  ("h" origami-close-node-recursively "Hide")
  ("o" origami-open-node-recursively  "Open")
  ("t" origami-toggle-all-nodes  "Toggle buffer")
  ("n" origami-next-fold "Next")
  ("p" origami-previous-fold "Previous"))
;; Text Folding with [[https://github.com/gregsexton/origami.el][Origami-mode]]:4 ends here

;; [[file:~/.emacs.d/init.org::*Helpful Utilities & Shortcuts][Helpful Utilities & Shortcuts:1]]
;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable ‘possibly confusing commands’
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
;; Helpful Utilities & Shortcuts:1 ends here

;; [[file:~/.emacs.d/init.org::*Bind ~recompile~ to ~C-c C-m~ -- “m” for “m”ake][Bind ~recompile~ to ~C-c C-m~ -- “m” for “m”ake:1]]
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

;; [[file:~/.emacs.d/init.org::*Reload buffer with ~f5~][Reload buffer with ~f5~:1]]
(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t nil)))
;; Reload buffer with ~f5~:1 ends here

;; [[file:~/.emacs.d/init.org::*Reload buffer with ~f5~][Reload buffer with ~f5~:2]]
;; Auto update buffers that change on disk.
;; Will be prompted if there are changes that could be lost.
(global-auto-revert-mode 1)
;; Reload buffer with ~f5~:2 ends here

;; [[file:~/.emacs.d/init.org::*Kill to start of line][Kill to start of line:1]]
;; M-k kills to the left
(global-set-key "\M-k" '(lambda () (interactive) (kill-line 0)) )
;; Kill to start of line:1 ends here

;; [[file:~/.emacs.d/init.org::*~file-as-list~ and ~file-as-string~][~file-as-list~ and ~file-as-string~:1]]
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

;; [[file:~/.emacs.d/init.org::*~C-x k~ kills current buffer, ~C-u C-x k~ kills all others][~C-x k~ kills current buffer, ~C-u C-x k~ kills all others:1]]
(defun kill-other-buffers ()
  "Kill all other buffers and other windows."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
  (delete-other-windows))
;; ~C-x k~ kills current buffer, ~C-u C-x k~ kills all others:1 ends here

;; [[file:~/.emacs.d/init.org::*~C-x k~ kills current buffer, ~C-u C-x k~ kills all others][~C-x k~ kills current buffer, ~C-u C-x k~ kills all others:2]]
(global-set-key (kbd "C-x k")
  '(lambda (&optional all)
     "Kill current buffer, or all if prefix is provided.
      Prompt only if there are unsaved changes."
     (interactive "P")
     (if all (kill-other-buffers)
       (kill-buffer (current-buffer)))))
;; ~C-x k~ kills current buffer, ~C-u C-x k~ kills all others:2 ends here

;; [[file:~/.emacs.d/init.org::*Switching from 2 horizontal windows to 2 vertical windows][Switching from 2 horizontal windows to 2 vertical windows:1]]
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

;; [[file:~/.emacs.d/init.org::*Obtaining Values of ~#+KEYWORD~ Annotations][Obtaining Values of ~#+KEYWORD~ Annotations:1]]
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

;; [[file:~/.emacs.d/init.org::*Quickly pop-up a terminal, run a command, close it ---and zsh][Quickly pop-up a terminal, run a command, close it ---and zsh:1]]
(use-package shell-pop
  :config (setq
    ;; This binding toggles popping up a shell, or moving cursour to the shell pop-up.
    shell-pop-universal-key "C-t"

    ;; Percentage for shell-buffer window size.
    shell-pop-window-size 30

    ;; Position of the popped buffer: top, bottom, left, right, full
    shell-pop-window-position "bottom"

    ;; Please use an awesome shell.
    shell-pop-term-shell "/bin/zsh"
 ))
;; Quickly pop-up a terminal, run a command, close it ---and zsh:1 ends here

;; [[file:~/.emacs.d/init.org::*Quickly pop-up a terminal, run a command, close it ---and zsh][Quickly pop-up a terminal, run a command, close it ---and zsh:2]]
;; Be default, Emacs please use zsh
;; E.g., M-x shell
(setq shell-file-name "/bin/zsh")
;; Quickly pop-up a terminal, run a command, close it ---and zsh:2 ends here

;; [[file:~/.emacs.d/init.org::*Publishing articles to my personal blog][Publishing articles to my personal blog:1]]
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

;; [[file:~/.emacs.d/init.org::*Excellent PDF Viewer][Excellent PDF Viewer:1]]
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

;; [[file:~/.emacs.d/init.org::*Undo tree][Undo tree:1]]
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

;; [[file:~/.emacs.d/init.org::*Elementary Version Control][Elementary Version Control:1]]
;; Don't ask for confirmation when opening symlinked files.
(setq vc-follow-symlinks t)
;; Elementary Version Control:1 ends here
