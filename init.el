;; [[file:init.org::*Personal instructions for a new machine][Personal instructions for a new machine:4]]
    (setq org-image-actual-width nil)
;; Personal instructions for a new machine:4 ends here

;; [[file:init.org::*Personal instructions for a new machine][Personal instructions for a new machine:5]]
    ;; Clicking on a URL, or running M-x browse-url, should open the URL *within* Emacs.
    (setq browse-url-browser-function #'xwidget-webkit-browse-url)

    ;; (use-package xwwp) ;; Enhance the Emacs xwidget-webkit browser
;; Personal instructions for a new machine:5 ends here

(setq custom-file "~/.emacs.d/custom.el")
(ignore-errors (load custom-file)) ;; It may not yet exist.

(setq user-full-name    "Musa Al-hassy"
      user-mail-address "alhassy@gmail.com")

;; Make all commands of the “package” module present.
(require 'package)

;; Internet repositories for new packages.
(setq package-archives '(("gnu"    . "http://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa"  . "http://melpa.org/packages/")))

;; Update local list of available packages:
;; Get descriptions of all configured ELPA packages,
;; and make them available for download.
(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(setq use-package-always-ensure t)

(use-package auto-package-update
  :config
  ;; Delete residual old versions
  (setq auto-package-update-delete-old-versions t)
  ;; Do not bother me when updates have taken place.
  (setq auto-package-update-hide-results t)
  ;; Update installed packages at startup if there is an update pending.
  (auto-package-update-maybe))

;; Making it easier to discover Emacs key presses.
(use-package which-key
  :defer nil
  :config (which-key-mode)
          (which-key-setup-side-window-bottom)
          (setq which-key-idle-delay 0.05))

;; Haskell's cool
(use-package haskell-mode )

;; Lisp libraries with Haskell-like naming.
(use-package dash)    ;; “A modern list library for Emacs”
(use-package s   )    ;; “The long lost Emacs string manipulation library”.

;; Let's use the “s” library.
(defvar my/personal-machine?
  (equal "Musa’s MacBook Air " (s-collapse-whitespace (shell-command-to-string "scutil --get ComputerName")))
  "Is this my personal machine, or my work machine?

 At one point, on my work machine I run the following command to give the machine a sensible name.

     sudo scutil --set ComputerName work-machine
     dscacheutil -flushcache")

(defvar my/work-machine? (not my/personal-machine?))

;; Library for working with system files;
;; e.g., f-delete, f-mkdir, f-move, f-exists?, f-hidden?
(use-package f)

  ;; Allow tree-semantics for undo operations.
  (use-package undo-tree
    :defer nil
    :bind ("C-x u" . undo-tree-visualize)
    :hook (org-mode . undo-tree-mode) ;; For some reason, I need this. FIXME.
    :config
      ;; Always have it on
      (global-undo-tree-mode)
  
      ;; Each node in the undo tree should have a timestamp.
      (setq undo-tree-visualizer-timestamps t)
  
      ;; Show a diff window displaying changes between undo nodes.
      (setq undo-tree-visualizer-diff t)
  
      ;; Prevent undo tree files from polluting your git repo
      (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))
  
  ;; Execute (undo-tree-visualize) then navigate along the tree to witness
  ;; changes being made to your file live!

(use-package quelpa
  :custom (quelpa-upgrade-p t "Always try to update packages")
  :config
  ;; Get ‘quelpa-use-package’ via ‘quelpa’
  (quelpa
   '(quelpa-use-package
     :fetcher git
     :url "https://github.com/quelpa/quelpa-use-package.git"))
  (require 'quelpa-use-package))

;; Auto installing OS system packages
(use-package system-packages :config (system-packages-update))

;; Install OS packages using `use-package`.
(use-package use-package-ensure-system-package)

;; Caching the installed pkgs list makes system-package-ensure return nearly immediately for things already installed!
(setq my/installed-packages (shell-command-to-string "brew list"))
(defun system-packages-ensure (pkg)
  (unless (s-contains-p pkg  my/installed-packages)
      (shell-command-to-string (format "brew list %s || brew install %s --force" pkg pkg))))

;; Please don't bother me when shell buffer names are in use, just make a new
;; buffer.
(setq async-shell-command-buffer 'new-buffer)

;; Display the output buffer for asynchronous shell commands only when the
;; command generates output.
(setq async-shell-command-display-buffer nil)

;; Don't ask me if I want to kill a buffer with a live process attached to it;
;; just kill it please.
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

;; Ensure our operating system is always up to date.
;; This is run whenever we open Emacs & so wont take long if we're up to date.
;; It happens in the background ^_^
;;
;; After 5 seconds of being idle, after starting up.


(defvar my/installed-packages
  (shell-command-to-string "brew list")
   "What is on my machine already?

Sometimes when I install a GUI based application and do not have access to it everywhere in my path,
it may seem that I do not have that application installed. For instance,
   (system-packages-package-installed-p \"google-chrome\")
returns nil, even though Google Chrome is on my machine.

As such, we advise the `system-packages-ensure' installtion method to only do
installs of pacakges that are not in our `my/installed-packages' listing.
")
(advice-add 'system-packages-ensure   :before-until (lambda (pkg) (s-contains-p pkg my/installed-packages)))

;; An Emacs-based interface to the package manager of your operating system.
(use-package helm-system-packages)

(setq system-packages-noconfirm :do-not-prompt-me-about-confirms)

;; After 1 minute after startup, kill all buffers created by ensuring system
;; packages are present.
(run-with-timer 60 nil
 (lambda () (kill-matching-buffers ".*system-packages.*" t :kill-without-confirmation)))

;; Unlike the Helm variant, we need to specify our OS pacman.
(when (eq system-type 'darwin)
  (setq system-packages-package-manager 'brew))

;; If the given system package doesn't exist; install it.
(when (eq system-type 'darwin)
  (system-packages-ensure "amethyst")) ;; This is a MacOS specific package.

(ignore-errors (system-packages-ensure "google-chrome")) ;; My choice of web browser
(system-packages-ensure "microsoft-teams") ;; For remote work meetings

;; Gif maker; needs privileges to capture screen.
;;
;; ⇒ Move the screen capture frame while recording.
;; ⇒ Pause and restart recording, with optional inserted text messages.
;; ⇒ Global hotkey (shift+space) to toggle pausing while recording
(system-packages-ensure "licecap") ;; Use: ⌘-SPACE licecap

;; Pack, ship and run any application as a lightweight container
(system-packages-ensure "docker")
;; Free universal database tool and SQL client
(system-packages-ensure "dbeaver-community")
;; Kubernetes IDE
(system-packages-ensure "lens")
;; Platform built on V8 to build network applications
;; Also known as: node.js, node@16, nodejs, npm
(system-packages-ensure "node") ;; https://nodejs.org/
;; Nice: https://nodesource.com/blog/an-absolute-beginners-guide-to-using-npm/
;; Manage multiple Node.js versions
(shell-command "curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.38.0/install.sh | bash")
;; According to https://github.com/nvm-sh/nvm, nvm shouldn't be installed via brew.

;; ;; Use “brew cask install” instead of “brew install” for installing programs.;
;; (setf (nth 2 (assoc 'brew system-packages-supported-package-managers))
;;      '(install . "brew cask install"))

(defun ⌘-quit (app)
  "Kill application APP; e.g., “amethyst” or “Safari”"
  (shell-command (format "osascript -e 'quit app \"%s\"'" app)))

(defun ⌘-open (app)
 "Open application APP; e.g., “amethyst” or “Safari”"
  (async-shell-command (format "osascript -e 'launch app \"%s\"'" app)))

;; (bind-key "???-a r" #'my/relaunch-amethyst)
(defun my/relaunch-amethyst () (interactive)
       (⌘-quit "amethyst")
       (⌘-open "amethyst"))

;; (bind-key "???-a c" #'amethyst/cycle-layout)
(defun amethyst/cycle-layout ()
  (interactive)
  (shell-command "osascript -e 'tell application \"System Events\" to keystroke space using {shift down, option down}'"))

(use-package exec-path-from-shell
  :defer nil
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Provides only the command “restart-emacs”.
(use-package restart-emacs
  :defer nil
  ;; If I ever close Emacs, it's likely because I want to restart it.
  :bind ("C-x C-c" . restart-emacs)
  ;; Let's define an alias so there's no need to remember the order.
  :config (defalias 'emacs-restart #'restart-emacs))

(setq-default save-place  t)
(setq save-place-file "~/.emacs.d/etc/saveplace")

(use-package helm
  :defer nil
 :init (helm-mode t)
 :bind (("M-x"     . helm-M-x)
        ("C-x C-f" . helm-find-files)
        ("C-x b"   . helm-mini)     ;; See buffers & recent files; more useful.
        ("C-x r b" . helm-filtered-bookmarks)
        ("C-x C-r" . helm-recentf)  ;; Search for recently edited files
        ("C-c i"   . helm-imenu) ;; C.f. “C-x t m” (imenu-list)
        ;; ("C-u C-c i" . imenu-list)  ;; TODO FIXME  Key sequence C-u C-c i starts with non-prefix key C-u
        ("C-h a"   . helm-apropos)
        ;; Look at what was cut recently & paste it in.
        ("M-y" . helm-show-kill-ring)
        ("C-x C-x" . helm-all-mark-rings)
        :map helm-map
        ;; We can list ‘actions’ on the currently selected item by C-z.
        ("C-z" . helm-select-action)
        ;; Let's keep tab-completetion anyhow.
        ("TAB"   . helm-execute-persistent-action)
        ("<tab>" . helm-execute-persistent-action)))

;; Show me nice file icons when using, say, “C-x C-f” or “C-x b”
;; (use-package helm-icons
;;   :defer nil
;;   :custom (helm-icons-provider 'all-the-icons)
;;   :config (helm-icons-enable))

;; When I want to see the TOC of an Org file, show me down to 3 subheadings.
(setq org-imenu-depth 3)

(setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf
                                    helm-source-bookmarks
                                    helm-source-bookmark-set
                                    helm-source-buffer-not-found))

(system-packages-ensure "surfraw")
; ⇒  “M-x helm-surfraw” or “C-x c s”

(use-package helm-swoop
  :defer nil
  :bind  (("C-s"     . 'helm-swoop)           ;; search current buffer
          ("C-M-s"   . 'helm-multi-swoop-all) ;; Search all buffer
          ;; Go back to last position where ‘helm-swoop’ was called
          ("C-S-s" . 'helm-swoop-back-to-last-point)
          ;; swoop doesn't work with PDFs, use Emacs' default isearch instead.
          ; :map pdf-view-mode-map ("C-s" . isearch-forward)
          )
  :custom (helm-swoop-speed-or-color nil "Give up colour for speed.")
          (helm-swoop-split-with-multiple-windows nil "Do not split window inside the current window."))

(system-packages-ensure "ag")

;; Save/mark a location with “C-u M-m”, jump back to it with “M-m”.
(bind-key* "M-m"
           (lambda ()
             (interactive)
             (if (not current-prefix-arg)
                 (helm-mark-ring)
               (push-mark)
               (message "[To return to this location, press M-m] ∷ %s"
                        (s-trim (substring-no-properties (thing-at-point 'line)))))))

;; Make `links' from elisp symbols (quoted functions, variables and fonts) in Gnu-Emacs Info viewer to their help documentation.
(use-package inform
  :defer nil
  :config (require 'inform))

(use-package emacs
    :defer nil
    :ensure org-contrib
    :config (require 'ox-extra)
            (ox-extras-activate '(ignore-headlines)))

;; Replace the content marker, “⋯”, with a nice unicode arrow.
(setq org-ellipsis " ⤵")

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

(setq initial-major-mode 'org-mode)

(defun org-special-block-extras-short-names ())
;;
;; org-special-block-extras.el:681:1:Error: Symbol’s value as variable is void: o--supported-blocks
(setq o--supported-blocks nil)

;; TODO org-special-block-extras.el:681:1:Error: Symbol’s value as variable is void: o--supported-blocks
;;
(use-package org-special-block-extras
  :defer nil
  :hook (org-mode . org-special-block-extras-mode)
  :custom
    ;; The places where I keep my ‘#+documentation’
    (org-special-block-extras--docs-libraries
     '("~/org-special-block-extras/documentation.org"))
    ;; Disable the in-Emacs fancy-links feature?
    (org-special-block-extras-fancy-links
     '(elisp badge kbd link-here doc tweet))
    ;; Details heading “flash pink” whenever the user hovers over them?
    (org-html-head-extra (concat org-html-head-extra "<style>  summary:hover {background:pink;} </style>"))
    ;; The message prefixing a ‘tweet:url’ badge
    (org-special-block-extras-link-twitter-excitement
     "This looks super neat (•̀ᴗ•́)و:")
  :config
  ;; Use short names like ‘defblock’ instead of the fully qualified name
  ;; ‘org-special-block-extras--defblock’
    (org-special-block-extras-short-names))

;; Let's execute Lisp code with links, as in “elisp:view-hello-file”.
(setq org-confirm-elisp-link-function nil)

;; Invoke all possible key extensions having a common prefix by
;; supplying the prefix only once.
(use-package hydra :defer nil)

;; Show hydras overlayed in the middle of the frame
(use-package hydra-posframe
  :disabled "TODO Fix me, breaking Github Actions test setup"
  :quelpa (hydra-posframe :fetcher git :url
                          "https://github.com/Ladicle/hydra-posframe.git")
  :hook (after-init . hydra-posframe-mode)
  :custom (hydra-posframe-border-width 5))

;; Neato doc strings for hydras
(use-package pretty-hydra :defer nil)

;; TODO convert my existing defhydras to my/defhydra.
(defmacro my/defhydra (key title icon-name &rest body)
"Make a hydra whose heads appear in a pretty pop-up window.
Heads are signalled by keywords and the hydra has an icon in its title.

KEY [String]: Global keybinding for the new hydra.

TITLE [String]: Either a string or a plist, as specified for pretty-hydra-define.
       The underlying Lisp function's name is derived from the TITLE;
       which is intentional since hydra's are for interactive, pretty, use.

       One uses a plist TITLE to specify what a hydra should do *before*
       any options, or to specify an alternate quit key (:q by default).

ICON-NAME [Symbol]: Possible FontAwesome icon-types: C-h v `all-the-icons-data/fa-icon-alist'.

BODY: A list of columns and entries. Keywords indicate the title
      of a column; 3-lists (triples) indicate an entry key and
      the associated operation to perform and, optionally, a name
      to be shown in the pop-up. See DEFHYDRA for more details.


For instance, the verbose mess:

    ;; Use ijkl to denote ↑←↓→ arrows.
    (global-set-key
     (kbd \"C-c w\")
     (pretty-hydra-define my/hydra/\\t\\tWindow\\ Adjustment
       ;; Omitting extra work to get an icon into the title.
       (:title \"\t\tWindow Adjustment\" :quit-key \"q\")
       (\"Both\"
        ((\"b\" balance-windows                 \"balance\")
         (\"s\" switch-window-then-swap-buffer  \"swap\"))
        \"Vertical adjustment\"
        ((\"h\" enlarge-window                  \"heighten\")
         (\"l\" shrink-window                   \"lower\"))
        \"Horizontal adjustment\"
        ((\"n\" shrink-window-horizontally      \"narrow\")
         (\"w\" enlarge-window-horizontally \"widen\" )))))

Is replaced by:

    ;; Use ijkl to denote ↑←↓→ arrows.
    (my/defhydra \"C-c w\" \"\t\tWindow Adjustment\" windows
       :Both
       (\"b\" balance-windows                 \"balance\")
       (\"s\" switch-window-then-swap-buffer  \"swap\")
       :Vertical_adjustment
       (\"h\" enlarge-window                  \"heighten\")
       (\"l\" shrink-window                   \"lower\")
       :Horizontal_adjustment
       (\"n\" shrink-window-horizontally      \"narrow\")
       (\"w\" enlarge-window-horizontally     \"widen\"))"
  (let* ((name (intern (concat "my/hydra/"
                              (if (stringp title)
                                  title
                                (plist-get title :title)))))
         (icon-face `(:foreground ,(face-background 'highlight)))
         (iconised-title
          (concat
           (when icon-name
                 (require 'all-the-icons)
             (concat
              (all-the-icons-faicon (format "%s" icon-name) :face icon-face :height 1.0 :v-adjust -0.1)
              " "))
           (propertize title 'face icon-face))))
    `(global-set-key
      (kbd ,key)
      (pretty-hydra-define ,name
        ,(if (stringp title)
             (list :title iconised-title
                   :quit-key "q")
           title)
        ,(thread-last body
           (-partition-by-header #'keywordp)
           (--map (cons (s-replace "_" " " (s-chop-prefix ":" (symbol-name (car it)))) (list (cdr it))))
           (-flatten-n 1))))))

(my/defhydra "C-n" "\t\t\t\t\tTextual Navigation" arrows
   :Line
   ("n" next-line)
   ("p" previous-line)
   ("a" beginning-of-line)
   ("e" move-end-of-line)
   ("g" goto-line)
   :Word
   ("f" forward-word "Next")
   ("b" backward-word "Previous")
   ("{" org-backward-element "Next Element")
   ("}" org-forward-element "Previous Element")
   :Screen
   ("v" scroll-up-command "Scroll Down")
   ("V" scroll-down-command "Scroll Up")
   ("l" recenter-top-bottom "Center Page")
   ("r" move-to-window-line-top-bottom "Relocate Point")
   ("m" helm-imenu "Textual Menu"))

;; C-n, next line, inserts newlines when at the end of the buffer
(setq next-line-add-newlines t)

;; Use ijkl to denote ↑←↓→ arrows.
(my/defhydra "C-c w" "\t\tWindow Adjustment" windows
   :Both
   ("b" balance-windows                 "balance")
   ("s" switch-window-then-swap-buffer  "swap")
   :Vertical_adjustment
   ("h" enlarge-window                  "heighten")
   ("l" shrink-window                   "lower")
   :Horizontal_adjustment
   ("n" shrink-window-horizontally      "narrow")
   ("w" enlarge-window-horizontally     "widen"))

;; Provides a *visual* way to choose a window to switch to.
;; (use-package switch-window )
;; :bind (("C-x o" . switch-window)
;;        ("C-x w" . switch-window-then-swap-buffer))

;; Have a thick ruler between vertical windows
(window-divider-mode)

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Make RETURN key act the same way as “y” key for “y-or-n” prompts.
;; E.g., (y-or-n-p "Happy?") accepts RETURN as “yes”.
(define-key y-or-n-p-map [return] 'act)

;; Enable all ‘possibly confusing commands’ such as helpful but
;; initially-worrisome “narrow-to-region”, C-x n n.
(setq-default disabled-command-function nil)

(use-package vterm) ;; Shell with a nearly universal compatibility with terminal applications 💝

;; "Intelligent" switching to vterm; eg creates it if it's not open, non-intrusive windowing, saves window setup, etc.
(use-package vterm-toggle
    :bind* ("C-t" . vterm-toggle))

;; Be default, Emacs please use zsh
;; E.g., M-x shell
(unless noninteractive (setq shell-file-name "/bin/zsh"))

(system-packages-ensure "tldr")

;; [[file:init.org::*Manipulating Sections][Manipulating Sections:1]]
(setq org-use-speed-commands t)
;; Manipulating Sections:1 ends here

;; [[file:init.org::*Manipulating Sections][Manipulating Sections:2]]
;; When refiling, only show me top level headings [Default]. Sometimes 2 is useful.
;; When I'm refiling my TODOS, then give me all the freedom.
(setq org-refile-targets '((nil :maxlevel . 1)
                           (org-agenda-files :maxlevel . 9)))

;; Maybe I want to refile into a new heading; confirm with me.
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Use full outline paths for refile targets
;; When refiling, using Helm, show me the hierarchy paths
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path 'file-path)
;; Manipulating Sections:2 ends here

;; [[file:init.org::*Manipulating Sections][Manipulating Sections:3]]
;; TODO FIXME Crashes upon startup.
(when nil (add-to-list 'org-speed-commands (cons "P" #'org-set-property)))
;; Use ‘:’ and ‘e’ to set tags and effort, respectively.
;; Manipulating Sections:3 ends here

;; [[file:init.org::*Seamless Navigation Between Source Blocks][Seamless Navigation Between Source Blocks:1]]
;; Overriding keys for printing buffer, duplicating gui frame, and isearch-yank-kill.
;;
(require 'org)
(use-package emacs
  :bind (:map org-mode-map
              ("s-p" . org-babel-previous-src-block)
              ("s-n" . org-babel-next-src-block)
              ("s-e" . org-edit-special)
              :map org-src-mode-map
              ("s-e" . org-edit-src-exit)))
;; Seamless Navigation Between Source Blocks:1 ends here

;; [[file:init.org::*Modifying \[\[kbd:⟨return⟩\]\]][Modifying [[kbd:⟨return⟩]]:1]]
(add-hook 'org-mode-hook '(lambda ()
   (local-set-key (kbd "<return>") 'org-return-indent))
   (local-set-key (kbd "C-M-<return>") 'electric-indent-just-newline))
;; Modifying [[kbd:⟨return⟩]]:1 ends here

;; [[file:init.org::*Executing code from ~src~ blocks][Executing code from ~src~ blocks:1]]
;; Seamless use of babel: No confirmation upon execution.
;; Downside: Could accidentally evaluate harmful code.
(setq org-confirm-babel-evaluate nil)

;; Never evaluate code blocks upon export and replace results when evaluation does occur.
;; For a particular language 𝑳, alter ‘org-babel-default-header-args:𝑳’.
(setq org-babel-default-header-args
      '((:results . "replace")
        (:session . "none")
        (:exports . "both")
        (:cache .   "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "no")
        (:eval . "never-export")))
;; Executing code from ~src~ blocks:1 ends here

;; [[file:init.org::*Executing code from ~src~ blocks][Executing code from ~src~ blocks:2]]
(defvar my/programming-languages
  '(emacs-lisp shell python haskell
      ;; rust ;; FIXME: There's an error wrt ob-rust: Cannot open load file: No such file or directory, ob-rust
    ruby ocaml dot latex org js css
               sqlite C) ;; Captial “C” gives access to C, C++, D
  "List of languages I have used in Org-mode, for literate programming.")

;; Load all the languagues
;; FIXME: There's an error wrt ob-rust: Cannot open load file: No such file or directory, ob-rust
(ignore-errors (cl-loop for lang in my/programming-languages
                        do (require (intern (format "ob-%s" lang)))))
;;
(org-babel-do-load-languages
 'org-babel-load-languages
 (--map (cons it t) my/programming-languages))

;; Preserve my indentation for source code during export.
(setq org-src-preserve-indentation t)

;; The export process hangs Emacs, let's avoid this.
;; MA: For one reason or another, this crashes more than I'd like.
;; (setq org-export-in-background t)
;; Executing code from ~src~ blocks:2 ends here

;; [[file:init.org::*Executing all =#+name: startup-code= for local configurations][Executing all =#+name: startup-code= for local configurations:1]]
(defun my/execute-startup-blocks ()
  "Execute all startup blocks, those named ‘startup-code’.

I could not use ORG-BABEL-GOTO-NAMED-SRC-BLOCK since it only goes
to the first source block with the given name, whereas I'd like to
visit all blocks with such a name."
  (interactive)
  (save-excursion
    (goto-char 0)
    (while (ignore-errors (re-search-forward "^\\#\\+name: startup-code"))
      (org-babel-execute-src-block))))
;; Executing all =#+name: startup-code= for local configurations:1 ends here

;; [[file:init.org::*Executing all =#+name: startup-code= for local configurations][Executing all =#+name: startup-code= for local configurations:2]]
;; Please ask me on a file by file basis whether its local variables are ‘safe’
;; or not. Use ‘!’ to mark them as permanently ‘safe’ to avoid being queried
;; again for the same file.
(setq enable-local-variables t)
;; Executing all =#+name: startup-code= for local configurations:2 ends here

;; [[file:init.org::*Word Completion and Documentation Pop-ups][Word Completion and Documentation Pop-ups:1]]
(use-package company

  :config
  (global-company-mode 1)
  (setq ;; Only 2 letters required for completion to activate.
   company-minimum-prefix-length 2

   ;; Search other buffers for compleition candidates
   company-dabbrev-other-buffers t
   company-dabbrev-code-other-buffers t

   ;; Show candidates according to importance, then case, then in-buffer frequency
   company-transformers '(company-sort-by-backend-importance
                          company-sort-prefer-same-case-prefix
                          company-sort-by-occurrence)

   ;; Flushright any annotations for a compleition;
   ;; e.g., the description of what a snippet template word expands into.
   company-tooltip-align-annotations t

   ;; Allow (lengthy) numbers to be eligible for completion.
   company-complete-number t

   ;; M-⟪num⟫ to select an option according to its number.
   company-show-numbers t

   ;; Show 10 items in a tooltip; scrollbar otherwise or C-s ^_^
   company-tooltip-limit 10

   ;; Edge of the completion list cycles around.
   company-selection-wrap-around t

   ;; Do not downcase completions by default.
   company-dabbrev-downcase nil

   ;; Even if I write something with the ‘wrong’ case,
   ;; provide the ‘correct’ casing.
   company-dabbrev-ignore-case nil

   ;; Immediately activate completion.
   company-idle-delay 0)

  ;; Use C-/ to manually start company mode at point. C-/ is used by undo-tree.
  ;; Override all minor modes that use C-/; bind-key* is discussed below.
  (bind-key* "C-/" #'company-manual-begin)

  ;; Bindings when the company list is active.
  :bind (:map company-active-map
              ("C-d" . company-show-doc-buffer) ;; In new temp buffer
              ("<tab>" . company-complete-selection)
              ;; Use C-n,p for navigation in addition to M-n,p
              ("C-n" . (lambda () (interactive) (company-complete-common-or-cycle 1)))
              ("C-p" . (lambda () (interactive) (company-complete-common-or-cycle -1)))))

;; It's so fast that we don't need a key-binding to start it!
;; Word Completion and Documentation Pop-ups:1 ends here

;; [[file:init.org::*Word Completion and Documentation Pop-ups][Word Completion and Documentation Pop-ups:3]]
(defun my/ensure-machine-works-as-expected ()
  "Run all my personal tests to ensure Emacs behaves as I expect it to."
  (interactive)
  (load-file "init-test.el")
  (ert t)
  (ert-results-pop-to-timings))
;; Word Completion and Documentation Pop-ups:3 ends here

;; [[file:init.org::*Word Completion and Documentation Pop-ups][Word Completion and Documentation Pop-ups:4]]
(use-package company-emoji
  :config (add-to-list 'company-backends 'company-emoji))
;; Word Completion and Documentation Pop-ups:4 ends here

;; [[file:init.org::*Word Completion and Documentation Pop-ups][Word Completion and Documentation Pop-ups:5]]
(use-package emojify
 :config (setq emojify-display-style 'image)
 :init (global-emojify-mode 1)) ;; Will install missing images, if need be.
;; Word Completion and Documentation Pop-ups:5 ends here

;; [[file:init.org::*Documentation Pop-Ups][Documentation Pop-Ups:1]]
(use-package company-quickhelp
  :config
  (setq company-quickhelp-delay 0.1)
  (company-quickhelp-mode)
  ;; Especially when learning a new language, looking up its definition/docstring can be helpful.
  ;; Note: I use “M-!” everywhere else to mean “define word at point”.
  )

;; TODO: Consider adding a hook to company mode to immediately call (company-quickhelp--show), in prog-modes
;; Documentation Pop-Ups:1 ends here

;; [[file:init.org::*Intro to Snippets][Intro to Snippets:1]]
;; Add yasnippet support for all company backends
;;
(cl-defun my/company-backend-with-yankpad (backend)
  "There can only be one main completition backend, so let's
   enable yasnippet/yankpad as a secondary for all completion
   backends.

   Src: https://emacs.stackexchange.com/a/10520/10352"

  (if (and (listp backend) (member 'company-yankpad backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yankpad))))
;; Intro to Snippets:1 ends here

;; [[file:init.org::*Intro to Snippets][Intro to Snippets:2]]
;; Yet another snippet extension program
(use-package yasnippet
  :config
    (yas-global-mode 1) ;; Always have this on for when using yasnippet syntax within yankpad
    ;; respect the spacing in my snippet declarations
    (setq yas-indent-line 'fixed))

;; Alternative, Org-based extension program
(use-package yankpad

  :config
    ;; Location of templates
    (setq yankpad-file "~/.emacs.d/yankpad.org")

    ;; Ignore major mode, always use defaults.
    ;; Yankpad will freeze if no org heading has the name of the given category.
    (setq yankpad-category "Default")

    ;; Load the snippet templates ---useful after yankpad is altered
    (yankpad-reload)

    ;; Set company-backend as a secondary completion backend to all existing backends.
    (setq company-backends (mapcar #'my/company-backend-with-yankpad company-backends)))
;; Intro to Snippets:2 ends here

;; [[file:init.org::*Intro to Snippets][Intro to Snippets:5]]
(cl-defun org-insert-link ()
  "Makes an org link by inserting the URL copied to clipboard and
  prompting for the link description only.

  Type over the shown link to change it, or tab to move to the
  description field.

  This overrides Org-mode's built-in ‘org-insert-link’ utility;
  whence C-c C-l uses the snippet."
  (interactive)
  (insert "my_org_insert_link")
  (yankpad-expand))
;; Intro to Snippets:5 ends here

;; [[file:init.org::*Emojis][Emojis:2]]
;; Get all unicode emojis to appear within Emacs
;; See also: https://emacs.stackexchange.com/questions/5689/force-a-single-font-for-all-unicode-glyphs?rq=1
(unless noninteractive (set-fontset-font t nil "Apple Color Emoji"))
;; Emojis:2 ends here

;; [[file:init.org::*Prettify inline source code][Prettify inline source code:1]]
;; Show “ src_emacs-lisp[:exports results]{ 𝒳 } ” as “ ℰ𝓁𝒾𝓈𝓅﴾ 𝒳 ﴿ ”.
;;
(font-lock-add-keywords 'org-mode
  '(("\\(src_emacs-lisp\\[.*]{\\)\\([^}]*\\)\\(}\\)"
  (1 '(face (:inherit (bold) :foreground "gray65") display "ℰ𝓁𝒾𝓈𝓅﴾"))
  (2 '(face (:foreground "blue")))
  (3 '(face (:inherit (bold) :foreground "gray65") display "﴿"))
    )))
;;
;; Let's do this for all my languages:
;; Show “ src_LANGUAGE[…]{ ⋯ } ” as “ ﴾ ⋯ ﴿ ”.
(cl-loop for lang in my/programming-languages
         do (font-lock-add-keywords 'org-mode
               `(( ,(format "\\(src_%s\\[.*]{\\)\\([^}]*\\)\\(}\\)" lang)
                  (1 '(face (:inherit (bold) :foreground "gray65") display "﴾"))
                  (2 '(face (:foreground "blue")))
                  (3 '(face (:inherit (bold) :foreground "gray65") display "﴿"))
                  ))))

;;
(defun my/toggle-line-fontification ()
  "Toggle the fontification of the current line"
  (interactive)
  (defvar my/toggle-fontify/current-line -1)
  (defvar my/toggle-fontify/on? nil)
  (add-to-list 'font-lock-extra-managed-props 'display)
  (let ((start (line-beginning-position)) (end (line-end-position)))
    (cond
     ;; Are we toggling the current line?
     ((= (line-number-at-pos) my/toggle-fontify/current-line)
      (if my/toggle-fontify/on?
          (font-lock-fontify-region start end)
        (font-lock-unfontify-region start end))
      (setq my/toggle-fontify/on? (not my/toggle-fontify/on?)))
     ;; Nope, we've moved on to another line.
     (:otherwise
      (setq my/toggle-fontify/current-line (line-number-at-pos)
            my/toggle-fontify/on? :yes_please_fontify)
      (font-lock-unfontify-region  start end)))))

  ;; TODO FIXME; maybe ignore: Wasted too much time here already.
;; (add-hook 'post-command-hook #'my/toggle-line-fontification nil t)
;; (font-lock-add-keywords nil '((my/toggle-line-fontification)) t)
;; Prettify inline source code:1 ends here

;; [[file:init.org::*Unfold Org Headings when I perform a search][Unfold Org Headings when I perform a search:1]]
(setq org-fold-core-style 'overlays)
;; Unfold Org Headings when I perform a search:1 ends here

;; [[file:init.org::*Undo-tree: Very Local Version Control][Undo-tree: Very Local Version Control:2]]
;; By default C-z is suspend-frame, i.e., minimise, which I seldom use.
(global-set-key (kbd "C-z")
  (lambda () (interactive)
   (undo-tree-mode) ;; Ensure the mode is on
   (undo-tree-visualize)))
;; Undo-tree: Very Local Version Control:2 ends here

;; [[file:init.org::*Automatic Backups][Automatic Backups:1]]
;; New location for backups.
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Silently delete execess backup versions
(setq delete-old-versions t)

;; Only keep the last 1000 backups of a file.
(setq kept-old-versions 1000)

;; Even version controlled files get to be backed up.
(setq vc-make-backup-files t)

;; Use version numbers for backup files.
(setq version-control t)
;; Automatic Backups:1 ends here

;; [[file:init.org::*Automatic Backups][Automatic Backups:2]]
(setq confirm-kill-processes nil
      create-lockfiles nil)
;; Automatic Backups:2 ends here

;; [[file:init.org::*What changed? ---Walking through backups][What changed? ---Walking through backups:1]]
(use-package backup-walker
  :commands backup-walker-start)
;; What changed? ---Walking through backups:1 ends here

;; [[file:init.org::*Save ≈ Backup][Save ≈ Backup:1]]
;; Make Emacs backup everytime I save

(defun my/force-backup-of-buffer ()
  "Lie to Emacs, telling it the curent buffer has yet to be backed up."
  (setq buffer-backed-up nil))

(add-hook 'before-save-hook  'my/force-backup-of-buffer)

;; [Default settings]
;; Autosave when idle for 30sec or 300 input events performed
(setq auto-save-timeout 30
      auto-save-interval 300)
;; Save ≈ Backup:1 ends here

;; [[file:init.org::*Intro][Intro:1]]
;; Bottom of Emacs will show what branch you're on
;; and whether the local file is modified or not.
(use-package magit
  :init (require 'magit-files)
  :bind (("C-c M-g" . magit-file-dispatch))
  :custom ;; Do not ask about this variable when cloning.
    (magit-clone-set-remote.pushDefault t))
;; Intro:1 ends here

;; [[file:init.org::*Intro][Intro:2]]
;; When we invoke magit-status, show green/red the altered lines, with extra
;; green/red on the subparts of a line that got alerted.
(system-packages-ensure "git-delta")
(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

;; Don't forget to copy/paste the delta config into the global ~/.gitconfig file.
;; Copy/paste this: https://github.com/dandavison/delta#get-started
;; Intro:2 ends here

;; [[file:init.org::*Credentials: I am who I am][Credentials: I am who I am:1]]
;; Only set these creds up if there is no Git email set up ---ie at work I have an email set up, so don't
;; override it with my personal creds.
;;
;; See here for a short & useful tutorial:
;; https://alvinalexander.com/git/git-show-change-username-email-address
(when (equal "" (shell-command-to-string "git config user.email "))
  (shell-command (format "git config --global user.name \"%s\"" user-full-name))
  (shell-command (format "git config --global user.email \"%s\"" user-mail-address)))
;; Credentials: I am who I am:1 ends here

;; [[file:init.org::*Credentials: I am who I am][Credentials: I am who I am:2]]
;; We want to reuse an existing Emacs process from the command line
;; E.g.,  emacsclient --eval '(+ 1 2)'    # ⇒ 3
(server-start)

;; Or use it whenever we are editing a git message from the terminal
(shell-command "git config --global core.editor 'emacsclient -t -a=\\\"\\\"'")
;; Credentials: I am who I am:2 ends here

;; [[file:init.org::*Encouraging useful commit messages][Encouraging useful commit messages:1]]
(defun my/git-commit-reminder ()
  (insert "\n\n# The commit subject line ought to finish the phrase:
# “If applied, this commit will ⟪your subject line here⟫.” ")
  (beginning-of-buffer))

(add-hook 'git-commit-setup-hook 'my/git-commit-reminder)
;; Encouraging useful commit messages:1 ends here

;; [[file:init.org::*Maybe clone ... everything?][Maybe clone ... everything?:1]]
;; Clone git repo from clipboard
(cl-defun maybe-clone (remote &optional local)
  "Clone a REMOTE repository [from clipboard] if the LOCAL directory does not exist.

If called interactively, clone URL in clipboard into ~/Downloads then open in dired.

Yields ‘repo-already-exists’ when no cloning transpires, otherwise yields ‘cloned-repo’.

LOCAL is optional and defaults to the base name; e.g.,
if REMOTE is https://github.com/X/Y then LOCAL becomes ∼/Y."
  (interactive "P")

  (when (interactive-p)
    (setq remote (substring-no-properties (current-kill 0)))
    (cl-assert (string-match-p "^\\(http\\|https\\|ssh\\)://" remote) nil "No URL in clipboard"))

  (unless local
    (setq local (concat "~/" (if (interactive-p) "Downloads/" "") (file-name-base remote))))

  ;; (require 'magit-repos) ;; Gets us the magit-repository-directories variable.
  ;; (add-to-list 'magit-repository-directories `(,local . 0))

  (if (file-directory-p local)
      'repo-already-exists
    (shell-command (concat "git clone " remote " " local))
    (dired local)
    'cloned-repo))


(maybe-clone "https://github.com/alhassy/emacs.d" "~/.emacs.d")
(maybe-clone "https://github.com/alhassy/alhassy.github.io" "~/blog")
(maybe-clone "https://github.com/alhassy/holy-books")
;; Maybe clone ... everything?:1 ends here

;; [[file:init.org::*Maybe clone ... everything?][Maybe clone ... everything?:2]]
(maybe-clone "https://github.com/alhassy/melpa")
(maybe-clone "https://github.com/alhassy/org-special-block-extras")


;; (maybe-clone "https://github.com/alhassy/next-700-module-systems-proposal.git" "~/thesis-proposal")
;; (maybe-clone "https://github.com/JacquesCarette/MathScheme")
;; (maybe-clone "https://github.com/alhassy/gentle-intro-to-reflection" "~/reflection/")
;; (maybe-clone "https://github.com/alhassy/org-agda-mode")
;; (maybe-clone "https://github.com/JacquesCarette/TheoriesAndDataStructures")
;; (maybe-clone "https://gitlab.cas.mcmaster.ca/RATH/RATH-Agda"     "~/RATH-Agda")
;; (maybe-clone "https://github.com/alhassy/MyUnicodeSymbols") ;; Deleted?

(maybe-clone "https://github.com/alhassy/islam")
(maybe-clone "https://github.com/alhassy/CheatSheet")
(maybe-clone "https://github.com/alhassy/ElispCheatSheet")
;; (maybe-clone "https://github.com/alhassy/CatsCheatSheet")
;; (maybe-clone "https://github.com/alhassy/OCamlCheatSheet")
;; (maybe-clone "https://github.com/alhassy/AgdaCheatSheet")
(maybe-clone "https://github.com/alhassy/RubyCheatSheet")
;; (maybe-clone "https://github.com/alhassy/PrologCheatSheet")
;; (maybe-clone "https://github.com/alhassy/FSharpCheatSheet")


;; (maybe-clone "https://gitlab.cas.mcmaster.ca/armstmp/cs3mi3.git" "~/3mi3")
;; (maybe-clone "https://gitlab.cas.mcmaster.ca/alhassm/CAS781" "~/cas781") ;; cat adventures
;; (maybe-clone "https://gitlab.cas.mcmaster.ca/carette/cs3fp3.git" "~/3fp3")
;; (maybe-clone "https://github.com/alhassy/interactive-way-to-c")
;; (maybe-clone "https://gitlab.cas.mcmaster.ca/3ea3-winter2019/assignment-distribution.git" "~/3ea3/assignment-distribution")
;; (maybe-clone "https://gitlab.cas.mcmaster.ca/3ea3-winter2019/notes.git" "~/3ea3/notes")
;; (maybe-clone "https://gitlab.cas.mcmaster.ca/3ea3-winter2019/assignment-development.git" "~/3ea3/assignment-development")
;; (maybe-clone "https://gitlab.cas.mcmaster.ca/3ea3-winter2019/kandeeps.git" "~/3ea3/sujan")
;; (maybe-clone "https://gitlab.cas.mcmaster.ca/3ea3-winter2019/horsmane.git" "~/3ea3/emily")
;; (maybe-clone "https://gitlab.cas.mcmaster.ca/3ea3-winter2019/anderj12.git" "~/3ea3/jacob")
;; (maybe-clone "https://gitlab.cas.mcmaster.ca/alhassm/3EA3.git" "~/3ea3/_2018")
;; (maybe-clone "https://gitlab.cas.mcmaster.ca/2DM3/LectureNotes.git" "~/2dm3")
;; Maybe clone ... everything?:2 ends here

;; [[file:init.org::*Gotta love that time machine][Gotta love that time machine:1]]
(use-package git-timemachine )
;; Gotta love that time machine:1 ends here

;; [[file:init.org::*Jump to a (ma)git repository with ~C-u C-x g~][Jump to a (ma)git repository with ~C-u C-x g~:1]]
;; Jump to a (ma)git repository with C-u C-x g.
;;
;; To get a selection of repositories (that have been visited at least once),
;; call with “C-u M-x magit-status” or “C-u C-x g”; use “C-u C-u C-x g” to
;; manually enter a path to a repository.
;;
;; We use projectile's record of known projects, and keep only projects with
;; .git directory.
(with-eval-after-load 'projectile
  (setq magit-repository-directories
        (thread-last (projectile-relevant-known-projects)
          (--filter (unless (file-remote-p it)
                      (file-directory-p (concat it "/.git/"))))
          (--map (list (substring it 0 -1) 0)))))

;; Follow-up utility
(defun my/update-repos ()
  "Update (git checkout main & pull) recently visited repositories."
  (interactive)
  (cl-loop for (repo _depth) in magit-repository-directories
        ;; Is it “main” or “master”
        for trunk = (s-trim (shell-command-to-string (format "cd %s; git symbolic-ref refs/remotes/origin/HEAD | sed 's@^refs/remotes/origin/@@'" repo)))
        do (message (format "🤖 %s ∷ Checking out & pulling main" repo))
           (shell-command (format "cd %s; git checkout %s; git pull" repo trunk)))
  (message "🥳 Happy coding!"))
;; Jump to a (ma)git repository with ~C-u C-x g~:1 ends here

;; [[file:init.org::*Pretty Magit Commit Leaders][Pretty Magit Commit Leaders:1]]
(cl-defmacro pretty-magit (WORD ICON PROPS &optional (description "") NO-PROMPT?)
  "Replace sanitized WORD with ICON, PROPS and by default add to prompts."
  `(prog1
     (add-to-list 'pretty-magit-alist
                  (list (rx bow (group ,WORD (eval (if ,NO-PROMPT? "" ":"))))
                        ,ICON ',PROPS))
     (unless ,NO-PROMPT?
       (add-to-list 'pretty-magit-prompt (cons (concat ,WORD ": ") ,description)))))

(setq pretty-magit-alist nil)
(setq pretty-magit-prompt nil)
;; Pretty Magit Commit Leaders:1 ends here

;; [[file:init.org::*Pretty Magit Commit Leaders][Pretty Magit Commit Leaders:2]]
(pretty-magit "Add"      ?➕ (:foreground "#375E97" :height 1.2) "✅ Create a capability e.g. feature, test, dependency.")
(pretty-magit "Delete"   ?❌ (:foreground "#375E97" :height 1.2) "❌ Remove a capability e.g. feature, test, dependency.")
(pretty-magit "Fix"      ?🔨 (:foreground "#FB6542" :height 1.2) "🐛 Fix an issue e.g. bug, typo, accident, misstatement.")
(pretty-magit "Clean"    ?🧹 (:foreground "#FFBB00" :height 1.2) "✂ Refactor code; reformat say by altering whitespace; refactor performance.")
(pretty-magit "Document" ?📚 (:foreground "#3F681C" :height 1.2) "ℹ Refactor of documentation, e.g. help files.")
(pretty-magit "Feature"  ?⛲ (:foreground "slate gray" :height 1.2) "⛳ 🇮🇶🇨🇦 A milestone commit - flagpost")
(pretty-magit "Generate"  ?🔭 (:foreground "slate gray" :height 1.2) "Export PDF/HTML or tangle raw code from a literate program") ;; Generating artefacts
(pretty-magit "master"   ? (:box t :height 1.2) "" t)
(pretty-magit "origin"   ?🐙 (:box t :height 1.2) "" t)
;; Commit leader examples: https://news.ycombinator.com/item?id=13889155.
;;
;; Cut ~ Remove a capability e.g. feature, test, dependency.
;; Bump ~ Increase the version of something e.g. dependency.
;; Make ~ Change the build process, or tooling, or infra.
;; Start ~ Begin doing something; e.g. create a feature flag.
;; Stop ~ End doing something; e.g. remove a feature flag.
;; Pretty Magit Commit Leaders:2 ends here

;; [[file:init.org::*Pretty Magit Commit Leaders][Pretty Magit Commit Leaders:3]]
(defun add-magit-faces ()
  "Add face properties and compose symbols for buffer from pretty-magit."
  (interactive)
  (with-silent-modifications
    (--each pretty-magit-alist
      (-let (((rgx icon props) it))
        (save-excursion
          (goto-char (point-min))
          (while (search-forward-regexp rgx nil t)
            (compose-region
             (match-beginning 1) (match-end 1) icon)
            (when props
              (add-face-text-property
               (match-beginning 1) (match-end 1) props))))))))

(advice-add 'magit-status :after 'add-magit-faces)
(advice-add 'magit-refresh-buffer :after 'add-magit-faces)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq use-magit-commit-prompt-p nil)
(defun use-magit-commit-prompt (&rest args)
  (setq use-magit-commit-prompt-p t))

(defun magit-commit-prompt ()
  "Magit prompt and insert commit header with faces."
  (interactive)
  (when use-magit-commit-prompt-p
    (setq use-magit-commit-prompt-p nil)
    (thread-last (--map (format "%s %s" (car it) (cdr it)) pretty-magit-prompt)
      (completing-read "Insert commit leader ∷ ")
      ;; My “Generate:” commit type has one use case, for now; so let's insert it filled-in.
      (funcall (lambda (it) (if (s-starts-with? "Generate:" it) it (car (s-split " " it)))))
      (insert)
      (end-of-line))
    (add-magit-faces)))


(remove-hook 'git-commit-setup-hook 'with-editor-usage-message)
(add-hook 'git-commit-setup-hook 'magit-commit-prompt)
(advice-add 'magit-commit :after 'use-magit-commit-prompt)
;; Pretty Magit Commit Leaders:3 ends here

;; [[file:init.org::*Highlighting TODO-s & Showing them in Magit][Highlighting TODO-s & Showing them in Magit:1]]
;; NOTE that the highlighting works even in comments.
(use-package hl-todo
  ;; I want todo-words highlighted in prose, not just in code fragements.
  :hook (org-mode . hl-todo-mode)
  :config
    ;; Adding new keywords
    (cl-loop for kw in '("TEST" "MA" "WK" "JC")
             do (add-to-list 'hl-todo-keyword-faces (cons kw "#dc8cc3")))
    ;; Enable it everywhere.
    (global-hl-todo-mode))
;; Highlighting TODO-s & Showing them in Magit:1 ends here

;; [[file:init.org::*Highlighting TODO-s & Showing them in Magit][Highlighting TODO-s & Showing them in Magit:3]]
(defun add-watchwords () "Add TODO: words to font-lock keywords."
  (font-lock-add-keywords nil
                          '(("\\(\\<TODO\\|\\<FIXME\\|\\<HACK\\|@.+\\):" 1
                             font-lock-warning-face t))))

(add-hook 'prog-mode-hook #'add-watchwords)
;; Highlighting TODO-s & Showing them in Magit:3 ends here

;; [[file:init.org::*Highlighting TODO-s & Showing them in Magit][Highlighting TODO-s & Showing them in Magit:4]]
;; MA: The todo keywords work in code too!
(use-package magit-todos
  :after magit
  :after hl-todo
  ;; :hook (org-mode . magit-todos-mode)
  :config
  ;; For some reason cannot use :custom with this package.
  (custom-set-variables
    '(magit-todos-keywords (list "TODO" "FIXME" "MA" "WK" "JC")))
  ;; Ignore TODOs mentioned in exported HTML files; they're duplicated from org src.
  (setq magit-todos-exclude-globs '("*.html"))
  (magit-todos-mode))
;; Highlighting TODO-s & Showing them in Magit:4 ends here

;; [[file:init.org::*delete-by-moving-to-trash t][delete-by-moving-to-trash t:1]]
;; Move to OS’ trash can when deleting stuff
;; instead of deleting things outright!
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/")
;; delete-by-moving-to-trash t:1 ends here

;; [[file:init.org::*Jumping to extreme semantic units][Jumping to extreme semantic units:1]]
;; M-< and M-> jump to first and final semantic units.
;; If pressed twice, they go to physical first and last positions.
(use-package beginend
  :config (beginend-global-mode))
;; Jumping to extreme semantic units:1 ends here

;; Get org-headers to look pretty! E.g., * → ⊙, ** ↦ ◯, *** ↦ ★
;; https://github.com/emacsorphanage/org-bullets
(use-package org-bullets
  :defer nil
  :hook (org-mode . org-bullets-mode))

;; Silence the usual message: Get more info using the about page via C-h C-a.
(setq inhibit-startup-message t)

(defun display-startup-echo-area-message ()
  "The message that is shown after ‘user-init-file’ is loaded."
  (message
      (concat "Welcome "      user-full-name
              "! Emacs "      emacs-version
              "; Org-mode "   org-version
              "; System "     (symbol-name system-type)
              "/"             (system-name)
              "; Time "       (emacs-init-time))))

;; Keep self motivated!
(setq frame-title-format '("" "%b - Living The Dream (•̀ᴗ•́)و"))

;; If work machine, then show notes; otherwise show my todos & init side-by-side.
(unless noninteractive
  ;; Only run the following when we're in GUI mode;
  ;; i.e., don't run it in Github Actions when testing.
  (if (not my/personal-machine?)
      (find-file "~/Documents/notes.org")) ;; Org-journal for work
  (find-file "~/Dropbox/todo.org")
  ;; After startup, if Emacs is idle for 10 seconds, then open my work file;
  ;; which is a GPG file and so requires passphrase before other things can load.
  ;; (run-with-idle-timer 10 nil (lambda () (find-file "~/Desktop/work.org.gpg")))
  (split-window-right)                          ;; C-x 3
  (other-window 1)                              ;; C-x 0
  (let ((enable-local-variables :all)           ;; Load *all* locals.
        (org-confirm-babel-evaluate nil))       ;; Eval *all* blocks.
    (ignore-errors (find-file "~/.emacs.d/init.org"))))

;; The modeline looks really nice with doom-themes, e.g., doom-solarised-light.
(use-package doom-modeline
  :defer nil
  :config (doom-modeline-mode))

  ;; Use minimal height so icons still fit; modeline gets slightly larger when
  ;; buffer is modified since the "save icon" shows up.  Let's disable the icon.
  ;; Let's also essentially disable the hud bar, a sort of progress-bar on where we are in the buffer.
  (setq doom-modeline-height 1)
  (setq doom-modeline-buffer-state-icon nil)
  (setq doom-modeline-hud t)
  (setq doom-modeline-bar-width 1)

  ;; Show 3 Flycheck numbers: “red-error / yellow-warning / green-info”, which
  ;; we can click to see a listing.
  ;; If not for doom-modeline, we'd need to use flycheck-status-emoji.el.
  (setq doom-modeline-checker-simple-format nil)

  ;; Don't display the buffer encoding, E.g., “UTF-8”.
  (setq doom-modeline-buffer-encoding nil)

  ;; Inactive buffers' modeline is greyed out.
  ;; (let ((it "Source Code Pro Light" ))
  ;;   (set-face-attribute 'mode-line nil :family it :height 100)
  ;;   (set-face-attribute 'mode-line-inactive nil :family it :height 100))

  ;; A quick hacky way to add stuff to doom-modeline is to add to the mode-line-process list.
  ;; E.g.:  (add-to-list 'mode-line-process '(:eval (format "%s" (count-words (point-min) (point-max)))))
  ;; We likely want to add this locally, to hooks on major modes.

  (setq doom-modeline-minor-modes t)
  (use-package minions

    :init (minions-mode 1))

;; If not for doom-modeline, we'd need to use fancy-battery-mode.el.
(display-battery-mode +1)

;; Show date and time as well.

;; [Simple Approach]
;; (setq display-time-day-and-date t)
;; (display-time)

;; [More Controlled Approach: Set date&time format]
;; a ≈ weekday; b ≈ month; d ≈ numeric day, R ≈ 24hr:minute.
(setq display-time-format "%a %b %d ╱ %r") ;; E.g.,:  Fri Mar 04 ╱ 03:42:08 pm
(setq display-time-interval 1) ;; Please update the time every second.
(display-time-mode)

;; I don't need the system load average in the modeline.
(setq display-time-default-load-average nil)
(setq display-time-load-average nil)

;; ;; Do not show me line numbers, nor column numbers, in the modeline
(column-number-mode -1)
(line-number-mode   -1)

;; Likewise, no need to show me “Top∣Mid∣Bot” in the modeline.
(setq-default mode-line-percent-position nil)

;; (setq display-line-numbers-width-start t)
;; (global-display-line-numbers-mode      t)

;; Treat all themes as safe; no query before use.
(setf custom-safe-themes t)

;; Nice looking themes ^_^
(use-package solarized-theme   )
(use-package doom-themes )
(use-package spacemacs-common

  :ensure spacemacs-theme)
(use-package stimmung-themes )
(use-package shanty-themes )

;; Infinite list of my commonly used themes.
(setq my/themes '(doom-laserwave shanty-themes-light stimmung-themes-light stimmung-themes-dark doom-solarized-light doom-vibrant spacemacs-light solarized-gruvbox-dark solarized-gruvbox-light))
(setcdr (last my/themes) my/themes)

(cl-defun my/load-theme (&optional (new-theme (completing-read "Theme: " (custom-available-themes))))
  "Disable all themes and load the given one ---read from user when called interactively."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme new-theme)
  (message "Theme %s" new-theme))

(cl-defun my/toggle-theme (&optional (new-theme (pop my/themes)))
  "Disable all themes and load NEW-THEME, which defaults from ‘my/themes’.

When a universal prefix is given, “C-u C-c t”, we load a random
theme from all possible themes.  Nice way to learn about more
themes (•̀ᴗ•́)و"
  (interactive)
  (-let [theme (if current-prefix-arg
                   (nth (random (length (custom-available-themes)))
                        (custom-available-themes))
                 new-theme)]
    (my/load-theme theme)))

(global-set-key "\C-c\ t" 'my/toggle-theme)

;; (my/toggle-theme)
(my/toggle-theme 'doom-laserwave)

(unless nil ;; my/work-machine?

  ;; Infinite list of my commonly used fonts
  (setq my/fonts
        '(;; NOPE: Breaks Gerrit! "Roboto Mono Light 14" ;; Sleek
          "Input Mono 14"
          "Source Code Pro Light 14" ;; thin, similar to Inconsolata Light
          "Papyrus 14"
          "Bradley Hand Light 12"
          ;; "Chalkduster 14" ;; Laggy!
          "Courier Light 12"
          "Noteworthy 9"
          "Savoye LET 14"
          "Fantasque Sans Mono 16"
          ))
  (setcdr (last my/fonts) my/fonts)

  ;; Let's ensure they're on our system
  ;; brew search "/font-/"   # List all fonts

  (shell-command "brew tap homebrew/cask-fonts")
  (system-packages-ensure "svn") ;; Required for the following font installs
  ;; No thanks! (system-packages-ensure "font-roboto-mono") ;; Makes Gerrit in Chrome look like Gibberish!
  (system-packages-ensure "font-input")
  (system-packages-ensure "font-source-code-pro")
  (system-packages-ensure "font-fira-mono")
  (system-packages-ensure "font-mononoki")
  (system-packages-ensure "font-monoid")
  (system-packages-ensure "font-menlo-for-powerline")
  (system-packages-ensure "font-fantasque-sans-mono")

;; Use “M-x set-face-font RET default RET”, or...
;; (set-face-font 'default "Source Code Pro Light14")

;; See ~2232 fonts
;; (append (fontset-list) (x-list-fonts "*" nil))

  (cl-defun my/toggle-font (&optional (new-font (pop my/fonts)))
  "Load NEW-FONT, which defaults from ‘my/fonts’.

When a universal prefix is given, “C-u C-c F”, we load a random
font from all possible themes.  Nice way to learn about more
fonts (•̀ᴗ•́)و"
  (interactive)
  (let* ((all-fonts (append (fontset-list) (x-list-fonts "*" nil)))
         (font (if current-prefix-arg
                   (nth (random (length all-fonts)) all-fonts)
                 new-font)))
    (set-face-font 'default font)
    (message "Font: %s" font)))

  (global-set-key "\C-c\ F" 'my/toggle-font)

  ;; Default font; the “ignore-⋯” is for users who may not have the font.
  (ignore-errors (my/toggle-font "Fantasque Sans Mono 12"))
  (ignore-errors (my/toggle-font "Source Code Pro Light 14")))

(unless noninteractive
  ;; Breaks Gerrit: (my/toggle-font "Roboto Mono Light 14")
  (my/toggle-theme 'solarized-gruvbox-light))

;; Make it very easy to see the line with the cursor.
(global-hl-line-mode t)

(use-package beacon
  :defer nil
  :config (setq beacon-color "#666600")
  :hook   ((org-mode text-mode) . beacon-mode))

(use-package dimmer
  :defer nil
  :config (dimmer-mode))

;; (setq visible-bell 1) ;; On MacOS, this shows a caution symbol ^_^

;; The doom themes package comes with a function to make the mode line flash on error.
(use-package doom-themes   :defer nil)
(require 'doom-themes-ext-visual-bell)
(doom-themes-visual-bell-config)

(blink-cursor-mode 1)

(unless noninteractive
  (tool-bar-mode   -1)    ;; No large icons please
  (scroll-bar-mode -1))   ;; No visual indicator please
  ;; (menu-bar-mode   -1) ;; The Mac OS top pane has menu options

(setq show-paren-delay  0)
(setq show-paren-style 'mixed)
(show-paren-mode)

(use-package rainbow-delimiters
  :disabled
  :hook ((org-mode prog-mode text-mode) . rainbow-delimiters-mode))

(electric-pair-mode 1)

;; The ‘<’ and ‘>’ are not ‘parenthesis’, so give them no compleition.
(setq electric-pair-inhibit-predicate
      (lambda (c)
        (or (member c '(?< ?> ?~)) (electric-pair-default-inhibit c))))

;; Treat ‘<’ and ‘>’ as if they were words, instead of ‘parenthesis’.
(modify-syntax-entry ?< "w<")
(modify-syntax-entry ?> "w>")

(set-face-attribute 'org-document-title nil :height 2.0)
;; (set-face-attribute 'org-level-1 nil :height 1.0)
;; Remaining org-level-𝒾 have default height 1.0, for 𝒾 : 1..8.
;;
;; E.g., reset org-level-1 to default.
;; (custom-set-faces '(org-level-1 nil))

  (defvar-local rasmus/org-at-src-begin -1
    "Variable that holds whether last position was a ")

  (defvar rasmus/ob-header-symbol ?☰
    "Symbol used for babel headers")

  (defun rasmus/org-prettify-src--update ()
    (let ((case-fold-search t)
          (re "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*")
          found)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (goto-char (match-end 0))
          (let ((args (org-trim
                       (buffer-substring-no-properties (point)
                                                       (line-end-position)))))
            (when (org-string-nw-p args)
              (let ((new-cell (cons args rasmus/ob-header-symbol)))
                (cl-pushnew new-cell prettify-symbols-alist :test #'equal)
                (cl-pushnew new-cell found :test #'equal)))))
        (setq prettify-symbols-alist
              (cl-set-difference prettify-symbols-alist
                                 (cl-set-difference
                                  (cl-remove-if-not
                                   (lambda (elm)
                                     (eq (cdr elm) rasmus/ob-header-symbol))
                                   prettify-symbols-alist)
                                  found :test #'equal)))
        ;; Clean up old font-lock-keywords.
        (font-lock-remove-keywords nil prettify-symbols--keywords)
        (setq prettify-symbols--keywords (prettify-symbols--make-keywords))
        (font-lock-add-keywords nil prettify-symbols--keywords)
        (while (re-search-forward re nil t)
          (font-lock-flush (line-beginning-position) (line-end-position))))))

  (defun rasmus/org-prettify-src ()
    "Hide src options via `prettify-symbols-mode'.

  `prettify-symbols-mode' is used because it has uncollpasing. It's
  may not be efficient."
    (let* ((case-fold-search t)
           (at-src-block (save-excursion
                           (beginning-of-line)
                           (looking-at "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*"))))
      ;; Test if we moved out of a block.
      (when (or (and rasmus/org-at-src-begin
                     (not at-src-block))
                ;; File was just opened.
                (eq rasmus/org-at-src-begin -1))
        (rasmus/org-prettify-src--update))
      ;; Remove composition if at line; doesn't work properly.
      ;; (when at-src-block
      ;;   (with-silent-modifications
      ;;     (remove-text-properties (match-end 0)
      ;;                             (1+ (line-end-position))
      ;;                             '(composition))))
      (setq rasmus/org-at-src-begin at-src-block)))

  (defun rasmus/org-prettify-symbols ()
    (mapc (apply-partially 'add-to-list 'prettify-symbols-alist)
          (cl-reduce 'append
                     (mapcar (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                             `(("#+begin_src" . ?✎) ;; ➤ 🖝 ➟ ➤ ✎
                               ("#+end_src"   . ?□) ;; ⏹
                               ("#+header:" . ,rasmus/ob-header-symbol)
                               ("#+begin_quote" . ?»)
                               ("#+end_quote" . ?«)))))
    (turn-on-prettify-symbols-mode)
    (add-hook 'post-command-hook 'rasmus/org-prettify-src t t))


;; Last up­dated: 2019-06-09

(add-hook 'org-mode-hook #'rasmus/org-prettify-symbols)
(org-mode-restart)

(global-prettify-symbols-mode)

(defvar my/prettify-alist nil
  "Musa's personal prettifications.")

(cl-loop for pair in '(;; Example of how pairs like this to beautify org block delimiters
                       ("#+begin_example" . (?ℰ (Br . Bl) ?⇒)) ;; ℰ⇒
                       ("#+end_example"   . ?⇐)                 ;; ⇐
                       ;; Actuall beautifications
                       ("==" . ?≈) ("===" . ?≈) ;; ("=" . ?≔) ;; Programming specific prettifications
                       ("i32" . ?ℤ) ("u32" . ?ℕ) ("f64" . ?ℝ) ;; Rust specific
                       ("bool" . ?𝔹)
                       ;; ("\"\"\"\n" . ?“) ("\"\"\"" . ?”)
                       ("\"\"\"" . ?“)
                       ("fn" . ?λ)
                       ("<=" . ?≤) (">=" . ?≥)
                       ("->" . ?→) ("-->". ?⟶) ;; threading operators
                       ("[ ]" . ?□) ("[X]" . ?☑) ("[-]" . ?◐)) ;; Org checkbox symbols
         do (push pair my/prettify-alist))

;; Replace all Org [metadata]keywords with the “▷” symbol; e.g., “#+title: Hello” looks like “▷ Hello”.
(cl-loop for keyword in '(title author email date description options property startup export_file_name html_head fileimage filetags)
         do (push (cons (format "#+%s:" keyword) ?▷) my/prettify-alist))

(cl-loop for hk in '(text-mode-hook prog-mode-hook org-mode-hook)
      do (add-hook hk (lambda ()
                        (setq prettify-symbols-alist
                              (append my/prettify-alist prettify-symbols-alist)))))


(add-hook 'org-mode-hook (lambda () (push '("# " . (?🎶 (Br . Bl) ?\ )) prettify-symbols-alist)))

;; Un-disguise a symbol when cursour is inside it or at the right-edge of it.
(setq prettify-symbols-unprettify-at-point 'right-edge)

;; org-mode math is now highlighted ;-)
(setq org-highlight-latex-and-related '(latex))

;; Extra space between text and underline line
(setq x-underline-at-descent-line t)

;; Hide the *,=,/ markers
(setq org-hide-emphasis-markers t)

;; Let’s limit the width of images inlined in org buffers to 400px.
(setq org-image-actual-width 400)

;; Visually, I prefer to hide the markers of macros, so let’s do that:
;;  {{{go(here)}}} is shown in Emacs as go(here)
(setq org-hide-macro-markers t)

;; On HTML exports, Org-mode tries to include a validation link for the exported HTML. Let’s disable that since I never use it.
;; (setq org-html-validation-link nil)

;; Musa: This is super annoying, in practice.
(setq org-pretty-entities nil) ;; Also makes subscripts (x_{sub script}) and superscripts (x^{super script}) appear in org in a WYSIWYG fashion.
;; to have \alpha, \to and others display as utf8
;; http://orgmode.org/manual/Special-symbols.html
;;
;; Be default, any consectuive string after “_” or “^” will be shown in WYSIWYG fashion; the following requires “^{⋯}” instead.
;; (setq org-use-sub-superscripts (quote {}))

(use-package org-appear
  :disabled t
  :hook (org-mode . org-appear-mode)
  :init (setq org-appear-autoemphasis  t
              org-appear-autolinks nil
              org-appear-autosubmarkers nil))

;; Automatically toggle LaTeX previews when cursour enters/leaves them
(use-package org-fragtog
  :disabled t
  :hook (org-mode . org-fragtog-mode))

;; Make previews a bit larger
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

;; I use a lot of Unicode, so let's always include a unicode header.
(maybe-clone "https://armkeh.github.io/unicode-sty/")
(setq org-format-latex-header
      (concat org-format-latex-header
              "\n\\usepackage{\\string~\"/unicode-sty/unicode\"}"))
;;
;; Now this looks nice too!
;; $\substack{𝔹 \\ ↓ \\ 𝒜}$ and $\mathbb{B}$.

;; Always support unicode upon LaTeX export
;; No need to explicitly import armkeh's unicode-sty in each org file.
(add-to-list 'org-latex-packages-alist
  "\n\\usepackage{\\string~\"/unicode-sty/unicode\"}")

;; Support “latex-as-png” src blocks, which show LaTeX as PNGs
(use-package ob-latex-as-png :disabled t)

;; Use the “#+name” the user provides, instead of generating label identifiers.
(setq org-latex-prefer-user-labels t)

 (use-package org-sticky-header
  :defer nil
  :hook (org-mode . org-sticky-header-mode)
  :config
  (setq-default
   org-sticky-header-full-path 'full
   ;; Child and parent headings are seperated by a /.
   org-sticky-header-outline-path-separator " / "))

(use-package persistent-scratch

  ;; In this mode, the usual save key saves to the underlying persistent file.
  :bind (:map persistent-scratch-mode-map
              ("C-x C-s" . persistent-scratch-save)))

(defun scratch ()
   "Recreate the scratch buffer, loading any persistent state."
   (interactive)
   (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
   (condition-case nil (persistent-scratch-restore) (insert initial-scratch-message))
   (org-mode)
   (persistent-scratch-mode)
   (persistent-scratch-autosave-mode 1))

;; This doubles as a quick way to avoid the common formula: C-x b RET *scratch*

;; Upon startup, close the default scratch buffer and open one as specfied above
(ignore-errors (kill-buffer "*scratch*") (scratch))

(setq initial-scratch-message (concat
  "#+title: Persistent Scratch Buffer"
  "\n#\n# Welcome! This’ a place for trying things out."
  "\n#\n# ⟨ ‘C-x C-s’ here saves to ~/.emacs.d/.persistent-scratch ⟩ \n\n"))

  (quelpa '(org-remoteimg :fetcher github :repo "gaoDean/org-remoteimg"))
  (require 'org-remoteimg)
  (setq url-cache-directory "~/emacs.d/.cache/")
  (setq org-display-remote-inline-images 'cache)

;; Add a note whenever a task's deadline or scheduled date is changed.
(setq org-log-redeadline 'time)
(setq org-log-reschedule 'time)

(define-key global-map "\C-ca" 'org-agenda)

;; List of all the files & directories where todo items can be found. Only one
;; for now: My default notes file.
(setq org-agenda-files (list org-default-notes-file))

;; Display tags really close to their tasks.
(setq org-agenda-tags-column -10)

;; How many days ahead the default agenda view should look
(setq org-agenda-span 'day)
;; May be any number; the larger the slower it takes to generate the view.
;; One day is thus the fastest ^_^

;; How many days early a deadline item will begin showing up in your agenda list.
(setq org-deadline-warning-days 14)

;; In the agenda view, days that have no associated tasks will still have a line showing the date.
(setq org-agenda-show-all-dates t)

;; Scheduled items marked as complete will not show up in your agenda view.
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done  t)

(setq org-agenda-start-on-weekday nil)

;; Start each agenda item with ‘○’, then show me it's %timestamp and how many
;; times it's been re-%scheduled.
(setq org-agenda-prefix-format " ○ %t%s")

(use-package origami )
(use-package org-super-agenda

  :hook (org-agenda-mode . origami-mode) ;; Easily fold groups via TAB.
  :bind (:map org-super-agenda-header-map ("<tab>" . origami-toggle-node))
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
        '((:name "Important" :priority "A")
          (:name "Personal" :habit t)
          ;; For everything else, nicely display their heading hierarchy list.
          (:auto-map (lambda (e) (org-format-outline-path (org-get-outline-path)))))))

;; MA: No noticable effect when using org-super-agenda :/
;;
;; Leave new line at the end of an entry.
;; (setq org-blank-before-new-entry '((heading . t) (plain-list-item . t)))

(setq org-lowest-priority ?C) ;; Now org-speed-eky ‘,’ gives 3 options
(setq org-priority-faces
'((?A :foreground "red"            :weight bold) ;; :background "LightCyan1")
  (?B :foreground "orange"         :weight bold)
  (?C :foreground "green"          :weight bold)))
;; See all colours with: M-x list-colors-display

(use-package org-fancy-priorities

  :hook   (org-mode . org-fancy-priorities-mode)
  :custom (org-fancy-priorities-list '("High" "MID" "LOW")) ;; "OPTIONAL"
  ;; Let's use the “Eisenhower map of priority”…
  ;; :custom (org-fancy-priorities-list '("Urgent and Important"     ;; Do now!
  ;;                                      "Not Urgent But Important" ;; Do schedule this.
  ;;                                      "Urgent But Not Important" ;; Delegate?
  ;;                                      "Not Urgent and Not Important")) ;; Don't do / Optional
  )

(require 'org-agenda)

;; How should the columns view look?
(setq org-columns-default-format   "%60ITEM(Task) %6Effort(Estim){:} %3PRIORITY %TAGS")

;; Press “c” in Org agenda to see the columns view; (default binding C-c C-x C-c is too long!)
(org-defkey org-agenda-mode-map "c" #'org-agenda-columns)
(org-defkey org-agenda-mode-map "C" #'org-agenda-goto-calendar)

;; Press “e” in columns view to alter “e”ffort “e”stimates.
(require 'org-colview)
(org-defkey org-columns-map "e"
            ;; Refresh after making an effort estimate.
            (lambda () (interactive) (org-agenda-set-effort) (org-agenda-columns)))

 (setq org-tags-column -77) ;; the default

;; Press ‘:’ on a heading to add a tag, press TAB to see all tags, press RETURN on a tag to add it, press TAB again to add more tags, when all done press RETURN twice.
(use-package helm-org ) ;; Helm for org headlines and keywords completion.
(add-to-list 'helm-completing-read-handlers-alist
             '(org-set-tags-command . helm-org-completing-read-tags))

;; Also provides: helm-org-capture-templates

(use-package org-pretty-tags

  :config
   (setq org-pretty-tags-surrogate-strings
         '(("Neato"    . "💡")
           ("Blog"     . "✍")
           ("Audio"    . "♬")
           ("Video"    . "📺")
           ("Book"     . "📚")
           ("Running"  . "🏃")
           ("Question" . "❓")
           ("Wife"     . "💕")
           ("Text"     . "💬") ; 📨 📧
           ("Friends"  . "👪")
           ("Self"     . "🍂")
           ("Finances" . "💰")
           ("Car"      . "🚗") ; 🚙 🚗 🚘
           ("Urgent"   . "🔥"))) ;; 📥 📤 📬
   (org-pretty-tags-global-mode 1))

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
     (message-box "The basic 25 minutes on this difficult task are not up; it's a shame to see you leave."))
     (org-timer-stop)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s@/!)" "|" "DONE(d/!)")
        (sequence "WAITING(w@/!)" "ON_HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

;; Since DONE is a terminal state, it has no exit-action.
;; Let's explicitly indicate time should be noted.
(setq org-log-done 'time)

(setq org-todo-keyword-faces
      '(("TODO"      :foreground "red"          :weight bold)
        ("STARTED"   :foreground "blue"         :weight bold)
        ("DONE"      :foreground "forest green" :weight bold)
        ("WAITING"   :foreground "orange"       :weight bold)
        ("ON_HOLD"   :foreground "magenta"      :weight bold)
        ("CANCELLED" :foreground "forest green" :weight bold)))

(setq org-use-fast-todo-selection t)

;; Install the tool
; (async-shell-command "brew tap adoptopenjdk/openjdk; brew cask install adoptopenjdk13") ;; Dependency
; (async-shell-command "brew install plantuml")

;; Tell emacs where it is.
;; E.g., (async-shell-command "find / -name plantuml.jar")
(setq org-plantuml-jar-path
      "/usr/local/Cellar/plantuml/1.2022.14/libexec/plantuml.jar")

;; Enable C-c C-c to generate diagrams from plantuml src blocks.
(add-to-list 'org-babel-load-languages '(plantuml . t) )
(require 'ob-plantuml)

; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes '("plantuml" . fundamental))

;; Record a note on what was accomplished when clocking out of an item.
(setq org-log-note-clock-out t)

(setq confirm-kill-emacs 'yes-or-no-p)

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)

;; Show lot of clocking history
(setq org-clock-history-length 23)

;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)

;; Sometimes I change tasks I'm clocking quickly ---this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)

;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)

;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

 (push '("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
       org-global-properties)

(setq org-clock-sound "~/.emacs.d/school-bell.wav")

;; Show habits for every day in the agenda.
(setq org-habit-show-habits t)
(setq org-habit-show-habits-only-for-today nil)

;; This shows the ‘Seinfeld consistency’ graph closer to the habit heading.
(setq org-habit-graph-column 90)

;; In order to see the habit graphs, which I've placed rightwards, let's
;; always open org-agenda in ‘full screen’.
;; (setq org-agenda-window-setup 'only-window)

;; Obtain a notifications and text-to-speech utilities
(system-packages-ensure "say") ;; Built-into MacOS, but can be downloaded in Ubuntu
(system-packages-ensure "terminal-notifier") ;; MacOS specific
;; System Preferences → Notifications → Terminal Notifier → Allow “alerts”.
;; E.g.,: (shell-command "terminal-notifier -title \"Hiya\" -message \"hello\"")

(cl-defun my/notify (message &key (titled "") at repeat-every-hour open)
  "Notify user with both an visual banner, with a beep sound, and a text-to-speech recitation.

When the user clicks on the resulting notification, unless a
given OPEN url is provided, the Emacs application is brough into
focus.

MESSAGE and TITLE are strings; AT is a time string as expected of
`run-at-time' such as \"11.23pm\" or \"5 sec\"; REPEAT-EVERY-HOUR
is a floating-point number of hours to continuously repeat the
alert.  OPEN is a URL that is opened when the user clicks the
notification. This can be a web or file URL, or any custom URL
scheme.

I initially used optional arguments, but realised that in due time
it would be more informative to use named arguments instead.

Example uses:

;; In 5 minutes from now, remind me to watch this neato video!
(my/notify \"🔔 Get things done! 📎 💻 \"
  :open \"https://www.youtube.com/watch?v=23tusPiiNZk&ab_channel=Motiversity\"
  :at \"5 minutes\")
  ;; :at \"5 sec\"

;; Remind me to exercise every 1.5hours; starting at 8:00am.
(my/notify \"Take a 5min break and get your blood flowing!\"
           :titled \"Exercise\"
           :at \"8:00am\"
           :repeat-every-hour 1.5)

;; Actually getting things done!
(my/notify \"Is what you're doing actually in alignment with your goals?
           Maybe it's time to do another task?\"
           :titled \"Check your agenda!\"
           :at \"10:00am\"
           :repeat-every-hour 2)

"
  (run-at-time at ;; the time to make the alert
               (when repeat-every-hour (* 60 60 repeat-every-hour))
               #'async-shell-command
               (format "%s" (s-replace "\n" ""
                   (s-join " " (--map (format "%s" it)
                       `(terminal-notifier
                         -title    ,(pp-to-string titled)
                         -message  ,(s-replace "\\n" "\n" (pp-to-string message))
                         ;; Play a random sound when the notification appears. See sound names with: ls /System/Library/Sounds
                         ;; Use the special NAME “default” for the default notification sound.
                         -sound ,(progn (require 'seq) (seq-random-elt (s-split "\n" (shell-command-to-string "ls /System/Library/Sounds"))))
                         ;; Don't create duplicates of the notification, just one instance;
                         ;; i.e., each notification belongs to a group and only one alert of the group may be present at any one time.
                         -group  ,(pp-to-string titled)
                         ;; Activate the application specified by ID when the user clicks the notification.
                         -activate org.gnu.Emacs
                         ,@(when open `(-open ,(pp-to-string open)))
                         ;; Run the shell command COMMAND when the user clicks the notification.
                         ;; -execute COMMAND
                         ;; & ;; … and then speak! …
                         ;; NOTE:This was getting annyoning in the middle of work meetings.
                         ;; say ,(s-replace "\\n" " " (pp-to-string message))
                         )))))))

;; (Emojis look terrible in Lisp; but much better when the alert is actually made!)

;; Remind me to exercise every 1.5hours; starting at 8:00am.
(my/notify "Take a 5min break and get your blood flowing!\n\t\t🚣 🏃‍♂️ 🧗‍♂️ 🧘‍♂️ 🏊 🏋 🚴‍♂️"
           :titled "🤾‍♀️ Exercise  🚵‍♂️"
           :at "8:00am"
           :repeat-every-hour 1.5
           :open "https://www.youtube.com/watch?v=23tusPiiNZk&ab_channel=Motiversity")

;; Actually getting things done!
(my/notify "Is what you're doing actually in alignment with your goals? ✔️📉
           Maybe it's time to do another task? 📋"
           :titled "📆 Check your agenda! 🔔"
           :at "10:00am"
           :repeat-every-hour 2)

;; Make every other line of a buffer grey (or whatever you like).
;; Useful for buffers that list things.
;; I want it to make my Org tables look nice. Even better when org-modern is activated.
(use-package stripe-buffer
  :defer 100
  :config (add-hook 'org-mode-hook 'turn-on-stripe-table-mode))

;; [[file:init.org::*Basic Agenda Config][Basic Agenda Config:1]]
(setq org-agenda-files (list "~/Documents/notes.org"))

(setq org-agenda-span 'week)

(setq org-agenda-custom-commands '(("o" "Open Loops" tags-tree "TODO=\"STARTED\"" )))

(setq org-log-into-drawer t) ;; hide the log state change history a bit better
;; Basic Agenda Config:1 ends here

;; [[file:init.org::*See all motivational images I have, and indent things so it's clearer when something is a child of something else][See all motivational images I have, and indent things so it's clearer when something is a child of something else:1]]
(defun my/org-settings ()
    (org-display-inline-images)
    (org-indent-mode)
    nil)

(add-hook 'org-mode-hook #'my/org-settings)
;; See all motivational images I have, and indent things so it's clearer when something is a child of something else:1 ends here

;; [[file:init.org::*org-num-mode][org-num-mode:1]]
;; Please dynamically number all headlines
(setq org-startup-numerated t)
    ;; More options @ https://orgmode.org/manual/Dynamic-Headline-Numbering.html

;; Prefer “𝓏” instead of “𝓍.𝓎.𝓏” ---indentation ‘org-indent-mode’ resolves ambiguity
(setq org-num-format-function (defun my/org-num-format (number-path) (format "%s " (car (last number-path)))))
;; org-num-mode:1 ends here

;; [[file:init.org::*How tasks look in org agenda][How tasks look in org agenda:1]]
;; Start each agenda item with ‘○’, then show me it's %timestamp and how many
;; times it's been re-%scheduled.
(setq org-agenda-prefix-format " ○ %?-12t%-6e%s ")

;;  (setq org-agenda-deadline-leaders '("DUE:       " "In %3d d.: " "%2d d. ago:  "))

;; Don't say “Scheduled ⟨Task⟩”, just show “⟨Task⟩”.
;; If something's overdue, say “Overdue 𝓃× ⟨Task⟩”.
(setq org-agenda-scheduled-leaders '("" "Overdue%2dx "))
;; How tasks look in org agenda:1 ends here

;; [[file:init.org::*Todo states][Todo states:1]]
;; These denote ‘progress’ on a task.
;; For contextual information, one uses “tags”.
;; For example, a delegated task could be in state “STARTED” and tagged “:delegate:James:”.
(setq org-todo-keyword-faces
      ;; Transitions: TODO → INVESTIGATED → STARTED ⟷ {AWAITING_REVIEW | PAUSED} → {DONE | CANCELLED}

      '(
        ;; Tasks that are not started and not planned. They could be the
        ;; backlogs or the GTD’s someday/maybe. These tasks could be converted
        ;; to NEXT during a weekly review.
        ("TODO"      :foreground "red"          :weight bold)

        ;; A task moves from TODO to NEXT only when I've actually split the task
        ;; into small achievable chunks; ie i've done some investigation into
        ;; the task and thought about what steps I need to do to actually get
        ;; the task done. With this planning in place, I can then ensure I
        ;; allocate sufficient timeblocks to work on the subtasks of this
        ;; task. (Aside, a “project” is a task with multiple subtasks.)
        ;;
        ;;
        ;; Use my 1/1 time with my manager/peers to review my INVESTIGATED/NEXT
        ;; findings for the tickets of the current sprint. I just want to make
        ;; sure I'm on the right track, before starting to work on them. Or, if
        ;; a ticket is not investigated, do that with my manager.  I find that while it
        ;; might take me half an hour or more, it'll take like 5 minutes with
        ;; him since he's familiar with the code-base.
        ;;
        ;;
        ;; ≈ NEXT. Tasks that are not started but planned to do as soon as I
        ;; can. When there is no actionable STARTED (e.g., blocked), I start one
        ;; of those and convert it to STARTED.
        ("INVESTIGATED"   :foreground "blue"         :weight bold)

        ;; Tasks that are working in progress (“open loops”). I work on these
        ;; tasks before starting another NEXT task to avoid too many open loops
        ;; at any moment.
        ("STARTED"   :foreground "blue"         :weight bold)

        ;; AWAITING_REVIEW/IN_PROGRESS = Tasks that are working in progress
        ;; (open loops). I work on these tasks before starting another NEXT task
        ;; to avoid too many open loops at any moment.
        ;;
        ;; When a task goes into AWAITING_REVIEW, I've finished the task
        ;; scheduled for that day and now my agenda will show “Sched. 𝓃× MyTask”
        ;; for 𝓃 days /after/ the schedule date. Now, I interpret this 𝓃 to mean
        ;; “This work was ready for review 𝓃 days ago, and if 𝓃 is large (ie
        ;; ~5), then I should message the relevant people to review/unblock that
        ;; work.”
        ("AWAITING_REVIEW" :foreground "orange"       :weight bold) ;; “In progress, but blocked by others”

        ("DONE"            :foreground "forest green" :weight bold)
        ("PAUSED"          :foreground "magenta"      :weight bold)
        ("CANCELLED"       :foreground "forest green" :weight bold)))
;; Todo states:1 ends here

;; [[file:init.org::*I use lots of “checkbox lists” in my routines: I want to think about things once, then follow the routine on auto-pilot][I use lots of “checkbox lists” in my routines: I want to think about things once, then follow the routine on auto-pilot:1]]
;; list items will be treated like low-level headlines; i.e., folded by default.
(setq org-cycle-include-plain-lists 'integrate)
;; I use lots of “checkbox lists” in my routines: I want to think about things once, then follow the routine on auto-pilot:1 ends here

;; [[file:init.org::*my/work-links][my/work-links:1]]
 (cl-defmacro my/work-links (type url &optional (export-display '(format "%s-%s" type label)))
   "Given a link of TYPE with a URL, produce the correct org-link.

 EXPORT-DISPLAY is string-valued term that may mention the symbolic names ‘type’ and ‘label’.
 This is how the link looks upon export."
   `(org-link-set-parameters
    ,type
    :follow (lambda (label) (browse-url (format ,url label)))
    :export (lambda (label description backend)
              (-let [full-url (format ,url label)]
                (pcase backend
                  ('html  (format "<a href=\"%s\">%s</a>" full-url (-let [type ,type] ,export-display)))
                  ('latex (format "\\href{%s}{%s}" full-url label))
                  (_  full-url))))
    :face '(:foreground "green" :weight bold
            :underline "blue" :overline "blue")))
;; my/work-links:1 ends here

;; [[file:init.org::*Work links][Work links:1]]
(defvar COMPANY (getenv "COMPANY") "Name of place I work at.")

(my/work-links "FWD" (lf-string "https://bugs.local.${(downcase COMPANY)}.com/browse/FWD-%s"))
(my/work-links "OUT" (lf-string "https://bugs.local.${(downcase COMPANY)}.com/browse/OUT-%s"))
(my/work-links "gerrit" (lf-string "https://gerrit.local.${(downcase COMPANY)}.com/c/fwd/+/%s"))

;; Open all such links in Chrome
(setq browse-url-browser-function 'browse-url-default-macosx-browser)
;; Work links:1 ends here

;; [[file:init.org::*Quick Capture][Quick Capture:1]]
;; Location of my todos / captured notes file
(setq org-default-notes-file "~/Documents/notes.org")
;; Quick Capture:1 ends here

;; [[file:init.org::*Quick Capture][Quick Capture:2]]
(defmacro def-capture (name location template)
"Creates a method “my/capture-NAME”, which opens a capture buffer named NAME showing TEMPLATE.
When you press `C-c C-c`, the note is saved as an entry (ie TEMPLATE should start with “* ”.)
in `org-default-notes-file' section named LOCATION.

+ NAME, LOCATION, TEMPLATE are all strings that may contain spaces.
+ Example:  (def-capture \"Friends Info\" \"Journal\" \"* %t\")
  This can be used as “M-x my/capture-friends-info” or via an Org link: “[[elisp:( my/capture-friends-info)]]”.

  Note: My “Journal” is nested in a section called “Workflow”, and capture finds it anyways (｡◕‿◕｡)
  (More precisely, Org-Capture looks for the first (sub)headline called “Journal” /anywhere/ and uses that as the target location.)

Usage:
1.         M-x my/capture-NAME  ⇒ Capture something to my LOCATION; no menu used.
2.     C-u M-x my/capture-NAME  ⇒ Jump to my LOCATION.
3. C-u C-u M-x my/capture-NAME  ⇒ Goto last note stored (by any my/capture-* method).
"
  `(defun ,(intern (concat "my/capture-" (s-replace-regexp " " "-" (downcase name)))) (&optional prefix)
      (interactive "p")
      (-let [org-capture-templates
       ;; I'm using omega 𝓌 as a placeholder; i.e., a gensym-like key.
        `(("𝓌" ,,name entry (file+headline ,,org-default-notes-file ,,location) ,,template))]
        (org-capture (list prefix) "𝓌")
      (unless (> prefix 1) (rename-buffer ,name)))))
;; Quick Capture:2 ends here

;; [[file:init.org::*C-c c ⇒ Capture inbox entry][C-c c ⇒ Capture inbox entry:1]]
;; I have “* Inbox [%]”, so when a new item is captured the “%” changes to reflect that.
;; When I've processed by inbox, I see “* Inbox [100%]” and feel good about it (✿◠‿◠)
(bind-key* "C-c c" (def-capture "Inbox Entry" "Inbox" "* TODO %?\n Captured: %U \n"))
;; C-c c ⇒ Capture inbox entry:1 ends here

;; [[file:init.org::*Capture morning journal entry][Capture morning journal entry:1]]
(defalias #'my/new-journal-entry (def-capture "Journal Entry" "Journal"
  (s-join "\n\n" (list
    (format-time-string "* %Y-%m-%d %a %R :mood_𝒏%?:")
    "My most important goal for the day is: "
    (let (todays-agenda) (org-agenda-list 1) (setq todays-agenda (buffer-string)) (org-agenda-quit)
    (concat "Here's what my day looks like so far:\n" todays-agenda))))))
;; Capture morning journal entry:1 ends here

;; [[file:init.org::*Capture evening journal entry][Capture evening journal entry:1]]
(def-capture "Daily Retrospective" "Journal"
             (concat
              ;; The “:mood_𝓃:” tag is so that I can easily see my moods across my entries.
              ;; Keeping track of mood is a simple way to monitor success; e.g., whether GTD is working for me or not.
   (format-time-string "* %Y-%m-%d %a %R :mood_𝒏%?:retro:daily:\n")
"
Instructions: Read section contents and replace them with your own words.

** Monitoring Mood For Success
  - Why do I feel the way I do?
  - 1 ≈ 😢 extremely negative; 5 ≈ :neutral_face: neutral; 10 ≈ 😁 extremely positive.

** Celebrating Successes
  - Acknowledge and celebrate your achievements, no matter how small.
  - Recognising your progress boosts morale and motivation, inspiring you to continue striving for success.
  - /Give yourself credit for your efforts!/

** Goal Alignment
  - Were today's actions directed at achieving the sprint's goals? Stay focused!
  - Review your day: use my/what-did-i-do-today to review the tasks you  clocked into.
      [[elisp:(let ((org-clock-clocktable-default-properties (-snoc org-clock-clocktable-default-properties :block 'today :timestamp 'nil))) (beginning-of-line) (kill-line) (org-clock-report))][What Did I Work on Today?]]
  - Set SMART goals for tomorrow.

** Stress Reduction
  - Any challenges or frustrations encountered today?

** Continuous Improvement
  - What went well, what didn't go as planned, and were there any unexpected challenges today?
  - Learn from your experiences & brainstorm strategies better outcomes in the future.
  - Consider what factors contributed to these challenges and how you responded to them.
  - What thought patterns were influencing my outcomes?

** Express Gratitude
  - Conclude your retrospective by expressing gratitude for the positive aspects of your day.
  - Reflect on the people, opportunities, and experiences that brought you joy or fulfilment.
"))
;; Capture evening journal entry:1 ends here

;; [[file:init.org::*my/see-sorted-todos][my/see-sorted-todos:1]]
(defun my/see-sorted-todos ()
  "See TODOs sorted by state, priority, then effort.

This view shows me all tasks, sorted, and from there I can pick & schedule & edit as desired.

Much nicer than me physically sorting tasks!

The main benefit here is I can keep /active/ tasks under their
own disjoint ‘projects’, yet still have a nice unified view of
all of my /active/ *and* non-active tasks.

⇒ You can mark DONE and archive from the sorted view.
⇒ But leave that to the Weekly Review, so that you can extract any
  reference information or Lessons Learned out of the tasks to be archived.

See: https://emacs.stackexchange.com/questions/9585/org-how-to-sort-headings-by-todo-and-then-by-priority
"
  (interactive)
  (let ((org-agenda-custom-commands '(("𝓌" nil todo "*"
                                       ((org-agenda-overriding-header "\nTODOs sorted by state, priority, effort")
                                        (org-agenda-sorting-strategy '(todo-state-down priority-down effort-up)))))))
    (org-agenda nil "𝓌" t)
    (beginning-of-buffer)))
;; my/see-sorted-todos:1 ends here

;; [[file:init.org::*highlight quoted symbols][highlight quoted symbols:1]]
(use-package highlight-quoted
  :defer nil
  :config (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode))

;; If everything worked fine, then “ 'b ” below should be coloured nicely in Emacs Lisp mode.
(when nil
  (-let [x 'somevar]
    (list x 'b "c" :e)))
;; highlight quoted symbols:1 ends here

;; [[file:init.org::*Bidirectional Text][Bidirectional Text:1]]
;; Sometimes I have Arabic in my buffers, however I am an English speaker
;; and so Left-to-Right is most natural to me. As such, even when Arabic
;; is present, or any bidirectional text, just use Left-to-Right.
(setq-default bidi-paragraph-direction 'left-to-right)
;; Bidirectional Text:1 ends here

;; [[file:init.org::*Whitespace][Whitespace:1]]
(add-hook 'before-save-hook 'whitespace-cleanup)
;; Whitespace:1 ends here

;; [[file:init.org::*Formatting Text][Formatting Text:1]]
(local-set-key (kbd "C-c f") #'my/org-mode-format)
(defun my/org-mode-format (&optional text)
"Surround selected region with the given Org emphasises marker.

E.g., if this command is bound to “C-c f” then the sequence
“C-c f b” would make the currenly selected text be bold.
Likewise, “C-c f *” would achieve the same goal.

When you press “C-c f”, a message is shown with a list of
useful single-character completions.

Note: “C-c f 𝓍”, for an unrecognised marker 𝓍, just inserts
the character 𝓍 before and after the selected text."
  (interactive "P") ;; Works on a region
  ; (message "b,* ⟨Bold⟩; i,/ ⟨Italics⟩; u,_ ⟨Underline⟩; c,~ ⟨Monotype⟩")
  (message "⟨Bold b,*⟩ ⟨Italics i,/⟩ ⟨Underline u,_⟩ ⟨Monotype c,~⟩")
  (let ((kind (read-char)))
    ;; Map letters to Org formatting symbols
    (setq kind (or (plist-get '(b ?\*   i ?\/   u ?\_   c ?\~)
                              (intern (string kind)))
                   kind))
    (insert-pair text kind kind)))
;; Formatting Text:1 ends here

;; [[file:init.org::*Fill-mode ---Word Wrapping][Fill-mode ---Word Wrapping:1]]
(setq-default fill-column 80          ;; Let's avoid going over 80 columns
              truncate-lines nil      ;; I never want to scroll horizontally
              indent-tabs-mode nil)   ;; Use spaces instead of tabs
;; Fill-mode ---Word Wrapping:1 ends here

;; [[file:init.org::*Fill-mode ---Word Wrapping][Fill-mode ---Word Wrapping:2]]
;; Wrap long lines when editing text
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
;; Fill-mode ---Word Wrapping:2 ends here

;; [[file:init.org::*Fill-mode ---Word Wrapping][Fill-mode ---Word Wrapping:3]]
;; Bent arrows at the end and start of long lines.
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(global-visual-line-mode 1)
;; Fill-mode ---Word Wrapping:3 ends here

;; [[file:init.org::*Pretty Lists Markers][Pretty Lists Markers:1]]
;; (x y z) ≈ (existing-item replacement-item positivity-of-preceding-spaces)
(cl-loop for (x y z) in '(("+" "◦" *)
                       ("-" "•" *)
                       ("*" "⋆" +))
      do (font-lock-add-keywords 'org-mode
                                 `((,(format "^ %s\\([%s]\\) " z x)
                                    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) ,y)))))))
;; Pretty Lists Markers:1 ends here

;; [[file:init.org::*Fix spelling as you type ---thesaurus & dictionary too!][Fix spelling as you type ---thesaurus & dictionary too!:1]]
(system-packages-ensure "aspell")
(system-packages-ensure "wordnet")
;; Fix spelling as you type ---thesaurus & dictionary too!:1 ends here

;; [[file:init.org::*Fix spelling as you type ---thesaurus & dictionary too!][Fix spelling as you type ---thesaurus & dictionary too!:2]]
(use-package flyspell

  :hook ((prog-mode . flyspell-prog-mode)
         ((org-mode text-mode) . flyspell-mode)))
;; Fix spelling as you type ---thesaurus & dictionary too!:2 ends here

;; [[file:init.org::*Fix spelling as you type ---thesaurus & dictionary too!][Fix spelling as you type ---thesaurus & dictionary too!:3]]
(setq ispell-program-name (s-trim (shell-command-to-string "which aspell")))
(setq ispell-dictionary "en_GB") ;; set the default dictionary
;; Fix spelling as you type ---thesaurus & dictionary too!:3 ends here

;; [[file:init.org::*Fix spelling as you type ---thesaurus & dictionary too!][Fix spelling as you type ---thesaurus & dictionary too!:5]]
(eval-after-load "flyspell"
  ' (progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))
;; Fix spelling as you type ---thesaurus & dictionary too!:5 ends here

;; [[file:init.org::*Fix spelling as you type ---thesaurus & dictionary too!][Fix spelling as you type ---thesaurus & dictionary too!:6]]
(global-font-lock-mode t)
(custom-set-faces '(flyspell-incorrect ((t (:inverse-video t)))))
;; Fix spelling as you type ---thesaurus & dictionary too!:6 ends here

;; [[file:init.org::*Fix spelling as you type ---thesaurus & dictionary too!][Fix spelling as you type ---thesaurus & dictionary too!:7]]
(setq ispell-silently-savep t)
;; Fix spelling as you type ---thesaurus & dictionary too!:7 ends here

;; [[file:init.org::*Fix spelling as you type ---thesaurus & dictionary too!][Fix spelling as you type ---thesaurus & dictionary too!:8]]
(setq ispell-personal-dictionary "~/.emacs.d/.aspell.en.pws")
;; Fix spelling as you type ---thesaurus & dictionary too!:8 ends here

;; [[file:init.org::*Fix spelling as you type ---thesaurus & dictionary too!][Fix spelling as you type ---thesaurus & dictionary too!:9]]
(add-hook          'c-mode-hook 'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
;; Fix spelling as you type ---thesaurus & dictionary too!:9 ends here

;; [[file:init.org::*Fix spelling as you type ---thesaurus & dictionary too!][Fix spelling as you type ---thesaurus & dictionary too!:10]]
(use-package synosaurus
  :defer 100
  :init    (synosaurus-mode)
  :config  (setq synosaurus-choose-method 'popup) ;; 'ido is default.
           (global-set-key (kbd "M-#") 'synosaurus-choose-and-replace))
;; Fix spelling as you type ---thesaurus & dictionary too!:10 ends here

;; [[file:init.org::*Fix spelling as you type ---thesaurus & dictionary too!][Fix spelling as you type ---thesaurus & dictionary too!:11]]
;; (shell-command "brew cask install xquartz &") ;; Dependency
;; (shell-command "brew install wordnet &")
;; Fix spelling as you type ---thesaurus & dictionary too!:11 ends here

;; [[file:init.org::*Fix spelling as you type ---thesaurus & dictionary too!][Fix spelling as you type ---thesaurus & dictionary too!:12]]
(use-package wordnut
 :defer 100
 :bind ("M-!" . wordnut-lookup-current-word))

;; Use M-& for async shell commands.
;; Fix spelling as you type ---thesaurus & dictionary too!:12 ends here

;; [[file:init.org::*Lightweight Prose Proofchecking][Lightweight Prose Proofchecking:1]]
(use-package writegood-mode
  ;; Load this whenver I'm composing prose.
  :hook (text-mode org-mode)
  ;; Don't show me the “Wg” marker in the mode line

  :defer 100

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

;; [[file:init.org::*Placeholder Text ---For Learning & Experimenting][Placeholder Text ---For Learning & Experimenting:1]]
(use-package lorem-ipsum )
;; Placeholder Text ---For Learning & Experimenting:1 ends here

;; [[file:init.org::*Some text to make us smile][Some text to make us smile:1]]
(use-package dad-joke

  :config (defun dad-joke () (interactive) (insert (dad-joke-get))))
;; Some text to make us smile:1 ends here

;; [[file:init.org::*Unicode Input via Agda Input][Unicode Input via Agda Input:1]]
;; (load (shell-command-to-string "agda-mode locate"))
;;
;; Seeing: One way to avoid seeing this warning is to make sure that agda2-include-dirs is not bound.
; (makunbound 'agda2-include-dirs)
;; Unicode Input via Agda Input:1 ends here

;; [[file:init.org::*Unicode Input via Agda Input][Unicode Input via Agda Input:2]]
(system-packages-ensure "agda")
;; Unicode Input via Agda Input:2 ends here

;; [[file:init.org::*Unicode Input via Agda Input][Unicode Input via Agda Input:4]]
(unless noninteractive
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate"))))
;; Unicode Input via Agda Input:4 ends here

;; [[file:init.org::*Unicode Input via Agda Input][Unicode Input via Agda Input:5]]
;; TODO: Maybe don't bother installing Agda, and just get agda-input.el
;; from: https://github.com/agda/agda/blob/master/src/data/emacs-mode/agda-input.el
;; then loading that!
(url-copy-file "https://raw.githubusercontent.com/agda/agda/master/src/data/emacs-mode/agda-input.el" "~/.emacs.d/elpa/agda-input.el" :ok-if-already-exists)
(load-file "~/.emacs.d/elpa/agda-input.el")

;; MA: This results in "Package cl is deprecated" !?
(unless noninteractive
  (use-package agda-input
  :ensure nil ;; I have it locally.
  :demand t
  :hook ((text-mode prog-mode) . (lambda () (set-input-method "Agda")))
  :custom (default-input-method "Agda")))
  ;; Now C-\ or M-x toggle-input-method turn it on and offers


;; TODO add a hook that when the input method becomes Agda, just don't bother showing me in the modeline.
;; E.g., "Π" when using unicode input with Agda
;; Useful to have in the modeline, say when typing in Arabic.
;; (add-variable-watcher
;;  'current-input-method
;;  (lambda (_ newvalue 'set _)
;;    (setq current-input-method-title
;;          (if (equal newvalue "Agda") nil newvalue))))
;; Unicode Input via Agda Input:5 ends here

;; [[file:init.org::*Unicode Input via Agda Input][Unicode Input via Agda Input:6]]
;;(setq agda2-program-args (quote ("RTS" "-M4G" "-H4G" "-A128M" "-RTS")))
;; Unicode Input via Agda Input:6 ends here

;; [[file:init.org::*Unicode Input via Agda Input][Unicode Input via Agda Input:7]]
(unless noninteractive (add-to-list 'agda-input-user-translations '("set" "𝒮ℯ𝓉")))
;; Unicode Input via Agda Input:7 ends here

;; [[file:init.org::*Unicode Input via Agda Input][Unicode Input via Agda Input:8]]
(unless noninteractive
(cl-loop for item
      in '(;; Arabic ornate parenthesis U+FD3E / U+FD3F
          ("(" "﴾")
          (")" "﴿")
          ("cmd" "⌘")
           ;; categorial ;;
           ("alg" "𝒜𝓁ℊ")
           ("split" "▵")
           ("join" "▿")
           ("adj" "⊣")
           (";;" "﹔")
           (";;" "⨾")
           (";;" "∘")
           ;; logic
           ("if" "⇐")
           ("onlyif" "⇒")
           ;; lattices ;;
           ("meet" "⊓")
           ("join" "⊔")
           ;; tortoise brackets, infix relations
           ("((" "〔")
           ("))" "〕")
           ;; residuals
           ("syq"  "╳")
           ("over" "╱")
           ("under" "╲")
           ;; Z-quantification range notation ;;
           ;; e.g., “∀ x ❙ R • P” ;;
           ("|"    "❙")
           ("with" "❙")
           ;; Z relational operators
           ("domainrestriction" "◁")
           ("domr" "◁")
           ("domainantirestriction" "⩤")
           ("doma" "⩤")
           ("rangerestriction" "▷")
           ("ranr" "▷")
           ("rangeantirestriction" "⩥")
           ("rana" "⩥")
           ;; adjunction isomorphism pair ;;
           ("floor"  "⌊⌋")
           ("lower"  "⌊⌋")
           ("lad"    "⌊⌋")
           ("ceil"   "⌈⌉")
           ("raise"  "⌈⌉")
           ("rad"    "⌈⌉")
           ;; Replies
           ("yes"  "✔")
           ("no"    "❌")
           ;; Arrows
           ("<=" "⇐")
        ;; more (key value) pairs here
        )
      do (add-to-list 'agda-input-user-translations item)))
;; Unicode Input via Agda Input:8 ends here

;; [[file:init.org::*Unicode Input via Agda Input][Unicode Input via Agda Input:9]]
(unless noninteractive
;; Add to the list of translations using “emot” and the given, more specfic, name.
;; Whence, \emot shows all possible emotions.
(cl-loop for emot
      in `(;; angry, cry, why-you-no
           ("whyme" "ლ(ಠ益ಠ)ლ" "ヽ༼ಢ_ಢ༽ﾉ☂" "щ(゜ロ゜щ)" "‿︵(ಥ﹏ಥ)‿︵" "ಠ_ಠ" "(╬ ಠ益ಠ)" "･ﾟ(*❦ω❦)*･ﾟ" "(╯°□°）╯︵ ┻━┻") ;; flip the table
           ;; confused, disapprove, dead, shrug, awkward
           ("what" "「(°ヘ°)" "(ಠ_ಠ)" "(✖╭╮✖)" "¯\\_(ツ)_/¯"  "(´°ω°`)" "･✧_✧･")
           ;; dance, csi
           ("cool" "┏(-_-)┓┏(-_-)┛┗(-_-﻿ )┓"
            ,(s-collapse-whitespace "•_•)
                                      ( •_•)>⌐■-■
                                      (⌐■_■)"))
           ;; love, pleased, success, yesss, smile, excited, yay
           ("smile" "♥‿♥" "(─‿‿─)" "(•̀ᴗ•́)و" "ᕦ( ᴼ ڡ ᴼ )ᕤ" "(งಠ_ಠ)ง" "(｡◕‿◕｡)" "(◕‿◕)" "( ˃ ヮ˂)" "[ ⇀ ‿ ↼ ]" "٩(⁎❛ᴗ❛⁎)۶" "ᴵ’ᵐ ᵇᵉᵃᵘᵗⁱᶠᵘˡ" "(✿◠‿◠)")
           ;; flower high-5
           ("hug" "♡(✿ˇ◡ˇ)人(ˇ◡ˇ✿)♡" "(づ｡◕‿◕｡)づ" "(づ｡◕‿‿‿‿◕｡)づ"))
      do
      (add-to-list 'agda-input-user-translations emot)
      (add-to-list 'agda-input-user-translations (cons "emot" (cdr emot)))))
;; Unicode Input via Agda Input:9 ends here

;; [[file:init.org::*Unicode Input via Agda Input][Unicode Input via Agda Input:10]]
;; activate translations
(unless noninteractive (agda-input-setup))
;; Unicode Input via Agda Input:10 ends here

;; [[file:init.org::*Increase/decrease text size][Increase/decrease text size:1]]
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
;; C-x C-0 restores the default font size
;; Increase/decrease text size:1 ends here

;; [[file:init.org::*Moving Text Around][Moving Text Around:1]]
;; M-↑,↓ moves line, or marked region; prefix is how many lines.
(use-package move-text

  :config (move-text-default-bindings))
;; Moving Text Around:1 ends here

;; [[file:init.org::*Enabling CamelCase Aware Editing Operations][Enabling CamelCase Aware Editing Operations:1]]
(global-subword-mode 1)
;; Enabling CamelCase Aware Editing Operations:1 ends here

;; [[file:init.org::*Delete Selection Mode][Delete Selection Mode:1]]
(delete-selection-mode 1)
;; Delete Selection Mode:1 ends here

;; [[file:init.org::*visual-regexp][visual-regexp:1]]
;; While constructing the regexp in the minibuffer, get live visual feedback for the (group) matches.
;; E.g., try: M-% use-\(.+?\) \(.+\)\b ENTER woah \1 and \2
;;
;; C-u M-%  do to regexp replace, without querying.
(use-package visual-regexp

  :config (define-key global-map (kbd "M-%")
            (lambda (&optional prefix) (interactive "P") (call-interactively (if prefix  #'vr/replace #'vr/query-replace)))))
;; visual-regexp:1 ends here

;; [[file:init.org::*HTML ⇐ Org-mode][HTML ⇐ Org-mode:1]]
(use-package htmlize )
;; Main use: Org produced htmls are coloured.
;; Can be used to export a file into a coloured html.
;; HTML ⇐ Org-mode:1 ends here

;; [[file:init.org::*HTML “Folded Drawers”][HTML “Folded Drawers”:1]]
(defun my/org-drawer-format (name contents)
  "Export to HTML the drawers named with prefix ‘fold_’, ignoring case.

The resulting drawer is a ‘code-details’ and so appears folded;
the user clicks it to see the information therein.
Henceforth, these are called ‘fold drawers’.

Drawers without such a prefix may be nonetheless exported if their
body contains ‘:export: t’ ---this switch does not appear in the output.
Thus, we are biased to generally not exporting non-fold drawers.

One may suspend export of fold drawers by having ‘:export: nil’
in their body definition.

Fold drawers naturally come with a title.
Either it is specfied in the drawer body by ‘:title: ⋯’,
or otherwise the drawer's name is used with all underscores replaced
by spaces.
"
  (let* ((contents′ (replace-regexp-in-string ":export:.*\n?" "" contents))
         (fold? (s-prefix? "fold_" name 'ignore-case))
         (export? (string-match ":export:\s+t" contents))
         (not-export? (string-match ":export:\s+nil" contents))
         (title′ (and (string-match ":title:\\(.*\\)\n" contents)
                      (match-string 1 contents))))

    ;; Ensure we have a title.
    (unless title′ (setq title′ (s-join " " (cdr (s-split "_" name)))))

    ;; Output
    (cond
     ((and export? (not fold?)) contents′)
     (not-export? nil)
     (fold?
      (thread-last contents′
        (replace-regexp-in-string ":title:.*\n" "")
        (format "<details class=\"code-details\"> <summary> <strong>
            <font face=\"Courier\" size=\"3\" color=\"green\"> %s
            </font> </strong> </summary> %s </details>" title′))))))

(setq org-html-format-drawer-function 'my/org-drawer-format)
;; HTML “Folded Drawers”:1 ends here

;; [[file:init.org::*  \[\[https:/revealjs.com/?transition=zoom#/\]\[Reveal.JS\]\] -- The HTML Presentation Framework][  [[https://revealjs.com/?transition=zoom#/][Reveal.JS]] -- The HTML Presentation Framework:1]]
(use-package ox-reveal

  :custom (org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))
;;   [[https://revealjs.com/?transition=zoom#/][Reveal.JS]] -- The HTML Presentation Framework:1 ends here

;; [[file:init.org::*  \[\[https:/revealjs.com/?transition=zoom#/\]\[Reveal.JS\]\] -- The HTML Presentation Framework][  [[https://revealjs.com/?transition=zoom#/][Reveal.JS]] -- The HTML Presentation Framework:3]]
(setq org-reveal-title-slide "<h1>%t</h1> <h3>%a</h3>
<font size=\"1\">
<a href=\"?print-pdf&showNotes=true\">
⟪ Flattened View ; Press <code>?</code> for Help ⟫
</a>
</font>")
;;   [[https://revealjs.com/?transition=zoom#/][Reveal.JS]] -- The HTML Presentation Framework:3 ends here

;; [[file:init.org::*C-c C-l Org-mode ⇐ HTML][C-c C-l Org-mode ⇐ HTML:2]]
(use-package org-web-tools

  :config
  ;; Insert an Org-mode link to the URL in the clipboard or kill-ring. Downloads
  ;; the page to get the HTML title.
  ;; (bind-key* "C-c C-l" #'org-web-tools-insert-link-for-url) ;; Instead, see my/org-insert-link-dwim below.
  )
;; C-c C-l Org-mode ⇐ HTML:2 ends here

;; [[file:init.org::*C-c C-l Org-mode ⇐ HTML][C-c C-l Org-mode ⇐ HTML:3]]
;; C-u C-c C-l ⇒ Paste URL with title, WITHOUT prompting me for anything.
;; C-c C-l ⇒ Prompt me for title.
(bind-key* "C-c C-l"
           (lambda () (interactive)
             (call-interactively
              (if current-prefix-arg
                  #'org-web-tools-insert-link-for-url
                #'my/org-insert-link-dwim))))
;; From:
(defun my/org-insert-link-dwim ()
  "Like `org-insert-link' but with personal dwim preferences.

- When text is selected, use that as the link description --and prompt for link type
- When a URL is in the clipboard, use that as the link type
- On an existing Org link, prompt to alter the link then to alter the description
- With a ‘C-u’ prefix, prompts for a file to link to.
  - It is relative to the current directory; use ‘C-u C-u’ to get an absolute path.

It fallsback to `org-insert-link' when possible.

Functin Source: https://xenodium.com/emacs-dwim-do-what-i-mean/"
  (interactive)
  (let* ((point-in-link (org-in-regexp org-link-any-re 1))
         (clipboard-url (when (string-match-p "^http" (current-kill 0))
                          (current-kill 0)))
         (region-content (when (region-active-p)
                           (buffer-substring-no-properties (region-beginning)
                                                           (region-end)))))
    (cond ((and region-content clipboard-url (not point-in-link))
           (delete-region (region-beginning) (region-end))
           (insert (org-make-link-string clipboard-url region-content)))
          ((and clipboard-url (not point-in-link))
           (insert (org-make-link-string
                    clipboard-url
                    (read-string "title: "
                                 (with-current-buffer (url-retrieve-synchronously clipboard-url)
                                   (dom-text (car
                                              (dom-by-tag (libxml-parse-html-region
                                                           (point-min)
                                                           (point-max))
                                                          'title))))))))
          (t
           (call-interactively 'org-insert-link)))))
;; C-c C-l Org-mode ⇐ HTML:3 ends here

;; [[file:init.org::*loading work.el][loading work.el:1]]
(load-file "~/Desktop/work.el")
;; loading work.el:1 ends here

;; [[file:init.org::*get the pkg][get the pkg:1]]
(use-package repl-driven-development)
(setq repl-driven-development-echo-duration 10)
;; get the pkg:1 ends here

;; [[file:init.org::*terminal][terminal:1]]
;; Sometimes I see a bunch of shell incantations in a README or something and I'd like to execute them right there and then,
;; and not have to bother with copying them over to a terminal and execute there. As such, here's a quick key binding to execute
;; shell commands from anywhere.
;; (repl-driven-development [C-x C-t] "bash"  :prompt "bash-3.2\\$")
(repl-driven-development [C-x C-t] terminal)
;; terminal:1 ends here

;; [[file:init.org::*jshell][jshell:1]]
;; Set “C­x C­j” to evaluate Java code in a background REPL.
(repl-driven-development
 [C-x C-j]
 ;; enable assertions, and add everything installed, via `mvn', in scope.
 (format "jshell --class-path %s --enable-preview -R -ea --feedback silent"
         (concat ".:" (shell-command-to-string "find ~/.m2/repository -name \"*.jar\" -type f 2>/dev/null | tr '\n' ':'")))
 :prompt "jshell>"
 :init "\n /set mode EmacsJavaMode normal -command
        \n /set format EmacsJavaMode display \"{pre}added import {name}{post}\" import-added
        \n /set format EmacsJavaMode display \"{pre}re-added import {name}{post}\" import-modified,replaced
        \n /set format EmacsJavaMode result \"{type} {name} = {value}{post}\" added,modified,replaced-primary-ok
        \n /set truncation EmacsJavaMode 40000
        \n /set feedback EmacsJavaMode
        \n System.out.println(\"Enjoy Java with Emacs (｡◕‿◕｡))\")")
;; TODO [Truncation; Low] https://github.com/xiongtx/eros/blob/master/eros.el#L202
;; jshell:1 ends here

;; [[file:init.org::*mvn][mvn:1]]
(defun mvn (groupId artifactId)
  "Quickly install a library from Maven Central."
  (async-shell-command (format "mvn org.apache.maven.plugins:maven-dependency-plugin:2.8:get -Dartifact=%s:%s:LATEST:jar:sources" groupId  artifactId)))

(when nil
  ⨾⨾ Example use of `mvn`

  ;; ⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾
  ;; First confirm C-x C-j works as intended
  IntStream.range(0, 15).mapToObj(i -> i % 15 == 0 ? "FizzBuzz" : i % 3 == 0 ? "Fizz" : i % 5 == 0 ? "Buzz" : String.valueOf(i)).toList()

  ;; ⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾
  (mvn "org.antlr" "antlr4") ;; C-x C-e
  ;; Now re-start the Java C-x C-j repl via a C-x C-e (lame! not ergonomic!)
  ;; Now check that you have access to antrl in your repl by importing it and looking at one of its classes:
  ;; ⦃ jshell --class-path /Users/musa/.m2/repository/org/antlr/antlr4-runtime/4.13.0/antlr4-runtime-4.13.0.jar ⦄
  import org.antlr.v4.runtime.*;
  CommonTokenStream.class
  ;; NOTE: This is the runtime, to use the actual tool:
  java -jar /Users/musa/.m2/repository/org/antlr/antlr4/4.13.0/antlr4-4.13.0-complete.jar

  ;; Alternatively,
  ;; $ jshell
  ;; > var x = 5
  ;; > import org.antlr.v4.runtime.*;  // CRASHES since antlr is not in scope
  ;; > /reset --class-path /Users/musa/.m2/repository/org/antlr/antlr4-runtime/4.13.0/antlr4-runtime-4.13.0.jar
  ;; > import org.antlr.v4.runtime.*;  // WORKS, yay
  ;; > x // CRASHES, not in scope

  ;; ⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾
  import org.apache.commons.lang3.StringUtils;
  StringUtils.class;
  /imports  // JShell command to list all imports, it now contains apache!

  // Guava is the Google core libraries for Java
  import com.google.common.collect.ImmutableMap;

  ImmutableMap.of(1, "A", 2, "B") // ⇒ {1=A, 2=B}

  ;; (mvn "com.google.code.gson" "gson")
  // Then C-x C-e to update the repl definition of C-x C-j to include the updated gson library.
  import com.google.gson.*;
  String json = new Gson().toJson(Map.of("me", List.of(1, 2,3), "you", Map.of("Love", "Lisp", "Hate", "Verbosity")))

  ;; ⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾
  ;; TestNG is a testing framework; supporting tests configured by annotations, data-driven testing, parametric tests, etc.
  (mvn "org.testng" "testng")
  import org.testng.*;
  /imports  // Now can see org.testng at the end of the list

  ;; ⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾⨾
  ;; TODO[Low]: For working with Lombok annotations, use the jshell `/reset --class-path` command to include the lombok compiled file into
  ;; the current Jshell session.
  ;;
  ;; See: https://stackoverflow.com/questions/74084364/how-to-use-lombok-in-jshell
  ;;
  ;; (mvn "org.projectlombok" "lombok")
  ;; import lombok.*;
  ;; @lombok.Data class Test { private String name; }
  ;; new Test().equals(new Test())
  )
;; mvn:1 ends here

;; [[file:init.org::*Adding support for “/!use” & “/!omit” top level repl commands][Adding support for “//!use” & “//!omit” top level repl commands:1]]
;; Adding support for “//!use” & “//!omit” top level repl commands
(setq repl/jshell/classpath (shell-command-to-string "find ~/.m2/repository -name \"*.jar\" -type f | tr '\n' ':'"))
(advice-add 'repl/jshell
            :around (lambda (repl &rest args)
                      (if (equal nil current-prefix-arg)
                          ;; No prefix supplied
                          (progn
                            (setq rdd---current-input (s-replace-regexp "\n" "" (s-trim-left
                                                                                 (if (region-active-p) (buffer-substring-no-properties  (region-beginning) (region-end))
                                                                                   (substring-no-properties (thing-at-point 'line))))))
                            (if (s-starts-with? "//!use" rdd---current-input)
                                (-let [jar (s-trim (s-chop-prefix "//!use" rdd---current-input))]
                                  (repl/top-level//!use jar))
                              (if (s-starts-with? "//!omit" rdd---current-input)
                                  (-let [jar (s-trim (s-chop-prefix "//!omit" rdd---current-input))]
                                    (repl/top-level//!omit jar))
                                ;; otherwise business as usual
                                (apply repl args))))
                        (pcase current-prefix-arg
                          (-1
                           ;; reset classpath to default, then business as usual.
                           (setq repl/jshell/classpath (shell-command-to-string "find ~/.m2/repository -name \"*.jar\" -type f | tr '\n' ':'"))
                           (apply repl args))
                          (999
                           (message-box "It worked"))
                          ;; otherwise business as usual
                          (t (apply repl args))))))

;; remove all advice
;; (-let [sym 'repl/jshell] (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(cl-defun repl/top-level//!omit (str)
  (with-temp-buffer
    (setq repl/jshell/classpath (s-replace-regexp (format ":[^:]*%s[^:]*:" str) ":" repl/jshell/classpath))
    (insert "/env --class-path ")
    (insert repl/jshell/classpath)
    (repl/jshell (point-min) (point-max))))

(cl-defun repl/top-level//!use (str)
  "If the given jar cannot be added successful, the existing classpath remains untouched.

Return to your default classpath by invoking the repl with the -1 prefix.

Example usage:

    //!use ~/path/to/compiled/java/classes
    import com.x.y.z;

Where directory hierarchy com/x/y/z denotes a Java package under the above //!use path.
"
  (with-temp-buffer
    (setq repl/jshell/classpath (concat (s-trim str) ":" repl/jshell/classpath))
    (insert "/env --class-path ")
    (insert repl/jshell/classpath)
    (repl/jshell (point-min) (point-max))))
;; Adding support for “//!use” & “//!omit” top level repl commands:1 ends here

;; [[file:init.org::*Don't show updating/installation shell buffers][Don't show updating/installation shell buffers:1]]
;; By default, say, (async-shell-command "date") produces a buffer
;; with the result. In general, such commands in my init.el are for
;; updating/installing things to make sure I have the same up-to-date
;; setup where-ever I use my Emacs. As such, I don't need to see such buffers.
(add-to-list 'display-buffer-alist
             '("\\*Async Shell Command\\*.*" display-buffer-no-window))

;; For an approach that does not inhibit async-shell-command this way,
;; see https://emacs.stackexchange.com/questions/299/how-can-i-run-an-async-process-in-the-background-without-popping-up-a-buffer
;; Don't show updating/installation shell buffers:1 ends here

;; [[file:init.org::*Programming][Programming:1]]
(when my/work-machine?
  (setq doom-modeline-buffer-file-name-style 'truncate-except-project))
;; Programming:1 ends here

;; [[file:init.org::*Sleek Semantic Selection][Sleek Semantic Selection:1]]
(use-package expand-region

  :bind (("s-r" . #'er/expand-region)))
;; Sleek Semantic Selection:1 ends here

;; [[file:init.org::*Project management & navigation][Project management & navigation:1]]
;; More info & key bindings: https://docs.projectile.mx/projectile/usage.html
(use-package projectile

  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)

  ;; Replace usual find-file with a project-wide version :smile:
  (global-set-key (kbd "C-x f") #'projectile-find-file)

  ;; Makes indexing large projects much faster, after first time.
  ;; Since its caching, some files may be out of sync; you can delete the cache
  ;; with: C-u C-x f
  (setq projectile-enable-caching t))

(use-package projectile

    :config
  (define-key projectile-mode-map (kbd "C-x p s")
    ;; I prefer helm-do-grep-ag since it shows me a live search
    (lambda () (interactive)
       (let ((default-directory (car (projectile-get-project-directories (projectile-acquire-root)))))
         ;; (shell-command-to-string "echo $PWD")
         (helm-do-grep-ag nil))))) ;; “p”roject “s”earch
;; Project management & navigation:1 ends here

;; [[file:init.org::*Project management & navigation][Project management & navigation:2]]
(use-package projectile
    :defer nil
    :config
(define-key projectile-mode-map (kbd "C-x p c")
  (defun my/copy-current-file-path ()
    "Add current file path to kill ring."
    (interactive)
    (message (kill-new buffer-file-name)))))
;; Project management & navigation:2 ends here

;; [[file:init.org::*Projectile][Projectile:1]]
;; https://cestlaz.github.io/posts/using-emacs-33-projectile-jump/
;; https://github.com/bbatsov/projectile
(use-package projectile

:config (projectile-global-mode)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))
;; Projectile:1 ends here

;; [[file:init.org::*Aggressive Indentation][Aggressive Indentation:1]]
;; Always stay indented: Automatically have blocks reindented after every change.
(use-package aggressive-indent
    :defer nil
  :config (global-aggressive-indent-mode t))

;; Use 4 spaces in places of tabs when indenting.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; Aggressive Indentation:1 ends here

;; [[file:init.org::*Coding with a Fruit Salad: Semantic Highlighting][Coding with a Fruit Salad: Semantic Highlighting:1]]
(use-package color-identifiers-mode

  :config (global-color-identifiers-mode))

;; Sometimes just invoke: M-x color-identifiers:refresh
;; Coding with a Fruit Salad: Semantic Highlighting:1 ends here

;; [[file:init.org::*Text Folding ---Selectively displaying portions of a program][Text Folding ---Selectively displaying portions of a program:1]]
(use-package vimish-fold

  :config (vimish-fold-global-mode 1))
;; Text Folding ---Selectively displaying portions of a program:1 ends here

;; [[file:init.org::*Actual Setup][Actual Setup:1]]
(use-package hideshow
  :defer nil
  :init
  ;; https://github.com/emacsmirror/emacswiki.org/blob/master/hideshowvis.el
  (quelpa '(hideshowvis :fetcher wiki))

  ;; Press “C-c TAB” to toggle a block's visibility or “C-c f” for my folding hydra.
  :bind (("C-c TAB" . hs-toggle-hiding))

  ;; https://github.com/shanecelis/hideshow-org/tree/master
  ;; This extension bring Org-mode tab behaviour to folding, at the block level
  ;; and buffer level ---but not cycling visibility.
  ;; (use-package hideshow-org) ;; Disabled as commented below.

  :hook ((prog-mode . (lambda () (hs-minor-mode +1)
                        (hideshowvis-minor-mode t)
                        (hideshowvis-symbols)
                        ;; This hook along with hs-org mode breaks editing of src blocks in Org files.
                        ;; That's OK, since my folding hydra does a better job for my needs.
                        ;; (hs-org/minor-mode t)
                        (hs-hide-all)))))
;; Actual Setup:1 ends here

;; [[file:init.org::*Actual Setup][Actual Setup:2]]
(my/defhydra "C-c f" "Folding text" archive
  :Current
  ("h" hs-hide-block "Hide")
  ("s" hs-show-block "Show")
  ("t" hs-toggle-hiding "Toggle")
  ;; "l" hs-hide-level "Hide blocks n levels below this block"; TODO: Enable folding feature
  :Buffer
  ("H" hs-hide-all "Hide")
  ("S" hs-show-all "Show")
  ("T" my/hs-toggle-buffer "Toggle")
  :Style
  ("i" my/clever-selective-display "Fold along current indentation" :toggle selective-display)
  ("e" auto-set-selective-display-mode  "Explore; walk and see" :toggle t)
  :Region
  ("f" (lambda () (interactive) (vimish-fold-toggle) (vimish-fold (region-beginning) (region-end))) "Fold/Toggle")
  ("d" vimish-fold-delete "Delete fold")
  ("U" vimish-fold-unfold-all "Unfold all")
  ("D" vimish-fold-delete-all "Delete all")
  ("n" vimish-fold-next-fold "Next fold")
  ("p" vimish-fold-previous-fold "Previous fold")
  :...
  ("w" hl-todo-occur "Show WIPs/TODOs" :exit t)
  ("m" lsp-ui-imenu "Menu of TLIs" :exit t) ;; TLI ≈ Top Level Items
  ;; ("i" imenu-list "iMenu (General)") ;; It seems the above is enough for both prog and otherwise.
  ("r" (progn (hs-minor-mode -1) (hs-minor-mode +1)) "Reset Hideshow")  ;; Remove all folds from the buffer and reset all hideshow-mode. Useful if it messes up!
  ("q" nil "Quit" :color blue))

;; Features from origami/yafolding that maybe I'd like to implement include:
;; narrowing to block or folding everything except block, navigating back and forth between folded blocks.
;; Finally, if we want to cycle the visibility of a block (as in Org-mode), we can use a combination of hs-show-block and hs-hide-level.
;; Actual Setup:2 ends here

;; [[file:init.org::*Actual Setup][Actual Setup:3]]
(defvar my/hs-hide nil "Current state of hideshow for toggling all.")
(defun my/hs-toggle-buffer () "Toggle hideshow all."
       (interactive)
       (setq my/hs-hide (not my/hs-hide))
       (if my/hs-hide
           (hs-hide-all)
         (hs-show-all)))
;; Actual Setup:3 ends here

;; [[file:init.org::*Actual Setup][Actual Setup:4]]
(defun my/clever-selective-display (&optional level)
"Fold text indented same of more than the cursor.

This function toggles folding according to the level of
indentation at point. It's convenient not having to specify a
number nor move point to the desired column.
"
  (interactive "P")
  (if (eq selective-display (1+ (current-column)))
      (set-selective-display 0)
    (set-selective-display (or level (1+ (current-column))))))
;; Actual Setup:4 ends here

;; [[file:init.org::*Actual Setup][Actual Setup:5]]
;; Src: https://emacs.stackexchange.com/questions/52588/dynamically-hide-lines-indented-more-than-current-line
(define-minor-mode auto-set-selective-display-mode
  "Automatically apply `set-selective-display' at all times based on current indentation."
  nil "$" nil
  (if auto-set-selective-display-mode
      (add-hook 'post-command-hook #'auto-set-selective-display nil t)
    (remove-hook 'post-command-hook #'auto-set-selective-display t)
    (with-temp-message ""
      (set-selective-display nil))))
;;
(defun auto-set-selective-display ()
  "Apply `set-selective-display' such that current and next line are visible.

Scroll events are excluded in order to prevent wild flickering while navigating."
  (unless (eq last-command #'mwheel-scroll)
    (let*((this-line-indent (current-indentation))
          (next-line-indent (save-excursion (forward-line) (current-indentation))))
      (with-temp-message "" ; Suppress messages.
        (set-selective-display (1+ (max this-line-indent next-line-indent)))))))
;; Actual Setup:5 ends here

;; [[file:init.org::*Actual Setup][Actual Setup:6]]
;; Open folded nodes if a search stops there.
(add-hook 'helm-swoop-after-goto-line-action-hook #'my/search-hook-function)
(defun my/search-hook-function ()
  (when hs-minor-mode (set-mark-command nil) (hs-show-block) (pop-to-mark-command)))
;; Actual Setup:6 ends here

;; [[file:init.org::*hr: \[\[https:/github.com/LuRsT/hr\]\[A horizontal for your terminal\]\]][hr: [[https://github.com/LuRsT/hr][A horizontal for your terminal]]:1]]
(system-packages-ensure "hr") ;; ≈ brew install hr
;; hr: [[https://github.com/LuRsT/hr][A horizontal for your terminal]]:1 ends here

;; [[file:init.org::*w-screencapture][w-screencapture:1]]
(bind-key "C-c s"
  (cl-defun w-screencapture ()
    "Interactively capture screen and save to clipboard; then paste in Slack, etc, with ⌘-c.

  After we run this command, we can swipe up on mousepad to select different desktops, then
  click & drag to select portition of screen to capture.

  Captured screen is NOT saved to disk, only copied to clipboard.

In MacOs,
+ Command + Shift + 5  ⇒  Select screen record
+ Command + Shift + 4  ⇒  Selection Screenshot
+ Command + Shift + 3  ⇒  Screenshot

See: https://osxdaily.com/2011/08/11/take-screen-shots-terminal-mac-os-x"
    (interactive)
    (async-shell-command "screencapture -i -c")))

(cl-defun w-delete-all-screenshots ()
    "Delete all “Screen Shot ⋯” files in ~/Desktop."
    (interactive)
    (thread-last (shell-command-to-string "cd ~/Desktop; ls")
      (s-split "\n")
      (--filter (s-starts-with-p "Screen Shot" it))
      (--map (f-delete (format "~/Desktop/%s" it)))))
;; w-screencapture:1 ends here

;; [[file:init.org::*Comment-boxes up to the fill-column ---or banner instead?][Comment-boxes up to the fill-column ---or banner instead?:1]]
(defun my/comment-box (b e)
  "Draw a box comment around the region but arrange for the region
to extend to at least the fill column. Place the point after the
comment box.

Source: http://irreal.org/blog/?p=374

To do fancy stuff like removing boxes, centering them, etc
see https://github.com/lewang/rebox2/blob/master/rebox2.el"
  (interactive "r")
  (let ((e (copy-marker e t)))
    (goto-char b)
    (end-of-line)
    (insert-char ?  (- fill-column (current-column)))
    (comment-box b e 1)
    (goto-char e)
    (set-marker e nil)))
;; Comment-boxes up to the fill-column ---or banner instead?:1 ends here

;; [[file:init.org::*Comment-boxes up to the fill-column ---or banner instead?][Comment-boxes up to the fill-column ---or banner instead?:2]]
(use-package banner-comment   :defer nil)
;; Comment-boxes up to the fill-column ---or banner instead?:2 ends here

;; [[file:init.org::*Searching Hydra][Searching Hydra:1]]
(my/defhydra "s-f" "\t\tLocate Everything" search
   :Buffer
   ;; find all the occurrences of a string, pull out the lines containing the string to another buffer where [F2] I can edit and save,
   ("e" helm-swoop  "Editable")
    ;; Implicit Regex, colourful
   ("c" swiper "Classic")

   :Project
   ;; “:toggle ℰ”: ℰ is a Boolean expression that is evaluated to tell us whether the state is on-or-off
   ("t"  (lambda  () (interactive)) "Ignore specs/jsons"
    :toggle (let* ((with-hole "ag %s --line-numbers -S --color --nogroup %%s %%s %%s") ;; ≈ original value of ‘helm-grep-ag-command’
                   (ignores "--ignore=\"*spec.js\" --ignore=\"*.json\" --ignore=\"*.json5\"")
                   (on (equal helm-grep-ag-command (format with-hole ignores))))
              (if on (progn (setq helm-grep-ag-command (format with-hole "")) nil) ;; ≈ turn off the toggle
                (setq  helm-grep-ag-command (format with-hole ignores)))))
   ("f" (lambda () (interactive) (helm-do-grep-ag t)) "File type")
   ("d" (lambda () (interactive) (-let [default-directory (read-directory-name "Where do you want to search? ")] (helm-do-grep-ag nil)))  "Directory")
   ("D" (lambda () (interactive) (-let [default-directory (read-directory-name "Where do you want to search? ")] (helm-do-grep-ag t)))  "Directory & type"))
;; Searching Hydra:1 ends here

;; [[file:init.org::*⌘-e: Edit Everything in a separate buffer][⌘-e: Edit Everything in a separate buffer:1]]
(use-package separedit   :defer nil)
;;
;; # Example Usage
;;
;; 1. Press ⌘-e on this line, to edit this entire comment.
;; 2. Press ⌘-e to exit the edit session.
;;
;; Since my ⌘-e is context sensitive, to determine whether to continue editing or
;; exit; you can explicitly request an edit with C-c ' and an exit with C-c C-c.
;;
;; ```
;; ;; 3. Press ⌘-e on this line, to edit this source block!
;; ;; 4. Press ⌘-e on this line, to edit this inner-most comment!
;; ;; 5. At start of next line, press “⌘-r ⌘-e” to edit just the source block
;; ;;
;; (cl-defun index (&rest args)
;;   "6. Press ⌘-e to edit this string, \"7. and again in these quotes\""
;;   "<p>8. Press ⌘-e to edit this <strong> HTML </strong> block, in Web-mode </p>")
;;
;; ;; 9. Press C-u ⌘-e to guess the language of the next string (Rust); then ⌘-r C-c C-r to quickly run the code.
;; "fn main() { println!(\"{}\", \"hello!\"); }"
;;
;; ;; 10. Select & press “C-u ⌘-e” on the following, to edit it in whatever mode you want.
;; ;; select * from table -- Or just press ⌘-e and have the mode detected.
;;
;; ```

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup to make the above ⌘-e behaviour happen.

;; Make “⌘-e” toggle editing string literals / select region / [Org/markdown] code block / comment block when programming.
(--map (bind-key "s-e" #'separedit it)
       '(prog-mode-map minibuffer-local-map help-mode-map)) ;; TODO: helpful-mode-map
;; ⌘-e: Edit Everything in a separate buffer:1 ends here

;; [[file:init.org::*⌘-e: Edit Everything in a separate buffer][⌘-e: Edit Everything in a separate buffer:2]]
;; TODO:Merge these changes upstream

;; I'm focusing on a specific region to edit, so let's not be distracted by anything else.
;; This makes the “editing stack” feel like a stack, with ⌘-e pushing new editing session buffers,
;; and C-c C-c, or ⌘-e on non-editable lines, to pop-off the stack.
;; (advice-add #'separedit :after (lambda (&rest _) (delete-other-windows)))
;;
;; NOTE: This actually breaks the stack nature of popping with ⌘-e; we need to actually save the stack via some list of buffers than push/pop buffers on that variable.

;; I don't want to be bothered for what mode I'm in, when a region is selected using current major mode.
;; I'll use a prefix, “C-u ⌘-e”, if I want to select a mode for my current selected text.
(advice-add #'separedit--select-mode :before-until
            (lambda (&rest _)
              (when (and (not current-prefix-arg) (region-active-p)) (pp-to-string major-mode))))

;; Also: When on a string ∷
(advice-add #'separedit--select-mode :before-until
            (lambda (&rest _)
              "When on a string ∷
+ ⌘-e ⇒ Edit string at point
+ C-u ⌘-e ⇒ Auto-detect my string's major mode
+ C-u C-u ⌘-e ⇒ Let me select a major mode"
              (-let [str? (ignore-errors (thing-at-point 'string))]
                (case (car current-prefix-arg)
                  (4 (when str? (pp-to-string (my/detect-prog-mode str?))))
                  (_ nil)))))

;; NOTE: By default, separedit provides colouring for 'strings', "strings", and `strings'
;; This doesn't look very good when I have a single quote within double quotes:
;; In an Emacs Lisp buffer, editing the string "Bob's Work" gives unexpected highlighting.
;; ```
;; (advice-add #'separedit :after
;;             (lambda (&rest _)
;;               (when (s-ends-with? "string-mode" (pp-to-string major-mode))
;;                 (text-mode))))
;; ```
;; ⌘-e: Edit Everything in a separate buffer:2 ends here

;; [[file:init.org::*⌘-e: Edit Everything in a separate buffer][⌘-e: Edit Everything in a separate buffer:3]]
;; In the indirect buffer, make ⌘-e finish editing.
(use-package edit-indirect
  :config (bind-key "s-e"
                    (lambda ()
                      (interactive)
                      (or (ignore-errors (call-interactively #'separedit))
                          (call-interactively #'edit-indirect-commit)))
                    #'edit-indirect-mode-map))

;; I also have “s-e” bound to `org-edit-src-exit'.
(advice-add 'org-edit-src-exit :before-until
            (lambda (&rest r)
              (when (ignore-errors (separedit)) t)))
;; ⌘-e: Edit Everything in a separate buffer:3 ends here

;; [[file:init.org::*⌘-e: Edit Everything in a separate buffer][⌘-e: Edit Everything in a separate buffer:4]]
;; → ⌘-e on an Org paragraph pops-up an edit session in Org mode.
;; → ⌘-e on a selection in Org mode pops-up an edit session in Org mode.
;; TODO: Consider forming an alist for special blocks to refer to their preferred
;; edit mode, defaulting to Org-mode? Perhaps something to consider /after/
;; addressing the bug below.
;; (advice-unadvice 'org-edit-special) MA: TODO: FIXME: Delete this?
(advice-add 'org-edit-special :around
            (lambda (orginal-function &rest r)
              (cond
               ((region-active-p) (call-interactively #'edit-indirect-region) (org-mode))
               ((equal 'paragraph (car (org-element-at-point)))
                (mark-paragraph) (call-interactively #'edit-indirect-region) (org-mode))
               (t (or (ignore-errors (apply orginal-function r))
                      ;; We try to edit a special block when orginal-function fails.
                      ;; This way src blocks are not confused with the more generic idea of special blocks.
                      (when
                          (my/org-in-any-block-p)
                        ;; Note using org-element-at-point doesn't work well with special blocks when you're somewhere within the block.
                        ;; It only works correctly when you're on the boundary of the special block; which is not ideal.
                        ;; This is why I'm not using: (org-element-property :begin elem).
                        (-let [(start . end) (my/org-in-any-block-p)]
                          (set-mark-command start)
                          (goto-char end) (previous-line 2) (end-of-line) ;; FIXME: Still shows #+end_XXX for some reason.
                          (call-interactively #'edit-indirect-region) (org-mode))))))))
;; ⌘-e: Edit Everything in a separate buffer:4 ends here

;; [[file:init.org::*⌘-e: Edit Everything in a separate buffer][⌘-e: Edit Everything in a separate buffer:5]]
;; where...
(defun my/org-in-any-block-p ()
  "Return non-nil if the point is in any Org block.

The Org block can be *any*: src, example, verse, etc., even any
Org Special block.

This function is heavily adapted from `org-between-regexps-p'.

Src: https://scripter.co/splitting-an-org-block-into-two/"
  (save-match-data
    (let ((pos (point))
          (case-fold-search t)
          (block-begin-re "^[[:blank:]]*#\\+begin_\\(?1:.+?\\)\\(?: .*\\)*$")
          (limit-up (save-excursion (outline-previous-heading)))
          (limit-down (save-excursion (outline-next-heading)))
          beg end)
      (save-excursion
        ;; Point is on a block when on BLOCK-BEGIN-RE or if
        ;; BLOCK-BEGIN-RE can be found before it...
        (and (or (org-in-regexp block-begin-re)
                 (re-search-backward block-begin-re limit-up :noerror))
             (setq beg (match-beginning 0))
             ;; ... and BLOCK-END-RE after it...
             (let ((block-end-re (concat "^[[:blank:]]*#\\+end_"
                                         (match-string-no-properties 1)
                                         "\\( .*\\)*$")))
               (goto-char (match-end 0))
               (re-search-forward block-end-re limit-down :noerror))
             (> (setq end (match-end 0)) pos)
             ;; ... without another BLOCK-BEGIN-RE in-between.
             (goto-char (match-beginning 0))
             (not (re-search-backward block-begin-re (1+ beg) :noerror))
             ;; Return value.
             (cons beg end))))))
;; ⌘-e: Edit Everything in a separate buffer:5 ends here

;; [[file:init.org::*⌘-e: Edit Everything in a separate buffer][⌘-e: Edit Everything in a separate buffer:6]]
(use-package language-detection   :defer nil)
;; Usage: M-x language-detection-buffer ⇒ Get programming language of current buffer
;; Also, (language-detection-string "select * from t") ;; ⇒ sql

;; TODO: Push this upstream; https://github.com/andreasjansson/language-detection.el/issues/1
(cl-defun my/detect-prog-mode (&optional string)
  "Guess programming mode of the current buffer, or STRING if it is provided.

When called interactively, it enables the mode;
from Lisp it just returns the name of the associated mode.

    ;; Example Lisp usage
    (call-interactively #'my/detect-prog-mode)

`language-detection-buffer' returns a string which is not always the name of the
associated major mode; that's what we aim to do here."
  (interactive)

  (defvar my/detect-prog-mode/special-names
    '((c           . c-mode)
      (cpp         . c++-mode)
      (emacslisp   . emacs-lisp-mode)
      (html        . web-mode) ;; I intentionally want to use this alternative.
      (matlab      . octave-mode)
      (shell       . shell-script-mode)
      (visualbasic . visual-basic-mode)
      (xml         . sgml-mode))
    "Names in this alist map a language to its mode; all other languages 𝒳 have mode ‘𝒳-mode’ afaik.")

  (let* ((lang (if string (language-detection-string string) (language-detection-buffer)))
         (mode (or (cdr (assoc lang my/detect-prog-mode/special-names))
                   (intern (format "%s-mode" lang)))))
    (if (called-interactively-p 'any)
        (progn (call-interactively mode) (message "%s enabled!" mode))
      mode)))
;; ⌘-e: Edit Everything in a separate buffer:6 ends here

;; [[file:init.org::*⌘-e: Edit Everything in a separate buffer][⌘-e: Edit Everything in a separate buffer:7]]
(advice-add #'org-edit-special :before-until
            (lambda (&rest r)
              (when (equal 'table-row (car (org-element-at-point)))
                (call-interactively #'org-table-edit-field))))
;; ⌘-e: Edit Everything in a separate buffer:7 ends here

;; [[file:init.org::*Eldoc for Lisp and Haskell ---documentation in the mini-buffer][Eldoc for Lisp and Haskell ---documentation in the mini-buffer:1]]
(use-package eldoc
  :hook (emacs-lisp-mode . turn-on-eldoc-mode)
        (lisp-interaction-mode . turn-on-eldoc-mode)
        (haskell-mode . turn-on-haskell-doc-mode)
        (haskell-mode . turn-on-haskell-indent))

;; Slightly shorten eldoc display delay.
(setq eldoc-idle-delay 0.4) ;; Default 0.5
;; Eldoc for Lisp and Haskell ---documentation in the mini-buffer:1 ends here

;; [[file:init.org::*Open PDFs in Emacs][Open PDFs in Emacs:1]]
    ;; In Org-mode, clicking on PDF should open it in Emacs
    ;; Example:          [[~/Desktop/stuff-I'm-learning.pdf::12]]       ;; Opens the pdf to page 12
    ;; Another Example:  docview:~/Desktop/stuff-I'm-learning.pdf::12
  (ignore-errors (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs)))

    ;; Required code to make the above links work as expected.
    ;; Source: https://www.reddit.com/r/emacs/comments/re0dx8/how_do_you_link_a_specific_pdf_page_in_org_mode/
    (defun my-org-docview-open-hack (orig-func &rest args)
      (let* ((link (car args)) path page)
        (string-match "\\(.*?\\)\\(?:::\\([0-9]+\\)\\)?$" link)
        (setq path (match-string 1 link))
        (setq page (and (match-beginning 2)
                        (string-to-number (match-string 2 link))))
        (org-open-file path 1)
        (when page
          (cond
           ((eq major-mode 'pdf-view-mode)
            (pdf-view-goto-page page))
           (t
            (doc-view-goto-page page))))))
    (advice-add 'org-docview-open :around #'my-org-docview-open-hack)

    ;; Alternatively, there's a dedicated package for this
    ;; https://github.com/fuxialexander/org-pdftools/tree/967f48fb5038bba32915ee9da8dc4e8b10ba3376
;; Open PDFs in Emacs:1 ends here

;; [[file:init.org::*empv][empv:1]]
(use-package empv )

;; Then, M-x empv-play https://invidious.fdn.fr/watch?v=hlTqCmpP5eo, to listen to Dua Ifitiah in the background
;; Or: M-x empv-play https://invidious.fdn.fr/watch?v=9m9yE7qtq5w
;; lol maybe make a hydra(ie playlist) of things I commonly listen to in the background while working.
;; Require: brew install mpv
;; See also:
;; Maybe better? https://github.com/spiderbit/ytdious/tree/941460b51e43ef6764e15e2b9c4af54c3e56115f
;; Maybe better? https://melpa.org/#/yeetube
;; https://github.com/maximus12793/helm-youtube/tree/e7272f1648c7fa836ea5ac1a61770b4931ab4709
;; https://github.com/isamert/empv.el/tree/1721a581d68f211a7f0104554858ea2afb1723ff
;;
(setq empv-invidious-instance "https://invidious.fdn.fr.com")
;; empv:1 ends here

;; [[file:init.org::*DONE?][DONE?:1]]
(find-file "~/.emacs.d/init.org")
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(message-box "Done")
;; DONE?:1 ends here
