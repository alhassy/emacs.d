;; [[file:init.org::#Personal-instructions-for-a-new-machine][Personal instructions for a new machine:4]]
    (setq org-image-actual-width nil)
;; Personal instructions for a new machine:4 ends here

;; [[file:init.org::#Personal-instructions-for-a-new-machine][Personal instructions for a new machine:5]]
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
  :config (which-key-mode)
          (which-key-setup-side-window-bottom)
          (setq which-key-idle-delay 0.05))

(use-package dash) ;; “A modern list library for Emacs”
(use-package s)    ;; “The long lost Emacs string manipulation library”.
(use-package f)    ;; Library for working with system files; ;; e.g., f-delete, f-mkdir, f-move, f-exists?, f-hidden?

(defvar my/personal-machine?
  (equal "Musa’s MacBook Air " (s-collapse-whitespace (shell-command-to-string "scutil --get ComputerName")))
  "Is this my personal machine, or my work machine?

 At one point, on my work machine I run the following command to give the machine a sensible name.

     sudo scutil --set ComputerName work-machine
     dscacheutil -flushcache")

(defvar my/work-machine? (not my/personal-machine?))

  ;; Allow tree-semantics for undo operations.
  (use-package undo-tree
    :bind ("C-x u" . undo-tree-visualize)
    :config
      ;; Each node in the undo tree should have a timestamp.
      (setq undo-tree-visualizer-timestamps t)
  
      ;; Show a diff window displaying changes between undo nodes.
      (setq undo-tree-visualizer-diff t)
  
      ;; Prevent undo tree files from polluting your git repo
      (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))
  
  ;; Always have it on
  (global-undo-tree-mode)
  
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
(use-package system-packages)

;; Ensure our operating system is always up to date.
;; This is run whenever we open Emacs & so wont take long if we're up to date.
;; It happens in the background ^_^
;;
;; After 5 seconds of being idle, after starting up.
(run-with-idle-timer 5 nil #'system-packages-update) ;; ≈ (async-shell-command "brew update && brew upgrade")

(defvar my/installed-packages
  (shell-command-to-string "brew list")
  "What is on my machine already?

Sometimes when I install a GUI based application and do not have access to it everywhere in my path,
it may seem that I do not have that application installed. For instance,
   (system-packages-package-installed-p \"google-chrome\")
returns nil, even though Google Chrome is on my machine.

As such, we advise the `system-packages-ensure' installtion method to only do
installs of packages that are not in our `my/installed-packages' listing.
")
(advice-add 'system-packages-ensure   :before-until (lambda (pkg) (s-contains-p pkg my/installed-packages)))

;; Please don't bother me when shell buffer names are in use, just make a new buffer.
(setq async-shell-command-buffer 'new-buffer)

;; Display the output buffer for asynchronous shell commands only when the
;; command generates output.
(setq async-shell-command-display-buffer nil)

;; Don't ask me if I want to kill a buffer with a live process attached to it;
;; just kill it please.
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; An Emacs-based interface to the package manager of your operating system.
(use-package helm-system-packages)

(setq system-packages-noconfirm :do-not-prompt-me-about-confirms)

;; After 1 minute after startup, kill all buffers created by ensuring system
;; packages are present.
(run-with-timer 60 nil
 (lambda () (kill-matching-buffers ".*system-packages.*" t :kill-without-confirmation)))

;; Unlike the Helm variant, we need to specify our OS pacman.
(setq system-packages-package-manager 'brew)

;; If the given system package doesn't exist; install it.
;; (system-packages-ensure "amethyst") ;; This is a MacOS specific package.

;; (ignore-errors (system-packages-ensure "google-chrome")) ;; My choice of web browser
;; (system-packages-ensure "microsoft-teams") ;; For remote work meetings

;; Gif maker; needs privileges to capture screen.
;;
;; ⇒ Move the screen capture frame while recording.
;; ⇒ Pause and restart recording, with optional inserted text messages.
;; ⇒ Global hotkey (shift+space) to toggle pausing while recording
(system-packages-ensure "licecap") ;; Use: ⌘-SPACE licecap

;; Pack, ship and run any application as a lightweight container
;; (system-packages-ensure "docker")
;; Free universal database tool and SQL client
;; (system-packages-ensure "dbeaver-community")
;; Kubernetes IDE
;; (system-packages-ensure "lens")
;; Platform built on V8 to build network applications
;; Also known as: node.js, node@16, nodejs, npm
(system-packages-ensure "node") ;; https://nodejs.org/
;; Nice: https://nodesource.com/blog/an-absolute-beginners-guide-to-using-npm/
;; Manage multiple Node.js versions
;; (shell-command "curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.38.0/install.sh | bash")
;; According to https://github.com/nvm-sh/nvm, nvm shouldn't be installed via brew.

;; ;; Use “brew cask install” instead of “brew install” for installing programs.;
;; (setf (nth 2 (assoc 'brew system-packages-supported-package-managers))
;;      '(install . "brew cask install"))

;; By default, say, (async-shell-command "date") produces a buffer
;; with the result. In general, such commands in my init.el are for
;; updating/installing things to make sure I have the same up-to-date
;; setup where-ever I use my Emacs. As such, I don't need to see such buffers.
(add-to-list 'display-buffer-alist
             '("\\*Async Shell Command\\*.*" display-buffer-no-window))

;; For an approach that does not inhibit async-shell-command this way,
;; see https://emacs.stackexchange.com/questions/299/how-can-i-run-an-async-process-in-the-background-without-popping-up-a-buffer

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Provides only the command “restart-emacs”.
(use-package restart-emacs
  ;; If I ever close Emacs, it's likely because I want to restart it.
  :bind ("C-x C-c" . restart-emacs)
  ;; Let's define an alias so there's no need to remember the order.
  :config (defalias 'emacs-restart #'restart-emacs))

(setq-default save-place  t)
(setq save-place-file "~/.emacs.d/etc/saveplace")

(use-package helm
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
;;   :custom (helm-icons-provider 'all-the-icons)
;;   :config (helm-icons-enable))

;; When I want to see the TOC of an Org file, show me down to 3 subheadings.
(setq org-imenu-depth 7)

(setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf
                                    helm-source-bookmarks
                                    helm-source-bookmark-set
                                    helm-source-buffer-not-found))

(use-package helm-swoop
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

(use-package emacs
    :ensure org-contrib
    :config (require 'ox-extra)
            (ox-extras-activate '(ignore-headlines)))

;; Replace the content marker, “⋯”, with a nice unicode arrow.
(setq org-ellipsis "  ⮛")
;; Other candidates:
;; (setq org-ellipsis "   📖")
;; (setq org-ellipsis "  ◦◦◦")
;; (setq org-ellipsis "  ⟨🫣⟩")
;; (setq org-ellipsis "  ⟨👀⟩")
;; (setq org-ellipsis " ⤵")


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
(use-package hydra)

;; Show hydras overlayed in the middle of the frame
(use-package hydra-posframe
  :disabled "TODO Fix me, breaking Github Actions test setup"
  :quelpa (hydra-posframe :fetcher git :url
                          "https://github.com/Ladicle/hydra-posframe.git")
  :hook (after-init . hydra-posframe-mode)
  :custom (hydra-posframe-border-width 5))

;; Neato doc strings for hydras
(use-package pretty-hydra)

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

(system-packages-ensure "hr") ;; ≈ brew install hr

;; Usage: M-x helm-shell-history
(use-package helm-shell-history
  :config
  (setq helm-shell-history-file "~/.zsh_history")
  (bind-key "M-r" #'helm-shell-history shell-mode-map))

;; MacOS's default ⌘-SPC does not let us do either of the following scenarios:
;; Usage: M-x helm-osx-app RET preferences bat RET ⇒ See battery preferences settings
;; Another Usage: M-x helm-osx-app RET ⇒ See all apps, maybe we forgot about one of them from an install a long time ago, and open it
;; See https://www.alfredapp.com/ as an alternative (for non-Emacs users), which can do more.
(use-package helm-osx-app)
;; For non-MacOS, we can use [[https://github.com/d12frosted/counsel-osx-app][counsel-osx-app]], whose name is misleading.

;; [[file:init.org::#Manipulating-Sections][Manipulating Sections:1]]
(setq org-use-speed-commands t)
;; Manipulating Sections:1 ends here

;; [[file:init.org::#Manipulating-Sections][Manipulating Sections:2]]
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

;; [[file:init.org::#Manipulating-Sections][Manipulating Sections:3]]
;; TODO FIXME Crashes upon startup.
(when nil (add-to-list 'org-speed-commands (cons "P" #'org-set-property)))
;; Use ‘:’ and ‘e’ to set tags and effort, respectively.
;; Manipulating Sections:3 ends here

;; [[file:init.org::#Seamless-Navigation-Between-Source-Blocks][Seamless Navigation Between Source Blocks:1]]
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

;; [[file:init.org::#Modifying-return][Modifying [[kbd:⟨return⟩]]:1]]
(add-hook 'org-mode-hook '(lambda ()
   (local-set-key (kbd "<return>") 'org-return-indent))
   (local-set-key (kbd "C-M-<return>") 'electric-indent-just-newline))
;; Modifying [[kbd:⟨return⟩]]:1 ends here

;; [[file:init.org::#Executing-code-from-src-blocks][Executing code from ~src~ blocks:1]]
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

;; [[file:init.org::#Executing-code-from-src-blocks][Executing code from ~src~ blocks:2]]
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

;; [[file:init.org::#Executing-all-name-startup-code-for-local-configurations][Executing all =#+name: startup-code= for local configurations:1]]
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

;; [[file:init.org::#Executing-all-name-startup-code-for-local-configurations][Executing all =#+name: startup-code= for local configurations:2]]
;; Please ask me on a file by file basis whether its local variables are ‘safe’
;; or not. Use ‘!’ to mark them as permanently ‘safe’ to avoid being queried
;; again for the same file.
(setq enable-local-variables t)
;; Executing all =#+name: startup-code= for local configurations:2 ends here

;; [[file:init.org::#Prettify-inline-source-code][Prettify inline source code:1]]
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

;; [[file:init.org::*The “∶Disabled∶” tag ---Stolen from AlBasmala.el, and improved][The “∶Disabled∶” tag ---Stolen from AlBasmala.el, and improved:1]]
(defmacro org-deftag (name args docstring &rest body)
  "Re-render an Org section in any way you like, by tagging the section with NAME.

That is to say, we essentially treat tags as functions that act on Org headings:
We redefine Org sections for the same purposes as Org special blocks.

The “arguments” to the function-tag can be declared as Org properties, then
the function can access them using the `o-properties' keyword as in
   (-let [(&plist :file :date :color) o-properties]
       (insert \"%s: %s\" file date))

Anyhow:
ARGS are the sequence of items seperated by underscores after the NAME of the new tag.
BODY is a form that may anaphorically mention:
- O-BACKEND: The backend we are exporting to, such as `latex' or `html'.
- O-HEADING: The string denoting the title of the tagged section heading.
- O-PROPERTIES: A plist of the Org properties at point.

DOCSTRING is mandatory; everything should be documented for future maintainability.

The result of this anaphoric macro is a symbolic function name `org-deftag/NAME',
which is added to `org-export-before-parsing-hook'.

----------------------------------------------------------------------

Below is the motivating reason for inventing this macro. It is used:

     ** Interesting, but low-priority, content   :details_red:
     Blah blah blah blah blah blah blah blah blah blah blah.
     Blah blah blah blah blah blah blah blah blah blah blah.

Here is the actual implementation:

(org-deftag details (color)
   \"HTML export a heading as if it were a <details> block; COLOR is an optional
   argument indicating the background colour of the resulting block.\"
   (insert \"\n#+html:\"
           (format \"<details style=\\\"background-color: %s\\\">\" color)
           \"<summary>\" (s-replace-regexp \"^\** \" \"\" o-heading) \"</summary>\")
   (org-next-visible-heading 1)
   (insert \"#+html: </details>\"))

"
  (let ((func-name (intern (format "org-deftag/%s" name))))
    `(progn
       (cl-defun ,func-name (o-backend)
         ,docstring
         (outline-show-all)
         (org-map-entries
          (lambda ()
            (-let [(&alist ,@ (mapcar #'symbol-name args)) (map-apply (lambda (k v) (cons (downcase k) v)) (org-entry-properties (point)))]
              ;; MA: Maybe get rid of o-heading and o-properties and let people operate on raw Org secitons
              ;; as they do with org-agenda. That might provide a more unified approach.
              (let ((o-properties (map-into (map-apply (lambda (k v) (cons (intern (concat ":" (downcase k))) v)) (org-entry-properties (point))) 'plist))
                    (o-heading (progn (kill-line) (car kill-ring))))
                (if (not (s-contains? (format ":%s" (quote ,name)) o-heading 'ignoring-case))
                    (insert o-heading)
                  (setq o-heading (s-replace-regexp (format ":%s[^:]*:" (quote ,name)) "" o-heading))
                  ,@body)
                ;; Otherwise we impede on the auto-inserted “* footer :ignore:”
                (insert "\n"))))))
       (add-hook 'org-export-before-parsing-hook (quote ,func-name))
       )))


;; MA: This is new stuff.
(put 'org-deflink 'lisp-indent-function 'defun)
(put 'org-deftag 'lisp-indent-function 'defun)

       ;; Example use
       (org-deftag identity ()
         "Do nothing to Org headings"
         (insert o-heading)) ;; Wait, I think this strips tags?


       (org-deftag disabled (color)
         "Render the body of a heading in a <details> element, titled “Disabled”.

The heading remains in view, and so appears in the TOC."
         (insert "\n") (insert  o-heading) (insert "\n")
         (insert "\n#+html:"
                 (format "<div> <details class=\"float-child\" style=\"background-color: %s\">"
                         (or color "pink"))
                 "<summary> <strong> <font face=\"Courier\" size=\"3\" color=\"green\">"
                 "Details ﴾This is disabled, I'm not actively using it.﴿"
                 "</font> </strong> </summary>")
         ;; Something to consider: (org-set-property "UNNUMBERED" "nil")
         (org-next-visible-heading 1)
         (insert "#+html: </details> </div>"))
;; The “∶Disabled∶” tag ---Stolen from AlBasmala.el, and improved:1 ends here

;; [[file:init.org::#Jumping-to-extreme-semantic-units][Jumping to extreme semantic units:1]]
;; M-< and M-> jump to first and final semantic units.
;; If pressed twice, they go to physical first and last positions.
(use-package beginend
  :config (beginend-global-mode))
;; Jumping to extreme semantic units:1 ends here

;; [[file:init.org::#Folding-within-a-subtree][Folding within a subtree:1]]
(bind-key "C-c C-h"
          (defun my/org-fold-current-subtree-anywhere-in-it ()
            (interactive)
            (save-excursion (save-restriction
                              (org-narrow-to-subtree)
                              (org-shifttab)
                              (widen))))
          org-mode-map)
;; Folding within a subtree:1 ends here

;; [[file:init.org::#Draw-pretty-unicode-tables-in-org-mode][Draw pretty unicode tables in org-mode:1]]
(quelpa '(org-pretty-table
         :repo "Fuco1/org-pretty-table"
         :fetcher github))

(add-hook 'org-mode-hook 'org-pretty-table-mode)
;; Draw pretty unicode tables in org-mode:1 ends here

;; [[file:init.org::#Buffer-defaults][Buffer default mode is org-mode:1]]
(setq-default major-mode 'org-mode)
;; Buffer default mode is org-mode:1 ends here

;; [[file:init.org::#Use-Org-Mode-links-in-other-modes-Links-can-be-opened-and-edited-like-in-Org-Mode][Use Org Mode links in other modes: Links can be opened and edited like in Org Mode.:1]]
;; E.g., in ELisp mode, the following is clickable and looks nice: [[info:man][Read the docs!]]
;;
;; In particular, when I tangle my init.org into a Lisp file, init.el, it has Org links
;; back to the original source section in Org, which I can then click to jump to, quickly.
;;
(use-package orglink
  :config
  (global-orglink-mode)
  ;; Only enable this in Emacs Lisp mode, for now.
  (setq orglink-activate-in-modes '(emacs-lisp-mode)))
;; Use Org Mode links in other modes: Links can be opened and edited like in Org Mode.:1 ends here

;; [[file:init.org::*No code evaluation upon export][No code evaluation upon export:1]]
;; Ignore all header arguments relating to “:eval”. Do not evaluate code when I export to HTML or LaTeX or anything else.
(setq org-export-use-babel nil)
;; No code evaluation upon export:1 ends here

;; [[file:init.org::#Undo-tree-Very-Local-Version-Control][Undo-tree: Very Local Version Control:2]]
;; By default C-z is suspend-frame, i.e., minimise, which I seldom use.
(global-set-key (kbd "C-z")
  (lambda () (interactive)
   (undo-tree-mode) ;; Ensure the mode is on
   (undo-tree-visualize)))
;; Undo-tree: Very Local Version Control:2 ends here

;; [[file:init.org::#Automatic-Backups][Automatic Backups:1]]
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

;; [[file:init.org::#Automatic-Backups][Automatic Backups:2]]
(setq confirm-kill-processes nil
      create-lockfiles nil)
;; Automatic Backups:2 ends here

;; [[file:init.org::#What-changed][What changed? ---Walking through backups:1]]
(use-package backup-walker
  :commands backup-walker-start)
;; What changed? ---Walking through backups:1 ends here

;; [[file:init.org::#Save-Backup][Save ≈ Backup:1]]
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

;; [[file:init.org::#delete-by-moving-to-trash-t][delete-by-moving-to-trash t:1]]
;; Move to OS’ trash can when deleting stuff
;; instead of deleting things outright!
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/")
;; delete-by-moving-to-trash t:1 ends here

;; [[file:init.org::*Intro][Intro:1]]
;; Bottom of Emacs will show what branch you're on
;; and whether the local file is modified or not.
(use-package magit
  :bind (("C-c M-g" . magit-file-dispatch))
  :config (global-set-key (kbd "C-x g") 'magit-status)
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

;; [[file:init.org::#Credentials-I-am-who-I-am][Credentials: I am who I am:1]]
;; Only set these creds up if there is no Git email set up ---ie at work I have an email set up, so don't
;; override it with my personal creds.
;;
;; See here for a short & useful tutorial:
;; https://alvinalexander.com/git/git-show-change-username-email-address
(when (equal "" (shell-command-to-string "git config user.email "))
  (shell-command (format "git config --global user.name \"%s\"" user-full-name))
  (shell-command (format "git config --global user.email \"%s\"" user-mail-address)))
;; Credentials: I am who I am:1 ends here

;; [[file:init.org::#Credentials-I-am-who-I-am][Credentials: I am who I am:2]]
;; We want to reuse an existing Emacs process from the command line
;; E.g.,  emacsclient --eval '(+ 1 2)'    # ⇒ 3
(server-start)

;; Or use it whenever we are editing a git message from the terminal
(shell-command "git config --global core.editor 'emacsclient -t -a=\\\"\\\"'")
;; Credentials: I am who I am:2 ends here

;; [[file:init.org::#Encouraging-useful-commit-messages][Encouraging useful commit messages:1]]
(defun my/git-commit-reminder ()
  (insert "\n\n# The commit subject line ought to finish the phrase:
# “If applied, this commit will ⟪your subject line here⟫.” ")
  (beginning-of-buffer))

(add-hook 'git-commit-setup-hook 'my/git-commit-reminder)
;; Encouraging useful commit messages:1 ends here

;; [[file:init.org::#Maybe-clone-everything][Maybe clone ... everything?:1]]
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
;; (maybe-clone "https://github.com/alhassy/holy-books")
;; Maybe clone ... everything?:1 ends here

;; [[file:init.org::#Maybe-clone-everything][Maybe clone ... everything?:2]]
;; (maybe-clone "https://github.com/alhassy/melpa")
(maybe-clone "https://github.com/alhassy/org-special-block-extras")


;; (maybe-clone "https://github.com/alhassy/next-700-module-systems-proposal.git" "~/thesis-proposal")
;; (maybe-clone "https://github.com/JacquesCarette/MathScheme")
;; (maybe-clone "https://github.com/alhassy/gentle-intro-to-reflection" "~/reflection/")
;; (maybe-clone "https://github.com/alhassy/org-agda-mode")
;; (maybe-clone "https://github.com/JacquesCarette/TheoriesAndDataStructures")
;; (maybe-clone "https://gitlab.cas.mcmaster.ca/RATH/RATH-Agda"     "~/RATH-Agda")
;; (maybe-clone "https://github.com/alhassy/MyUnicodeSymbols") ;; Deleted?

(maybe-clone "https://github.com/alhassy/islam")
;; (maybe-clone "https://github.com/alhassy/CheatSheet")
;; (maybe-clone "https://github.com/alhassy/ElispCheatSheet")
;; (maybe-clone "https://github.com/alhassy/CatsCheatSheet")
;; (maybe-clone "https://github.com/alhassy/OCamlCheatSheet")
;; (maybe-clone "https://github.com/alhassy/AgdaCheatSheet")
;; (maybe-clone "https://github.com/alhassy/RubyCheatSheet")
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

;; [[file:init.org::#Gotta-love-that-time-machine][Gotta love that time machine:1]]
(use-package git-timemachine )
;; Gotta love that time machine:1 ends here

;; [[file:init.org::#Pretty-Magit-Commit-Leaders][Pretty Magit Commit Leaders:1]]
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

;; [[file:init.org::#Pretty-Magit-Commit-Leaders][Pretty Magit Commit Leaders:2]]
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

;; [[file:init.org::#Pretty-Magit-Commit-Leaders][Pretty Magit Commit Leaders:3]]
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

;; [[file:init.org::#Highlighting-TODO-s-Showing-them-in-Magit][Highlighting TODO-s & Showing them in Magit:1]]
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

;; [[file:init.org::#Highlighting-TODO-s-Showing-them-in-Magit][Highlighting TODO-s & Showing them in Magit:3]]
(defun add-watchwords () "Add TODO: words to font-lock keywords."
  (font-lock-add-keywords nil
                          '(("\\(\\<TODO\\|\\<FIXME\\|\\<HACK\\|@.+\\):" 1
                             font-lock-warning-face t))))

(add-hook 'prog-mode-hook #'add-watchwords)
;; Highlighting TODO-s & Showing them in Magit:3 ends here

;; [[file:init.org::#Highlighting-TODO-s-Showing-them-in-Magit][Highlighting TODO-s & Showing them in Magit:4]]
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

;; Get org-headers to look pretty! E.g., * → ⊙, ** ↦ ◯, *** ↦ ★
;; https://github.com/emacsorphanage/org-bullets
(use-package org-bullets :hook (org-mode . org-bullets-mode))

(if (member "Apple Color Emoji" (font-family-list))
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)
  (message-box "Musa: Install the font!"))
;; E.g., Download font such as https://fonts.google.com/noto/specimen/Noto+Color+Emoji
;; Double-click on the ttf file then select “install” to have it installed on your system
;; (Note: Noto does not work on my personal machine.)


;; Render ASCII such as “ :-) ” as emoji 🙂.
(use-package emojify)
(setq emojify-display-style 'unicode) ;; unicode is the way to go!
(setq emojify-emoji-styles '(unicode))
(global-emojify-mode 1) ;; Will install missing images, if need be.

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

;; I have symlinks for various things, just follow them, do not ask me.
(setq vc-follow-symlinks t)

;; After my settings have been loaded, e.g., fancy priorities
;; and cosmetics, then open my notes files.
(add-hook 'emacs-startup-hook
          (lambda ()
            (-let [my-life.el (getenv "MY_LIFE_ELISP")]
              (unless org-default-notes-file
                (error "Add to .zshrc “ export MY_LIFE_ELISP=\"/full/path/to/my-life.el\" ”, then load my-life.el"))
              (load-file my-life.el))))

;; The modeline looks really nice with doom-themes, e.g., doom-solarised-light.
(use-package doom-modeline
  :config (doom-modeline-mode))

  ;; Use minimal height so icons still fit; modeline gets slightly larger when
  ;; buffer is modified since the "save icon" shows up.  Let's disable the icon.
  ;; Let's also essentially disable the hud bar, a sort of progress-bar on where we are in the buffer.
  (setq doom-modeline-height 21)
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

;; Infinite list of my commonly used themes.
(setq my/themes
      (cl-loop for (package . theme-variants-I-like) in
               ;; I like theme doom-flatwhite <3 It feels “warm”.
               ;; (I found out thanks to C-u C-c t!)
               '((doom-themes doom-flatwhite doom-snazzy doom-monokai-ristretto doom-laserwave doom-solarized-light doom-vibrant)
                 (solarized-theme solarized-gruvbox-dark solarized-gruvbox-light)
                 (stimmung-themes stimmung-themes-light stimmung-themes-dark)
                 (shanty-themes shanty-themes-light)
                 (apropospriate-theme apropospriate-light) ;; /super/ nice! Super “clean”, like writing on paper
                 (tao-theme tao-yang) ;; nice light theme.
                 (leuven-theme leuven-dark leuven) ;; Nice minimal variant
                 (material-theme material-light)
                 (moe-theme moe-light)
                 (organic-green-theme organic-green)
                 (tango-plus-theme tango-plus)
                 ;; I like all 3 variants.
                 (minimal-theme minimal minimal-black minimal-light)
                 (espresso-theme espresso)
                 (emacs dichromacy)
                 (modus-themes modus-operandi-tinted))
               do (package-install package)
               append theme-variants-I-like))

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

(my/toggle-theme)

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
  :config (setq beacon-color "#666600")
  :hook   ((org-mode text-mode) . beacon-mode))

(use-package dimmer
  :config (dimmer-mode))

;; (setq visible-bell 1) ;; On MacOS, this shows a caution symbol ^_^

;; The doom themes package comes with a function to make the mode line flash on error.
;; (use-package doom-themes)
;; (require 'doom-themes-ext-visual-bell)
;; (doom-themes-visual-bell-config)

(blink-cursor-mode 1)

(unless noninteractive
  (tool-bar-mode   -1)    ;; No large icons please
  (scroll-bar-mode -1))   ;; No visual indicator please
  ;; (menu-bar-mode   -1) ;; The Mac OS top pane has menu options

(setq show-paren-delay  0)
(setq show-paren-style 'mixed)
(show-paren-mode)

(use-package rainbow-delimiters
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
  :hook (org-mode . org-appear-mode)
  :init (setq org-appear-autoemphasis  t
              org-appear-autolinks nil
              org-appear-autosubmarkers nil))

;; Automatically toggle LaTeX previews when cursour enters/leaves them
(use-package org-fragtog
  :disabled t
  :hook (org-mode . org-fragtog-mode))

;; Support “latex-as-png” src blocks, which show LaTeX as PNGs
(use-package ob-latex-as-png :disabled t)

;; Use the “#+name” the user provides, instead of generating label identifiers.
(setq org-latex-prefer-user-labels t)

 (use-package org-sticky-header
  :hook (org-mode . org-sticky-header-mode)
  :config
  (setq-default
   org-sticky-header-full-path 'full
   ;; Child and parent headings are seperated by a /.
   org-sticky-header-outline-path-separator " ▷ "))

  (quelpa '(org-remoteimg :fetcher github :repo "gaoDean/org-remoteimg"))
  (require 'org-remoteimg)
  (setq url-cache-directory "~/emacs.d/.cache/")
  (setq org-display-remote-inline-images 'cache)

(use-package bufler
  :config (bind-key "C-x C-b" #'bufler-list))
;; I still prefer “C-x b” to be “helm-mini”, since when looking for a buffer it also shows me recently visited files.

(use-package all-the-icons
    :config (all-the-icons-install-fonts 'install-without-asking))

;; [[file:init.org::#ELisp][ELisp:1]]
;; Evaluation Result OverlayS for Emacs Lisp
(use-package eros :init (eros-mode t))
;; ELisp:1 ends here

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

;; [[file:init.org::#e-Edit-Everything-in-a-separate-buffer][⌘-e: Edit Everything in a separate buffer:1]]
(use-package separedit)
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

;; [[file:init.org::#e-Edit-Everything-in-a-separate-buffer][⌘-e: Edit Everything in a separate buffer:2]]
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

;; [[file:init.org::#e-Edit-Everything-in-a-separate-buffer][⌘-e: Edit Everything in a separate buffer:3]]
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

;; [[file:init.org::#e-Edit-Everything-in-a-separate-buffer][⌘-e: Edit Everything in a separate buffer:4]]
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

;; [[file:init.org::#e-Edit-Everything-in-a-separate-buffer][⌘-e: Edit Everything in a separate buffer:5]]
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

;; [[file:init.org::#e-Edit-Everything-in-a-separate-buffer][⌘-e: Edit Everything in a separate buffer:6]]
(use-package language-detection)
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

;; [[file:init.org::#e-Edit-Everything-in-a-separate-buffer][⌘-e: Edit Everything in a separate buffer:7]]
(advice-add #'org-edit-special :before-until
            (lambda (&rest r)
              (when (equal 'table-row (car (org-element-at-point)))
                (call-interactively #'org-table-edit-field))))
;; ⌘-e: Edit Everything in a separate buffer:7 ends here

;; [[file:init.org::#Sleek-Semantic-Selection][⌘-r, ⌘-i, ⌘-o: Sleek Semantic Selection:1]]
(use-package expand-region

  :bind (("s-r" . #'er/expand-region)))
;; ⌘-r, ⌘-i, ⌘-o: Sleek Semantic Selection:1 ends here

;; [[file:init.org::#Editor-Documentation-with-Contextual-Information][Editor Documentation with Contextual Information:1]]
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
    (cond
     (current-prefix-arg (funcall #'describe-symbol it))
     ((or (functionp it) (macrop it) (commandp it)) (helpful-callable it))
     (t (helpful-symbol it)))))

;; Keybindings.
(global-set-key (kbd "C-h o") #'my/describe-symbol)
(global-set-key (kbd "C-h k") #'helpful-key)
;; Editor Documentation with Contextual Information:1 ends here

;; [[file:init.org::#Let's-make-working-with-Emacs-Lisp-even-better][[[https://github.com/xuchunyang/elisp-demos][Append existing ELisp docstrings with example use and actual output.]]:1]]
(use-package elisp-demos
  :config
  ;; Show demos when I do a `C-h o'.
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
  ;; Show demos in tooltips when I pause to select a completion, in Emacs Lisp mode.
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1))
;; [[https://github.com/xuchunyang/elisp-demos][Append existing ELisp docstrings with example use and actual output.]]:1 ends here

;; [[file:init.org::#Eldoc-for-Lisp-and-Haskell][Elisp live documentation in the mini-buffer:1]]
(use-package eldoc
  :hook (emacs-lisp-mode . turn-on-eldoc-mode))

;; Slightly shorten eldoc display delay.
(setq eldoc-idle-delay 0.4) ;; Default 0.5
;; Elisp live documentation in the mini-buffer:1 ends here

;; [[file:init.org::#Jumping-to-definitions-references][Jumping to definitions & references:1]]
(use-package dumb-jump
  :bind (("M-g q"     . dumb-jump-quick-look) ;; Show me in a tooltip.
         ("M-g ."     . dumb-jump-go-other-window)
         ("M-g b"     . dumb-jump-back)
         ("M-g p"     . dumb-jump-go-prompt)
         ("M-g a"     . xref-find-apropos)) ;; aka C-M-.
  :config
  ;; If source file is visible, just shift focus to it.
  (setq dumb-jump-use-visible-window t))
;; Jumping to definitions & references:1 ends here

;; [[file:init.org::#Commenting][Commenting:1]]
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

 ;; Not ideal: M-; comments a parent Org heading and not the current line.
 ;; (define-key org-mode-map (kbd "M-;") 'org-comment-dwim-2)
;; Commenting:1 ends here

;; [[file:init.org::#Emphasised-Comments][Emphasised Comments: Useful for warnings:1]]
;; In VSCode, with the “Better Comments” extension, comments starting with a “bang” are made to stand out, via bold red.
;; Let's do the same thing in Emacs.
;; I did not look around, there might be a package/option for this 🤷
(add-hook 'prog-mode-hook
          (defun emphasize-comments-starting-with-! ()
            (highlight-lines-matching-regexp ".*\\*.*!.*" 'hi-red-b)
            (highlight-lines-matching-regexp ".*//!.*" 'hi-red-b)
            (highlight-lines-matching-regexp ";;!.*" 'hi-red-b))) ;;! Look it works (｡◕‿◕｡)
;; Emphasised Comments: Useful for warnings:1 ends here

;; [[file:init.org::#Comment-boxes-up-to-the-fill-column][Comment-boxes up to the fill-column ---or banner instead?:1]]
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

;; [[file:init.org::#Comment-boxes-up-to-the-fill-column][Comment-boxes up to the fill-column ---or banner instead?:2]]
(use-package banner-comment)
;; Comment-boxes up to the fill-column ---or banner instead?:2 ends here

;; [[file:init.org::#Text-Folding][Text Folding ---Selectively displaying portions of a program:1]]
(require 'cl-lib)

(defun my/disable-hs-hide-all (orig-fun &rest args)
  "Advise `org-export-dispatch` to disable `hs-hide-all` temporarily."
  ;; Without this, export hangs “Hiding all blocks...”
  (cl-letf (((symbol-function 'hs-hide-all) (lambda (&rest _) nil)))
    ;; Without this, export shows “*hideshowvis*” markers in my exported code blocks.
    (cl-letf (((symbol-function 'hideshowvis-highlight-hs-regions-in-fringe) (lambda (&rest _) nil)))
      (apply orig-fun args))))

(advice-add 'org-export-dispatch :around #'my/disable-hs-hide-all)
;; Text Folding ---Selectively displaying portions of a program:1 ends here

;; [[file:init.org::#Text-Folding][Text Folding ---Selectively displaying portions of a program:2]]
(use-package vimish-fold

  :config (vimish-fold-global-mode 1))
;; Text Folding ---Selectively displaying portions of a program:2 ends here

;; [[file:init.org::#Actual-Setup][Actual Setup:1]]
(use-package hideshow
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

;; [[file:init.org::#Actual-Setup][Actual Setup:2]]
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

;; [[file:init.org::#Actual-Setup][Actual Setup:3]]
(defvar my/hs-hide nil "Current state of hideshow for toggling all.")
(defun my/hs-toggle-buffer () "Toggle hideshow all."
       (interactive)
       (setq my/hs-hide (not my/hs-hide))
       (if my/hs-hide
           (hs-hide-all)
         (hs-show-all)))
;; Actual Setup:3 ends here

;; [[file:init.org::#Actual-Setup][Actual Setup:4]]
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

;; [[file:init.org::#Actual-Setup][Actual Setup:5]]
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

;; [[file:init.org::#Actual-Setup][Actual Setup:6]]
;; Open folded nodes if a search stops there.
(add-hook 'helm-swoop-after-goto-line-action-hook #'my/search-hook-function)
(defun my/search-hook-function ()
  (when hs-minor-mode (set-mark-command nil) (hs-show-block) (pop-to-mark-command)))
;; Actual Setup:6 ends here

;; [[file:init.org::#Aggressive-Indentation][Aggressive Indentation:1]]
;; Always stay indented: Automatically have blocks reindented after every change.
(use-package aggressive-indent :config (global-aggressive-indent-mode t))

;; Use 4 spaces in places of tabs when indenting.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; Aggressive Indentation:1 ends here

;; [[file:init.org::#Indentation-Guide][Indentation Guide:1]]
;; Add a visual indent guide
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-character ?|)
  (highlight-indent-guides-responsive 'stack))


(load-file "~/.emacs.d/elpa/highlight-indent-guides-20200820.2328/highlight-indent-guides.el")
;; Indentation Guide:1 ends here

;; [[file:init.org::#Which-function-are-we-writing][Which function are we writing?:1]]
(add-hook 'prog-mode-hook #'which-function-mode)
(add-hook 'org-mode-hook  #'which-function-mode)
;; Which function are we writing?:1 ends here

;; [[file:init.org::#Which-function-are-we-writing][Which function are we writing?:2]]
(add-hook 'emacs-lisp-mode-hook #'check-parens)
;; Which function are we writing?:2 ends here

;; [[file:init.org::#Coding-with-a-Fruit-Salad-Semantic-Highlighting][Coding with a Fruit Salad: Semantic Highlighting:1]]
(use-package color-identifiers-mode

  :config (global-color-identifiers-mode))

;; Sometimes just invoke: M-x color-identifiers:refresh
;; Coding with a Fruit Salad: Semantic Highlighting:1 ends here

;; [[file:init.org::#highlight-quoted-symbols][Highlight Quoted Symbols:1]]
(use-package highlight-quoted
  :config (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode))

;; If everything worked fine, then “ 'b ” below should be coloured nicely in Emacs Lisp mode.
(when nil
  (-let [x 'somevar]
    (list x 'b "c" :e)))
;; Highlight Quoted Symbols:1 ends here

;; [[file:init.org::#Syntax-highlighting-numbers-and-escape-characters][Highlighting Numbers and Escape Characters:1]]
(use-package highlight-numbers
  :hook (prog-mode separedit-double-quote-string-mode)) ;; The latter is for when I do ⌘-e on a quoted string to edit it.

(use-package highlight-escape-sequences
  :hook ((prog-mode . hes-mode)
         (separedit-double-quote-string-mode . hes-mode)) ;; Wont work since this mode has no font-lock-builtin-face
  :config
  ;; Colour the escapes as if they were builtin keywords.
  (put 'hes-escape-backslash-face 'face-alias 'font-lock-builtin-face)
  (put 'hes-escape-sequence-face 'face-alias 'font-lock-builtin-face))


;; TODO: My Emacs seems to have trouble loading the following and so I'm doint it manually.
(load-file "~/.emacs.d/elpa/company-posframe-20230104.1229/company-posframe.el")
(load-file "~/.emacs.d/elpa/highlight-escape-sequences-20201214.1730/highlight-escape-sequences.el")
(load-file "~/.emacs.d/elpa/parent-mode-20240210.1906/parent-mode.el")
(load-file "~/.emacs.d/elpa/highlight-numbers-20181013.1744/highlight-numbers.el")


;; If the above two worked fine, then you should see \n and 3 highlighted below
(when nil "Look: 1 and \\ and \n 2" (setq three 3))
;; Highlighting Numbers and Escape Characters:1 ends here

;; [[file:init.org::#Highlight-defined-Lisp-symbols][Highlight /defined/ Lisp symbols:1]]
;; Emacs Lisp specific
(use-package highlight-defined :hook emacs-lisp-mode)
(load-file "~/.emacs.d/elpa/highlight-defined-20210411.222/highlight-defined.el")
;; Highlight /defined/ Lisp symbols:1 ends here

;; [[file:init.org::#Get-nice-child-frames-when-looking-at-completions-candidates-In-particular-when-editing-Lisp-code-the][Get nice child frames when looking at completions candidates:1]]
(use-package company-posframe :hook prog-mode)

;; I want to see the doc pop-ups nearly instantaneously 😅
(setq company-posframe-quickhelp-delay 0)
;; Get nice child frames when looking at completions candidates:1 ends here

;; [[file:init.org::*interactive macro-expander][interactive macro-expander:1]]
(use-package macrostep)
(define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand)
;; interactive macro-expander:1 ends here

;; [[file:init.org::*Smart jumping to definitions][Smart jumping to definitions:1]]
(use-package elisp-def)
(bind-key*  "M-." #'elisp-def emacs-lisp-mode-map)

;; Example usage:
(when nil
  (let ((foo 1))
    (setq foo 2))) ;; “M-.” on this “foo” will now take us to the start of the let-clause.
;; Smart jumping to definitions:1 ends here

;; [[file:init.org::*Bidirectional Text][Bidirectional Text:1]]
;; Sometimes I have Arabic in my buffers, however I am an English speaker
;; and so Left-to-Right is most natural to me. As such, even when Arabic
;; is present, or any bidirectional text, just use Left-to-Right.
(setq-default bidi-paragraph-direction 'left-to-right)
;; Bidirectional Text:1 ends here

;; [[file:init.org::#Whitespace][Whitespace:1]]
(add-hook 'before-save-hook 'whitespace-cleanup)
;; Whitespace:1 ends here

;; [[file:init.org::#Formatting-Text][Formatting Text:1]]
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

;; [[file:init.org::#Fill-mode-Word-Wrapping][Fill-mode ---Word Wrapping:1]]
(setq-default fill-column 80          ;; Let's avoid going over 80 columns
              truncate-lines nil      ;; I never want to scroll horizontally
              indent-tabs-mode nil)   ;; Use spaces instead of tabs
;; Fill-mode ---Word Wrapping:1 ends here

;; [[file:init.org::#Fill-mode-Word-Wrapping][Fill-mode ---Word Wrapping:2]]
;; Wrap long lines when editing text
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
;; Fill-mode ---Word Wrapping:2 ends here

;; [[file:init.org::#Fill-mode-Word-Wrapping][Fill-mode ---Word Wrapping:3]]
;; Bent arrows at the end and start of long lines.
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(global-visual-line-mode 1)
;; Fill-mode ---Word Wrapping:3 ends here

;; [[file:init.org::#Pretty-Lists-Markers][Pretty Lists Markers:1]]
;; (x y z) ≈ (existing-item replacement-item positivity-of-preceding-spaces)
(cl-loop for (x y z) in '(("+" "◦" *)
                       ("-" "•" *)
                       ("*" "⋆" +))
      do (font-lock-add-keywords 'org-mode
                                 `((,(format "^ %s\\([%s]\\) " z x)
                                    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) ,y)))))))
;; Pretty Lists Markers:1 ends here

;; [[file:init.org::#Fix-spelling-as-you-type-thesaurus-dictionary-too][Fix spelling as you type ---thesaurus & dictionary too!:1]]
(system-packages-ensure "aspell")
(system-packages-ensure "wordnet")
;; Fix spelling as you type ---thesaurus & dictionary too!:1 ends here

;; [[file:init.org::#Fix-spelling-as-you-type-thesaurus-dictionary-too][Fix spelling as you type ---thesaurus & dictionary too!:2]]
(use-package flyspell

  :hook ((prog-mode . flyspell-prog-mode)
         ((org-mode text-mode) . flyspell-mode)))
;; Fix spelling as you type ---thesaurus & dictionary too!:2 ends here

;; [[file:init.org::#Fix-spelling-as-you-type-thesaurus-dictionary-too][Fix spelling as you type ---thesaurus & dictionary too!:3]]
(setq ispell-program-name (s-trim (shell-command-to-string "which aspell")))
(setq ispell-dictionary "en_GB") ;; set the default dictionary
;; Fix spelling as you type ---thesaurus & dictionary too!:3 ends here

;; [[file:init.org::#Fix-spelling-as-you-type-thesaurus-dictionary-too][Fix spelling as you type ---thesaurus & dictionary too!:5]]
(eval-after-load "flyspell"
  ' (progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))
;; Fix spelling as you type ---thesaurus & dictionary too!:5 ends here

;; [[file:init.org::#Fix-spelling-as-you-type-thesaurus-dictionary-too][Fix spelling as you type ---thesaurus & dictionary too!:6]]
(global-font-lock-mode t)
(custom-set-faces '(flyspell-incorrect ((t (:inverse-video t)))))
;; Fix spelling as you type ---thesaurus & dictionary too!:6 ends here

;; [[file:init.org::#Fix-spelling-as-you-type-thesaurus-dictionary-too][Fix spelling as you type ---thesaurus & dictionary too!:7]]
(setq ispell-silently-savep t)
;; Fix spelling as you type ---thesaurus & dictionary too!:7 ends here

;; [[file:init.org::#Fix-spelling-as-you-type-thesaurus-dictionary-too][Fix spelling as you type ---thesaurus & dictionary too!:8]]
(setq ispell-personal-dictionary "~/.emacs.d/.aspell.en.pws")
;; Fix spelling as you type ---thesaurus & dictionary too!:8 ends here

;; [[file:init.org::#Fix-spelling-as-you-type-thesaurus-dictionary-too][Fix spelling as you type ---thesaurus & dictionary too!:9]]
(add-hook          'c-mode-hook 'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
;; Fix spelling as you type ---thesaurus & dictionary too!:9 ends here

;; [[file:init.org::#Fix-spelling-as-you-type-thesaurus-dictionary-too][Fix spelling as you type ---thesaurus & dictionary too!:10]]
(use-package synosaurus
  :defer 100
  :init    (synosaurus-mode)
  :config  (setq synosaurus-choose-method 'popup) ;; 'ido is default.
           (global-set-key (kbd "M-#") 'synosaurus-choose-and-replace))
;; Fix spelling as you type ---thesaurus & dictionary too!:10 ends here

;; [[file:init.org::#Fix-spelling-as-you-type-thesaurus-dictionary-too][Fix spelling as you type ---thesaurus & dictionary too!:11]]
;; (shell-command "brew cask install xquartz &") ;; Dependency
;; (shell-command "brew install wordnet &")
;; Fix spelling as you type ---thesaurus & dictionary too!:11 ends here

;; [[file:init.org::#Fix-spelling-as-you-type-thesaurus-dictionary-too][Fix spelling as you type ---thesaurus & dictionary too!:12]]
(use-package wordnut
 :defer 100
 :bind ("M-!" . wordnut-lookup-current-word))

;; Use M-& for async shell commands.
;; Fix spelling as you type ---thesaurus & dictionary too!:12 ends here

;; [[file:init.org::#Lightweight-Prose-Proofchecking][Lightweight Prose Proofchecking:1]]
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

;; [[file:init.org::#Placeholder-Text-For-Learning-Experimenting][Placeholder Text ---For Learning & Experimenting:1]]
(use-package lorem-ipsum )
;; Placeholder Text ---For Learning & Experimenting:1 ends here

;; [[file:init.org::#Some-text-to-make-us-smile][Some text to make us smile:1]]
(use-package dad-joke

  :config (defun dad-joke () (interactive) (insert (dad-joke-get))))
;; Some text to make us smile:1 ends here

;; [[file:init.org::#Unicode-Input-via-Agda-Input][Unicode Input via Agda Input:1]]
;; (load (shell-command-to-string "agda-mode locate"))
;;
;; Seeing: One way to avoid seeing this warning is to make sure that agda2-include-dirs is not bound.
; (makunbound 'agda2-include-dirs)
;; Unicode Input via Agda Input:1 ends here

;; [[file:init.org::#Unicode-Input-via-Agda-Input][Unicode Input via Agda Input:2]]
(system-packages-ensure "agda")
;; Unicode Input via Agda Input:2 ends here

;; [[file:init.org::#Unicode-Input-via-Agda-Input][Unicode Input via Agda Input:4]]
(unless noninteractive
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate"))))
;; Unicode Input via Agda Input:4 ends here

;; [[file:init.org::#Unicode-Input-via-Agda-Input][Unicode Input via Agda Input:5]]
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

;; [[file:init.org::#Unicode-Input-via-Agda-Input][Unicode Input via Agda Input:6]]
;;(setq agda2-program-args (quote ("RTS" "-M4G" "-H4G" "-A128M" "-RTS")))
;; Unicode Input via Agda Input:6 ends here

;; [[file:init.org::#Unicode-Input-via-Agda-Input][Unicode Input via Agda Input:7]]
(unless noninteractive (add-to-list 'agda-input-user-translations '("set" "𝒮ℯ𝓉")))
;; Unicode Input via Agda Input:7 ends here

;; [[file:init.org::#Unicode-Input-via-Agda-Input][Unicode Input via Agda Input:8]]
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

;; [[file:init.org::#Unicode-Input-via-Agda-Input][Unicode Input via Agda Input:9]]
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

;; [[file:init.org::#Unicode-Input-via-Agda-Input][Unicode Input via Agda Input:10]]
;; activate translations
(unless noninteractive (agda-input-setup))
;; Unicode Input via Agda Input:10 ends here

;; [[file:init.org::#Increase-decrease-text-size][Increase/decrease text size:1]]
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
;; C-x C-0 restores the default font size
;; Increase/decrease text size:1 ends here

;; [[file:init.org::#Moving-Text-Around][Moving Text Around:1]]
;; M-↑,↓ moves line, or marked region; prefix is how many lines.
(use-package move-text

  :config (move-text-default-bindings))
;; Moving Text Around:1 ends here

;; [[file:init.org::#Enabling-CamelCase-Aware-Editing-Operations][Enabling CamelCase Aware Editing Operations:1]]
(global-subword-mode 1)
;; Enabling CamelCase Aware Editing Operations:1 ends here

;; [[file:init.org::#Delete-Selection-Mode][Delete Selection Mode:1]]
(delete-selection-mode 1)
;; Delete Selection Mode:1 ends here

;; [[file:init.org::#visual-regexp][visual-regexp:1]]
;; While constructing the regexp in the minibuffer, get live visual feedback for the (group) matches.
;; E.g., try: M-% use-\(.+?\) \(.+\)\b ENTER woah \1 and \2
;;
;; C-u M-%  do to regexp replace, without querying.
(use-package visual-regexp

  :config (define-key global-map (kbd "M-%")
            (lambda (&optional prefix) (interactive "P") (call-interactively (if prefix  #'vr/replace #'vr/query-replace)))))
;; visual-regexp:1 ends here

;; [[file:init.org::#HTML-Org-mode][HTML ⇐ Org-mode:1]]
(use-package htmlize )
;; Main use: Org produced htmls are coloured.
;; Can be used to export a file into a coloured html.
;; HTML ⇐ Org-mode:1 ends here

;; [[file:init.org::#HTML-Folded-Drawers][HTML “Folded Drawers”:1]]
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

;; [[file:init.org::#https-revealjs-com-transition-zoom-Reveal-JS-The-HTML-Presentation-Framework][  [[https://revealjs.com/?transition=zoom#/][Reveal.JS]] -- The HTML Presentation Framework:1]]
(use-package ox-reveal

  :custom (org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))
;;   [[https://revealjs.com/?transition=zoom#/][Reveal.JS]] -- The HTML Presentation Framework:1 ends here

;; [[file:init.org::#https-revealjs-com-transition-zoom-Reveal-JS-The-HTML-Presentation-Framework][  [[https://revealjs.com/?transition=zoom#/][Reveal.JS]] -- The HTML Presentation Framework:3]]
(setq org-reveal-title-slide "<h1>%t</h1> <h3>%a</h3>
<font size=\"1\">
<a href=\"?print-pdf&showNotes=true\">
⟪ Flattened View ; Press <code>?</code> for Help ⟫
</a>
</font>")
;;   [[https://revealjs.com/?transition=zoom#/][Reveal.JS]] -- The HTML Presentation Framework:3 ends here

;; [[file:init.org::#Org-mode-HTML][C-c C-l Org-mode ⇐ HTML:2]]
(use-package org-web-tools

  :config
  ;; Insert an Org-mode link to the URL in the clipboard or kill-ring. Downloads
  ;; the page to get the HTML title.
  ;; (bind-key* "C-c C-l" #'org-web-tools-insert-link-for-url) ;; Instead, see my/org-insert-link-dwim below.
  )
;; C-c C-l Org-mode ⇐ HTML:2 ends here

;; [[file:init.org::#Org-mode-HTML][C-c C-l Org-mode ⇐ HTML:3]]
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

;; [[file:init.org::*Done!][Done!:1]]
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(message-box "Enjoy life (｡◕‿◕｡))")
;; Done!:1 ends here
