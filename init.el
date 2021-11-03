;; [[file:init.org::#title][title:1]]
(require 'cl) ;; to get loop instead of cl-loop, etc.

;; Required for Github Actions; i.e., testing.
;; TODO Clean me!
(defun quelpa-read-cache ()) ;; Used somewhere, but not defined.
;; See: quelpa-persistent-cache-file
(setq quelpa-cache nil)
(defun org-special-block-extras-short-names ())


;; before this: init time: 13
;; after: 12 seconds.
; (setq gc-cons-threshold 50000000) ;; orginaly 800,000
;; reduce number of times GC occurs.
;; title:1 ends here

;; [[file:init.org::#emacs-vs-init-org][  =~/.emacs= vs. =init.org=:4]]
(setq custom-file "~/.emacs.d/custom.el")
(ignore-errors (load custom-file)) ;; It may not yet exist.
;;   =~/.emacs= vs. =init.org=:4 ends here

;; [[file:init.org::#Who-am-I][Who am I?:1]]
(setq user-full-name    "Musa Al-hassy"
      user-mail-address "alhassy@gmail.com")
;; Who am I?:1 ends here

;; [[file:init.org::#Emacs-Package-Manager][Emacs Package Manager:1]]
;; Make all commands of the “package” module present.
(require 'package)

;; Internet repositories for new packages.
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.org/packages/")))

;; Update local list of available packages:
;; Get descriptions of all configured ELPA packages,
;; and make them available for download.
(package-refresh-contents)
;; Emacs Package Manager:1 ends here

;; [[file:init.org::#Emacs-Package-Manager][Emacs Package Manager:2]]
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
;; Emacs Package Manager:2 ends here

;; [[file:init.org::#Emacs-Package-Manager][Emacs Package Manager:3]]
(setq use-package-always-ensure t)
;; Emacs Package Manager:3 ends here

;; [[file:init.org::#Emacs-Package-Manager][Emacs Package Manager:4]]
(use-package auto-package-update
  :config
  ;; Delete residual old versions
  (setq auto-package-update-delete-old-versions t)
  ;; Do not bother me when updates have taken place.
  (setq auto-package-update-hide-results t)
  ;; Update installed packages at startup if there is an update pending.
  (auto-package-update-maybe))
;; Emacs Package Manager:4 ends here

;; [[file:init.org::#Emacs-Package-Manager][Emacs Package Manager:5]]
;; Making it easier to discover Emacs key presses.
(use-package which-key
  :diminish
  :config (which-key-mode)
          (which-key-setup-side-window-bottom)
          (setq which-key-idle-delay 0.05))
;; Emacs Package Manager:5 ends here

;; [[file:init.org::#Emacs-Package-Manager][Emacs Package Manager:6]]
(use-package diminish)
;; Emacs Package Manager:6 ends here

;; [[file:init.org::#Emacs-Package-Manager][Emacs Package Manager:7]]
;; Haskell's cool
(use-package haskell-mode :defer t)

;; Lisp libraries with Haskell-like naming.
(use-package dash)    ;; “A modern list library for Emacs”
(use-package s   )    ;; “The long lost Emacs string manipulation library”.

;; Let's use the “s” library.
(defvar my/personal-machine?
  (not (s-contains? "-MBP" (shell-command-to-string "uname -a")))
  "Is this my personal machine, or my work machine?")

(ignore-errors (load-file "~/Desktop/work_secrets.el"))

;; Library for working with system files;
;; e.g., f-delete, f-mkdir, f-move, f-exists?, f-hidden?
(use-package f)
;; Emacs Package Manager:7 ends here

;; [[file:init.org::#Emacs-Package-Manager][Emacs Package Manager:8]]
  ;; Allow tree-semantics for undo operations.
  (use-package undo-tree
    :diminish                       ;; Don't show an icon in the modeline
    :bind ("C-x u" . undo-tree-visualize)
    :hook (org-mode . undo-tree-mode) ;; For some reason, I need this. FIXME.
    :config
      ;; Always have it on
      (global-undo-tree-mode)
  
      ;; Each node in the undo tree should have a timestamp.
      (setq undo-tree-visualizer-timestamps t)
  
      ;; Show a diff window displaying changes between undo nodes.
      (setq undo-tree-visualizer-diff t))
  
  ;; Execute (undo-tree-visualize) then navigate along the tree to witness
  ;; changes being made to your file live!
;; Emacs Package Manager:8 ends here

;; [[file:init.org::#Emacs-Package-Manager][Emacs Package Manager:10]]
(use-package quelpa
  :defer 5
  :custom (quelpa-upgrade-p t "Always try to update packages")
  :config
  ;; Get ‘quelpa-use-package’ via ‘quelpa’
  (quelpa
   '(quelpa-use-package
     :fetcher git
     :url "https://github.com/quelpa/quelpa-use-package.git"))
  (require 'quelpa-use-package))
;; Emacs Package Manager:10 ends here

;; [[file:init.org::#Installing-OS-packages-and-automatically-keeping-my-system-up-to-data-from-within-Emacs][Installing OS packages, and automatically keeping my system up to data, from within Emacs:1]]
;; Auto installing OS system packages
(use-package use-package-ensure-system-package
  :defer 5
  :config (system-packages-update))

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
;; Installing OS packages, and automatically keeping my system up to data, from within Emacs:1 ends here

;; [[file:init.org::#Installing-OS-packages-and-automatically-keeping-my-system-up-to-data-from-within-Emacs][Installing OS packages, and automatically keeping my system up to data, from within Emacs:3]]
;; An Emacs-based interface to the package manager of your operating system.
(use-package helm-system-packages :defer t)
;; Installing OS packages, and automatically keeping my system up to data, from within Emacs:3 ends here

;; [[file:init.org::#Installing-OS-packages-and-automatically-keeping-my-system-up-to-data-from-within-Emacs][Installing OS packages, and automatically keeping my system up to data, from within Emacs:4]]
(setq system-packages-noconfirm :do-not-prompt-me-about-confirms)

;; After 1 minute after startup, kill all buffers created by ensuring system
;; packages are present.
(run-with-timer 60 nil
 (lambda () (kill-matching-buffers ".*system-packages.*" t :kill-without-confirmation)))
;; Installing OS packages, and automatically keeping my system up to data, from within Emacs:4 ends here

;; [[file:init.org::#Installing-OS-packages-and-automatically-keeping-my-system-up-to-data-from-within-Emacs][Installing OS packages, and automatically keeping my system up to data, from within Emacs:5]]
;; Unlike the Helm variant, we need to specify our OS pacman.
(when (eq system-type 'darwin)
  (setq system-packages-package-manager 'brew))

;; If the given system package doesn't exist; install it.
(when (eq system-type 'darwin)
  (system-packages-ensure "amethyst")) ;; This is a MacOS specific package.

(ignore-errors (system-packages-ensure "google-chrome")) ;; My choice of web browser
(system-packages-ensure "microsoft-teams") ;; For remote work meetings
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
;; Installing OS packages, and automatically keeping my system up to data, from within Emacs:5 ends here

;; [[file:init.org::#Installing-OS-packages-and-automatically-keeping-my-system-up-to-data-from-within-Emacs][Installing OS packages, and automatically keeping my system up to data, from within Emacs:6]]
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
;; Installing OS packages, and automatically keeping my system up to data, from within Emacs:6 ends here

;; [[file:init.org::#Installing-OS-packages-and-automatically-keeping-my-system-up-to-data-from-within-Emacs][Installing OS packages, and automatically keeping my system up to data, from within Emacs:7]]
;; (bind-key "???-a c" #'amethyst/cycle-layout)
(defun amethyst/cycle-layout ()
  (interactive)
  (shell-command "osascript -e 'tell application \"System Events\" to keystroke space using {shift down, option down}'"))
;; Installing OS packages, and automatically keeping my system up to data, from within Emacs:7 ends here

;; [[file:init.org::#Syncing-to-the-System's-PATH][Syncing to the System's =$PATH=:1]]
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))
;; Syncing to the System's =$PATH=:1 ends here

;; [[file:init.org::#Restarting-Emacs-Keeping-buffers-open-across-sessions][Restarting Emacs ---Keeping buffers open across sessions?:1]]
;; Provides only the command “restart-emacs”.
(use-package restart-emacs
  ;; If I ever close Emacs, it's likely because I want to restart it.
  :bind ("C-x C-c" . restart-emacs)
  ;; Let's define an alias so there's no need to remember the order.
  :config (defalias 'emacs-restart #'restart-emacs))
;; Restarting Emacs ---Keeping buffers open across sessions?:1 ends here

;; [[file:init.org::#Restarting-Emacs-Keeping-buffers-open-across-sessions][Restarting Emacs ---Keeping buffers open across sessions?:2]]
(setq-default save-place  t)
(setq save-place-file "~/.emacs.d/etc/saveplace")
;; Restarting Emacs ---Keeping buffers open across sessions?:2 ends here

;; [[file:init.org::#Being-at-the-Helm-Completion-Narrowing-Framework][ “Being at the Helm” ---Completion & Narrowing Framework:1]]
(use-package helm
 :diminish
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

        :map helm-map
        ;; We can list ‘actions’ on the currently selected item by C-z.
        ("C-z" . helm-select-action)
        ;; Let's keep tab-completetion anyhow.
        ("TAB"   . helm-execute-persistent-action)
        ("<tab>" . helm-execute-persistent-action)))

;; Show me nice file icons when using, say, “C-x C-f” or “C-x b”
(use-package helm-icons
  :custom (helm-icons-provider 'all-the-icons)
  :config (helm-icons-enable))
;;  “Being at the Helm” ---Completion & Narrowing Framework:1 ends here

;; [[file:init.org::#Being-at-the-Helm-Completion-Narrowing-Framework][ “Being at the Helm” ---Completion & Narrowing Framework:2]]
(setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf
                                    helm-source-bookmarks
                                    helm-source-bookmark-set
                                    helm-source-buffer-not-found))
;;  “Being at the Helm” ---Completion & Narrowing Framework:2 ends here

;; [[file:init.org::#Being-at-the-Helm-Completion-Narrowing-Framework][ “Being at the Helm” ---Completion & Narrowing Framework:3]]
(system-packages-ensure "surfraw")
; ⇒  “M-x helm-surfraw” or “C-x c s”
;;  “Being at the Helm” ---Completion & Narrowing Framework:3 ends here

;; [[file:init.org::#Being-at-the-Helm-Completion-Narrowing-Framework][ “Being at the Helm” ---Completion & Narrowing Framework:4]]
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
;;  “Being at the Helm” ---Completion & Narrowing Framework:4 ends here

;; [[file:init.org::#Being-at-the-Helm-Completion-Narrowing-Framework][ “Being at the Helm” ---Completion & Narrowing Framework:7]]
(system-packages-ensure "ag")
;;  “Being at the Helm” ---Completion & Narrowing Framework:7 ends here

;; [[file:init.org::#Org-Mode-Administrivia][Org-Mode Administrivia:2]]
  (use-package org
    :ensure org-plus-contrib
    :diminish org-indent-mode
    :config (require 'ox-extra)
            (ox-extras-activate '(ignore-headlines)))
;; Org-Mode Administrivia:2 ends here

;; [[file:init.org::#Org-Mode-Administrivia][Org-Mode Administrivia:3]]
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
;; Org-Mode Administrivia:3 ends here

;; [[file:init.org::#Org-Mode-Administrivia][Org-Mode Administrivia:4]]
(setq initial-major-mode 'org-mode)
;; Org-Mode Administrivia:4 ends here

;; [[file:init.org::#Org-Mode-Administrivia][Org-Mode Administrivia:5]]
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
;; Org-Mode Administrivia:5 ends here

;; [[file:init.org::#Password-locking-files-encryption][Password-locking files  ---“encryption”:1]]
(system-packages-ensure "gnupg") ;; i.e.,  brew install gnupg

;; “epa” ≈ EasyPG Assistant

;; Need the following in init to have gpg working fine:
;; force Emacs to use its own internal password prompt instead of an external pin entry program.
(setq epa-pinentry-mode 'loopback)

;; https://emacs.stackexchange.com/questions/12212/how-to-type-the-password-of-a-gpg-file-only-when-opening-it
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
;; No more needing to enter passphrase at each save ^_^
;;
;; Caches passphrase for the current emacs session?
;; Password-locking files  ---“encryption”:1 ends here

;; [[file:init.org::#Hydra-Supply-a-prefix-only-once][Hydra: Supply a prefix only once:1]]
;; Invoke all possible key extensions having a common prefix by
;; supplying the prefix only once.
(use-package hydra)
;; Hydra: Supply a prefix only once:1 ends here

;; [[file:init.org::#Hydra-Supply-a-prefix-only-once][Hydra: Supply a prefix only once:2]]
;; TODO Fix me, breaking Github Actions test setup
;; Show hydras overlyaed in the middle of the frame
;; (use-package hydra-posframe
;;   :quelpa (hydra-posframe :fetcher git :url
;;                           "https://github.com/Ladicle/hydra-posframe.git")
;;   :hook (after-init . hydra-posframe-mode)
;;   :custom (hydra-posframe-border-width 5))

;; Neato doc strings for hydras
(use-package pretty-hydra)
;; Hydra: Supply a prefix only once:2 ends here

;; [[file:init.org::#Hydra-Supply-a-prefix-only-once][Hydra: Supply a prefix only once:3]]
(defmacro my/pretty-defhydra (key title &rest body)
"Make a hydra whose heads appear in a pretty pop-up window.

KEY: Global keybinding for the new hydra.

TITLE: Either a string or a plist, as specified for pretty-hydra-define.
       The underlying Lisp function's name is derived from the TITLE;
       which is intentional since hydra's are for interactive, pretty, use.

       One uses a plist TITLE to specify what a hydra should do *before*
       any options, or to specify an alternate quit key (:q by default).

BODY: A list of columns and entries. Keywords indicate the title
      of a column; 3-lists (triples) indicate an entry key and
      the associated operation to perform and, optionally, a name
      to be shown in the pop-up. See DEFHYDRA for more details.

For instance, the verbose mess:

    ;; Use ijkl to denote ↑←↓→ arrows.
    (global-set-key
     (kbd \"C-c w\")
     (pretty-hydra-define my/hydra/\\t\\tWindow\\ Adjustment
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
    (my/pretty-defhydra \"C-c w\" \"\t\tWindow Adjustment\"
       :Both
       (\"b\" balance-windows                 \"balance\")
       (\"s\" switch-window-then-swap-buffer  \"swap\")
       :Vertical_adjustment
       (\"h\" enlarge-window                  \"heighten\")
       (\"l\" shrink-window                   \"lower\")
       :Horizontal_adjustment
       (\"n\" shrink-window-horizontally      \"narrow\")
       (\"w\" enlarge-window-horizontally     \"widen\"))"
  (let ((name (intern (concat "my/hydra/"
                              (if (stringp title)
                                  title
                                (plist-get title :title))))))
    `(global-set-key
      (kbd ,key)
      (pretty-hydra-define ,name
        ,(if (stringp title)
             (list :title title :quit-key "q")
           title)
        ,(thread-last body
           (-partition-by-header #'keywordp)
           (--map (cons (s-replace "_" " " (s-chop-prefix ":" (symbol-name (car it)))) (list (cdr it))))
           (-flatten-n 1))))))
;; Hydra: Supply a prefix only once:3 ends here

;; [[file:init.org::#Textual-Navigation-Look-Ma-no-CTRL-key][Textual Navigation ---“Look Ma, no CTRL key!”:1]]
(my/pretty-defhydra "C-n" "\t\t\t\t\tTextual Navigation"
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
;; Textual Navigation ---“Look Ma, no CTRL key!”:1 ends here

;; [[file:init.org::#Textual-Navigation-Look-Ma-no-CTRL-key][Textual Navigation ---“Look Ma, no CTRL key!”:2]]
;; C-n, next line, inserts newlines when at the end of the buffer
(setq next-line-add-newlines t)
;; Textual Navigation ---“Look Ma, no CTRL key!”:2 ends here

;; [[file:init.org::#Window-Navigation][Window Navigation:1]]
;; Use ijkl to denote ↑←↓→ arrows.
(my/pretty-defhydra "C-c w" "\t\tWindow Adjustment"
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
;; (use-package switch-window :defer t)
;; :bind (("C-x o" . switch-window)
;;        ("C-x w" . switch-window-then-swap-buffer))

;; Have a thick ruler between vertical windows
(window-divider-mode)
;; Window Navigation:1 ends here

;; [[file:init.org::#Staying-Safe][Staying Sane:1]]
(system-packages-ensure "dropbox")
(system-packages-ensure "megasync")
;; Staying Sane:1 ends here

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

;; [[file:init.org::#What-changed][What changed?:1]]
(use-package backup-walker
  :commands backup-walker-start)
;; What changed?:1 ends here

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

;; [[file:init.org::#magit-Emacs'-porcelain-interface-to-gitq][  =magit= ---Emacs' porcelain interface to git:1]]
;; Bottom of Emacs will show what branch you're on
;; and whether the local file is modified or not.
(use-package magit
  :bind (("C-c g" . magit-file-dispatch))
  :custom ;; Do not ask about this variable when cloning.
    (magit-clone-set-remote.pushDefault t))
;;   =magit= ---Emacs' porcelain interface to git:1 ends here

;; [[file:init.org::#Credentials-I-am-who-I-am][Credentials: I am who I am:1]]
;; See here for a short & useful tutorial:
;; https://alvinalexander.com/git/git-show-change-username-email-address
(when (equal "" (shell-command-to-string "git config user.email "))
  (shell-command (format "git config --global user.name \"%s\"" user-full-name))
  (shell-command (format "git config --global user.email \"%s\"" user-mail-address)))

;; Also need to customise email routes per organization
;; https://docs.github.com/en/github/managing-subscriptions-and-notifications-on-github/configuring-notifications#customizing-email-routes-per-organization
(unless my/personal-machine?
  (shell-command "git config --global user.email \"musa@weeverapps.com\""))

;; If we ever need to use Git in the terminal, it should be done with Emacs as
;; the underlying editor
(shell-command "git config --global core.editor emacs")
;; Credentials: I am who I am:1 ends here

;; [[file:init.org::#Encouraging-useful-commit-messages][Encouraging useful commit messages:1]]
(defun my/git-commit-reminder ()
  (insert "\n\n# The commit subject line ought to finish the phrase:
# “If applied, this commit will ⟪your subject line here⟫.” ")
  (beginning-of-buffer))

(add-hook 'git-commit-setup-hook 'my/git-commit-reminder)
;; Encouraging useful commit messages:1 ends here

;; [[file:init.org::#Maybe-clone-everything][Maybe clone ... everything?:1]]
(cl-defun maybe-clone (remote &optional (local (concat "~/" (file-name-base remote))))
  "Clone a REMOTE repository if the LOCAL directory does not exist.

Yields ‘repo-already-exists’ when no cloning transpires,
otherwise yields ‘cloned-repo’.

LOCAL is optional and defaults to the base name; e.g.,
if REMOTE is https://github.com/X/Y then LOCAL becomes ∼/Y."
  (defvar magit-repository-directories nil) ;; Define it, if it's not yet loaded
  (add-to-list 'magit-repository-directories `(,local . 0))
  (if (file-directory-p local)
      'repo-already-exists
    (async-shell-command (concat "git clone " remote " " local))
    'cloned-repo))

(maybe-clone "https://github.com/alhassy/emacs.d" "~/.emacs.d")
(maybe-clone "https://github.com/alhassy/alhassy.github.io" "~/blog")
(maybe-clone "https://github.com/alhassy/holy-books")
;; Maybe clone ... everything?:1 ends here

;; [[file:init.org::#Maybe-clone-everything][Maybe clone ... everything?:2]]
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

;; [[file:init.org::#Gotta-love-that-time-machine][Gotta love that time machine:1]]
(use-package git-timemachine :defer t)
;; Gotta love that time machine:1 ends here

;; [[file:init.org::#Version-Control-with-SVN-Using-Magit][Version Control with SVN ---Using Magit!:1]]
(use-package magit-svn
  :hook (magit-mode . magit-svn-mode))
;; Version Control with SVN ---Using Magit!:1 ends here

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

;; [[file:init.org::*Silently show me when a line was modified and by whom][Silently show me when a line was modified and by whom:1]]
(unless noninteractive

  (use-package blamer
    :quelpa (blamer :fetcher github :repo "artawower/blamer.el")
    :custom
    (blamer-idle-time 0.3)
    (blamer-min-offset 70)
    (blamer-max-commit-message-length 80) ;; Show me a lot of the commit title
    :custom-face
    (blamer-face ((t :foreground "#7a88cf"
                      :background nil
                      :height 140
                      :italic t)))
    :config
    (global-blamer-mode 1)))
;; Silently show me when a line was modified and by whom:1 ends here

;; [[file:init.org::#Manipulating-Sections][Manipulating Sections:1]]
(setq org-use-speed-commands t)
;; Manipulating Sections:1 ends here

;; [[file:init.org::#Manipulating-Sections][Manipulating Sections:2]]
;; [Default]
;; When refiling, only show me top level headings
(setq org-refile-targets
      '((nil :maxlevel . 1))) ;; Sometimes 2 is useful.

;; Maybe I want to refile into a new heading; confirm with me.
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Use full outline paths for refile targets
;; When refiling, using Helm, show me the hierarchy paths
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path 'file-path)
;; Manipulating Sections:2 ends here

;; [[file:init.org::#Manipulating-Sections][Manipulating Sections:3]]
(add-to-list 'org-speed-commands-user (cons "P" #'org-set-property))
;; Use ‘:’ and ‘e’ to set tags and effort, respectively.
;; Manipulating Sections:3 ends here

;; [[file:init.org::#Seamless-Navigation-Between-Source-Blocks][Seamless Navigation Between Source Blocks:1]]
;; Overriding keys for printing buffer, duplicating gui frame, and isearch-yank-kill.
;;
(use-package org
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
 (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell      . t)
     (python     . t)
     (haskell    . t)
     (ruby       . t)
     (ocaml      . t)
     (C          . t)  ;; Captial “C” gives access to C, C++, D
     (dot        . t)
     (latex      . t)
     (org        . t)
     (makefile   . t)))

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

;; [[file:init.org::#Quickly-pop-up-a-terminal-run-a-command-close-it-and-zsh][Quickly pop-up a terminal, run a command, close it ---and zsh:1]]
(use-package shell-pop
  :custom
    ;; This binding toggles popping up a shell, or moving cursour to the shell pop-up.
    (shell-pop-universal-key "C-t")

    ;; Percentage for shell-buffer window size.
    (shell-pop-window-size 30)

    ;; Position of the popped buffer: top, bottom, left, right, full.
    (shell-pop-window-position "bottom")

    ;; Please use an awesome shell.
    (shell-pop-term-shell "/bin/zsh"))
;; Quickly pop-up a terminal, run a command, close it ---and zsh:1 ends here

;; [[file:init.org::#Quickly-pop-up-a-terminal-run-a-command-close-it-and-zsh][Quickly pop-up a terminal, run a command, close it ---and zsh:2]]
;; Be default, Emacs please use zsh
;; E.g., M-x shell
(unless noninteractive (setq shell-file-name "/bin/zsh"))
;; Quickly pop-up a terminal, run a command, close it ---and zsh:2 ends here

;; [[file:init.org::#Quickly-pop-up-a-terminal-run-a-command-close-it-and-zsh][Quickly pop-up a terminal, run a command, close it ---and zsh:3]]
(system-packages-ensure "tldr")
;; Quickly pop-up a terminal, run a command, close it ---and zsh:3 ends here

;; [[file:init.org::#Jumping-to-extreme-semantic-units][Jumping to extreme semantic units:1]]
;; M-< and M-> jump to first and final semantic units.
;; If pressed twice, they go to physical first and last positions.
(use-package beginend
  :diminish 'beginend-global-mode
  :config (beginend-global-mode)
    (cl-loop for (_ . m) in beginend-modes do (diminish m)))
;; Jumping to extreme semantic units:1 ends here

;; [[file:init.org::#Capturing-ideas-notes-without-interrupting-the-current-workflow][Capturing ideas & notes without interrupting the current workflow:1]]
(cl-defun my/org-capture-buffer (&optional keys no-additional-remarks
                                           (heading-regexp "Subject: \\(.*\\)"))
  "Capture the current [narrowed] buffer as a todo/note.

This is mostly intended for capturing mail as todo tasks ^_^

When NO-ADDITIONAL-REMARKS is provided, and a heading is found,
then make and store the note without showing a pop-up.
This is useful for when we capture self-contained mail.

The HEADING-REGEXP must have a regexp parenthesis construction
which is used to obtain a suitable heading for the resulting todo/note."
  (interactive "P")
  (let* ((current-content (substring-no-properties (buffer-string)))
         (heading         (progn (string-match heading-regexp current-content)
                                 (or (match-string 1 current-content) ""))))
    (org-capture keys)
    (insert heading "\n\n\n\n" (s-repeat 80 "-") "\n\n\n" current-content)

    ;; The overtly verbose conditions are for the sake of clarity.
    ;; Moreover, even though the final could have “t”, being explicit
    ;; communicates exactly the necessary conditions.
    ;; Being so verbose leads to mutual exclusive clauses, whence order is irrelevant.
    (cond
     ((s-blank? heading)
        (beginning-of-buffer) (end-of-line))
     ((and no-additional-remarks (not (s-blank? heading)))
        (org-capture-finalize))
     ((not (or no-additional-remarks (s-blank? heading)))
        (beginning-of-buffer) (forward-line 2) (indent-for-tab-command)))))
;; Capturing ideas & notes without interrupting the current workflow:1 ends here

;; [[file:init.org::#Capturing-ideas-notes-without-interrupting-the-current-workflow][Capturing ideas & notes without interrupting the current workflow:2]]
(defun my/org-capture (&optional prefix keys)
  "Capture something!

      C-c c   ⇒ Capture something; likewise for “C-uⁿ C-c c” where n ≥ 3.
C-u   C-c c   ⇒ Capture current [narrowed] buffer.
C-u 5 C-c c   ⇒ Capture current [narrowed] buffer without adding additional remarks.
C-u C-u C-c c ⇒ Goto last note stored."
  (interactive "p")
  (case prefix
    (4     (my/org-capture-buffer keys))
    (5     (my/org-capture-buffer keys :no-additional-remarks))
    (t     (org-capture prefix keys))))
;; Capturing ideas & notes without interrupting the current workflow:2 ends here

;; [[file:init.org::#Capturing-ideas-notes-without-interrupting-the-current-workflow][Capturing ideas & notes without interrupting the current workflow:3]]
(s-join "\n" (--map (concat "+  [[kbd:" (s-replace "⇒" "]]" it))  (cddr (s-split "\n" (documentation #'my/org-capture)))))
;; Capturing ideas & notes without interrupting the current workflow:3 ends here

;; [[file:init.org::#Capturing-ideas-notes-without-interrupting-the-current-workflow][Capturing ideas & notes without interrupting the current workflow:4]]
;; Location of my todos/notes file
(unless noninteractive (setq org-default-notes-file "~/Dropbox/todo.org"))

;; “C-c c” to quickly capture a task/note
(define-key global-map "\C-cc" #'my/org-capture) ;; See below.
;; Capturing ideas & notes without interrupting the current workflow:4 ends here

;; [[file:init.org::#Capturing-ideas-notes-without-interrupting-the-current-workflow][Capturing ideas & notes without interrupting the current workflow:5]]
(cl-defun my/make/org-capture-template
   (shortcut heading &optional (no-todo nil) (description heading) (scheduled nil))
  "Quickly produce an org-capture-template.

  After adding the result of this function to ‘org-capture-templates’,
  we will be able perform a capture with “C-c c ‘shortcut’”
  which will have description ‘description’.
  It will be added to the tasks file under heading ‘heading’.

  ‘no-todo’ omits the ‘TODO’ tag from the resulting item; e.g.,
  when it's merely an interesting note that needn't be acted upon.

  Default for ‘description’ is ‘heading’. Default for ‘no-todo’ is ‘nil’.

  Scheduled items appear in the agenda; true by default.

  The target is ‘file+headline’ and the type is ‘entry’; to see
  other possibilities invoke: C-h o RET org-capture-templates.
  The “%?” indicates the location of the Cursor, in the template,
  when forming the entry.
  "
  `(,shortcut ,description entry
      (file+headline org-default-notes-file ,heading)
         ,(concat "*" (unless no-todo " TODO") " %?\n"
                (when nil ;; this turned out to be a teribble idea.
                  ":PROPERTIES:\n:"
                (if scheduled
                    "SCHEDULED: %^{Any time ≈ no time! Please schedule this task!}t"
                  "CREATED: %U")
                "\n:END:") "\n\n ")
      :empty-lines 1 :time-prompt t))
;; Capturing ideas & notes without interrupting the current workflow:5 ends here

;; [[file:init.org::#Capturing-ideas-notes-without-interrupting-the-current-workflow][Capturing ideas & notes without interrupting the current workflow:6]]
(setq org-capture-templates
      (cl-loop for (shortcut heading)
            in (-partition 2 '("t" "Tasks, Getting Things Done"
                               "r" "Research"
                               "2" "2FA3"
                               "m" "Email"
                               "e" "Emacs (•̀ᴗ•́)و"
                               "i" "Islam"
                               "b" "Blog"
                               "a" "Arbitrary Reading and Learning"
                               "l" "Programming Languages"
                               "p" "Personal Matters"))
            collect  (my/make/org-capture-template shortcut heading)))
;; Capturing ideas & notes without interrupting the current workflow:6 ends here

;; [[file:init.org::#Capturing-ideas-notes-without-interrupting-the-current-workflow][Capturing ideas & notes without interrupting the current workflow:7]]
;; Update: Let's schedule tasks during the GTD processing phase.
;;
;; For now, let's automatically schedule items a week in advance.
;; TODO: FIXME: This overwrites any scheduling I may have performed.
;; (defun my/org-capture-schedule ()
;;   (org-schedule nil "+7d"))
;;
;; (add-hook 'org-capture-before-finalize-hook 'my/org-capture-schedule)
;; Capturing ideas & notes without interrupting the current workflow:7 ends here

;; [[file:init.org::#Capturing-ideas-notes-without-interrupting-the-current-workflow][Capturing ideas & notes without interrupting the current workflow:8]]
;; Cannot mark an item DONE if it has a  TODO child.
;; Conversely, all children must be DONE in-order for a parent to be DONE.
(setq org-enforce-todo-dependencies t)
;; Capturing ideas & notes without interrupting the current workflow:8 ends here

;; [[file:init.org::#Capturing-ideas-notes-without-interrupting-the-current-workflow][Capturing ideas & notes without interrupting the current workflow:9]]
  ;; Ensure notes are stored at the top of a tree.
  (setq org-reverse-note-order nil)
;; Capturing ideas & notes without interrupting the current workflow:9 ends here

;; [[file:init.org::#Step-2-Filing-your-tasks][Step 2: Filing your tasks:1]]
;; Add a note whenever a task's deadline or scheduled date is changed.
(setq org-log-redeadline 'time)
(setq org-log-reschedule 'time)
;; Step 2: Filing your tasks:1 ends here

;; [[file:init.org::#Step-3-Quickly-review-the-upcoming-week][Step 3: Quickly review the upcoming week:1]]
(define-key global-map "\C-ca" 'org-agenda)
;; Step 3: Quickly review the upcoming week:1 ends here

;; [[file:init.org::#Step-3-Quickly-review-the-upcoming-week][Step 3: Quickly review the upcoming week:2]]
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
;; Step 3: Quickly review the upcoming week:2 ends here

;; [[file:init.org::#Step-3-Quickly-review-the-upcoming-week][Step 3: Quickly review the upcoming week:3]]
(setq org-agenda-start-on-weekday nil)
;; Step 3: Quickly review the upcoming week:3 ends here

;; [[file:init.org::#Step-3-Quickly-review-the-upcoming-week][Step 3: Quickly review the upcoming week:4]]
(use-package org-super-agenda
  ;; :hook (org-agenda-mode . origami-mode) ;; Easily fold groups via TAB.
  ;; :bind (:map org-super-agenda-header-map ("<tab>" . origami-toggle-node))
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
;; Step 3: Quickly review the upcoming week:4 ends here

;; [[file:init.org::#Step-4-Getting-ready-for-the-day][Step 4: Getting ready for the day:1]]
(setq org-lowest-priority ?D) ;; Now org-speed-eky ‘,’ gives 4 options
(setq org-priority-faces
'((?A :foreground "red" :weight bold)
  (?B . "orange")
  (?C . "yellow")
  (?D . "green")))
;; Step 4: Getting ready for the day:1 ends here

;; [[file:init.org::#Step-4-Getting-ready-for-the-day][Step 4: Getting ready for the day:2]]
(use-package org-fancy-priorities
  :diminish org-fancy-priorities-mode
  :hook   (org-mode . org-fancy-priorities-mode)
  :custom (org-fancy-priorities-list '("HIGH" "MID" "LOW" "OPTIONAL")))
;; Step 4: Getting ready for the day:2 ends here

;; [[file:init.org::#Step-7-Archiving-Tasks][Step 7: Archiving Tasks:1]]
;; C-c a s ➩ Search feature also looks into archived files.
;; Helpful when need to dig stuff up from the past.
(setq org-agenda-text-search-extra-files '(agenda-archives))
;; Step 7: Archiving Tasks:1 ends here

;; [[file:init.org::#Step-7-Archiving-Tasks][Step 7: Archiving Tasks:2]]
;; Invoking the agenda command shows the agenda and enables
;; the org-agenda variables.
;; ➩ Show my agenda upon Emacs startup.
(unless noninteractive
  (when my/personal-machine?
    (org-agenda "a" "a"))) ;; Need this to have “org-agenda-custom-commands” defined.
;; Step 7: Archiving Tasks:2 ends here

;; [[file:init.org::#Step-7-Archiving-Tasks][Step 7: Archiving Tasks:3]]
;; Pressing ‘c’ in the org-agenda view shows all completed tasks,
;; which should be archived.
(add-to-list 'org-agenda-custom-commands
  '("c" todo "DONE|ON_HOLD|CANCELLED" nil))
;; Step 7: Archiving Tasks:3 ends here

;; [[file:init.org::#Step-7-Archiving-Tasks][Step 7: Archiving Tasks:4]]
(add-to-list 'org-agenda-custom-commands
  '("u" alltodo ""
     ((org-agenda-skip-function
        (lambda ()
              (org-agenda-skip-entry-if 'scheduled 'deadline 'regexp  "\n]+>")))
              (org-agenda-overriding-header "Unscheduled TODO entries: "))))
;; Step 7: Archiving Tasks:4 ends here

;; [[file:init.org::#Tag-You're-it][Tag! You're it!:1]]
 (setq org-tags-column -77) ;; the default
;; Tag! You're it!:1 ends here

;; [[file:init.org::#Tag-You're-it][Tag! You're it!:2]]
(use-package helm-org) ;; Helm for org headlines and keywords completion.
(add-to-list 'helm-completing-read-handlers-alist
             '(org-set-tags-command . helm-org-completing-read-tags))

;; Also provides: helm-org-capture-templates
;; Tag! You're it!:2 ends here

;; [[file:init.org::#Tag-You're-it][Tag! You're it!:3]]
(use-package org-pretty-tags
  :diminish org-pretty-tags-mode
  :demand t
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
;; Tag! You're it!:3 ends here

;; [[file:init.org::#Automating-https-en-wikipedia-org-wiki-Pomodoro-Technique-Pomodoro-Commit-for-only-25-minutes][Automating [[https://en.wikipedia.org/wiki/Pomodoro_Technique][Pomodoro]] ---“Commit for only 25 minutes!”:1]]
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
;; Automating [[https://en.wikipedia.org/wiki/Pomodoro_Technique][Pomodoro]] ---“Commit for only 25 minutes!”:1 ends here

;; [[file:init.org::#The-Setup][The Setup:1]]
(defun my/org-journal-new-entry (prefix)
  "Open today’s journal file and start a new entry.

  With a prefix, we use the work journal; otherwise the personal journal."
  (interactive "P")
  (-let [org-journal-file-format (if prefix "Work-%Y-%m-%d" org-journal-file-format)]
    (org-journal-new-entry nil)
    (org-mode)
    (org-show-all)))

(use-package org-journal
  ;; C-u C-c j ⇒ Work journal ;; C-c C-j ⇒ Personal journal
  :bind (("C-c j" . my/org-journal-new-entry))
  :config
  (setq org-journal-dir         "~/Dropbox/journal/"
        org-journal-file-type   'yearly
        org-journal-file-format "Personal-%Y-%m-%d"))
;; The Setup:1 ends here

;; [[file:init.org::#Workflow-States][Workflow States:1]]
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s@/!)" "|" "DONE(d/!)")
        (sequence "WAITING(w@/!)" "ON_HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

;; Since DONE is a terminal state, it has no exit-action.
;; Let's explicitly indicate time should be noted.
(setq org-log-done 'time)
;; Workflow States:1 ends here

;; [[file:init.org::#Workflow-States][Workflow States:2]]
(setq org-todo-keyword-faces
      '(("TODO"      :foreground "red"          :weight bold)
        ("STARTED"   :foreground "blue"         :weight bold)
        ("DONE"      :foreground "forest green" :weight bold)
        ("WAITING"   :foreground "orange"       :weight bold)
        ("ON_HOLD"   :foreground "magenta"      :weight bold)
        ("CANCELLED" :foreground "forest green" :weight bold)))
;; Workflow States:2 ends here

;; [[file:init.org::#Workflow-States][Workflow States:3]]
(setq org-use-fast-todo-selection t)
;; Workflow States:3 ends here

;; [[file:init.org::#Workflow-States][Workflow States:4]]
;; Install the tool
; (async-shell-command "brew tap adoptopenjdk/openjdk; brew cask install adoptopenjdk13") ;; Dependency
; (async-shell-command "brew install plantuml")

;; Tell emacs where it is.
;; E.g., (async-shell-command "find / -name plantuml.jar")
(setq org-plantuml-jar-path
      "/usr/local/Cellar/plantuml/1.2020.19/libexec/plantuml.jar")

;; Enable C-c C-c to generate diagrams from plantuml src blocks.
(add-to-list 'org-babel-load-languages '(plantuml . t) )
(require 'ob-plantuml)

; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes '("plantuml" . fundamental))
;; Workflow States:4 ends here

;; [[file:init.org::#Clocking-Work-Time][Clocking Work Time:1]]
;; Record a note on what was accomplished when clocking out of an item.
(setq org-log-note-clock-out t)
;; Clocking Work Time:1 ends here

;; [[file:init.org::#Clocking-Work-Time][Clocking Work Time:2]]
(setq confirm-kill-emacs 'yes-or-no-p)
;; Clocking Work Time:2 ends here

;; [[file:init.org::#Clocking-Work-Time][Clocking Work Time:3]]
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
;; Clocking Work Time:3 ends here

;; [[file:init.org::#Estimates-versus-actual-time][Estimates versus actual time:1]]
 (push '("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
       org-global-properties)
;; Estimates versus actual time:1 ends here

;; [[file:init.org::#Estimates-versus-actual-time][Estimates versus actual time:2]]
(setq org-clock-sound "~/.emacs.d/school-bell.wav")
;; Estimates versus actual time:2 ends here

;; [[file:init.org::#Habit-Formation][Habit Formation:1]]
;; Show habits for every day in the agenda.
(setq org-habit-show-habits t)
(setq org-habit-show-habits-only-for-today nil)

;; This shows the ‘Seinfeld consistency’ graph closer to the habit heading.
(setq org-habit-graph-column 90)

;; In order to see the habit graphs, which I've placed rightwards, let's
;; always open org-agenda in ‘full screen’.
;; (setq org-agenda-window-setup 'only-window)
;; Habit Formation:1 ends here

;; [[file:init.org::#Using-Gnus-for-Gmail][Using Gnus for Gmail:1]]
(setq user-full-name    "Musa Al-hassy"
      user-mail-address "alhassy@gmail.com")
;; Using Gnus for Gmail:1 ends here

;; [[file:init.org::#Using-Gnus-for-Gmail][Using Gnus for Gmail:3]]
     (setq message-send-mail-function 'smtpmail-send-it)
;; Using Gnus for Gmail:3 ends here

;; [[file:init.org::#Using-Gnus-for-Gmail][Using Gnus for Gmail:6]]
;; After startup, if Emacs is idle for 10 seconds, then start Gnus.
;; Gnus is slow upon startup since it fetches all mails upon startup.
(when my/personal-machine?
  (run-with-idle-timer 10 nil #'gnus))
;; Using Gnus for Gmail:6 ends here

;; [[file:init.org::#Using-Gnus-for-Gmail][Using Gnus for Gmail:8]]
(with-eval-after-load 'gnus
  (bind-key "t"
          (lambda (N) (interactive "P") (gnus-summary-move-article N "[Gmail]/Trash"))
          gnus-summary-mode-map))

;; Orginally: t ⇒ gnus-summary-toggle-header
;; Using Gnus for Gmail:8 ends here

;; [[file:init.org::#Using-Gnus-for-Gmail][Using Gnus for Gmail:9]]
;; Fancy icons for Emacs
;; Only do this once:
(use-package all-the-icons :defer t)
  ; :config (all-the-icons-install-fonts 'install-without-asking)

;; Make mail look pretty
(use-package all-the-icons-gnus
  :defer t
  :config (all-the-icons-gnus-setup))

;; While we're at it: Make dired, ‘dir’ectory ‘ed’itor, look pretty
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))
;; Using Gnus for Gmail:9 ends here

;; [[file:init.org::#Using-Gnus-for-Gmail][Using Gnus for Gmail:10]]
(setq gnus-sum-thread-tree-vertical        "│"
      gnus-sum-thread-tree-leaf-with-other "├─► "
      gnus-sum-thread-tree-single-leaf     "╰─► "
      gnus-summary-line-format
      (concat
       "%0{%U%R%z%}"
       "%3{│%}" "%1{%d%}" "%3{│%}"
       "  "
       "%4{%-20,20f%}"
       "  "
       "%3{│%}"
       " "
       "%1{%B%}"
       "%s\n"))
;; Using Gnus for Gmail:10 ends here

;; [[file:init.org::#Using-Gnus-for-Gmail][Using Gnus for Gmail:11]]
(defun my/email (to subject body)
  (compose-mail to subject)
  (insert body)
  (message-send-mail)     ;; Appends info to the message buffer
  ; (let ((kill-buffer-query-functions nil)) (kill-this-buffer))
  (ignore-errors (undo))                  ;; Undo that addition
  (message-kill-buffer)
  (message "Send email to %s" to)) ;; Close that message buffer
;; Using Gnus for Gmail:11 ends here

;; [[file:init.org::#Using-Gnus-for-Gmail][Using Gnus for Gmail:13]]
(use-package gmail2bbdb
  :defer t
  :custom (gmail2bbdb-bbdb-file "~/Dropbox/bbdb"))

(use-package bbdb
 :after company ;; The “com”plete “any”thig mode is set below in §Prose
 :hook   (message-mode . bbdb-insinuate-gnus)
         (gnus-startup-hook . bbdb-insinuate-gnus)
 :custom (bbdb-file gmail2bbdb-bbdb-file)
         (bbdb-use-pop-up t)                        ;; allow popups for addresses
 :config (add-to-list 'company-backends 'company-bbdb))
;; Using Gnus for Gmail:13 ends here

;; [[file:init.org::#Capturing-Mail-as-Todo-Notes][Capturing Mail as Todo/Notes:1]]
(with-eval-after-load 'gnus
  ;; Orginally: c ⇒ gnus-summary-catchup-and-exit
  (bind-key "c" #'my/org-capture-buffer gnus-article-mode-map)
  ;; Orginally: C ⇒ gnus-summary-cancel-article
  (bind-key "C"
            (lambda (&optional keys)
              (interactive "P") (my/org-capture-buffer keys 'no-additional-remarks))
            gnus-article-mode-map))
;; Capturing Mail as Todo/Notes:1 ends here

;; [[file:init.org::#Get-LaTeX][Get LaTeX::1]]
(system-packages-ensure "mactex-no-gui")
;; Get LaTeX::1 ends here

;; [[file:init.org::#Get-LaTeX][Get LaTeX::2]]
(system-packages-ensure "pygments")
;; Get LaTeX::2 ends here

;; [[file:init.org::#Bibliography-Coloured-LaTeX-using-Minted][Bibliography & Coloured LaTeX using Minted:1]]
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -output-directory %o %f"
        "biber %b"
        "pdflatex -shell-escape -output-directory %o %f"
        "pdflatex -shell-escape -output-directory %o %f"))
;; Bibliography & Coloured LaTeX using Minted:1 ends here

;; [[file:init.org::#HTML-Org-mode][HTML ⇐ Org-mode:1]]
(use-package htmlize :defer t)
;; Main use: Org produced htmls are coloured.
;; Can be used to export a file into a coloured html.
;; HTML ⇐ Org-mode:1 ends here

;; [[file:init.org::#Ensuring-Useful-HTML-Anchors][Ensuring Useful HTML Anchors:1]]
(defun my/ensure-headline-ids (&rest _)
  "Org trees without a

All non-alphanumeric characters are cleverly replaced with ‘-’.

If multiple trees end-up with the same id property, issue a
message and undo any property insertion thus far.

E.g., ↯ We'll go on a ∀∃⇅ adventure
   ↦  We'll-go-on-a-adventure
"
  (interactive)
  (let ((ids))
    (org-map-entries
     (lambda ()
       (org-with-point-at (point)
         (let ((id (org-entry-get nil "CUSTOM_ID")))
           (unless id
             (thread-last (nth 4 (org-heading-components))
               (s-replace-regexp "[^[:alnum:]']" "-")
               (s-replace-regexp "-+" "-")
               (s-chop-prefix "-")
               (s-chop-suffix "-")
               (setq id))
             (if (not (member id ids))
                 (push id ids)
               (message-box "Oh no, a repeated id!\n\n\t%s" id)
               (undo)
               (setq quit-flag t))
             (org-entry-put nil "CUSTOM_ID" id))))))))

;; Whenever html & md export happens, ensure we have headline ids.
(advice-add 'org-html-export-to-html   :before 'my/ensure-headline-ids)
(advice-add 'org-md-export-to-markdown :before 'my/ensure-headline-ids)
;; Ensuring Useful HTML Anchors:1 ends here

;; [[file:init.org::#Clickable-Headlines][Clickable Headlines:1]]
;; Src: https://writepermission.com/org-blogging-clickable-headlines.html
(setq org-html-format-headline-function
      (lambda (todo todo-type priority text tags info)
        "Format a headline with a link to itself."
        (let* ((headline (get-text-property 0 :parent text))
               (id (or (org-element-property :CUSTOM_ID headline)
                       (ignore-errors (org-export-get-reference headline info))
                       (org-element-property :ID headline)))
               (link (if id
                         (format "<a href=\"#%s\">%s</a>" id text)
                       text)))
          (org-html-format-headline-default-function todo todo-type priority link tags info))))
;; Clickable Headlines:1 ends here

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

;; [[file:init.org::#Diagrams-with-Mermaid-Not-Reccommended][Diagrams with Mermaid ---Not Reccommended:2]]
(use-package ob-mermaid
  :custom ob-mermaid-cli-path "~/node_modules/.bin/mmdc")
;; Diagrams with Mermaid ---Not Reccommended:2 ends here

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

;; [[file:init.org::#Org-mode-HTML][Org-mode ⇐ HTML:2]]
(use-package org-web-tools
  :config
  ;; Insert an Org-mode link to the URL in the clipboard or kill-ring. Downloads
  ;; the page to get the HTML title.
  (bind-key* "C-c C-l" #'org-web-tools-insert-link-for-url))
;; Org-mode ⇐ HTML:2 ends here
