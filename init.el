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

;; [[file:init.org::*Here][Here:1]]
(message "\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
(message " Look here ")
(message ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n")
;; Here:1 ends here

;; [[file:init.org::#Lost-Souls][Lost Souls:1]]
;; Move to OS’ trash can when deleting stuff
;; instead of deleting things outright!
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/")

;; An automatic window-resizing mechanism.
;; A “calmer” alternative to golden-ratio.
;; https://github.com/cyrus-and/zoom
(use-package zoom
  :diminish
  :config (zoom-mode t))

;; https://cestlaz.github.io/posts/using-emacs-33-projectile-jump/
;; https://github.com/bbatsov/projectile
(use-package projectile
:config (projectile-global-mode))
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)

;; Let's use an improved buffer list.
(use-package ibuffer ;; This is built-into Emacs.
  :bind ("C-x C-b" . ibuffer))
;; It uses similar commands as does dired; e.g.,
;; / . org
;; This filters (“/”) the list with extensions (“.”) being “org”.

(use-package ibuffer-vc
  :hook (ibuffer . (lambda ()
                     (ibuffer-vc-set-filter-groups-by-vc-root)
                     (unless (eq ibuffer-sorting-mode 'alphabetic)
                       (ibuffer-do-sort-by-alphabetic))))
  :custom
  (ibuffer-formats '((mark modified read-only " "
                           (name 18 18 :left :elide) " "
                           (size 9 -1 :right) " "
                           (mode 16 16 :left :elide) " "
                           (vc-status 16 16 :left) " "
                           (vc-relative-file)))))
;; Lost Souls:1 ends here

;; [[file:init.org::*Sleek Semantic Selection][Sleek Semantic Selection:1]]
(use-package expand-region
  :diminish
  :bind (("s-r" . #'er/expand-region)))
;; Sleek Semantic Selection:1 ends here

;; [[file:init.org::*Semantic Change][Semantic Change:1]]
(use-package change-inner
  :diminish
  :bind (("s-i" . #'change-inner)
         ("s-o" . #'change-outer)))
;; Semantic Change:1 ends here

;; [[file:init.org::*Indentation Guide][Indentation Guide:1]]
;; Add a visual indent guide
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-character ?|)
  (highlight-indent-guides-responsive 'stack))
;; Indentation Guide:1 ends here

;; [[file:init.org::*Commenting][Commenting:1]]
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

 ;; Not ideal: M-; comments a parent Org heading and not the current line.
 ;; (define-key org-mode-map (kbd "M-;") 'org-comment-dwim-2)
;; Commenting:1 ends here

;; [[file:init.org::#Having-a-workspace-manager-in-Emacs][Having a workspace manager in Emacs:1]]
(use-package perspective
  :defer t
  :config ;; Activate it.
          (persp-mode)
          ;; In the modeline, tell me which workspace I'm in.
          (persp-turn-on-modestring))
;; Having a workspace manager in Emacs:1 ends here

;; [[file:init.org::#Editor-Documentation-with-Contextual-Information][Editor Documentation with Contextual Information:1]]
(use-package helpful :defer t)

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

;; [[file:init.org::#The-my-make-init-el-and-README-function][startup-code]]
  (defun my/make-init-el-and-README ()
    "Tangle an el and a github README from my init.org."
    (interactive "P") ;; Places value of universal argument into: current-prefix-arg
    (when current-prefix-arg
      (let* ((time      (current-time))
             (_date     (format-time-string "_%Y-%m-%d"))
             (.emacs    "~/.emacs")
             (.emacs.el "~/.emacs.el"))
        ;; Make README.org
        (save-excursion
          (org-babel-goto-named-src-block "make-readme") ;; See next subsubsection.
          (org-babel-execute-src-block))

        ;; remove any other initialisation file candidates
        (ignore-errors
          (f-move .emacs    (concat .emacs _date))
          (f-move .emacs.el (concat .emacs.el _date)))

        ;; Make init.el
        (org-babel-tangle)
        ;; (byte-compile-file "~/.emacs.d/init.el")
        (load-file "~/.emacs.d/init.el")

        ;; Acknowledgement
        (message "Tangled, compiled, and loaded init.el; and made README.md … %.06f seconds"
                 (float-time (time-since time))))))

(add-hook 'after-save-hook 'my/make-init-el-and-README nil 'local-to-this-file-please)
;; startup-code ends here

;; [[file:init.org::#Table-of-Contents-for-Org-vs-Github][‘Table of Contents’ for Org vs. Github:1]]
(use-package toc-org
  ;; Automatically update toc when saving an Org file.
  :hook (org-mode . toc-org-mode)
  ;; Use both “:ignore_N:” and ":export_N:” to exlude headings from the TOC.
  :custom (toc-org-noexport-regexp
           "\\(^*+\\)\s+.*:\\(ignore\\|noexport\\)\\([@_][0-9]\\)?:\\($\\|[^ ]*?:$\\)"))
;; ‘Table of Contents’ for Org vs. Github:1 ends here

;; [[file:init.org::#Table-of-Contents-for-Org-vs-Github][‘Table of Contents’ for Org vs. Github:2]]
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

;; Erase :TOC: body ---provided we're using toc-org.
;; (my/org-replace-tree-contents "Table of Contents")
;; ‘Table of Contents’ for Org vs. Github:2 ends here

;; [[file:init.org::#Screencapturing-the-Current-Emacs-Frame][Screencapturing the Current Emacs Frame:1]]
(defun my/capture-emacs-frame (&optional prefix output)
"Insert a link to a screenshot of the current Emacs frame.

Unless the name of the OUTPUT file is provided, read it from the
user. If PREFIX is provided, let the user select a portion of the screen."
(interactive "p")
(defvar my/emacs-window-id
   (s-collapse-whitespace (shell-command-to-string "osascript -e 'tell app \"Emacs\" to id of window 1'"))
   "The window ID of the current Emacs frame.

    Takes a second to compute, whence a defvar.")

(let* ((screen  (if prefix "-i" (concat "-l" my/emacs-window-id)))
       (temp    (format "emacs_temp_%s.png" (random)))
       (default (format-time-string "emacs-%m-%d-%Y-%H:%M:%S.png")))
;; Get output file name
  (unless output
    (setq output (read-string (format "Emacs screenshot filename (%s): " default)))
    (when (s-blank-p output) (setq output default)))
;; Clear minibuffer before capturing screen or prompt user
(message (if prefix "Please select region for capture …" "♥‿♥"))
;; Capture current screen and resize
(thread-first
    (format "screencapture -T 2 %s %s" screen temp)
    (concat "; magick convert -resize 60% " temp " " output)
    (shell-command))
(f-delete temp)
;; Insert a link to the image and reload inline images.
(insert (concat "[[file:" output "]]")))
(org-display-inline-images nil t))

(bind-key* "C-c M-s" #'my/capture-emacs-frame)
;; Screencapturing the Current Emacs Frame:1 ends here

;; [[file:init.org::#Org-mode's-𝒳-Block-Expansions][Org-mode's ~<𝒳~ Block Expansions:1]]
(require 'org-tempo)
;; Org-mode's ~<𝒳~ Block Expansions:1 ends here

;; [[file:init.org::#What's-changed-who's-to-blame][What's changed & who's to blame?:1]]
;; Hunk navigation and commiting.
(use-package git-gutter
  :diminish
  :config (global-git-gutter-mode))
;; Diff updates happen in real time according when user is idle.
;; What's changed & who's to blame?:1 ends here

;; [[file:init.org::#What's-changed-who's-to-blame][What's changed & who's to blame?:2]]
(defhydra hydra-version-control (global-map "C-x v")
  "Version control"
  ;; Syntax: (extension method description)
  ("n" git-gutter:next-hunk      "Next hunk")
  ("p" git-gutter:previous-hunk  "Previous hunk")
  ("d" git-gutter:popup-hunk     "Show hunk diff")
  ("r" git-gutter:revert-hunk    "Revert hunk\n")
  ("c" git-gutter:stage-hunk     "Stage hunk")
  ("s" git-gutter:statistic      "How many added & deleted lines"))
;; What's changed & who's to blame?:2 ends here

;; [[file:init.org::#What's-changed-who's-to-blame][What's changed & who's to blame?:3]]
;; Colour fringe to indicate alterations.
;; (use-package diff-hl)
;; (global-diff-hl-mode)
;; What's changed & who's to blame?:3 ends here

;; [[file:init.org::#What's-changed-who's-to-blame][What's changed & who's to blame?:4]]
;; Popup for who's to blame for alterations.
(use-package git-messenger
  :custom ;; Always show who authored the commit and when.
          (git-messenger:show-detail t)
          ;; Message menu let's us use magit diff to see the commit change.
          (git-messenger:use-magit-popup t))

;; View current file in browser on github.
;; More generic is “browse-at-remote”.
(use-package github-browse-file :defer t)

;; Add these to the version control hydra.
;;
(defhydra hydra-version-control (global-map "C-x v")
  ("b" git-messenger:popup-message "Who's to blame?")
  ;; C-u C-x b ╱ u b ∷ Also show who authored the change and when.
  ("g" github-browse-file-blame "Show file in browser in github")
  ("s" magit-status "Git status of current buffer"))
;; What's changed & who's to blame?:4 ends here

;; [[file:init.org::#What's-changed-who's-to-blame][What's changed & who's to blame?:5]]
(use-package git-link :defer t)

(defhydra hydra-version-control (global-map "C-x v")
  ("l" git-link "Git URL for current location"))
;; What's changed & who's to blame?:5 ends here

;; [[file:init.org::#Helpful-Utilities-Shortcuts][Helpful Utilities & Shortcuts:1]]
;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable all ‘possibly confusing commands’ such as helpful but
;; initially-worrisome “narrow-to-region”, C-x n n.
(setq-default disabled-command-function nil)
;; Helpful Utilities & Shortcuts:1 ends here

;; [[file:init.org::#Documentation-Pop-Ups][Documentation Pop-Ups:1]]
(use-package company-quickhelp
 :config
   (setq company-quickhelp-delay 0.1)
   (company-quickhelp-mode))
;; Documentation Pop-Ups:1 ends here

;; [[file:init.org::#Reload-buffer-with-f5][Reload buffer with ~f5~:1]]
(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t nil)))
;; Reload buffer with ~f5~:1 ends here

;; [[file:init.org::#Reload-buffer-with-f5][Reload buffer with ~f5~:2]]
;; Auto update buffers that change on disk.
;; Will be prompted if there are changes that could be lost.
(global-auto-revert-mode 1)

;; Don't show me the “ARev” marker in the mode line
(diminish 'auto-revert-mode)
;; Reload buffer with ~f5~:2 ends here

;; [[file:init.org::#Kill-to-start-of-line][Kill to start of line:1]]
;; M-k kills to the left
(global-set-key "\M-k" '(lambda () (interactive) (kill-line 0)) )
;; Kill to start of line:1 ends here

;; [[file:init.org::#Killing-buffers-windows-C-x-k-has-a-family][Killing buffers & windows: ~C-x k~ has a family:1]]
(global-set-key (kbd "C-x k")
  (lambda (&optional prefix)
"C-x k     ⇒ Kill current buffer & window
C-u C-x k ⇒ Kill OTHER window and its buffer
C-u C-u C-x C-k ⇒ Kill all other buffers and windows

Prompt only if there are unsaved changes."
     (interactive "P")
     (pcase (or (car prefix) 0)
       ;; C-x k     ⇒ Kill current buffer & window
       (0  (kill-this-buffer)
           (unless (one-window-p) (delete-window)))
       ;; C-u C-x k ⇒ Kill OTHER window and its buffer
       (4  (other-window 1)
           (kill-this-buffer)
           (unless (one-window-p) (delete-window)))
       ;; C-u C-u C-x C-k ⇒ Kill all other buffers and windows
       (16   (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
             (delete-other-windows)))))
;; Killing buffers & windows: ~C-x k~ has a family:1 ends here

;; [[file:init.org::#Switching-from-2-horizontal-windows-to-2-vertical-windows][Switching from 2 horizontal windows to 2 vertical windows:1]]
(defun my/ensure-two-vertical-windows ()
  "I used this method often when programming in Coq.

When there are two vertical windows, this method ensures the left-most
window contains the buffer with the cursour in it."
  (interactive)
  (let ((otherBuffer (buffer-name)))
    (other-window 1)                ;; C-x 0
    (delete-window)                 ;; C-x 0
    (split-window-right)			;; C-x 3
    (other-window 1)                ;; C-x 0
    (switch-to-buffer otherBuffer)	;; C-x b RET
    (other-window 1)))

(global-set-key (kbd "C-|") 'my/ensure-two-vertical-windows)
;; Switching from 2 horizontal windows to 2 vertical windows:1 ends here

;; [[file:init.org::#Obtaining-Values-of-KEYWORD-Annotations][Obtaining Values of ~#+KEYWORD~ Annotations:1]]
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

;; [[file:init.org::#Publishing-articles-to-my-personal-blog][Publishing articles to my personal blog:1]]
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

;; [[file:init.org::#Jumping-without-hassle][Jumping without hassle:1]]
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
  (goto-line line))
;; Jumping without hassle:1 ends here
