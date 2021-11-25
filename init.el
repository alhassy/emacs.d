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
;; Hydra: Supply a prefix only once:3 ends here

;; [[file:init.org::#Textual-Navigation-Look-Ma-no-CTRL-key][Textual Navigation ---“Look Ma, no CTRL key!”:1]]
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
;; Textual Navigation ---“Look Ma, no CTRL key!”:1 ends here

;; [[file:init.org::#Textual-Navigation-Look-Ma-no-CTRL-key][Textual Navigation ---“Look Ma, no CTRL key!”:2]]
;; C-n, next line, inserts newlines when at the end of the buffer
(setq next-line-add-newlines t)
;; Textual Navigation ---“Look Ma, no CTRL key!”:2 ends here

;; [[file:init.org::#Window-Navigation][Window Navigation:1]]
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

;; [[file:init.org::#Automatic-Backups][Automatic Backups:2]]
(setq confirm-kill-processes nil
      create-lockfiles nil)
;; Automatic Backups:2 ends here

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

;; [[file:init.org::#Word-Completion][Word Completion:1]]
(use-package company
  :diminish
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
;; Word Completion:1 ends here

;; [[file:init.org::#Word-Completion][Word Completion:2]]
(use-package company-emoji
  :config (add-to-list 'company-backends 'company-emoji))
;; Word Completion:2 ends here

;; [[file:init.org::#Word-Completion][Word Completion:3]]
(use-package emojify
 :config (setq emojify-display-style 'image)
 :init (global-emojify-mode 1)) ;; Will install missing images, if need be.
;; Word Completion:3 ends here

;; [[file:init.org::#Intro-to-Snippets][Intro to Snippets:1]]
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

;; [[file:init.org::#Intro-to-Snippets][Intro to Snippets:2]]
;; Yet another snippet extension program
(use-package yasnippet
  :diminish yas-minor-mode
  :config
    (yas-global-mode 1) ;; Always have this on for when using yasnippet syntax within yankpad
    ;; respect the spacing in my snippet declarations
    (setq yas-indent-line 'fixed))

;; Alternative, Org-based extension program
(use-package yankpad
  :diminish
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

;; [[file:init.org::#Intro-to-Snippets][Intro to Snippets:5]]
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

;; [[file:init.org::#][Emojis:2]]
;; Get all unicode emojis to appear within Emacs
;; See also: https://emacs.stackexchange.com/questions/5689/force-a-single-font-for-all-unicode-glyphs?rq=1
(unless noninteractive (set-fontset-font t nil "Apple Color Emoji"))
;; Emojis:2 ends here

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
(use-package org-journal
  ;; C-u C-c j ⇒ Work journal ;; C-c C-j ⇒ Personal journal
  :bind (("C-c j" . my/org-journal-new-entry))
  :config
    (setq org-journal-dir         "~/Desktop/" ;; "~/Dropbox/journal/"
          org-journal-file-type   'yearly
          org-journal-file-format "Personal-%Y-%m-%d")

    (defun my/org-journal-new-entry (prefix)
      "Open today’s journal file and start a new entry.

  With a prefix, we use the work journal; otherwise the personal journal."
      (interactive "P")
      (let ((org-journal-dir (if prefix "~/Desktop/" org-journal-dir))
            (org-journal-file-format (if prefix "Work-%Y-%m-%d" org-journal-file-format)))
        (org-journal-new-entry nil)
        (org-mode)
        (org-show-all))))
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

;; [[file:init.org::*Hydra Timer][Hydra Timer:1]]
(setq org-clock-sound t) ;; Standard Emacs beep
(my/defhydra "C-c x" "Time Tracking" clock-o
  ;; Org-Clock ---must be on an Org header;; but the timer works from anywhere
  :Tasks
  ("n" (my/org-journal-new-entry :work) "New")
  ("v" (progn (my/org-journal-new-entry :work) (revert-buffer t t) (org-journal-mode)) "View all")
  :Timer
  ("s" org-timer-start "Start")
  ("S" org-timer-stop "Stop")
  ("x" org-timer-set-timer "Set")
  ("p" org-timer "Print")
  :Org-Clock
  ("i" org-clock-in "in")
  ("o" org-clock-out "out")
  ("c" org-clock-cancel "cancel" :color pink :column "Do")
  ("d" org-clock-display "display")
  ("e" org-clock-modify-effort-estimate "effort")

  ("j" org-clock-goto "Jump to task") ;; Jump to  the headline of the currently clocked in task. With a C-u prefix argument, select the target task from a list of recently clocked tasks.
  ("r" org-clock-report "Insert clocktable"))
;; Hydra Timer:1 ends here

;; [[file:init.org::#Cosmetics][Cosmetics:1]]
;; Get org-headers to look pretty! E.g., * → ⊙, ** ↦ ◯, *** ↦ ★
;; https://github.com/emacsorphanage/org-bullets
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))
;; Cosmetics:1 ends here

;; [[file:init.org::#Startup-message-Emacs-Org-versions][Startup message: Emacs & Org versions:1]]
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
;; Startup message: Emacs & Org versions:1 ends here

;; [[file:init.org::#Startup-message-Emacs-Org-versions][Startup message: Emacs & Org versions:3]]
;; Keep self motivated!
(setq frame-title-format '("" "%b - Living The Dream (•̀ᴗ•́)و"))
;; Startup message: Emacs & Org versions:3 ends here

;; [[file:init.org::#My-to-do-list-The-initial-buffer-when-Emacs-opens-up][My to-do list: The initial buffer when Emacs opens up:1]]
(unless noninteractive ;; Only run the following when we're in GUI mode;
                       ;; i.e., don't run it in Github Actions when testing.
  (if my/personal-machine?
      (find-file "~/Dropbox/todo.org")
    (find-file "~/Desktop/work.org.gpg"))
  (split-window-right)			  ;; C-x 3
  (other-window 1)                              ;; C-x 0
  (let ((enable-local-variables :all)           ;; Load *all* locals.
        (org-confirm-babel-evaluate nil))       ;; Eval *all* blocks.
    (ignore-errors (find-file "~/.emacs.d/init.org"))))
;; My to-do list: The initial buffer when Emacs opens up:1 ends here

;; [[file:init.org::#Exquisite-Fonts-and-Themes][Exquisite Fonts and Themes:1]]
;; Treat all themes as safe; no query before use.
(setf custom-safe-themes t)

;; Nice looking themes ^_^
(use-package solarized-theme :defer t)
(use-package doom-themes :defer t)
(use-package spacemacs-common
  :defer t
  :ensure spacemacs-theme)
;; Exquisite Fonts and Themes:1 ends here

;; [[file:init.org::#Exquisite-Fonts-and-Themes][Exquisite Fonts and Themes:2]]
;; Infinite list of my commonly used themes.
(setq my/themes '(doom-laserwave doom-solarized-light doom-vibrant spacemacs-light solarized-gruvbox-dark solarized-gruvbox-light))
(setcdr (last my/themes) my/themes)
;; Exquisite Fonts and Themes:2 ends here

;; [[file:init.org::#Exquisite-Fonts-and-Themes][Exquisite Fonts and Themes:3]]
(cl-defun my/disable-all-themes (&optional (new-theme (pop my/themes)))
  "Disable all themes and load NEW-THEME, which defaults from ‘my/themes’.

When a universal prefix is given, “C-u C-c t”, we load a random
theme from all possible themes.  Nice way to learn about more
themes (•̀ᴗ•́)و"
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (-let [theme (if current-prefix-arg
                   (nth (random (length (custom-available-themes)))
                        (custom-available-themes))
                 new-theme)]
    (when theme
      (load-theme theme)
      (message "Theme %s" theme))))


(defalias 'my/toggle-theme #' my/disable-all-themes)
(global-set-key "\C-c\ t" 'my/toggle-theme)


;; (my/toggle-theme)
(use-package solarized-theme)
(my/toggle-theme 'solarized-gruvbox-light)
;; Exquisite Fonts and Themes:3 ends here

;; [[file:init.org::#Exquisite-Fonts-and-Themes][Exquisite Fonts and Themes:4]]
;; Infinite list of my commonly used fonts
(setq my/fonts
      '("Roboto Mono Light 14" ;; Sleek
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
(system-packages-ensure "font-roboto-mono")
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
(ignore-errors (my/toggle-font "Source Code Pro Light 14"))
;; Exquisite Fonts and Themes:4 ends here

;; [[file:init.org::#A-sleek-informative-and-fancy-mode-line][A sleek, informative, & fancy mode line:1]]
;; This package requires the fonts included with all-the-icons to be installed. Run M-x all-the-icons-install-fonts to do so.
;; The modeline looks really nice with doom-themes, e.g., doom-solarised-light.
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config

  ;; Use minimal height so icons still fit; modeline gets slightly larger when
  ;; buffer is modified since the "save icon" shows up.  Let's disable the icon.
  ;; Let's also essentially disable the hud bar, a sort of progress-bar on where we are in the buffer.
  (setq doom-modeline-height 1)
  (setq doom-modeline-buffer-state-icon nil)
  (setq doom-modeline-hud t)
  (setq doom-modeline-bar-width 0.0001)

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

  ;; Whether display the minor modes in the mode-line.  Enabled minor modes
  ;; clutter up the modeline with their names, albeit some have useful status
  ;; information shown. We can either selectively pick which names/status are
  ;; shown using diminish.el, possibly forgetting which minor modes are enabled
  ;; or we can use minions.el to “gather up” all enabled minor modes, and
  ;; recently enabled ones, under a single menu which doom-modeline shows as a
  ;; simple configurations gear icon.
  (setq doom-modeline-minor-modes t)
  (use-package minions
    :init (minions-mode))
  ;;
  ;; A quick hacky way to add stuff to doom-modeline is to add to the mode-line-process list.
  ;; E.g.:  (add-to-list 'mode-line-process '(:eval (format "%s" (count-words (point-min) (point-max)))))
  ;; We likely want to add this locally, to hooks on major modes.

  ;; Makes Org/Markdown previewabvle as we type!!! ♥
  ;; Shows up as a magnifying glass in doom-modeline.
  (use-package grip-mode)
  ;;  :hook ((markdown-mode org-mode) . grip-mode)
  ;; Pretty annyoning actually; instead we should call it as needed.
  )

;; Nice battery icon alongside with percentage, in doom-modeline.
;; If not for doom-modeline, we'd need to use fancy-batter-mode.el.
(display-battery-mode +1)

;; Show date and time as well.
(setq display-time-day-and-date t)
(display-time)
;; A sleek, informative, & fancy mode line:1 ends here

;; [[file:init.org::#A-sleek-informative-and-fancy-mode-line][A sleek, informative, & fancy mode line:2]]
;; I don't need the system load average in the modeline.
(setq display-time-default-load-average nil)
(setq display-time-load-average nil)
;; A sleek, informative, & fancy mode line:2 ends here

;; [[file:init.org::#A-sleek-informative-and-fancy-mode-line][A sleek, informative, & fancy mode line:3]]
;; (column-number-mode                 t) ;; Enabled in doom-modeline by default
;; (line-number-mode                   t) ;; Not sure I want line numbers in modeline, since I have them in the left margin.
(setq display-line-numbers-width-start t)
(global-display-line-numbers-mode      t)
;; A sleek, informative, & fancy mode line:3 ends here

;; [[file:init.org::#Powerful-Directory-Editing-with-dired][Powerful Directory Editing with ~dired~:1]]
(use-package dired-subtree
  :bind (:map dired-mode-map
              ("i" . dired-subtree-toggle)))
;; Powerful Directory Editing with ~dired~:1 ends here

;; [[file:init.org::#Powerful-Directory-Editing-with-dired][Powerful Directory Editing with ~dired~:2]]
(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode))
;; Powerful Directory Editing with ~dired~:2 ends here

;; [[file:init.org::#Powerful-Directory-Editing-with-dired][Powerful Directory Editing with ~dired~:3]]
(use-package dired-filter
  :hook (dired-mode . (lambda () (dired-filter-group-mode)
                                 (dired-filter-by-garbage)))
  :custom
    (dired-garbage-files-regexp
      "\\(?:\\.\\(?:aux\\|bak\\|dvi\\|log\\|orig\\|rej\\|toc\\|out\\)\\)\\'")
    (dired-filter-group-saved-groups
      '(("default"
         ("Org"    (extension "org"))
         ("Executables" (exexutable))
         ("Directories" (directory))
         ("PDF"    (extension "pdf"))
         ("LaTeX"  (extension "tex" "bib"))
         ("Images" (extension "png"))
         ("Code"   (extension "hs" "agda" "lagda"))
         ("Archives"(extension "zip" "rar" "gz" "bz2" "tar"))))))
;; Powerful Directory Editing with ~dired~:3 ends here

;; [[file:init.org::#Never-lose-the-cursor][Never lose the cursor:1]]
;; Make it very easy to see the line with the cursor.
(global-hl-line-mode t)
;; Never lose the cursor:1 ends here

;; [[file:init.org::#Never-lose-the-cursor][Never lose the cursor:2]]
(use-package beacon
  :diminish
  :config (setq beacon-color "#666600")
  :hook   ((org-mode text-mode) . beacon-mode))
;; Never lose the cursor:2 ends here

;; [[file:init.org::#Dimming-Unused-Windows][Dimming Unused Windows:1]]
(use-package dimmer
  :config (dimmer-mode))
;; Dimming Unused Windows:1 ends here

;; [[file:init.org::#Buffer-names-are-necessarily-injective][Buffer names are necessarily injective:1]]
;; Note that ‘uniquify’ is builtin.
(require 'uniquify)
(setq uniquify-separator "/"               ;; The separator in buffer names.
      uniquify-buffer-name-style 'forward) ;; names/in/this/style
;; Buffer names are necessarily injective:1 ends here

;; [[file:init.org::#Flashing-when-something-goes-wrong-no-blinking][Flashing when something goes wrong ---no blinking:1]]
(setq visible-bell 1)
;; Flashing when something goes wrong ---no blinking:1 ends here

;; [[file:init.org::#Flashing-when-something-goes-wrong-no-blinking][Flashing when something goes wrong ---no blinking:2]]
(blink-cursor-mode 1)
;; Flashing when something goes wrong ---no blinking:2 ends here

;; [[file:init.org::#Hiding-Scrollbar-tool-bar-and-menu][Hiding Scrollbar, tool bar, and menu:1]]
(unless noninteractive
  (tool-bar-mode   -1)  ;; No large icons please
  (scroll-bar-mode -1)  ;; No visual indicator please
  (menu-bar-mode   -1))  ;; The Mac OS top pane has menu options
;; Hiding Scrollbar, tool bar, and menu:1 ends here

;; [[file:init.org::#Highlight-complete-parenthesis-pair-when-cursor-is-near][Highlight & complete parenthesis pair when cursor is near ;-):1]]
(setq show-paren-delay  0)
(setq show-paren-style 'mixed)
(show-paren-mode)
;; Highlight & complete parenthesis pair when cursor is near ;-):1 ends here

;; [[file:init.org::#Highlight-complete-parenthesis-pair-when-cursor-is-near][Highlight & complete parenthesis pair when cursor is near ;-):2]]
(use-package rainbow-delimiters
  :disabled
  :hook ((org-mode prog-mode text-mode) . rainbow-delimiters-mode))
;; Highlight & complete parenthesis pair when cursor is near ;-):2 ends here

;; [[file:init.org::#Highlight-complete-parenthesis-pair-when-cursor-is-near][Highlight & complete parenthesis pair when cursor is near ;-):4]]
(electric-pair-mode 1)
;; Highlight & complete parenthesis pair when cursor is near ;-):4 ends here

;; [[file:init.org::#Highlight-complete-parenthesis-pair-when-cursor-is-near][Highlight & complete parenthesis pair when cursor is near ;-):5]]
;; The ‘<’ and ‘>’ are not ‘parenthesis’, so give them no compleition.
(setq electric-pair-inhibit-predicate
      (lambda (c)
        (or (member c '(?< ?> ?~)) (electric-pair-default-inhibit c))))

;; Treat ‘<’ and ‘>’ as if they were words, instead of ‘parenthesis’.
(modify-syntax-entry ?< "w<")
(modify-syntax-entry ?> "w>")
;; Highlight & complete parenthesis pair when cursor is near ;-):5 ends here

;; [[file:init.org::#Persistent-Scratch-Buffer][Persistent Scratch Buffer:1]]
(use-package persistent-scratch
  :defer t
  ;; In this mode, the usual save key saves to the underlying persistent file.
  :bind (:map persistent-scratch-mode-map
              ("C-x C-s" . persistent-scratch-save)))
;; Persistent Scratch Buffer:1 ends here

;; [[file:init.org::#Persistent-Scratch-Buffer][Persistent Scratch Buffer:2]]
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
;; Persistent Scratch Buffer:2 ends here

;; [[file:init.org::#Persistent-Scratch-Buffer][Persistent Scratch Buffer:3]]
(setq initial-scratch-message (concat
  "#+Title: Persistent Scratch Buffer"
  "\n#\n# Welcome! This’ a place for trying things out."
  "\n#\n# ⟨ ‘C-x C-s’ here saves to ~/.emacs.d/.persistent-scratch ⟩ \n\n"))
;; Persistent Scratch Buffer:3 ends here

;; [[file:init.org::#Proportional-fonts-for-Headlines][Proportional fonts for Headlines:1]]
(set-face-attribute 'org-document-title nil :height 2.0)
;; (set-face-attribute 'org-level-1 nil :height 1.0)
;; Remaining org-level-𝒾 have default height 1.0, for 𝒾 : 1..8.
;;
;; E.g., reset org-level-1 to default.
;; (custom-set-faces '(org-level-1 nil))
;; Proportional fonts for Headlines:1 ends here

;; [[file:init.org::#Making-Block-Delimiters-Less-Intrusive][Making Block Delimiters Less Intrusive:1]]
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
;; Making Block Delimiters Less Intrusive:1 ends here

;; [[file:init.org::#Making-Block-Delimiters-Less-Intrusive][Making Block Delimiters Less Intrusive:2]]
(add-hook 'org-mode-hook #'rasmus/org-prettify-symbols)
(org-mode-restart)
;; Making Block Delimiters Less Intrusive:2 ends here

;; [[file:init.org::#Making-Block-Delimiters-Less-Intrusive][Making Block Delimiters Less Intrusive:3]]
(global-prettify-symbols-mode)

(defvar my/prettify-alist nil
  "Musa's personal prettifications.")

(cl-loop for pair in '(;; Example of how pairs like this to beautify org block delimiters
                    ("#+begin_example" . (?ℰ (Br . Bl) ?⇒)) ;; ℰ⇒
                    ("#+end_example"   . ?⇐)                 ;; ⇐
                    ;; Actuall beautifications
                    ("<=" . ?≤) (">=" . ?≥)
                    ("->" . ?→) ("-->". ?⟶) ;; threading operators
                    ("[ ]" . ?□) ("[X]" . ?☑) ("[-]" . ?◐)) ;; Org checkbox symbols

      do (push pair my/prettify-alist))

(cl-loop for hk in '(text-mode-hook prog-mode-hook org-mode-hook)
      do (add-hook hk (lambda ()
                        (setq prettify-symbols-alist
                              (append my/prettify-alist prettify-symbols-alist)))))
;; Making Block Delimiters Less Intrusive:3 ends here

;; [[file:init.org::#Making-Block-Delimiters-Less-Intrusive][Making Block Delimiters Less Intrusive:4]]
;; Un-disguise a symbol when cursour is inside it or at the right-edge of it.
(setq prettify-symbols-unprettify-at-point 'right-edge)
;; Making Block Delimiters Less Intrusive:4 ends here

;; [[file:init.org::#Hiding-Emphasise-Markers-Inlining-Images-and-LaTeX-as-PNG][Hiding Emphasise Markers, Inlining Images, and LaTeX-as-PNG:1]]
;; org-mode math is now highlighted ;-)
(setq org-highlight-latex-and-related '(latex))

;; Extra space between text and underline line
(setq x-underline-at-descent-line t)

;; Hide the *,=,/ markers
(setq org-hide-emphasis-markers t)

;; (setq org-pretty-entities t)
;; to have \alpha, \to and others display as utf8
;; http://orgmode.org/manual/Special-symbols.html
;; Hiding Emphasise Markers, Inlining Images, and LaTeX-as-PNG:1 ends here

;; [[file:init.org::#Hiding-Emphasise-Markers-Inlining-Images-and-LaTeX-as-PNG][Hiding Emphasise Markers, Inlining Images, and LaTeX-as-PNG:2]]
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :init (setq org-appear-autoemphasis  t
              org-appear-autolinks nil
              org-appear-autosubmarkers nil))
;; Hiding Emphasise Markers, Inlining Images, and LaTeX-as-PNG:2 ends here

;; [[file:init.org::#Hiding-Emphasise-Markers-Inlining-Images-and-LaTeX-as-PNG][Hiding Emphasise Markers, Inlining Images, and LaTeX-as-PNG:4]]
;; Automatically toggle LaTeX previews when cursour enters/leaves them
(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))
;; Hiding Emphasise Markers, Inlining Images, and LaTeX-as-PNG:4 ends here

;; [[file:init.org::#Hiding-Emphasise-Markers-Inlining-Images-and-LaTeX-as-PNG][Hiding Emphasise Markers, Inlining Images, and LaTeX-as-PNG:5]]
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
;; Hiding Emphasise Markers, Inlining Images, and LaTeX-as-PNG:5 ends here

;; [[file:init.org::#Hiding-Emphasise-Markers-Inlining-Images-and-LaTeX-as-PNG][Hiding Emphasise Markers, Inlining Images, and LaTeX-as-PNG:7]]
;; Support “latex-as-png” src blocks, which show LaTeX as PNGs
(use-package ob-latex-as-png)
;; Hiding Emphasise Markers, Inlining Images, and LaTeX-as-PNG:7 ends here

;; [[file:init.org::#Hiding-Emphasise-Markers-Inlining-Images-and-LaTeX-as-PNG][Hiding Emphasise Markers, Inlining Images, and LaTeX-as-PNG:8]]
;; Use the “#+name” the user provides, instead of generating label identifiers.
(setq org-latex-prefer-user-labels t)
;; Hiding Emphasise Markers, Inlining Images, and LaTeX-as-PNG:8 ends here

;; [[file:init.org::#Show-off-screen-heading-at-the-top-of-the-window][Show off-screen heading at the top of the window:1]]
 (use-package org-sticky-header
  :hook (org-mode . org-sticky-header-mode)
  :config
  (setq-default
   org-sticky-header-full-path 'full
   ;; Child and parent headings are seperated by a /.
   org-sticky-header-outline-path-separator " / "))
;; Show off-screen heading at the top of the window:1 ends here

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

;; [[file:init.org::#Fill-mode-Word-Wrapping][Fill-mode ---Word Wrapping:1]]
(setq-default fill-column 80          ;; Let's avoid going over 80 columns
              truncate-lines nil      ;; I never want to scroll horizontally
              indent-tabs-mode nil)   ;; Use spaces instead of tabs
;; Fill-mode ---Word Wrapping:1 ends here

;; [[file:init.org::#Fill-mode-Word-Wrapping][Fill-mode ---Word Wrapping:2]]
;; Wrap long lines when editing text
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; Do not show the “Fill” indicator in the mode line.
(diminish 'auto-fill-function)
;; Fill-mode ---Word Wrapping:2 ends here

;; [[file:init.org::#Fill-mode-Word-Wrapping][Fill-mode ---Word Wrapping:3]]
;; Bent arrows at the end and start of long lines.
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(diminish 'visual-line-mode)
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
  :diminish
  :hook ((prog-mode . flyspell-prog-mode)
         ((org-mode text-mode) . flyspell-mode)))
;; Fix spelling as you type ---thesaurus & dictionary too!:2 ends here

;; [[file:init.org::#Fix-spelling-as-you-type-thesaurus-dictionary-too][Fix spelling as you type ---thesaurus & dictionary too!:3]]
(setq ispell-program-name "/usr/local/bin/aspell")
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
  :diminish synosaurus-mode
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
 :bind ("M-!" . wordnut-lookup-current-word))

;; Use M-& for async shell commands.
;; Fix spelling as you type ---thesaurus & dictionary too!:12 ends here

;; [[file:init.org::#Using-a-Grammar-Style-Checker][Using a Grammar & Style Checker:1]]
(use-package langtool
 :defer t
 :custom
  (langtool-language-tool-jar
   "~/Applications/LanguageTool-4.5/languagetool-commandline.jar"))
;; Using a Grammar & Style Checker:1 ends here

;; [[file:init.org::#Using-a-Grammar-Style-Checker][Using a Grammar & Style Checker:2]]
;; Quickly check, correct, then clean up /region/ with M-^
(eval-after-load 'langtool
(progn
(add-hook 'langtool-error-exists-hook
  (lambda ()
     (langtool-correct-buffer)
     (langtool-check-done)))

(global-set-key "\M-^"
                (lambda ()
                  (interactive)
                  (message "Grammar checking begun ...")
                  (langtool-check)))))
;; Using a Grammar & Style Checker:2 ends here

;; [[file:init.org::#Lightweight-Prose-Proofchecking][Lightweight Prose Proofchecking:1]]
(use-package writegood-mode
  ;; Load this whenver I'm composing prose.
  :hook (text-mode org-mode)
  ;; Don't show me the “Wg” marker in the mode line
  :diminish
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
(use-package lorem-ipsum :defer t)
;; Placeholder Text ---For Learning & Experimenting:1 ends here

;; [[file:init.org::#Some-text-to-make-us-smile][Some text to make us smile:1]]
(use-package dad-joke
  :defer t
  :config (defun dad-joke () (interactive) (insert (dad-joke-get))))
;; Some text to make us smile:1 ends here

;; [[file:init.org::#Unicode-Input-via-Agda-Input][Unicode Input via Agda Input:1]]
; (load (shell-command-to-string "agda-mode locate"))
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
               (shell-command-to-string "/usr/local/bin/agda-mode locate"))))
;; Unicode Input via Agda Input:4 ends here

;; [[file:init.org::#Unicode-Input-via-Agda-Input][Unicode Input via Agda Input:5]]
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
           ("whyme" "ლ(ಠ益ಠ)ლ" "ヽ༼ಢ_ಢ༽ﾉ☂" "щ(゜ロ゜щ)" "‿︵(ಥ﹏ಥ)‿︵" "･ﾟ(*❦ω❦)*･ﾟ")
           ;; confused, disapprove, dead, shrug
           ("what" "「(°ヘ°)" "(ಠ_ಠ)" "(✖╭╮✖)" "¯\\_(ツ)_/¯" "･✧_✧･")
           ;; dance, csi
           ("cool" "┏(-_-)┓┏(-_-)┛┗(-_-﻿ )┓"
            ,(s-collapse-whitespace "•_•)
                                      ( •_•)>⌐■-■
                                      (⌐■_■)"))
           ;; love, pleased, success, yesss
           ("smile" "♥‿♥" "(─‿‿─)" "(•̀ᴗ•́)و" "(งಠ_ಠ)ง" "ᴵ’ᵐ ᵇᵉᵃᵘᵗⁱᶠᵘˡ"))
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
(diminish 'subword-mode)
;; Enabling CamelCase Aware Editing Operations:1 ends here

;; [[file:init.org::#Delete-Selection-Mode][Delete Selection Mode:1]]
(delete-selection-mode 1)
;; Delete Selection Mode:1 ends here

;; [[file:init.org::#M-n-p-Word-at-Point-Navigation][  ~M-n,p~: Word-at-Point Navigation ╱╲ Automatic highlighting current symbol/word:1]]
;; Default: M-→/← moves to the next/previous instance of the currently highlighted word
;; These are already meaningful commands in Org-mode, so we avoid these key re-bindings in Org-mode; TODO.
(use-package auto-highlight-symbol
  :hook ((text-mode . auto-highlight-symbol-mode)
         (prog-mode . auto-highlight-symbol-mode)))
;;   ~M-n,p~: Word-at-Point Navigation ╱╲ Automatic highlighting current symbol/word:1 ends here

;; [[file:init.org::#M-n-p-Word-at-Point-Navigation][  ~M-n,p~: Word-at-Point Navigation ╱╲ Automatic highlighting current symbol/word:2]]
(defun my/symbol-replace (replacement)
  "Replace all standalone symbols in the buffer matching the one at point."
  (interactive  (list (read-from-minibuffer "Replacement for thing at point: " nil)))
  (save-excursion
    (let ((symbol (or (thing-at-point 'symbol) (error "No symbol at point!"))))
      (beginning-of-buffer)
      ;; (query-replace-regexp symbol replacement)
      (replace-regexp (format "\\b%s\\b" (regexp-quote symbol)) replacement))))
;;   ~M-n,p~: Word-at-Point Navigation ╱╲ Automatic highlighting current symbol/word:2 ends here

;; [[file:init.org::#M-n-p-Word-at-Point-Navigation][  ~M-n,p~: Word-at-Point Navigation ╱╲ Automatic highlighting current symbol/word:3]]
(defmacro my/make-navigation-hydra (initial-action)
  `(defhydra word-navigation
    (:body-pre (,initial-action)) "Word-at-point Navigation"
    ("n" ahs-forward "Next instance")
    ("p" smartscan-symbol-go-backward "Previous instance")
    ("r" my/symbol-replace "Replace all occurances")
    ("s" ahs-display-stat "Stats")))

;; (bind-key* str func) ≈ (global-set-key (kbd str) func)
(bind-key* "M-n" (my/make-navigation-hydra ahs-forward))
(bind-key* "M-p" (my/make-navigation-hydra ahs-backward))
(bind-key* "M-'" (my/make-navigation-hydra my/symbol-replace))
;;   ~M-n,p~: Word-at-Point Navigation ╱╲ Automatic highlighting current symbol/word:3 ends here

;; [[file:init.org::#Letter-based-Navigation][Letter-based Navigation:1]]
(use-package ace-jump-mode
  :defer t
  :config (bind-key* "C-c SPC" 'ace-jump-mode))

;; See ace-jump issues to configure for use of home row keys.
;; Letter-based Navigation:1 ends here

;; [[file:init.org::#Letter-based-Navigation][Letter-based Navigation:2]]
;; C-x o ⇒ Switch to the other window
;; C-x O ⇒ Switch back to the previous window
(bind-key "C-x O" (lambda () (interactive) (other-window -1)))
;; Letter-based Navigation:2 ends here

;; [[file:init.org::#C-c-e-n-p-Taking-a-tour-of-one's-edits][  =C-c e n,p=: Taking a tour of one's edits:1]]
;; Give me a description of the change made at a particular stop.
(use-package goto-chg
  :defer t
  :custom (glc-default-span 0))

(my/defhydra "C-c e" "Look at them edits!" bus
  :\  ("p" goto-last-change "Goto nᵗʰ last change")
      ("n" goto-last-change-reverse "Goto more recent change"))
;;   =C-c e n,p=: Taking a tour of one's edits:1 ends here

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

;; [[file:init.org::*Managing Processes/Servers from within Emacs][Managing Processes/Servers from within Emacs:1]]
(use-package prodigy
  :defer t
  :config
  ;; C-h v prodigy-services ⇒ See possible properties.

  (prodigy-define-service
    :name "Orchestrator"
    :cwd "~/orchestrator/"
    :command "npm"
    :args '("run" "dev")
    :tags '(orchestrator))

  (prodigy-define-service
    :name "Orc/Inspections"
    :cwd "~/inspections/webui"
    :init (lambda () (shell-command "git checkout main; git pull"))
    :command "npm"
    :args '("run" "dev")
    :tags '(orchestrator))

  (prodigy-define-service
    :name "Orc/WxPortal"
    :cwd "~/wxPortal/"
    :init (lambda () (shell-command "git checkout orc-main; git pull"))
    :command "npm"
    :args '("run" "dev")
    :tags '(orchestrator RUN-BEFORE-ORC-REPO))


  (prodigy-define-service
    :name "platform"
    :cwd "~/api-platform-server"
    :init (lambda () (shell-command "git checkout main; git pull"))
    :command "npm"
    :args '("run" "dev")
    :tags '())

  (prodigy-define-service
    :name "WxPortal"
    :cwd "~/wxPortal/"
    :init (lambda () (shell-command "git checkout main; git pull"))
    :command "npm"
    :args '("run" "dev")
    :tags '())

  (prodigy-define-service
    :name "Setup Users"
    :cwd "~/ops-helm-deployment/"
    ;; :init (lambda () (shell-command "git checkout main; git pull"))
    :command "bash"
    :args '("login.sh")
    :tags '())
  )
;; Managing Processes/Servers from within Emacs:1 ends here

;; [[file:init.org::*Project management & navigation][Project management & navigation:1]]
;; More info & key bindings: https://docs.projectile.mx/projectile/usage.html
(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)

  ;; Replace usual find-file with a project-wide version :-)
  (global-set-key (kbd "C-x f") #'projectile-find-file)

  ;; Makes indexing large projects much faster, after first time.
  ;; Since its caching, some files may be out of sync; you can delete the cache
  ;; with: C-u C-x f
  (setq projectile-enable-caching t)

  (define-key projectile-mode-map (kbd "C-x p s")
    ;; I prefer helm-do-grep-ag since it shows me a live search
    (lambda () (interactive)
       (let ((default-directory (car (projectile-get-project-directories (projectile-acquire-root)))))
         ;; (shell-command-to-string "echo $PWD")
         (helm-do-grep-ag nil))))) ;; “p”roject “s”earch
;; Project management & navigation:1 ends here

;; [[file:init.org::*Are there any errors in my code?][Are there any errors in my code?:1]]
(use-package flycheck-status-emoji
  :config
  (load-library "flycheck-status-emoji")
  (diminish-undo 'flycheck-mode)
  (flycheck-status-emoji-mode))
;; Are there any errors in my code?:1 ends here

;; [[file:init.org::*Are there any errors in my code?][Are there any errors in my code?:2]]
(use-package helm-flycheck)
 (bind-key*
 "C-c !"
 (defhydra my/flycheck-hydra (:color blue :hint nil)
   "Move around flycheck errors and get info about them"
   ("n" flycheck-next-error "next" :column "Navigation")
   ("p" flycheck-previous-error "previous")
   ("f" flycheck-first-error "first")
   ("l" flycheck-list-errors "list")
   ("h" helm-flycheck "helm") ;; Jump to an error / see-errors from a nice interactive menu

   ("e" flycheck-explain-error-at-point "explain"  :column "Current errror")
   ("c" flycheck-copy-errors-as-kill "copy")

   ("d" flycheck-describe-checker "Describe checker"  :column "More")
   ("s" flycheck-select-checker "Select checker")
   ("S" flycheck-verify-setup "Suggest setup")
   ("m" flycheck-manual "manual")))
;; Are there any errors in my code?:2 ends here

;; [[file:init.org::*LSP: Making Emacs into a generic full-featured programming IDE][LSP: Making Emacs into a generic full-featured programming IDE:1]]
(use-package lsp-mode
  :init
  ;; Set prefix for lsp commands
  ;; (setq lsp-keymap-prefix "s-l") ;; default
  ;; Set how often highlights, lenses, links, etc will be refreshed while you type
  ;; (setq lsp-idle-delay 0.500) ;; default
  :hook  ;; Every programming mode should enter & start LSP, with which-key support
         (js-mode . lsp-mode) ;; Enter LSP mode
         (js-mode . lsp)      ;; Start LSP server
         (lsp-mode . lsp-enable-which-key-integration)
  ;; For some reason, my usual snippet setup does not work with LSP, so using “C-x y”
  :bind ("C-x y" . #'yankpad-insert)
  :commands lsp)

;; If a server crashes, restart it without asking me.
(setq lsp-restart 'auto-restart)


;; https://emacs-lsp.github.io/lsp-mode/page/languages/
;; M-x lsp-install-server ⟨return⟩ jsts-ls
;; M-x lsp-install-server ⟨return⟩ json-ls
;; M-x lsp-install-server ⟨return⟩ eslint
;; M-x lsp-install-server ⟨return⟩ css-ls
;; M-x lsp-install-server ⟨return⟩ html-ls

;; lsp-ui for fancy sideline, popup documentation, VScode-like peek UI, etc.
;; https://emacs-lsp.github.io/lsp-ui/#intro
;;
;; You only have to put (use-package lsp-ui) in your config and the package will
;; work out of the box: By default, lsp-mode automatically activates lsp-ui.
(use-package lsp-ui)

;; lsp-treemacs for various tree based UI controls (symbols, errors overview,
;; call hierarchy, etc.)
(use-package lsp-treemacs) ;; https://github.com/emacs-lsp/lsp-treemacs
;; M-x lsp-treemacs-errors-list

;; helm-lsp provides “on type completion” alternative of cross-referencing.
;; https://github.com/emacs-lsp/helm-lsp
(use-package helm-lsp)
(define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
;; Jump to a symbol's definition in the current workspace with “s-l g a” or “M-g
;; a” (The 'a' stands for apropos, which means appropriate nature)

;; Set the amount of data which Emacs reads from a process.
;; Some LSP responses are in the 8k-3MB range.
;; ⟦ 1 megabyte ≈ 1 million bytes ≈ 1 000 000 bytes ⟧
(setq read-process-output-max (* 1024 1024)) ;; ~1mb; [default 4k]
(setq gc-cons-threshold (* 2 8 1000 1024)) ;;; ~16mb; default is: 800 000
;; A large gc-cons-threshold will cause freezing and stuttering during long-term
;; interactive use. This one seems to be a good default.
;; LSP: Making Emacs into a generic full-featured programming IDE:1 ends here

;; [[file:init.org::*LSP: Making Emacs into a generic full-featured programming IDE][LSP: Making Emacs into a generic full-featured programming IDE:2]]
;; Load the various useful utils
(require 'lsp-ui-peek)
(require 'lsp-ui-sideline)
(require 'lsp-ui-doc)
(require 'lsp-ui-imenu)

; (setq lsp-mode-hook nil)
(add-hook 'lsp-mode-hook
          (lambda ()
            ;; Locally delete a file needed for work, but it's outdated and clashes with LSP.
            (shell-command "rm ~/wxPortal/.flowconfig")
            ;; Load the various useful utils
            (require 'lsp-ui)
            (lsp-ui-peek-enable t)
            (lsp-ui-doc-enable t)
            (lsp-ui-sideline-enable t)
            (lsp-ui-imenu-buffer--enable)
            ;; Set ⌘-l as the main mini-menu for LSP commands
            (bind-key* "s-l" #'my/lsp-hydra/body)))

(defun my/helm-lsp-workspace-symbol-at-point ()
    (interactive)
    (let ((current-prefix-arg t))
      (call-interactively #'helm-lsp-workspace-symbol)))

  (defun my/helm-lsp-global-workspace-symbol-at-point ()
    (interactive)
    (let ((current-prefix-arg t))
      (call-interactively #'helm-lsp-global-workspace-symbol)))

;; TODO: Add other cool features discussed/loaded above into this hydra!
(defhydra my/lsp-hydra (:color blue :hint nil)
  ;; Xref
  ("d" xref-find-definitions "Definitions" :column "Xref")
  ("D" xref-find-definitions-other-window "-> other win")
  ("r" xref-find-references "References")
  ("s" my/helm-lsp-workspace-symbol-at-point "Helm search")
  ("S" my/helm-lsp-global-workspace-symbol-at-point "Helm global search")

  ;; Peek
  ("C-d" lsp-ui-peek-find-definitions "Definitions" :column "Peek")
  ("C-r" lsp-ui-peek-find-references "References")
  ("C-i" lsp-ui-peek-find-implementation "Implementation")

  ;; LSP
  ("p" lsp-describe-thing-at-point "Describe at point" :column "LSP")
  ("C-a" lsp-execute-code-action "Execute code action")
  ("R" lsp-rename "Rename")
  ("t" lsp-goto-type-definition "Type definition")
  ("i" lsp-goto-implementation "Implementation")
  ("f" helm-imenu "Filter funcs/classes (Helm)")
  ("C-c" lsp-describe-session "Describe session")

  ;; Flycheck ---my “C-c !” flycheck hydra is much better than this simple lsp one.
  ;; ("l" lsp-ui-flycheck-list "List errs/warns/notes" :column "Flycheck")
  ("l" my/flycheck-hydra/body "List errs/warns/notes" :column "Flycheck")

  ;; Misc
  ("q" nil "Cancel" :column "Misc")
  ("b" pop-tag-mark "Back"))
;; LSP: Making Emacs into a generic full-featured programming IDE:2 ends here

;; [[file:init.org::*  ~M-x jsdoc~: Insert JSDocs with minimal type inference, useful with LSP-mode][  ~M-x jsdoc~: Insert JSDocs with minimal type inference, useful with LSP-mode:1]]
;; [Minimal Type Inference] When default values are provided, then we can infer
;; the type of the arguments.
;;
;; Use: Run “M-x jsdoc” on a JS function.
;;
(use-package jsdoc
  :ensure t
  :quelpa (jsdoc
           :fetcher github
           :repo "isamert/jsdoc.el")
  :config
   (use-package tree-sitter)        ;; Required dependencies
   (use-package tree-sitter-langs)
  :hook (js-mode . tree-sitter-mode))
;;   ~M-x jsdoc~: Insert JSDocs with minimal type inference, useful with LSP-mode:1 ends here

;; [[file:init.org::*Turbolog: What's the value of this expression? :JavaScript][Turbolog: What's the value of this expression? :JavaScript:2]]
(unless noninteractive

(setq turbo-log--prefix "%c ******* LOOK HERE *******")
(defun length> (x y) (> (length x) y))
(defun length= (x y) (= (length x) y))
(use-package turbo-log
  :quelpa (turbo-log :fetcher github :repo "artawower/turbo-log.el")
  :config (setq turbo-console--prefix "✰"))
(bind-key* "C-x l" #'my/turbo-log-hydra/body)
(defhydra my/turbo-log-hydra (:color blue :hint nil)
  ("l" turbo-log-print "Log selected expression" :column "TurboLog: Insert meaningful log message for selected expressions")
  ("c" turbo-log-comment-all-logs "Comment out all logs")
  ("u" turbo-log-uncomment-all-logs "Uncomment out all logs")
  ("d" turbo-log-delete-all-logs "Delete all TurboLog logs")
  ("q" nil "Cancel"))


(defun turbo-log--ecmascript-print (current-line-number formatted-selected-text prev-line-text multiple-logger-p)
  "Console log for ecmascript, js/ts modes.
CURRENT-LINE-NUMBER - line number under cursor
FORMATTED-SELECTED-TEXT - formatted text without space at start position
PREV-LINE-TEXT - text from previous line
MULTIPLE-LOGGER-P - should guess list of available loggers?"

  (let* ((is-empty-body (turbo-log--ecmascript-empty-body-p (turbo-log--get-line-text current-line-number)))
         (insert-line-number (turbo-log--ecmascript-find-insert-pos current-line-number prev-line-text))
         (meta-info (turbo-log--format-meta-info insert-line-number))
         (normalized-code (turbo-log--ecmascript-normilize-code formatted-selected-text))
         (turbo-log--message
          (concat
           (turbo-log--choose-logger turbo-log--ecmascript-loggers multiple-logger-p)
           "('"
           meta-info
           formatted-selected-text ": ', "
           ;; HACK this is my change: Does the TurboLog prefix have a %c console styling marker? If so, use it.
           (if (s-contains? "%c" meta-info) "'color: green; font-weight: bold;', " "")
           normalized-code ")"
           (if (plist-get turbo-log--ecmascript-configs :include-semicolon) ";"))))

    (if is-empty-body
        (progn
          (turbo-log--goto-line (- current-line-number 1))
          (beginning-of-line)
          (search-forward-regexp "}[[:blank:]]*")
          (replace-match "")
          (turbo-log--insert-with-indent current-line-number turbo-log--message)
          (turbo-log--insert-with-indent (+ current-line-number 1) "}")
          (indent-according-to-mode))
      (turbo-log--insert-with-indent insert-line-number turbo-log--message))))

)
;; Turbolog: What's the value of this expression? :JavaScript:2 ends here

;; [[file:init.org::#Which-function-are-we-writing][Which function are we writing?:1]]
(add-hook 'prog-mode-hook #'which-function-mode)
(add-hook 'org-mode-hook  #'which-function-mode)
;; Which function are we writing?:1 ends here

;; [[file:init.org::#Which-function-are-we-writing][Which function are we writing?:2]]
(add-hook 'emacs-lisp-mode-hook #'check-parens)
;; Which function are we writing?:2 ends here

;; [[file:init.org::#Highlight-defined-Lisp-symbols][Highlight defined Lisp symbols:1]]
;; Emacs Lisp specific
(use-package highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode))
;; Highlight defined Lisp symbols:1 ends here

;; [[file:init.org::#Eldoc-for-Lisp-and-Haskell][Eldoc for Lisp and Haskell ---documentation in the mini-buffer:1]]
(use-package eldoc
  :diminish eldoc-mode
  :hook (emacs-lisp-mode . turn-on-eldoc-mode)
        (lisp-interaction-mode . turn-on-eldoc-mode)
        (haskell-mode . turn-on-haskell-doc-mode)
        (haskell-mode . turn-on-haskell-indent))

;; Slightly shorten eldoc display delay.
(setq eldoc-idle-delay 0.4) ;; Default 0.5
;; Eldoc for Lisp and Haskell ---documentation in the mini-buffer:1 ends here

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

;; [[file:init.org::#Being-Generous-with-Whitespace][Being Generous with Whitespace:1]]
(use-package electric-operator
  :diminish
  :hook (c-mode . electric-operator-mode))
;; Being Generous with Whitespace:1 ends here

;; [[file:init.org::#On-the-fly-syntax-checking][On the fly syntax checking:1]]
(use-package flycheck
  :diminish
  :init (global-flycheck-mode)
  :config ;; There may be multiple tools; I have GHC not Stack, so let's avoid that.
  (setq-default flycheck-disabled-checkers '(haskell-stack-ghc emacs-lisp-checkdoc))
  :custom (flycheck-display-errors-delay .3))
;; On the fly syntax checking:1 ends here

;; [[file:init.org::#On-the-fly-syntax-checking][On the fly syntax checking:3]]
(use-package flymake
  :hook ((emacs-lisp-mode . (lambda () (flycheck-mode -1)))
         (emacs-lisp-mode . flymake-mode))
  :bind (:map flymake-mode-map
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)))
;; On the fly syntax checking:3 ends here

;; [[file:init.org::#Coding-with-a-Fruit-Salad-Semantic-Highlighting][Coding with a Fruit Salad: Semantic Highlighting:1]]
(use-package color-identifiers-mode
  :config (global-color-identifiers-mode))

;; Sometimes just invoke: M-x color-identifiers:refresh
;; Coding with a Fruit Salad: Semantic Highlighting:1 ends here

;; [[file:init.org::#Text-Folding][Text Folding ---Selectively displaying portions of a program:1]]
(use-package hideshow
  :init
  ;; https://github.com/emacsmirror/emacswiki.org/blob/master/hideshowvis.el
  (quelpa '(hideshowvis :fetcher wiki))

  ;; https://github.com/shanecelis/hideshow-org/tree/master
  ;; This extension bring Org-mode tab behaviour to folding, at the block level
  ;; and buffer level ---but not cycling visiblity.
  (use-package hideshow-org)

  :hook ((prog-mode . (lambda () (hs-minor-mode +1)
                        (hideshowvis-minor-mode t)
                        (hideshowvis-symbols)
                        (hs-org/minor-mode t)
                        (hs-hide-all)))))
;; Text Folding ---Selectively displaying portions of a program:1 ends here

;; [[file:init.org::#Text-Folding][Text Folding ---Selectively displaying portions of a program:2]]
(my/defhydra "C-c f" "Folding text" archive
  :Current
  ("h" hs-hide-block "Hide")
  ("s" hs-show-block "Show")
  ("t" hs-toggle-hiding "Toggle")
  :Buffer
  ("H" hs-hide-all "Hide")
  ("S" hs-show-all "Show")
  ("T" my/hs-toggle-buffer "Toggle")
  :Style
  ("i" my/clever-selective-display "Fold along current indentation" :toggle selective-display)
  ("e" auto-set-selective-display-mode  "Explore; walk and see" :toggle t)
  :...
  ("w" hl-todo-occur "Show WIPs/TODOs" :exit t)
  ("m" lsp-ui-imenu "Menu of TLIs" :exit t) ;; TLI ≈ Top Level Items
  ;; ("i" imenu-list "iMenu (General)") ;; It seems the above is enough for both prog and otherwise.
  ("r" (progn (hs-minor-mode -1) (hs-minor-mode +1)) "Reset")) ;; Remove all folds from the buffer and reset all hideshow-mode. Useful if it messes up!

;; Features from origami/yafolding that maybe I'd like to implement include:
;; folding region, narrowing to block or folding everything except block, navigating back and forth between folded blocks.
;; Finally, if we want to cycle the visibility of a block (as in Org-mode), we can use a combination of hs-show-block and hs-hide-level.
;; Text Folding ---Selectively displaying portions of a program:2 ends here

;; [[file:init.org::#Text-Folding][Text Folding ---Selectively displaying portions of a program:3]]
(defvar my/hs-hide nil "Current state of hideshow for toggling all.")
(defun my/hs-toggle-buffer () "Toggle hideshow all."
       (interactive)
       (setq my/hs-hide (not my/hs-hide))
       (if my/hs-hide
           (hs-hide-all)
         (hs-show-all)))
;; Text Folding ---Selectively displaying portions of a program:3 ends here

;; [[file:init.org::#Text-Folding][Text Folding ---Selectively displaying portions of a program:4]]
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
;; Text Folding ---Selectively displaying portions of a program:4 ends here

;; [[file:init.org::#Text-Folding][Text Folding ---Selectively displaying portions of a program:5]]
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
;; Text Folding ---Selectively displaying portions of a program:5 ends here

;; [[file:init.org::#Text-Folding][Text Folding ---Selectively displaying portions of a program:6]]
;; Open folded nodes if a search stops there.
(add-hook 'helm-swoop-after-goto-line-action-hook #'my/search-hook-function)
(defun my/search-hook-function ()
  (when hs-minor-mode (set-mark-command nil) (hs-show-block) (pop-to-mark-command)))
;; Text Folding ---Selectively displaying portions of a program:6 ends here

;; [[file:init.org::#Jump-between-windows-using-Cmd-Arrow-between-recent-buffers-with-Meta-Tab][Jump between windows using Cmd+Arrow & between recent buffers with Meta-Tab:1]]
(use-package windmove
  :config ;; use command key on Mac
          (windmove-default-keybindings 'super)
          ;; wrap around at edges
          (setq windmove-wrap-around t))
;; Jump between windows using Cmd+Arrow & between recent buffers with Meta-Tab:1 ends here

;; [[file:init.org::#Jump-between-windows-using-Cmd-Arrow-between-recent-buffers-with-Meta-Tab][Jump between windows using Cmd+Arrow & between recent buffers with Meta-Tab:2]]
(use-package buffer-flip
  :bind
   (:map buffer-flip-map
    ("M-<tab>"   . buffer-flip-forward)
    ("M-S-<tab>" . buffer-flip-backward)
    ("C-g"       . buffer-flip-abort))
  :config
    (setq buffer-flip-skip-patterns
        '("^\\*helm\\b")))
;; key to begin cycling buffers.
(global-set-key (kbd "M-<tab>") 'buffer-flip)
;; Jump between windows using Cmd+Arrow & between recent buffers with Meta-Tab:2 ends here

;; [[file:init.org::*hr: \[\[https:/github.com/LuRsT/hr\]\[A horizontal for your terminal\]\]][hr: [[https://github.com/LuRsT/hr][A horizontal for your terminal]]:1]]
(system-packages-install "hr") ;; ≈ brew install hr
;; hr: [[https://github.com/LuRsT/hr][A horizontal for your terminal]]:1 ends here

;; [[file:init.org::#Draw-pretty-unicode-tables-in-org-mode][Draw pretty unicode tables in org-mode:1]]
(quelpa '(org-pretty-table
         :repo "Fuco1/org-pretty-table"
         :fetcher github))

(add-hook 'org-mode-hook 'org-pretty-table-mode)
;; Draw pretty unicode tables in org-mode:1 ends here

;; [[file:init.org::*Toggles Hydra][Toggles Hydra:1]]
(my/defhydra "C-c t" "Toggles" toggle-on
   :Theme
   ("t t" my/toggle-theme "Theme")
   ("t s" (thread-last (all-completions "doom" (custom-available-themes))
            (completing-read "Load custom theme: ")
            intern
            load-theme)
    "Select Theme")

   :UI
   ("i" display-fill-column-indicator-mode :toggle t)
   ("f" my/toggle-font "font")
   ("d" treemacs "directory finder" :toggle t)
   ("n" global-linum-mode "line number" :toggle t)
   ("u f" (setq frame-title-format (completing-read "New frame title: " nil)) "New frame title") ;; Useful for random screenshots

   :Possibly_in_the_way
   ("e" electric-pair-mode "electric pair" :toggle t)
   ("c" flyspell-mode "spell check" :toggle t)
   ("s" prettify-symbols-mode "pretty symbol" :toggle t)
   ("w" whitespace-cleanup "Clean up whitespace on save" :toggle t)
   ;; ("a" global-aggressive-indent-mode "aggressive indent" :toggle t)
   ;; ("d" global-hungry-delete-mode "hungry delete" :toggle t)

   :Modeline
   ("m d" doom-modeline-mode "modern mode-line" :toggle t)
   ("m b" display-battery-mode "battery" :toggle t)
   ("m t" display-time-mode "time" :toggle t)
   ("m w" which-function-mode "which function" :toggle t)

   :Highlight
   ("h l" global-hl-line-mode "line" :toggle t)
   ("h p" show-paren-mode "paren" :toggle t)
   ;; ("h s" symbol-overlay-mode "symbol" :toggle t)
   ;; ("h r" rainbow-mode "rainbow" :toggle t)
   ;; ("h w" (setq-default show-trailing-whitespace (not show-trailing-whitespace))
   ;; "whitespace" :toggle show-trailing-whitespace)
   ;; ("h d" rainbow-delimiters-mode "delimiter" :toggle t)
   ("h i" highlight-indent-guides-mode "indent" :toggle t)
   ("h t" global-hl-todo-mode "todo" :toggle t)
   ;; ("x" highlight-sexp-mode "sexp" :toggle t)
   ;; ("t" hl-todo-mode "todo" :toggle t)

   :Program
   ("f" flycheck-mode "flycheck" :toggle t)
   ;; ("F" flymake-mode "flymake" :toggle t)
   ("o" origami-mode "folding" :toggle t)
   ;; ("O" hs-minor-mode "hideshow" :toggle t)
   ("W" subword-mode "subword" :toggle t)
   ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
   ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit))
   ;; ("v" global-diff-hl-mode "gutter" :toggle t)
   ;; ("V" diff-hl-flydiff-mode "live gutter" :toggle t)
   ;; ("M" diff-hl-margin-mode "margin gutter" :toggle t)
   ;; ("D" diff-hl-dired-mode "dired gutter" :toggle t)

   ("P" (if (profiler-running-p)
            (progn (profiler-report) (profiler-stop))
          (profiler-start 'cpu+mem))
    "Profiler start / report" :exit (profiler-running-p)))
;; Toggles Hydra:1 ends here

;; [[file:init.org::*Toggling System][Toggling System:1]]
(defhydra toggle-me-to-the-moon (global-map "C-x t") ;; (:color pink :columns 3)
  "Emacs, please toggle my [t]heme | [f]ont | [m]enu"
  ;; First row
  ("t" my/toggle-theme)
  ("f" my/toggle-font)
  ("m" imenu-list-smart-toggle)
  ("c" column-number-mode)
)

 ;; Shows a nice sidebar menu of the buffer's contents
(use-package imenu-list) ;; Main keys: SPC / ENTER / TAB / n / p / q
;; Toggling System:1 ends here

;; [[file:init.org::*Syntax highlighting ---numbers and escape characters][Syntax highlighting ---numbers and escape characters:1]]
(use-package highlight-numbers
  :hook ((text-mode prog-mode) . highlight-numbers-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))
;; Syntax highlighting ---numbers and escape characters:1 ends here

;; [[file:init.org::*shell-command-and-run][shell-command-and-run:1]]
(defun shell-command-and-run (cmd name &rest more-commands)
  "Run shell command CMD (possibly opening a new repl/terminal) and then MORE-COMMANDS.

When to use this function? Whenever you're finding yourself in the situation:
(1) Open a terminal, (2) start an interactive repl, (3) rename the buffer name to be informative,
(4) run some default/initial commands. See also the `term' function.

CMD and NAME are strings; MORE-COMMANDS is an arbitrary number of strings.

The name of this function does not contain my personal prefix ‘my’,
since I'd like it to show up as a possible completion when I type
‘shell-command’.

For example,

   (shell-command-and-run
    \"ghci\" \"Playing with Haskell\"
    \"let x = 4\"
    \":t x\")

This results in an interactive shell buffer named “*Playing with Haskell*” with contents:

   GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
   Prelude> let x = 4
   Prelude> :t x
   x :: Num p => p
   Prelude> ❙"
  (interactive)
  (let* ((default-directory "~/")
         (proc (get-buffer-process
                (ansi-term cmd name))))
    (term-send-string
     proc (concat (s-join "\n" more-commands) "\n"))))
;; shell-command-and-run:1 ends here

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

;; Make RETURN key act the same way as “y” key for “y-or-n” prompts.
;; E.g., (y-or-n-p "Happy?") accepts RETURN as “yes”.
(define-key y-or-n-p-map [return] 'act)

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
;; Auto refreshes every 2 seconds. Don’t forget to refresh the version control status as well.
(setq auto-revert-interval 2
      auto-revert-check-vc-info t
      global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

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
