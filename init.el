;; [[file:init.org::#Clean-this-section-up][Clean this section up!:1]]
;; cl-lib was published as a better (namespaced!) alternative to cl, which has a deprecation warning in Emacs27.
;; Yet some old pacakges require cl, and so the below setq silences the deprecation warning.
(setq byte-compile-warnings '(cl-functions))
(require 'cl-lib) ;; to get loop instead of cl-loop, etc.

;; Required for Github Actions; i.e., testing.
;; TODO Clean me!
(defun quelpa-read-cache ()) ;; Used somewhere, but not defined.
;; See: quelpa-persistent-cache-file
(setq quelpa-cache nil)
(defun org-special-block-extras-short-names ())
;;
;; org-special-block-extras.el:681:1:Error: Symbol’s value as variable is void: o--supported-blocks
(setq o--supported-blocks nil)
;; Eager macro-expansion failure: (void-function all-the-icons-faicon)
;; Symbol’s function definition is void: all-the-icons-faicon
;; Clean this section up!:1 ends here

;; [[file:init.org::#Clean-this-section-up][Clean this section up!:2]]
;; Error in kill-emacs-hook (org-clock-save): (void-function org-clocking-buffer)
(cl-defun org-clocking-buffer (&rest _))
;; Clean this section up!:2 ends here

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
(setq package-archives '(("gnu"    . "http://elpa.gnu.org/packages/")
                             ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa"  . "http://melpa.org/packages/")
                             ("org"    . "http://orgmode.org/elpa/")))

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
;; Installing OS packages, and automatically keeping my system up to data, from within Emacs:1 ends here

;; [[file:init.org::#Installing-OS-packages-and-automatically-keeping-my-system-up-to-data-from-within-Emacs][Installing OS packages, and automatically keeping my system up to data, from within Emacs:3]]
;; An Emacs-based interface to the package manager of your operating system.
(use-package helm-system-packages)
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
        ("C-x C-x" . helm-all-mark-rings)
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

;; [[file:init.org::#Being-at-the-Helm-Completion-Narrowing-Framework][ “Being at the Helm” ---Completion & Narrowing Framework:8]]
;; Save/mark a location with “C-u M-m”, jump back to it with “M-m”.
(bind-key* "M-m"
           (lambda ()
             (interactive)
             (if (not current-prefix-arg)
                 (helm-mark-ring)
               (push-mark)
               (message "[To return to this location, press M-m] ∷ %s"
                        (s-trim (substring-no-properties (thing-at-point 'line)))))))
;;  “Being at the Helm” ---Completion & Narrowing Framework:8 ends here

;; [[file:init.org::#Org-Mode-Administrivia][Org-Mode Administrivia:2]]
  (when nil use-package emacs
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
;; TODO org-special-block-extras.el:681:1:Error: Symbol’s value as variable is void: o--supported-blocks
;;
(when nil use-package org-special-block-extras
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

;; [[file:init.org::#all-the-icons][all-the-icons:1]]
 (use-package all-the-icons
    :config (all-the-icons-install-fonts 'install-without-asking))
;; (cl-defun all-the-icons-faicon (icon &rest _)
;;  #("" 0 1 (rear-nonsticky t display (raise -0.24) font-lock-face (:family "FontAwesome" :height 1.2) face (:family "FontAwesome" :height 1.2))))
;; all-the-icons:1 ends here

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

;; [[file:init.org::#magit-Emacs'-porcelain-interface-to-gitq][  =magit= ---Emacs' porcelain interface to git:1]]
;; Bottom of Emacs will show what branch you're on
;; and whether the local file is modified or not.
(use-package magit
  :bind (("C-c g" . magit-file-dispatch))
  :custom ;; Do not ask about this variable when cloning.
    (magit-clone-set-remote.pushDefault t))
;;   =magit= ---Emacs' porcelain interface to git:1 ends here

;; [[file:init.org::#magit-Emacs'-porcelain-interface-to-gitq][  =magit= ---Emacs' porcelain interface to git:2]]
;; When we invoke magit-status, show green/red the altered lines, with extra
;; green/red on the subparts of a line that got alerted.
(system-packages-ensure "git-delta")
(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

;; Don't forget to copy/paste the delta config into the global ~/.gitconfig file.
;; Copy/paste this: https://github.com/dandavison/delta#get-started
;;   =magit= ---Emacs' porcelain interface to git:2 ends here

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

  (require 'magit-repos) ;; Gets us the magit-repository-directories variable.
  (add-to-list 'magit-repository-directories `(,local . 0))

  (if (file-directory-p local)
      'repo-already-exists
    (shell-command (concat "git clone " remote " " local))
    (dired local)
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

;; [[file:init.org::#Jump-to-a-ma-git-repository-with-C-u-C-x-g][Jump to a (ma)git repository with ~C-u C-x g~:1]]
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

;; [[file:init.org::#Silently-show-me-when-a-line-was-modified-and-by-whom][Silently show me when a line was modified and by whom:1]]
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

;; [[file:init.org::#delete-by-moving-to-trash-t][delete-by-moving-to-trash t:1]]
;; Move to OS’ trash can when deleting stuff
;; instead of deleting things outright!
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/")
;; delete-by-moving-to-trash t:1 ends here

;; [[file:init.org::#Jumping-to-extreme-semantic-units][Jumping to extreme semantic units:1]]
;; M-< and M-> jump to first and final semantic units.
;; If pressed twice, they go to physical first and last positions.
(use-package beginend
  :diminish 'beginend-global-mode
  :config (beginend-global-mode)
    (cl-loop for (_ . m) in beginend-modes do (diminish m)))
;; Jumping to extreme semantic units:1 ends here

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

;; [[file:init.org::#Prettify-inline-source-code][Prettify inline source code:1]]
;; Show “ src_emacs-lisp[:exports results]{ 𝒳 } ” as “ ℰ𝓁𝒾𝓈𝓅﴾ 𝒳 ﴿ ”.
;;
(font-lock-add-keywords 'org-mode
  '(("\\(src_emacs-lisp\\[.*]{\\)\\(.*\\)\\(}\\)"
  (1 '(face (:inherit (bold) :foreground "gray65") display "ℰ𝓁𝒾𝓈𝓅﴾"))
  (2 '(face (:foreground "blue")))
  (3 '(face (:inherit (bold) :foreground "gray65") display "﴿"))
    )))
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

      C-c c   ⇒ Capture something
C-u   C-c c   ⇒ Capture current [narrowed] buffer.
C-u 5 C-c c   ⇒ Capture current [narrowed] buffer without adding additional remarks.
C-u C-u C-c c ⇒ Goto last note stored.

At work, ‘C-c c’ just captures notes under ‘Tasks’; no menu used."
  (interactive "p")
  (pcase prefix
    (4     (my/org-capture-buffer keys))
    (5     (my/org-capture-buffer keys :no-additional-remarks))
    (t     (if my/personal-machine?
               (org-capture prefix keys)
             (org-capture prefix "t")))))
;; Capturing ideas & notes without interrupting the current workflow:2 ends here

;; [[file:init.org::#Capturing-ideas-notes-without-interrupting-the-current-workflow][Capturing ideas & notes without interrupting the current workflow:3]]
(s-join "\n" (--map (concat "+  [[kbd:" (s-replace "⇒" "]]" it))  (cddr (s-split "\n" (documentation #'my/org-capture)))))
;; Capturing ideas & notes without interrupting the current workflow:3 ends here

;; [[file:init.org::#Capturing-ideas-notes-without-interrupting-the-current-workflow][Capturing ideas & notes without interrupting the current workflow:4]]
;; Location of my todos / captured notes file
(unless noninteractive
  (setq org-default-notes-file
        (if my/personal-machine?
            "~/Dropbox/todo.org"
          "~/Desktop/Work-2022-01-01.org")))

;; “C-c c” to quickly capture a task/note
(define-key global-map "\C-cc" #'my/org-capture) ;; See above.
;; Capturing ideas & notes without interrupting the current workflow:4 ends here

;; [[file:init.org::#Capturing-ideas-notes-without-interrupting-the-current-workflow][Capturing ideas & notes without interrupting the current workflow:5]]
(cl-defun my/make-org-capture-template
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
                               "r" "Reference Material"
                               "m" "Email"
                               "e" "Emacs (•̀ᴗ•́)و"
                               "i" "Islam"
                               "b" "Blog"
                               "a" "Arbitrary Reading and Learning"
                               "l" "Programming Languages"
                               "p" "Personal Matters"))
            collect  (my/make-org-capture-template shortcut heading)))
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

;; [[file:init.org::#Capturing-ideas-notes-without-interrupting-the-current-workflow][Capturing ideas & notes without interrupting the current workflow:10]]
(cl-defun my/reference (&optional (file org-default-notes-file))
  "Look up some reference material super quick.

By default we look in user's Org TODOs file.

FILE should be an ORG file with a top-level heading that starts with ‘Reference’.
We show its subheadings in a completing-read menu, then narrow to that entry."
  (interactive)
  (find-file file)
  (widen)
  (goto-char (point-min))
  (re-search-forward "^* Reference") ;; Start of line.
  (org-narrow-to-subtree)
  (org-cycle) (org-cycle)
  (let* ((headings (org-map-entries (lambda () (org-element-property :title (org-element-at-point)) ) "LEVEL=2"))
         (topic (completing-read "What to review? " headings)))
    (search-forward (concat "** " topic))
    (org-narrow-to-subtree)
    (org-cycle)))

(defalias 'my/review-reference-notes 'my/reference)
(defalias 'w-reference 'my/reference) ;; “w”ork
;; Capturing ideas & notes without interrupting the current workflow:10 ends here

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

;; Start each agenda item with ‘○’, then show me it's %timestamp and how many
;; times it's been re-%scheduled.
(setq org-agenda-prefix-format " ○ %t%s")
;; Step 3: Quickly review the upcoming week:3 ends here

;; [[file:init.org::#Step-3-Quickly-review-the-upcoming-week][Step 3: Quickly review the upcoming week:4]]
(use-package origami)
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
;; Step 3: Quickly review the upcoming week:4 ends here

;; [[file:init.org::#Step-4-Getting-ready-for-the-day][Step 4: Getting ready for the day:1]]
(setq org-lowest-priority ?C) ;; Now org-speed-eky ‘,’ gives 3 options
(setq org-priority-faces
'((?A :foreground "red"            :weight bold) ;; :background "LightCyan1")
  (?B :foreground "orange"         :weight bold)
  (?C :foreground "green"          :weight bold)))
;; See all colours with: M-x list-colors-display
;; Step 4: Getting ready for the day:1 ends here

;; [[file:init.org::#Step-4-Getting-ready-for-the-day][Step 4: Getting ready for the day:2]]
(use-package org-fancy-priorities
  :diminish org-fancy-priorities-mode
  :hook   (org-mode . org-fancy-priorities-mode)
  :custom (org-fancy-priorities-list '("High" "MID" "LOW")) ;; "OPTIONAL"
  ;; Let's use the “Eisenhower map of priority”…
  ;; :custom (org-fancy-priorities-list '("Urgent and Important"     ;; Do now!
  ;;                                      "Not Urgent But Important" ;; Do schedule this.
  ;;                                      "Urgent But Not Important" ;; Delegate?
  ;;                                      "Not Urgent and Not Important")) ;; Don't do / Optional
  )
;; Step 4: Getting ready for the day:2 ends here

;; [[file:init.org::#Step-4-Getting-ready-for-the-day][Step 4: Getting ready for the day:3]]
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
;; Step 4: Getting ready for the day:3 ends here

;; [[file:init.org::#Step-7-Archiving-Tasks][Step 7: Archiving Tasks:1]]
;; C-c a s ➩ Search feature also looks into archived files.
;; Helpful when need to dig stuff up from the past.
(setq org-agenda-text-search-extra-files '(agenda-archives))
;; Step 7: Archiving Tasks:1 ends here

;; [[file:init.org::#Step-7-Archiving-Tasks][Step 7: Archiving Tasks:2]]
;; enables the org-agenda variables.
(require 'org-agenda) ;; Need this to have “org-agenda-custom-commands” defined.

(unless noninteractive
    ;; ➩ Show my agenda upon Emacs startup.
    (org-agenda "a" "a"))
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
  ;; C-u C-c j ⇒ Work journal ;; C-c j ⇒ Personal journal
  :bind (("C-c j" . my/org-journal-new-entry))
  :config
    (setq org-journal-dir         "~/Desktop/" ;; "~/Dropbox/journal/"
          org-journal-file-type   'yearly
          org-journal-file-format "Personal-%Y-%m-%d.org")

    (defun my/org-journal-new-entry (prefix)
      "Open today’s journal file and start a new entry.

  With a prefix, we use the work journal; otherwise the personal journal."
      (interactive "P")
      (let ((org-journal-dir (if prefix "~/Desktop/" org-journal-dir))
            (org-journal-file-format (if prefix "Work-%Y-%m-%d.org" org-journal-file-format)))
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

;; [[file:init.org::#Actually-Doing-Things][Actually Doing Things ---or /Sending notifications from Emacs/:1]]
;; Obtain a notifications and text-to-speech utilities
(system-packages-ensure "espeak") ;; Alternatively: espeak-ng supports 109 languages
(system-packages-ensure "terminal-notifier") ;; MacOS specific
;; System Preferences → Notifications → Terminal Notifier → Allow “alerts”.
;; E.g.,: (shell-command "terminal-notifier -title \"Hiya\" -message \"hello\"")
;; Actually Doing Things ---or /Sending notifications from Emacs/:1 ends here

;; [[file:init.org::#Actually-Doing-Things][Actually Doing Things ---or /Sending notifications from Emacs/:2]]
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
               (format "%s"
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
                         & ;; … and then speak! …
                         espeak -s 125 ,(s-replace "\\n" " " (pp-to-string message))))))
;; Actually Doing Things ---or /Sending notifications from Emacs/:2 ends here

;; [[file:init.org::#Actually-Doing-Things][Actually Doing Things ---or /Sending notifications from Emacs/:3]]
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
;; Actually Doing Things ---or /Sending notifications from Emacs/:3 ends here

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
(unless noninteractive
  ;; Only run the following when we're in GUI mode;
  ;; i.e., don't run it in Github Actions when testing.
  (if my/personal-machine?
      (find-file "~/Dropbox/todo.org")
    ;; After startup, if Emacs is idle for 10 seconds, then open my work file;
    ;; which is a GPG file and so requires passphrase before other things can load.
    ;; (run-with-idle-timer 10 nil (lambda () (find-file "~/Desktop/work.org.gpg")))
      (find-file "~/Desktop/Work-2022-01-01.org")) ;; Org-journal for work
  (split-window-right)                          ;; C-x 3
  (other-window 1)                              ;; C-x 0
  (let ((enable-local-variables :all)           ;; Load *all* locals.
        (org-confirm-babel-evaluate nil))       ;; Eval *all* blocks.
    (ignore-errors (find-file "~/.emacs.d/init.org"))))
;; My to-do list: The initial buffer when Emacs opens up:1 ends here

;; [[file:init.org::#A-sleek-informative-and-fancy-mode-line][A sleek, informative, & fancy mode line:1]]
;; This package requires the fonts included with all-the-icons to be installed. Run M-x all-the-icons-install-fonts to do so.
;; The modeline looks really nice with doom-themes, e.g., doom-solarised-light.
(use-package doom-modeline
  :init (doom-modeline-mode))

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
;; A sleek, informative, & fancy mode line:1 ends here

;; [[file:init.org::#Menu-to-Toggle-Minor-Modes-A-quick-way-to-see-all-of-my-modes-and-which-are-enabled][Menu to Toggle Minor Modes: A quick way to see all of my modes, and which are enabled:1]]
  (setq doom-modeline-minor-modes t)
  (use-package minions
    :init (minions-mode))

  ;; A quick hacky way to add stuff to doom-modeline is to add to the mode-line-process list.
  ;; E.g.:  (add-to-list 'mode-line-process '(:eval (format "%s" (count-words (point-min) (point-max)))))
  ;; We likely want to add this locally, to hooks on major modes.
;; Menu to Toggle Minor Modes: A quick way to see all of my modes, and which are enabled:1 ends here

;; [[file:init.org::#Nice-battery-icon-alongside-with-percentage-in-doom-modeline][Nice battery icon alongside with percentage, in doom-modeline:1]]
;; If not for doom-modeline, we'd need to use fancy-battery-mode.el.
(display-battery-mode +1)
;; Nice battery icon alongside with percentage, in doom-modeline:1 ends here

;; [[file:init.org::#Time-date][Time & date:1]]
;; Show date and time as well.

;; [Simple Approach]
;; (setq display-time-day-and-date t)
;; (display-time)

;; [More Controlled Approach: Set date&time format]
;; a ≈ weekday; b ≈ month; d ≈ numeric day, R ≈ 24hr:minute.
(setq display-time-format "%a %b %d ╱ %r") ;; E.g.,:  Fri Mar 04 ╱ 03:42:08 pm
(setq display-time-interval 1) ;; Please update the time every second.
(display-time-mode)
;; Time & date:1 ends here

;; [[file:init.org::#Time-date][Time & date:2]]
;; I don't need the system load average in the modeline.
(setq display-time-default-load-average nil)
(setq display-time-load-average nil)
;; Time & date:2 ends here

;; [[file:init.org::#Column-Numbers][Column Numbers:1]]
;; (column-number-mode                 t) ;; Enabled in doom-modeline by default
;; (line-number-mode                   t) ;; Not sure I want line numbers in modeline, since I have them in the left margin.
(setq display-line-numbers-width-start t)
(global-display-line-numbers-mode      t)
;; Column Numbers:1 ends here

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
(my/toggle-theme 'doom-laserwave)
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

;; [[file:init.org::#Flashing-when-something-goes-wrong][Flashing when something goes wrong:1]]
;; (setq visible-bell 1) ;; On MacOS, this shows a caution symbol ^_^

;; The doom themes package comes with a function to make the mode line flash on error.
(use-package doom-themes)
(require 'doom-themes-ext-visual-bell)
(doom-themes-visual-bell-config)
;; Flashing when something goes wrong:1 ends here

;; [[file:init.org::#Flashing-when-something-goes-wrong][Flashing when something goes wrong:2]]
(blink-cursor-mode 1)
;; Flashing when something goes wrong:2 ends here

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

;; Replace all Org [metadata]keywords with the “▷” symbol; e.g., “#+title: Hello” looks like “▷ Hello”.
(cl-loop for keyword in '(title author email date description options property startup export_file_name html_head)
         do (push (cons (format "#+%s:" keyword) ?▷) my/prettify-alist))

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

;; [[file:init.org::#Preview-link-under-cursor][Preview link under cursor:1]]
(quelpa '(preview-it :repo "jcs-elpa/preview-it" :fetcher github))
(global-preview-it-mode)
;; Preview link under cursor:1 ends here

;; [[file:init.org::#Preview-link-under-cursor][Preview link under cursor:2]]
(quelpa '(goto-line-preview :repo "jcs-elpa/goto-line-preview" :fetcher github))
(global-set-key [remap goto-line] 'goto-line-preview)
;; Preview link under cursor:2 ends here

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
(diminish 'subword-mode)
;; Enabling CamelCase Aware Editing Operations:1 ends here

;; [[file:init.org::#Delete-Selection-Mode][Delete Selection Mode:1]]
(delete-selection-mode 1)
;; Delete Selection Mode:1 ends here

;; [[file:init.org::#M-n-p-Word-at-Point-Navigation][  ~M-n,p~: Word-at-Point Navigation ╱╲ Automatic highlighting current symbol/word:1]]
;; Default: M-→/← moves to the next/previous instance of the currently highlighted word
;; These are already meaningful commands in Org-mode, so we avoid these key re-bindings in Org-mode; TODO.
;; (use-package auto-highlight-symbol
;;   :hook ((text-mode . auto-highlight-symbol-mode)
;;          (prog-mode . auto-highlight-symbol-mode)))
;;
;; This breaks Org Exports; e.g.,
;; C-c C-e h o  ⇒  Match data clobbered by buffer modification hooks
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

;; [[file:init.org::#visual-regexp][visual-regexp:1]]
;; While constructing the regexp in the minibuffer, get live visual feedback for the (group) matches.
;; E.g., try: M-% use-\(.+?\) \(.+\)\b ENTER woah \1 and \2
;;
;; C-u M-%  do to regexp replace, without querying.
(use-package visual-regexp
  :config (define-key global-map (kbd "M-%")
            (lambda (&optional prefix) (interactive "P") (call-interactively (if prefix  #'vr/replace #'vr/query-replace)))))
;; visual-regexp:1 ends here

;; [[file:init.org::#Get-LaTeX][Get LaTeX::1]]
(system-packages-ensure "mactex")
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
  ;; (bind-key* "C-c C-l" #'org-web-tools-insert-link-for-url) ;; Instead, see my/org-insert-link-dwim below.
  )
;; Org-mode ⇐ HTML:2 ends here

;; [[file:init.org::#Org-mode-HTML][Org-mode ⇐ HTML:3]]
(bind-key* "C-c C-l" #'my/org-insert-link-dwim)
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
;; Org-mode ⇐ HTML:3 ends here

;; [[file:init.org::#Quickly-Run-Code-Snippets][Quickly Run Code Snippets:1]]
;; In any programming buffer, “M-x quickrun” to execute that program.
;; Super useful when wanting to quickly test things out, in a playground.
;;
;; E.g., Make a new file named “hello.py” containing “print "hi"”, then “M-x quickrun”.
;;
;; Enable “quickrun-autorun-mode” to run code after every save.
(use-package quickrun
  ;; ⇒ “C-c C-r” to see output, “q” to close output
  ;; ⇒ “C-u C-c C-r” prompts for a language (Useful when testing snippets different from current programming mode)
  ;; ⇒ In a non-programming buffer, “C-c C-r” runs selected region.
  :config (bind-key* "C-c C-r"
                     (lambda (&optional start end)
                       (interactive "r")
                       (if (use-region-p)
                           (quickrun-region start end)
                         (quickrun current-prefix-arg)))))
;; Quickly Run Code Snippets:1 ends here

;; [[file:init.org::#Quickly-Run-Code-Snippets][Quickly Run Code Snippets:2]]
(system-packages-ensure "rust") ;; Rust Compiler
;; C-c C-r on the following: fn main() { println!("Hello, World!"); }

;; Actually, let's get a full Rust development environment for Emacs
(use-package rustic)
;; Open any Rust file, and run “M-x lsp” which will then prompt you to install
;; rust-analyzer, the rust LSP.
;;
;; LSP for Rust ⇒ Goto definition (M-. / ⌘-l), code completion with types and
;; docstrings, colourful documentation on hover, “Run [Test] | Debug” overlays,
;; super nice stuff!
;;
;; Below, hover over “Vec” and see nice, scrollable, colourful docs on vectors.
;;    let v:Vec<_> = vec![1, 2, 3];
;; Quickly Run Code Snippets:2 ends here

;; [[file:init.org::#ELisp][ELisp:1]]
;; Evaluation Result OverlayS for Emacs Lisp
(use-package eros
  :init (eros-mode t))
;; ELisp:1 ends here

;; [[file:init.org::#JavaScript][JavaScript:1]]
(use-package skerrick
  :init
  ;; Needs to be run on the very first install of skerrick. Or when you want to upgrade.
  (unless (equal (shell-command-to-string "type skerrick") "skerrick not found\n")
    (skerrick-install-or-upgrade-server-binary)))

;; Should be run in a JS buffer; it is buffer specific.
;; (skerrick-start-server)

;; Now main function, entry point is:
;; M-x skerrick-eval-region
;; JavaScript:1 ends here

;; [[file:init.org::#JavaScript][JavaScript:2]]
(require 'js) ;; Defines js-mode-map

;; Evaluate a region, if any is selected; otherwise evaluate the current line.
(bind-key
 "C-x C-e"  (lambda ()
              (interactive)
              (if (use-region-p)
                  (skerrick-eval-region)
                (beginning-of-line)
                (set-mark-command nil)
                (end-of-line)
                (skerrick-eval-region)
                (pop-mark)))
 'js-mode-map)
;; JavaScript:2 ends here

;; [[file:init.org::#devdocs][devdocs:1]]
;; 1. Get docs of a languages: M-x devdocs-install
;; 2. Lookup docs: [C-u] M-x devdocs-lookup
;; 𝟚. Lookup docs: [C-u] C-c d
(use-package devdocs
  :bind ("C-c d" . #'devdocs-lookup)
  :config
  (when nil ;; “C-x C-e” the following once.
    (cl-loop for lang in '(javascript ramda typescript html css sass
                       vue~3 vuex~4 vue_router~4 "angularjs~1.6"
                       nginx webpack~5 web_extensions
                       ;;
                       eslint  jest jq jsdoc prettier
                       mocha chai jasmine
                       ;;
                       bash docker~19 git homebrew elisp
                       ;;
                       postgresql~14 redis sqlite
                       ;;
                       rust ruby~3 minitest "rails~7.0")
          do (devdocs-install (list (cons 'slug (format "%s" lang)))))))
;; devdocs:1 ends here

;; [[file:init.org::#How-do-I-do-something][How do I do something?:1]]
(system-packages-ensure "howdoi")

(cl-defun howdoi (&optional show-full-answer)
  "Instantly insert coding answers.

Replace a query with a code solution; replace it with an entire
answer if a prefix is provided.

Example usage:

   On a new line, write a question such as:

      search and replace buffer Emacs Lisp

   Then invoke ‘M-x howdoi’ anywhere on the line
   to get a code snippet; or ‘C-u M-x howdoi’ to get a full answer to your query.
"
  (interactive "P")
  (let ((query (s-collapse-whitespace (substring-no-properties (thing-at-point 'line))))
        (flag (if show-full-answer "-a" "")))
    (beginning-of-line)
    (kill-line)
    (insert (shell-command-to-string (format "howdoi %s %s" query flag)))))
;; How do I do something?:1 ends here

;; [[file:init.org::#Sleek-Semantic-Selection][Sleek Semantic Selection:1]]
(use-package expand-region
  :bind (("s-r" . #'er/expand-region)))
;; Sleek Semantic Selection:1 ends here

;; [[file:init.org::#LSP-Making-Emacs-into-a-generic-full-featured-programming-IDE][LSP: Making Emacs into a generic full-featured programming IDE:1]]
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

;; [[file:init.org::#LSP-Making-Emacs-into-a-generic-full-featured-programming-IDE][LSP: Making Emacs into a generic full-featured programming IDE:2]]
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

;; [[file:init.org::#Keeping-my-system-up-to-date][Keeping my system up to date:1]]
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
