;; [[file:init.org::#Clean-this-section-up][Clean this section up!:1]]
;; cl-lib was published as a better (namespaced!) alternative to cl, which has a deprecation warning in Emacs27.
;; Yet some old pacakges require cl, and so the below setq silences the deprecation warning.
(setq byte-compile-warnings '(cl-functions))
(require 'cl-lib) ;; to get loop instead of cl-loop, etc.


(setq  work/sqls-connections nil)(setq work/sqls-connections
      '("host=127.0.0.1 port=5432 user=SUPER_SECRET password=ALSO_SUPER_SECRET dbname=YET_AGAIN_SECRET sslmode=disable"))


;; (cl-defun org-duration-to-minutes (&rest _) )
;; (cl-defun org-id-find-id-file (&rest _))

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
                         ("melpa"  . "http://melpa.org/packages/")))

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
  (not (s-contains? "work-machine" (shell-command-to-string "scutil --get ComputerName")))
  "Is this my personal machine, or my work machine?

 At one point, on my work machine I run the following command to give the machine a sensible name.

     sudo scutil --set ComputerName work-machine
     dscacheutil -flushcache")

(defvar my/work-machine? (not my/personal-machine?))

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

;; [[file:init.org::#Being-at-the-Helm-Completion-Narrowing-Framework][ “Being at the Helm” ---Completion & Narrowing Framework:9]]
;; Make `links' from elisp symbols (quoted functions, variables and fonts) in Gnu-Emacs Info viewer to their help documentation.
(use-package inform
  :config (require 'inform))
;;  “Being at the Helm” ---Completion & Narrowing Framework:9 ends here

;; [[file:init.org::#Org-Mode-Administrivia][Org-Mode Administrivia:2]]
(use-package emacs
    :ensure org-contrib
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

;; [[file:init.org::*An “auto read only” detection mechanism ---when jumping to definitions][An “auto read only” detection mechanism ---when jumping to definitions:1]]
;; Usage: Press “M-”. “use-package” below and you can accidentally alter the source code!
;; But in this case you likely just wanted to see the 3ʳᵈ-party definition, not alter it.
;; As such, with this advice, the source will not be alterable (unless you toggle read-only mode).
(advice-add #'xref-find-definitions :after
            (lambda (&rest _)
              (when (--map (s-ends-with? it (f-parent buffer-file-name))
                           '("lisp/emacs-lisp" "/lisp" ".emacs.d/elpa/"))
                (read-only-mode))))
;; An “auto read only” detection mechanism ---when jumping to definitions:1 ends here

;; [[file:init.org::*highlight quoted symbols][highlight quoted symbols:1]]
(use-package highlight-quoted
  :config (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode))

;; If everything worked fine, then “ 'b ” below should be coloured nicely in Emacs Lisp mode.
(when nil
  (-let [x 'somevar]
    (list x 'b "c" :e)))
;; highlight quoted symbols:1 ends here

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
  :init (require 'magit-files)
  :bind (("C-c M-g" . magit-file-dispatch))
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
(ignore-error (unless my/personal-machine?
  (shell-command (format "git config --global user.email \"%s\"" work/email))))
;; Credentials: I am who I am:1 ends here

;; [[file:init.org::#Credentials-I-am-who-I-am][Credentials: I am who I am:2]]
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

;; [[file:init.org::#Get-CheatSheets-and-view-them-easily][Get CheatSheets and view them easily:1]]
(defvar my/cheatsheet/cached-topics nil)
(cl-defun my/cheatsheet (&optional topic)
  "Clone Al-hassy's ⟨TOPIC⟩CheatSheet repository when called from Lisp; visit the pretty HTML page when called interactively.

- Example usage: (my/cheatsheet \"Vue\")
- Example usage: M-x my/cheatsheet RET Vue RET."
  (interactive)
  (if (not topic)
      (browse-url (format "https://alhassy.github.io/%sCheatSheet" (completing-read "Topic: " my/cheatsheet/cached-topics)))
    (push topic my/cheatsheet/cached-topics)
    (maybe-clone (format "https://github.com/alhassy/%sCheatSheet" topic))))
;; Get CheatSheets and view them easily:1 ends here

;; [[file:init.org::#Get-CheatSheets-and-view-them-easily][Get CheatSheets and view them easily:2]]
(mapcar #'my/cheatsheet '("ELisp" "GojuRyu" "Rust")) ; Python Prolog Vue Agda JavaScript
                                              ; Clojure Ruby Oz Coq Cats Haskell FSharp OCaml
;; Get CheatSheets and view them easily:2 ends here

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

;; [[file:init.org::#Word-Completion][Word Completion:3]]
(defun my/ensure-machine-works-as-expected ()
  "Run all my personal tests to ensure Emacs behaves as I expect it to."
  (interactive)
  (load-file "init-test.el")
  (ert t)
  (ert-results-pop-to-timings))
;; Word Completion:3 ends here

;; [[file:init.org::#Word-Completion][Word Completion:4]]
(use-package company-emoji
  :config (add-to-list 'company-backends 'company-emoji))
;; Word Completion:4 ends here

;; [[file:init.org::#Word-Completion][Word Completion:5]]
(use-package emojify
 :config (setq emojify-display-style 'image)
 :init (global-emojify-mode 1)) ;; Will install missing images, if need be.
;; Word Completion:5 ends here

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
;; Column Numbers:1 ends here

;; [[file:init.org::#Column-Numbers][Column Numbers:2]]
;; (setq display-line-numbers-width-start t)
;; (global-display-line-numbers-mode      t)
(global-linum-mode -1)
;; Column Numbers:2 ends here

;; [[file:init.org::#Exquisite-Fonts-and-Themes][Exquisite Fonts and Themes:1]]
;; Treat all themes as safe; no query before use.
(setf custom-safe-themes t)

;; Nice looking themes ^_^
(use-package solarized-theme)
(use-package doom-themes :defer t)
(use-package spacemacs-common
  :defer t
  :ensure spacemacs-theme)
(use-package stimmung-themes :defer t)
(use-package shanty-themes :defer t)
;; Exquisite Fonts and Themes:1 ends here

;; [[file:init.org::#Exquisite-Fonts-and-Themes][Exquisite Fonts and Themes:2]]
;; Infinite list of my commonly used themes.
(setq my/themes '(doom-laserwave shany-themes-light stimmung-themes-light stimmung-themes-dark doom-solarized-light doom-vibrant spacemacs-light solarized-gruvbox-dark solarized-gruvbox-light))
(setcdr (last my/themes) my/themes)
;; Exquisite Fonts and Themes:2 ends here

;; [[file:init.org::#Exquisite-Fonts-and-Themes][Exquisite Fonts and Themes:3]]
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

;; [[file:init.org::#Exquisite-Fonts-and-Themes][Exquisite Fonts and Themes:5]]
(unless noninteractive
  (my/toggle-font "Roboto Mono Light 14")
  (my/toggle-theme 'solarized-gruvbox-light))
;; Exquisite Fonts and Themes:5 ends here

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
                    ("==" . ?≈) ("===" . ?≈) ("=" . ?≔) ;; Programming specific prettifications
                    ("i32" . ?ℤ) ("u32" . ?ℕ) ("f64" . ?ℝ) ;; Rust specific
                    ("bool" . ?𝔹)
                    ("fn" . ?λ)
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
;; (global-preview-it-mode)
;; Preview link under cursor:1 ends here

;; [[file:init.org::#Preview-link-under-cursor][Preview link under cursor:2]]
(quelpa '(goto-line-preview :repo "jcs-elpa/goto-line-preview" :fetcher github))
(global-set-key [remap goto-line] 'goto-line-preview)
;; Preview link under cursor:2 ends here

;; [[file:init.org::#Replace-phrases-with-nice-SVG-labels][Replace phrases with nice SVG labels:1]]
(use-package svg-tag-mode
  :hook (org-mode prog-mode)
  ;; :config (global-svg-tag-mode) ;; Nope: Breaks xwidget-webkit-browse-url, issue#28.
  :config
  (cl-defun my/svg-tag-declare-badge (template face &optional tooltip-message-upon-hover)
    ;; Example faces: 'org-level-1 'org-todo 'font-lock-doc-face
    "Given a TEMPLATE of the shape \"𝑿❙𝒀\", make SVG badge whose tag is 𝑿 and label is 𝒀.

     When `svg-tags-mode' is enabled, every occurence of  \"\\(𝑿\\)\\(𝒀\\)\"
     is replaced by an SVG image essentially displaying “[𝑿∣𝒀]” using the given FACE.
     This badge can be clicked to show all instances in the buffer.
     You can see the badges documentation / intentions / help-message when you hover over it;
     to see TOOLTIP-MESSAGE-UPON-HOVER.

     Both 𝑿 and 𝒀 are regeular expressions; “❙” serves as the SVG tag-label delimiter
     ---i.e., it saves as from writing \"\\(𝑿\\)\\(𝒀\\)\". Moreover, the SVG is only active
     when regexp \"\\(𝑿\\)\\(𝒀\\)\" matches an instance."

    ;; Append tooltip message with a notice on what happens upon click.
    (--> "Click on me to all see occurrences of this badge, in the current buffer!"
         (if tooltip-message-upon-hover (concat tooltip-message-upon-hover "\n\n" it) it)
         (setq tooltip-message-upon-hover it))

    (-let [(tag label) (s-split "❙" template)]
      (-let [click-to-show-all-buffer-occurrences `(lambda () (interactive) (occur (concat ,tag ,label)))]
       ;; Make an SVG for the tag.
       (push
        (cons (format "\\(%s\\)%s" tag label) `((lambda (tag) (svg-tag-make (s-chop-suffix ":" (s-chop-prefixes '("[" "<" "/*")  tag))    :face (quote ,face) :inverse t :margin 0 :crop-right t :crop-left nil))
                                                ,click-to-show-all-buffer-occurrences
                                                ,tooltip-message-upon-hover))
        svg-tag-tags)
       ;; Make an SVG for the label.
       (push
        (cons (format "%s\\(%s\\)" tag label) `((lambda (label) (svg-tag-make (s-chop-suffixes '("]" ">" "*/") label) :face (quote ,face) :crop-left t))
                                                ,click-to-show-all-buffer-occurrences
                                                ,tooltip-message-upon-hover))
        svg-tag-tags))))

  ;; Let's start off empty; then declare badges below.
  (setq svg-tag-tags nil)

  ;; Using caps so that these stick-out somewhat even when svg-tags-mode is not present.
  (my/svg-tag-declare-badge "TODO:❙.*" 'org-todo "This is something I would like to do, in the future.")
  (my/svg-tag-declare-badge "SILLY:❙.*" 'error "I’m experimenting; don't forget to clean-up when you’re done!")
  (my/svg-tag-declare-badge "HACK:❙.*" 'error "This works, but it’s far from ideal. Plan to clean this in the future.")
  (my/svg-tag-declare-badge "FIXME:❙.*" 'org-todo "This is busted! Plan to fix this in the future.")
  (my/svg-tag-declare-badge "NOTE:❙.*" 'org-done "Something to be aware of; to keep in mind.")

  ;; [In]Active Time stamps --- M-x org-time-stamp
  (my/svg-tag-declare-badge "\\[2022-.* ❙.*]" 'org-done "This is an inactive time stamp. It does not trigger the parent entry to appear in the agenda.")
  (my/svg-tag-declare-badge "<2022-.* ❙.*>" 'org-todo "This is an active time stamp. It causes the parent Org entry to appear in the agenda.")

  ;; JavaScript Lint Rules: \* eslint (.*) */
  (my/svg-tag-declare-badge "/\\* eslint ❙.* \\*/" 'org-done "It looks like you’ deviating from common conventions: Tread cautiously!")

  ;; TODO: Make SVG tags for other interesting “2-part” pieces of textual information
  )

;; If everything is setup, the following examples should look like SVGs.
;; NOTE: Do something
;; TODO: fix me later
;; HACK: hiya
;; FIXME: this thing is busted 🎭
;; SILLY: start
;; SILLY: end
;; [2022-04-20 Sun 16:30]
;; <2022-04-20 Sun 16:30>
;; /* eslint eqeqeq: 0, curly: 2 */

;; NOTE: Toggle svg-tags-mode; useful when experimenting with new tags.
;; (progn (svg-tag-mode-off) (svg-tag-mode-on))

;; NOTE: (my/toggle-line-fontification) works fine with svg-tag-mode :-)
;; Replace phrases with nice SVG labels:1 ends here

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
(setq-default fill-column 120         ;; Let's avoid going over 120 columns
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
(use-package auto-highlight-symbol)
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
;; Org-mode ⇐ HTML:3 ends here

;; [[file:init.org::#Programming][Programming:1]]
(when my/work-machine?
  (setq doom-modeline-buffer-file-name-style 'truncate-except-project))
;; Programming:1 ends here

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
;; Select the following then press C-c C-r: fn main() { println!("Hello, World!"); }
;; Quickly Run Code Snippets:2 ends here

;; [[file:init.org::#Quickly-Run-Code-Snippets][Quickly Run Code Snippets:3]]
(use-package rustic)
;; Open any Rust file, and run “M-x lsp” which will then prompt you to install
;; rust-analyzer, the rust LSP.
;;
;; LSP for Rust ⇒ Goto definition (M-. / ⌘-l), code completion with types and
;; docstrings, colourful documentation on hover, “Run [Test] | Debug” overlays,
;; super nice stuff! Run “M-!”/[M-x company-show-doc-buffer] if you want the doc in a colourful buffer.
;;
;; Below, hover over “Vec” and see nice, scrollable, colourful docs on vectors.
;;    let v:Vec<_> = vec![1, 2, 3];

;; The offical Rust toolchain installer
(system-packages-ensure "rustup")
(shell-command "rustup update")
;; Quickly Run Code Snippets:3 ends here

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

;; [[file:init.org::#JavaScript][JavaScript:3]]
(use-package js-comint)
(define-key js-mode-map (kbd "C-x C-e") (lambda () (interactive) (call-interactively #'js-comint-repl) (other-window -1) (js-comint-send-last-sexp)))
;; JavaScript:3 ends here

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

;; [[file:init.org::#Managing-Processes-Servers-from-within-Emacs-Work-specific-functions][Managing Processes/Servers from within Emacs ---Work-specific functions:1]]
;; “M-x prodigy”, then press “s” to start a service; “S” to stop it; “$” to see it; “r”estart
(use-package prodigy :disabled t)
  ;; C-h v prodigy-services ⇒ See possible properties.
;; Managing Processes/Servers from within Emacs ---Work-specific functions:1 ends here

;; [[file:init.org::#my-defaliases][my/defaliases:1]]
(defalias 'defaliases 'my/defaliases)
(defmacro my/defaliases (src &rest tgts)
  "Provide names TGTS as synonymous aliases for SRC, for discovarability.

Often a function SRC can be construed from different perspectives, names, purposes TGTS.
Another example is when I define things with the ‘my/’ prefix, but also want to use them without.

Example use: (my/defaliases view-hello-file greet-others learn-about-the-world)

In particular:  (my/defaliases OLD NEW) ≈ (defalias 'NEW 'OLD)."
  `(--map (eval (quote (defalias `,it (quote ,src)))) (quote ,tgts)))
;; my/defaliases:1 ends here

;; [[file:init.org::#Making-unkillable-buffers-shells][Making unkillable buffers & shells:1]]
(defun my/declare-unkillable-buffer (name)
  (add-hook 'kill-buffer-query-functions
            `(lambda () (or (not (equal (buffer-name) ,name))
                       (progn (message "Not allowed to kill %s, burying instead; otherwise use “M-x force-kill”" (buffer-name))
                              (bury-buffer))))))

(my/defaliases my/force-kill force-kill w-force-kill)
(cl-defun my/force-kill (&optional buffer-name)
  (interactive)
  (-let [kill-buffer-query-functions nil]
    (if buffer-name
        (kill-buffer buffer-name)
      (kill-current-buffer))
    (ignore-errors (delete-window))))

(cl-defun my/run-unkillable-shell (command &optional (buffer-name command))
  "Example use: (my/run-unkillable-shell \"cd ~/my-noejds-project; npm run dev\" \"my-nodejs-project\")"
  (-let [it (get-buffer buffer-name)]
    (if it
        (switch-to-buffer-other-window it)
      (async-shell-command command buffer-name)
      (my/declare-unkillable-buffer buffer-name))))
;; Making unkillable buffers & shells:1 ends here

;; [[file:init.org::#my-work-links][my/work-links:1]]
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
                  ('latex (format "\\href{%s}{FM-%s}" full-url label))
                  (_  full-url))))
    :face '(:foreground "green" :weight bold
            :underline "blue" :overline "blue")))
;; my/work-links:1 ends here

;; [[file:init.org::#w-start-stop-services][w-start/stop-services:1]]
(defvar my/services nil "List of all services defined; used with `w-start-services' and `w-stop-services'.")

(defun w-start-services ()
  (interactive)
  (cl-loop for 𝑺 in my/services
           do (funcall (intern (format "w-start-%s" 𝑺)))))

(defun w-stop-services ()
  (interactive)
  (cl-loop for 𝑺 in my/services
           do (funcall (intern (format "w-stop-%s" 𝑺)))))
;; w-start/stop-services:1 ends here

;; [[file:init.org::#w-status-of-services][w-status-of-services:1]]
;; It takes about ~3 seconds to build the Status of Services page, so let's jump to it if it's already built, and the user/me can request a refresh, if need be.
(global-set-key (kbd "M-S-SPC")
  (lambda () (interactive)
    (-let [buf (get-buffer "Status of Services")] (if buf (switch-to-buffer buf) (w-status-of-services)))))
;;
;; Since M-S-SPC brings up the transient menu, and most commands close the status buffer or are transient, we get the perception that the transient menu is "sticky"; i.e., stuck to the buffer, even though this is not true. I do not yet know how to make a transient menu stuck to a buffer.
;;
;; w-status-of-services:1 ends here

;; [[file:init.org::#w-status-of-services][w-status-of-services:2]]
(defun w-status-of-services ()
  "Show me status of all servers, including their current git branch, and most recent emitted output."
  (interactive)
  (defvar w-status-of-services/branch-name-width 12
    "What is the length of the longest branch name? Let's use that to ensure there's enough whitespace.")
  (-->
   (-let [ shells (--filter (s-starts-with? "Shell" (process-name it)) (process-list)) ]
     (cl-loop for 𝑺 in (mapcar #'pp-to-string my/services)
              for associated-shell = (--find (s-contains? (format "%s" 𝑺) (cl-third (process-command it))) shells)
              for status = (or (ignore-errors (process-status associated-shell)) '💥)
              for branch = (-let [default-directory (format "~/%s" 𝑺)]
                             (magit-get-current-branch))
              for _ = (setq w-status-of-services/branch-name-width (max (length branch) w-status-of-services/branch-name-width))
              for 𝑺-buffer = (--find (s-starts-with? (format "*Server:%s" 𝑺) it) (mapcar 'buffer-name (buffer-list)))
              for saying = (let (most-recent-shell-output (here (current-buffer)))
                             (if (not 𝑺-buffer)
                                 " ─Server not started─ "
                               (switch-to-buffer 𝑺-buffer)
                               (end-of-buffer)
                               (beginning-of-line)
                               (setq most-recent-shell-output (or (thing-at-point 'line t) ""))
                               (switch-to-buffer here)
                               ;; FIXME:here
                               (--> (s-truncate 135 (s-trim most-recent-shell-output))
                                  (if (s-contains? "|" it)
                                      (cl-second (s-split "|" it))
                                    it)
                                  (s-trim it)
                                  (if (<= (length it) 3) (s-repeat 70 " ") it))))
              for _ = (if (or (s-contains? "Error" saying) (not 𝑺-buffer)) (setq status  '💥))
              for keymap = (copy-keymap org-mouse-map)
              do (cl-loop for (key action)
                          on `( ;; Checkout branch/PR
                               c (w-pr-checkout (format "~/%s" ,𝑺))
                               ;; Restart service, remaining on current branch [not switching to “main”!]
                               r (-let [current-prefix-arg t]
                                   (funcall (intern (format "w-stop-%s" ,𝑺)))
                                   (funcall (intern (format "w-start-%s" ,𝑺))))
                               f (-let [default-directory  (format "~/%s" ,𝑺)]
                                   (call-interactively #' projectile-find-file))
                               t (vterm-shell-command (format "clear; cd ~/%s; git status" ,𝑺) (format "vterm/%s" ,𝑺))
                               ;; See the repo in the web
                               w (--> (format "%s" ,𝑺)
                                    (if (s-contains? "/" it) (f-parent it) it)
                                    (format "https://github.com/%s/%s" work/gh-user it)
                                    (browse-url it))
                               ;; Visit service shell
                               <return>
                               (when ,𝑺-buffer
                                 (delete-other-windows)
                                 (split-window-below)
                                 (switch-to-buffer ,𝑺-buffer)
                                 (end-of-buffer)
                                 (other-window 1))
                               ;; See service magit buffer
                               <tab> (progn (magit-status (format "~/%s" ,𝑺)) (delete-other-windows)))
                          by #'cddr
                          do (define-key keymap (kbd (format "%s" key))
                                         `(lambda () (interactive) ,action)))
              collect
              ;; “%𝑾s” ⇒ Print a string with at least width 𝑾: If length(str) ≤ 𝑾, then pad with spaces on the left side.
              ;; Use “%-𝑾s” to instead pad with spaces to the right.
              (list keymap (format (format "%%s %%-20s %%-%ss %%s" (+ 5 w-status-of-services/branch-name-width)) status 𝑺 branch saying))))

   ;; Setup buffer
   (-let [buf "Status of Services"]
     (ignore-errors (kill-buffer buf))
     (switch-to-buffer buf)
     (delete-other-windows)
     it)
   ;; Insert out buttons
   (--each it
     ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Overlay-Properties.html
     (-let [help (s-join "\n"
                         '("Keybindings:"
                           "[C-u] c   ∷  Checkout PR [or branch]   \t\t\t b  ∷  Browse an app"
                           "tab       ∷  See service magit buffer  \t\t\t i  ∷  Inject users"
                           "return    ∷  Visit service shell       \t\t\t s  ∷  SQL buffer"
                           "r         ∷  Restart service           \t\t\t w  ∷  See the repo in the web"
                           "f         ∷  Find file in project      \t\t\t t  ∷ Terminal"
                           "g         ∷  Refresh this view         \t\t\t q  ∷  Quit, and kill, this buffer"))]
       (insert-text-button (s-replace "\"" "″" (s-replace "run" "✅" (nth 1 it)))
                           'face nil
                           ;; 'mouse-face '(:box t) ;; I use the cursor more than the mouse, so don't want two distinct views.
                           'keymap (nth 0 it)
                           ;; NOTE: The functions are called only when the minor mode cursor-sensor-mode is turned on.
                           ;; When cursor enters the button, we temporarily make it a box and show shortcuts in message area.
                           'cursor-sensor-functions `((lambda (_ old-pos entered?)
                                                        (message ,help)
                                                        (setq entered? (equal entered? 'entered))
                                                        (-let [self (button-at (if entered? (point) old-pos))]
                                                          (read-only-mode 0) ;; Temporarily disable help-mode's read-only-mode setup.
                                                          (if entered?
                                                              (button-put self 'face '(:box "yellow" :weight bold))
                                                            (button-put self 'face nil)))))
                           'help-echo help)
       (insert "\n")))
   ;; Do some highlighting, as a cautionary measure.
   (highlight-regexp ".*crashed.*" 'hi-red-b)
   ;; Forbid editing
   (help-mode) ;; This wont do the button face changes I like when cursor moves; so I disable read-only-mode temporarily when making the changes.
   (cursor-sensor-mode)
   (stripe-buffer-mode)
   (visual-line-mode -1)
   (toggle-truncate-lines)
   ;; Add some specific work related bindings
   (local-set-key "b" #'w-browse-app)
   (local-set-key "i" #'w-inject-users)
   (local-set-key "s" (lambda () (interactive) (w-sql) (delete-other-windows)))
   ;; Add general view keys
   (local-set-key "g" (lambda () "Refresh this view" (interactive) (ignore-errors (kill-buffer-and-window)) (w-status-of-services)))
   (local-set-key "q" (lambda ()  "Quit buffer" (interactive) (ignore-errors (kill-buffer-and-window))))
   ;; Go to the first entry, so my “homemade echo menu” appears.
   (beginning-of-buffer)))
;; w-status-of-services:2 ends here

;; [[file:init.org::#my-defservice][my/defservice:1]]
;; Even though I'm doing frequent prunes, it helps to give docker some leeway.
;; NOTE: Docker Icon → Preferences → Resources →  4 CPUs; 8gb Memory; 2gb Swap; 120 DiskImageSize.
(cl-defmacro my/defservice
    (repo &key (main-setup "git checkout main; git pull; git status; hr; npm ci; hr; time docker system prune -af")
          (cmd "npm run docker:dev")
          (example ""))
  "Example use:

   (my/defservice 𝒟 :cmd 𝒞 :example ℰ)
  ⇒
    (w-start-𝒟)    ≈ Unkillable shell: cd 𝒟; 𝒞
    (w-is-up-𝒟?)   ≈ Open browser at ℰ
    (w-stop-𝒟)     ≈ Kill all emacs-buffers & docker-images containing 𝒟 in their name

  (w-[start|stop]-services)  ⇒ Starts/stops all defined services."
  (add-to-list 'my/services repo)
  `(list
     (cl-defun ,(intern (format "w-start-%s" repo)) ()
       "Start server off of ‘main’, with prefix just start server off of current branch."
       (interactive)
       (let ((command (format "cd ~/%s; pwd; hr; %s; hr; %s"
                              (quote ,repo)
                              (if current-prefix-arg "" ,main-setup)
                              ,cmd))
             (buf-name (format "*Server:%s/%s*" (quote ,repo)
                               (if current-prefix-arg "main"
                                 (-let [default-directory ,(format "~/%s" repo)]
                                   (magit-get-current-branch))))))
         (my/run-unkillable-shell
          (format "echo %s; hr; %s" (pp-to-string command) command) ;; Show command being run in output buffer, then run that command
          buf-name)
         (with-current-buffer buf-name (read-only-mode))))

     (cl-defun ,(intern (format "w-stop-%s" repo)) ()
       "Force-kill all unkillable buffers that mention REPO in their name. Also stop any docker services mentioning REPO in their name."
       (interactive)
       (my/docker-stop ,(pp-to-string repo))
       (thread-last (buffer-list)
         (mapcar 'buffer-name)
         (--filter (s-contains-p ,(pp-to-string repo) it))
         (mapcar #'my/force-kill)))

     (if ,example
         (cl-defun ,(intern (format "w-is-up-%s?" repo)) ()
           (interactive)
           (browse-url ,example)
           (message "If the URL is busted, then the repo is not up correctly or the server has an error!")))))
;; my/defservice:1 ends here

;; [[file:init.org::#my-defservice][my/defservice:2]]
(when my/work-machine?
  (load-file "~/Desktop/work.el"))
;; my/defservice:2 ends here

;; [[file:init.org::#Project-management-navigation][Project management & navigation:1]]
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

;; [[file:init.org::#Project-management-navigation][Project management & navigation:2]]
(define-key projectile-mode-map (kbd "C-x p c")
  (defun my/copy-current-file-path ()
    "Add current file path to kill ring."
    (interactive)
    (message (kill-new buffer-file-name))))
;; Project management & navigation:2 ends here

;; [[file:init.org::#Projectile][Projectile:1]]
;; https://cestlaz.github.io/posts/using-emacs-33-projectile-jump/
;; https://github.com/bbatsov/projectile
(use-package projectile
:config (projectile-global-mode))
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; Projectile:1 ends here

;; [[file:init.org::#Are-there-any-errors-in-my-code][Are there any errors in my code?:1]]
(use-package flycheck-status-emoji
  :config
  (load-library "flycheck-status-emoji")
  (diminish-undo 'flycheck-mode)
  (flycheck-status-emoji-mode))
;; Are there any errors in my code?:1 ends here

;; [[file:init.org::#Are-there-any-errors-in-my-code][Are there any errors in my code?:2]]
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

;; [[file:init.org::#Documentation-Pop-Ups][Documentation Pop-Ups:1]]
(use-package company-quickhelp
 :config
   (setq company-quickhelp-delay 0.1)
   (company-quickhelp-mode)
   ;; Especially when learning a new language, looking up its definition/docstring can be helpful.
   ;; Note: I use “M-!” everywhere else to mean “define word at point”.
   (bind-key "M-!" #'company-show-doc-buffer 'prog-mode-map))
;; Documentation Pop-Ups:1 ends here

;; [[file:init.org::#ll-debug][ll-debug:1]]
;; C-u C-v C-d ⇒ Log a message, printing values of expressions.
;; E.g., in JS this prints, console.log("DEBUG-5-del.js","  1 + 3:",1 + 3);
;; Note “5” is the fifth debug message, and “del.js” is the name of the buffer.
;; Works with Rust, Java, Lisps, JS, TS, Clojure, C/C++, Ruby, Matlab/Octave, Shell, Perl.
(use-package ll-debug
  :config
  (bind-key "C-x l" (lambda () (interactive) (ll-debug-insert 1)) #'prog-mode-map))

;; See variable `ll-debug-statement-alist' if you want to know which
;; modes are currently supported by ll-debug. You can add new modes
;; with `ll-debug-register-mode'.
;;
;; If you want to get rid of the debug messages, use
;; `ll-debug-revert'. It finds and removes the lines with the debug
;; output statements, asking for confirmation before it removes
;; anything.
;; ll-debug:1 ends here

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

;; [[file:init.org::*Aggressive Indentation][Aggressive Indentation:1]]
;; Always stay indented: Automatically have blocks reindented after every change.
(use-package aggressive-indent
  :config (global-aggressive-indent-mode t))

;; Use 4 spaces in places of tabs when indenting.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; Aggressive Indentation:1 ends here

;; [[file:init.org::#Being-Generous-with-Whitespace][Being Generous with Whitespace:1]]
(use-package electric-operator
  :diminish
  :hook (c-mode . electric-operator-mode))
;; Being Generous with Whitespace:1 ends here

;; [[file:init.org::#Coding-with-a-Fruit-Salad-Semantic-Highlighting][Coding with a Fruit Salad: Semantic Highlighting:1]]
(use-package color-identifiers-mode
  :config (global-color-identifiers-mode))

;; Sometimes just invoke: M-x color-identifiers:refresh
;; Coding with a Fruit Salad: Semantic Highlighting:1 ends here

;; [[file:init.org::#Text-Folding][Text Folding ---Selectively displaying portions of a program:1]]
(use-package vimish-fold
  :config (vimish-fold-global-mode 1))
;; Text Folding ---Selectively displaying portions of a program:1 ends here

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

;; [[file:init.org::#hr-https-github-com-LuRsT-hr-A-horizontal-for-your-terminal][hr: [[https://github.com/LuRsT/hr][A horizontal for your terminal]]:1]]
(system-packages-ensure "hr") ;; ≈ brew install hr
;; hr: [[https://github.com/LuRsT/hr][A horizontal for your terminal]]:1 ends here

;; [[file:init.org::#Browse-remote-files][Browse remote files:1]]
;; Usage: [Optionally select a region then] M-x browse-at-remote[-kill]
(use-package browse-at-remote)
;; Browse remote files:1 ends here

;; [[file:init.org::#A-nice-Emacs-interface-for-a-portion-of-the-gh-CLI][A nice Emacs interface for a portion of the “gh” CLI:1]]
;; A nice Emacs interface for the a portion of the “gh” CLI.
(my/defaliases my/gh-checkout gh-checkout w-pr-checkout w-branch-checkout)
(cl-defun my/gh-checkout (&optional repo)
  "With prefix, select a branch name; otherwise a Pull Request name.

If no REPO is provided, let the user select one from a menu.
Example use:      (w-pr-checkout \"~/my-repo\")
                  (w-pr-checkout)"
  (interactive)
  (let* ((repo (or repo (completing-read "Repo: " (projectile-relevant-known-projects))))
         (default-directory repo) ;; temporarily override this global variable, used with magit
         (current-branch (magit-get-current-branch))
         (all-branches (magit-list-local-branch-names))
         (status (format "cd %s; gh pr status" repo)))
    (if current-prefix-arg
        (-let [branch (completing-read (format "New branch (Currently “%s”): " current-branch) all-branches)]
          (shell-command-to-string (format "cd %s; git checkout %s" repo branch)))
      (let* ((PR-list (s-split "\n" (shell-command-to-string (format "cd %s; gh pr list" repo))))
             (pr♯ (car (s-split "\t" (completing-read "PR: " PR-list))))
             (_ (shell-command-to-string (format "cd %s; gh pr checkout %s" repo pr♯)))
             (new-branch (magit-get-current-branch)))
        ;; Show nice status
        (async-shell-command status)
        (magit-status repo)))))
;; A nice Emacs interface for a portion of the “gh” CLI:1 ends here

;; [[file:init.org::#https-github-com-sshaw-copy-as-format-copy-as-format-Emacs-function-to-copy-buffer-locations-as-GitHub-Slack-JIRA-etc-formatted-code][[[https://github.com/sshaw/copy-as-format][copy-as-format:]] Emacs function to copy buffer locations as GitHub/Slack/JIRA etc... formatted code.:1]]
;; Usage: [C-u] M-x copy-as-format ⇒ Copies selected region, or current line.
;; Also use: copy-as-format-𝒮, to format to a particular 𝒮tyle.
;; Without suffix 𝒮, format defaults to `copy-as-format-default`.
;; With a prefix argument prompt for the format style 𝒮.
;; Easy to add more formats.
(use-package copy-as-format)
;; [[https://github.com/sshaw/copy-as-format][copy-as-format:]] Emacs function to copy buffer locations as GitHub/Slack/JIRA etc... formatted code.:1 ends here

;; [[file:init.org::#See-all-company-related-PRs][See all company related PRs:1]]
(cl-defun w-PRs (&rest query-options)
  "See all company related PRs"
  (interactive)
  (thread-last `("is:open" "is:pr" "archived:false" "draft:false" ,@work/gh-tags  ,@query-options)
    (mapcar #'url-hexify-string)
    (s-join "+")
    (concat "https://github.com/pulls?q=")
    browse-url))
;;
(cl-loop for (name . query-options)
         in `((month ,(format-time-string "updated:>=%Y-%m-01"))
              (today ,(format-time-string "updated:>=%Y-%m-%d"))
              (created-this-week ,(format "created:>=%s"
                                          (org-read-date nil nil "++1" nil (org-read-date nil t "-sun")))) ;; Date of most recent Monday
              (stale!! ,(format "updated:<=%s" (org-read-date nil nil "-1w"))) ;; Items not touched in over a week
              (mentions-me "mentions:alhassy") ;; i.e., stuff I need to look at
              (involves-me "involves:alhassy")
              (process-manager "label:\"quick and easy\"" "repo:process-builder")
              (newts "label:\"Newts Priority Review\",Newts"))
         do (eval `(cl-defun ,(intern (format "w-PRs-%s" name)) () (interactive) (w-PRs ,@query-options))))
;; See all company related PRs:1 ends here

;; [[file:init.org::#SQL-When-doing-serious-database-work-I-love-using-DBeaver-But-when-I-only][SQL ---via LSP:1]]
;; Installation: go install github.com/lighttiger2505/sqls
(setq lsp-sqls-server "/Users/musa/go/bin/sqls")
(setq lsp-sqls-timeout 1)
(setq lsp-sqls-workspace-config-path nil)
;; https://emacs-lsp.github.io/lsp-mode/page/lsp-sqls/
(setq lsp-sqls-connections
      ;; (--map `((driver . "postgresql") (dataSourceName . ,it)) work/sqls-connections))
      `(((driver . "postgresql") (dataSourceName . ,(car work/sqls-connections)))))
;; TODO: Remove this 'car'!
(add-hook 'sql-mode-hook 'lsp)

(use-package org-modern)
(defun my/execute-query-at-point ()
  "Execute query at point and make resulting table an Org table and modernise it"
  (interactive)
  (lsp-sql-execute-paragraph)
  (other-window 1) (org-modern-global-mode) (org-mode) (read-only-mode -1)
  (while (re-search-forward "^\+" nil t) (replace-match "|"  nil t))
  (toggle-truncate-lines)
  (beginning-of-buffer) (execute-kbd-macro (read-kbd-macro "<tab>"))
  (read-only-mode) (other-window -1)
  (local-set-key "q" (lambda ()  "Quit buffer" (interactive) (ignore-errors (kill-buffer-and-window)))))

;; TODO: FIXME: Debugger entered--Lisp error: (void-variable sql-mode-map)
;; (bind-key "C-c C-c" #'my/execute-query-at-point 'sql-mode-map)
;; (bind-key "C-c C-<return>" #'my/execute-query-at-point 'sql-mode-map)

(defun w-sql ()
  "Quickly run a SQL query, then dispose of the buffer when done.

Uses the first connection available, to change connections
invoke M-x `lsp-sql-switch-connection'."
  (interactive)
  ;; LSP only works on files; not buffers; so I use this file.
  (find-file "~/.emacs.d/scratch.sql")
  (insert work/sql-queries) ;; docs and examples
  (sql-mode)
  (hs-minor-mode -1) ;; I don't want the above comments to be collapsed away.
  (beginning-of-buffer))
;; SQL ---via LSP:1 ends here

;; [[file:init.org::#Docker][Docker:1]]
;; Usage: M-x docker [RET ?]
(use-package docker
  :config
  (my/defaliases docker-containers w-show-docker-containers))

(defun w-stop&remove-docker-containers ()
  (interactive)
  (shell-command "docker stop $(docker ps -a -q)")
  (shell-command "docker rm $(docker ps -a -q)"))

(defun w-postgres-status ()
  (interactive)
  (display-message-or-buffer (s-replace "healthy" "🆙 healthy 🍏" (shell-command-to-string "docker ps -a | grep postgres"))))

(cl-defun w-kill-process-running-on-port (&optional (port (completing-read "Port: " '("3310" "80" "9000" "8000" "8080" "etc, whatever you want"))))
  "We use ‘lsof’ to list open files; as in:  lsof -i :3310 +c0
  The +c0 prints the full name of the command rather than truncating it.

  We then find the PID and kill the process."
  (interactive)
  (-let [process (shell-command-to-string (format "lsof -i :%s +c0" 3310))]
    (-let [pid (ignore-errors (cl-second (s-split " " (cl-second (s-split "\n" process)))))]
      (shell-command (format "kill %s" pid))
      (message process))))
;; Docker:1 ends here

;; [[file:init.org::#my-docker-stop][my/docker-stop:1]]
(defun my/docker-stop (ctr)
  "Stop all containers that mention CTR in their name, image, command, or container id"
  (thread-last (shell-command-to-string "docker ps -a")
    (s-split "\n")
    (--filter (s-contains-p ctr it))
    (--map (car (s-split " " it))) ;; Get docker container ids
    (--map (shell-command (concat "docker stop " it)))))
;; my/docker-stop:1 ends here

;; [[file:init.org::#my-open-in-terminal-'][my/open-in-terminal '⌘:1]]
(defalias 'my/open-in-terminal '⌘)
(cl-defun ⌘ (&rest cmds)
  "Run terminal commands CMDS in a new MacOS Terminal instance, and bring it to focus.

Example: (⌘ \"echo hello\" \"echo world\")

Useful for those cases where I have to interact with non-trivial ‘interactive terminal menus’."
  (shell-command (format "osascript -e 'tell app \"Terminal\" to activate do script %s'"
                         (pp-to-string (s-join ";" cmds)))))

;; (⌘ "echo hello" "echo world")
;; my/open-in-terminal '⌘:1 ends here

;; [[file:init.org::#Check-if-application-APP-is-currently-running-in-use][Check if application APP is currently running, in use.:1]]
(cl-defun my/application-running? (app)
  "Check if application APP is currently running, in use."
  (not (equal "0" (s-trim (shell-command-to-string (format "ps aux | grep -v grep | grep -ci %s" app))))))
;; Check if application APP is currently running, in use.:1 ends here

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
  ;; When I'm typing and possible completitions appear, I can press M-! to see their docstrings in a temporary buffer.
  ;; But on already written words, I'll use “C-u M-.” to toggle having their docstrings in a scrollable&growable tooltip-like-overlay-window.
  ;; Use “bind*” to override Js-mode's dumbjump, which is useless for me
  :bind* ("M-." . (lambda () (interactive)
                    (if (not current-prefix-arg)
                        (call-interactively #'lsp-ui-peek-find-definitions)
                      (if (lsp-ui-doc--visible-p)
                          (lsp-ui-doc-hide)
                        (lsp-ui-doc-show)))))
  :commands lsp)

;; If a server crashes, restart it without asking me.
(setq lsp-restart 'auto-restart)
;; LSP: Making Emacs into a generic full-featured programming IDE:1 ends here

;; [[file:init.org::#LSP-Making-Emacs-into-a-generic-full-featured-programming-IDE][LSP: Making Emacs into a generic full-featured programming IDE:2]]
;; https://emacs-lsp.github.io/lsp-mode/page/languages/
;; M-x lsp-install-server ⟨return⟩ jsts-ls
;; M-x lsp-install-server ⟨return⟩ json-ls
;; M-x lsp-install-server ⟨return⟩ eslint
;; M-x lsp-install-server ⟨return⟩ css-ls
;; M-x lsp-install-server ⟨return⟩ html-ls
;; LSP: Making Emacs into a generic full-featured programming IDE:2 ends here

;; [[file:init.org::#LSP-Making-Emacs-into-a-generic-full-featured-programming-IDE][LSP: Making Emacs into a generic full-featured programming IDE:3]]
(shell-command "npm i -g typescript-language-server; npm i -g typescript")
;; LSP: Making Emacs into a generic full-featured programming IDE:3 ends here

;; [[file:init.org::#LSP-Making-Emacs-into-a-generic-full-featured-programming-IDE][LSP: Making Emacs into a generic full-featured programming IDE:4]]
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
(require 'lsp-mode)
(define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
;; Jump to a symbol's definition in the current workspace with “s-l g a” or “M-g
;; a” (The 'a' stands for apropos, which means appropriate nature)

;; Set the amount of data which Emacs reads from a process.
;; Some LSP responses are in the 8k-3MB range.
;; ⟦ 1 megabyte ≈ 1 million bytes ≈ 1 000 000 bytes ⟧
;; NO! (setq read-process-output-max (* 1024 1024)) ;; ~1mb; [default 4k]
;; NO! (setq gc-cons-threshold (* 2 8 1000 1024)) ;;; ~16mb; default is: 800 000
;; A large gc-cons-threshold will cause freezing and stuttering during long-term
;; interactive use. This one seems to be a good default.
;; LSP: Making Emacs into a generic full-featured programming IDE:4 ends here

;; [[file:init.org::#LSP-Making-Emacs-into-a-generic-full-featured-programming-IDE][LSP: Making Emacs into a generic full-featured programming IDE:5]]
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
;; LSP: Making Emacs into a generic full-featured programming IDE:5 ends here

;; [[file:init.org::#JSON][JSON:1]]
(use-package json-mode)
;; JSON:1 ends here

;; [[file:init.org::#JSON][JSON:2]]
(my/defhydra nil "JSON Browser" gamepad
  :Buffer
  ("p" #'json-mode-show-path "Copy path to field at point")
  ;; ("f" #'json-mode-beautify "Format Buffer")
  ;; ("m"  (lambda () (interactive) (json-pretty-print-buffer t)) "Minify/ugligy buffer")
  ("t"  (lambda  () (interactive)
          (if my/json-hydra/pretty-printed?
              (json-pretty-print-buffer t)
            (json-mode-beautify (point-min) (point-max)))
          (setq my/json-hydra/pretty-printed? (not my/json-hydra/pretty-printed?)))
   "Toggle format/uglify of buffer"
   :toggle (progn (defvar my/json-hydra/pretty-printed? nil)
                  my/json-hydra/pretty-printed?)))

;; TODO: (bind-key "C-c SPC" 'my/hydra/JSON\ Browser/body 'json-mode-map)
;; NOTE: “C-x SPC” is for rectangle editing.
;; JSON:2 ends here

;; [[file:init.org::#w-screencapture][w-screencapture:1]]
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

;; [[file:init.org::#Comment-boxes-up-to-the-fill-column][Comment-boxes up to the fill-column:1]]
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
;; Comment-boxes up to the fill-column:1 ends here

;; [[file:init.org::#Auto-format-on-Save][Auto-format on Save:1]]
(use-package format-all
  ;; To enable format on save for most programming language buffers:
  :hook (prog-mode . format-all-mode)
  :config
  ;; Please use the default formatters; I don't care too much.
  (add-hook 'format-all-mode-hook 'format-all-ensure-formatter))
;; Auto-format on Save:1 ends here

;; [[file:init.org::#Auto-format-on-Save][Auto-format on Save:2]]
;; For JavaScript prettification: It automatically inserts semicolons, forces newlines, inserts parens, etc.
;; Lots of redundant stuff, but stuff to make it easy to work with others.
(shell-command "npm install --global prettier")
;; Specific package to do only JS prettification: https://github.com/prettier/prettier-emacs
;; Auto-format on Save:2 ends here

;; [[file:init.org::#Searching-Hydra][Searching Hydra:1]]
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

;; [[file:init.org::#Peer-Review-Pull-Request-Template-for-Work][Peer Review / Pull Request Template for Work:1]]
(cl-defun w-pr-template ()
  "Hi"
  (interactive)
  (-let [buf "PR Template ~ Press “C-c C-s” when done"]
    (ignore-errors (kill-buffer buf))
    (switch-to-buffer buf)
    (insert "w-pr-template")
    (yankpad-expand)
    (org-mode)
    (beginning-of-buffer)
    (use-local-map (copy-keymap org-mode-map))
    (local-set-key (kbd "C-c C-s")
                   `(lambda ()
                     (interactive)
                     (beginning-of-buffer)
                     (replace-string "[X]" "✅")
                     (beginning-of-buffer)
                     (replace-string "[ ]" "❌")
                     (beginning-of-buffer)
                     (replace-string "[-]" "🚧")
                     (-let [org-export-with-toc nil]
                       (org-md-export-as-markdown)
                       (kill-ring-save (point-min) (point-max)))
                     (kill-buffer-and-window) ;; Kills the new org-md-export buffer
                     (kill-buffer ,buf) ;; Kills this temporary PR template buffer
                     (message "PR notes saved to clipboard in Github markdown")))))
;; Peer Review / Pull Request Template for Work:1 ends here

;; [[file:init.org::*⌘-e: Edit Everything in a separate buffer][⌘-e: Edit Everything in a separate buffer:1]]
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
(bind-key "s-e"
          (lambda ()
            (interactive)
            (or (ignore-errors (call-interactively #'separedit))
                (call-interactively #'edit-indirect-commit)))
          #'edit-indirect-mode-map)

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

;; [[file:init.org::*⌘-e: Edit Everything in a separate buffer][⌘-e: Edit Everything in a separate buffer:7]]
(advice-add #'org-edit-special :before-until
            (lambda (&rest r)
              (when (equal 'table-row (car (org-element-at-point)))
                (call-interactively #'org-table-edit-field))))
;; ⌘-e: Edit Everything in a separate buffer:7 ends here

;; [[file:init.org::*Emphasised Comments][Emphasised Comments:1]]
;; In VSCode, with the “Better Comments” extension, comments starting with a “bang” are made to stand out, via bold red.
;; Let's do the same thing in Emacs.
;;I did not look around, there might be a package/option for this 🤷
(add-hook 'prog-mode-hook (lambda ()
                            (highlight-lines-matching-regexp ".*\\*.*!.*" 'hi-red-b)
                            (highlight-lines-matching-regexp ".*//!.*" 'hi-red-b)))
;; Emphasised Comments:1 ends here

;; [[file:init.org::#COMMENT-Web-Development][Web-Development:1]]
;; Get the repos locally, and use: M-x my/cheatsheet to view the pretty HTML sheets.
(mapcar #'my/cheatsheet '("JavaScript" "Vue" "AngularJS"))
;; Web-Development:1 ends here

;; [[file:init.org::#Quickly-produce-HTML-from-CSS-like-selectors][Quickly produce HTML from CSS-like selectors:2]]
;; USAGE: Place point in an emmet snippet and press C-j to expand it to appropriate tag structure;
;; e.g., #q.x>p C-j. Alternatively, press C-j then start typing an emmet snippet to see it preview live.
;; [C-j is just M-x emmet-expand-line]
;;
(use-package emmet-mode ;; C-j ! RET  === Makes an entire HTML template for you.
  :hook (web-mode . emmet-mode))
;;
;; Please show me an HTML expansion preview as I type
(setq emmet-preview-default t) ;; Press C-j then start typing; e.g., C-j #q.x.y>p>b RET
;;
;; After expanding, positioned the cursor between first empty quotes.
;; The preview can help with tricky CSS precedence rules; e.g., C-j gives the same thing for: a>b+c>d   ==  a>(b+(c>d))
(setq emmet-move-cursor-between-quotes t) ;; E.g., C-j #q[name] RET
;;
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
;; (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
;; Quickly produce HTML from CSS-like selectors:2 ends here

;; [[file:init.org::#Quickly-produce-HTML-from-CSS-like-selectors][Quickly produce HTML from CSS-like selectors:4]]
(cl-defun my/add-emmet-snippet (abbreviation expansion)
  "Add ABBREVIATION as a snippet in `emmet-mode' to be EXPANSION.

Both arguments are strings."
  (add-hook 'emmet-mode-hook
   ;; [Should this be added to “emmet-snippets” variable instead?]
            `(lambda () (puthash ,abbreviation ,expansion emmet-tag-snippets-table))))


(setq emmet-mode-hook nil)

(my/add-emmet-snippet "vue"
"<!doctype html>
<html lang=\"en\">
    <head>
        <title>Salamun Alaykum, world!</title>
        <script src=\"https://unpkg.com/vue@3\"></script>
        <!-- <link rel=\"stylesheet\" type=\"text/css\" href=\"styles.css\" /> -->
        <style type=\"text/css\">
         input, #reply { color: darkcyan; font-size: 14pt }
        </style>
    </head>
    <body>
        <div id=\"Hola\">
            <h1>Number Guessing Game</h1>
            <input type=\"number\" v-model=\"guess\" style=\"width: 25%;\" v-bind:placeholder=`${prompt}`>
            <button v-on:click=\"go(guess)\">Learn Something!</button>
            <div id=\"reply\"> {{reply(guess)}} </div>
        </div>
        <!-- <script src=\"myscripts.js\"></script> -->
        <script type=\"text/javascript\">
         let myApp = Vue.createApp({
             data() {
                 return { guess: null
                        , prompt: \"Enter a guess between 0 and 100\"
                        , secret: Math.floor(Math.random() * 100)
                        }
             },
             methods: { reply(gs) { return gs == this.secret ? \"You win!\" : (gs < this.secret ? \"Too low\" : \"Too high\"); }
                      , go(number) { window.location.href = \"https://www.wolframalpha.com/input?i=\" + number }
                      }
         }).mount('#Hola')
        </script>
    </body>
</html>")

(my/add-emmet-snippet "angular"
"<!doctype html>
<html lang=\"en\" ng-app=\"Hola\">
  <head>
    <title>Salamun Alaykum, world!</title>
    <script src=\"https://ajax.googleapis.com/ajax/libs/angularjs/1.8.2/angular.min.js\"></script>
    <!-- <script src=\"myscripts.js\"></script> -->
    <script type=\"text/javascript\">
      angular.module(\"Hola\", [])
        .controller(\"prompt\",
           ($scope, $window) => {
              $scope.prompt = \"Enter a guess between 0 and 100\"
              $scope.secret = Math.floor(Math.random() * 100)
              $scope.reply  = gs => gs == $scope.secret ? \"You win!\" : (gs < $scope.secret ? \"Too low\" : \"Too high\")
              $scope.go = number => { $window.location.href = \"https://www.wolframalpha.com/input?i=\" + number }
          })
    </script>
    <!-- <link rel=\"stylesheet\" type=\"text/css\" href=\"styles.css\" /> -->
    <style type=\"text/css\">
       input, #reply { color: darkcyan; font-size: 14pt }
    </style>
  </head>
  <body>
    <div ng-controller=\"prompt\">
      <h1>Number Guessing Game</h1>
      <input type=\"number\" ng-model=\"guess\" style=\"width: 25%;\" placeholder=\"{{prompt}}\">
      <button ng-click=\"go(guess)\">Learn Something!</button>
      <div id=\"reply\"> {{reply(guess)}} </div>
    </div>
  </body>
</html>")
;; Quickly produce HTML from CSS-like selectors:4 ends here

;; [[file:init.org::#Quickly-produce-HTML-from-CSS-like-selectors][Quickly produce HTML from CSS-like selectors:5]]
;; A way to show results of trying things out ---when not using a reactive framework.
(my/add-emmet-snippet "message"
"     // Append “text” node to the end of tag with “id”.
     // Example: <button onclick=\"message(\"myID\", \"Hello!\")\"> Speak! </button>
     function message(id, text = \"Hello, world\") {
         const tag = document.createElement(\"p\") // <p></p>
         const textNode = document.createTextNode(text)
         tag.appendChild(textNode); // <p>Hello, world</p>
         const element = document.getElementById(id);
         element.appendChild(tag);
     }")
;; Quickly produce HTML from CSS-like selectors:5 ends here

;; [[file:init.org::#Quickly-produce-HTML-from-CSS-like-selectors][Quickly produce HTML from CSS-like selectors:6]]
(my/add-emmet-snippet "form"
        "<h1> <a href=\"https://www.quackit.com/css/grid/tutorial/form_layout_with_auto_placement.cfm\">
            Automatically aligned form items</a> </h1>

        <form name=\"hola\"  onsubmit=\"go(hola.elements);\">
            <label>Name</label>
            <input name=\"name\" type=\"text\" required/>

            <label>Comments</label>
            <textarea name=\"comments\" maxlength=\"500\"></textarea>

            <input type=\"submit\"/>
        </form>

        <!-- <script src=\"myscripts.js\"></script> -->
        <script>
         let go = form => { alert(`${form.name.value}: “${form.comments.value}”`) }
        </script>

        <!-- <link rel=\"stylesheet\" type=\"text/css\" href=\"styles.css\" /> -->
        <style>
         form {
             /* We want the inputs&labels to be thought of as rows in a grid*/
             display: grid;
             grid-auto-flow: row;
             /* Each row has 2 columns. */
             grid-template-columns: [mylabels] auto [myinputs] 1fr;
             grid-gap: .8em;     /* Distance between form elements */
             background: beige;
             padding: 1.2em;
         }
         /* Let's attach column names to elements */
         form > label  {
             grid-column: mylabels;
             grid-row: auto;
         }
         form > input,
         form > textarea {
             grid-column: myinputs;
             grid-row: auto;
         }
         input, textarea { color: darkcyan; font-size: 14pt }
        </style>")
;; Quickly produce HTML from CSS-like selectors:6 ends here

;; [[file:init.org::#LSP-for-HTML-CSS][LSP for HTML + CSS:2]]
;; When I accidentally duplicate a property in a rule, please report that as an error.
(setq lsp-css-lint-duplicate-properties "error")

;; If I accidentally enter an unknown property (e.g., writing Canadian “colour” instead of American “color”),
;; then I'll be notified with an error notice.
(setq lsp-css-lint-unknown-properties "error")


(use-package lsp-mode
  :hook  ;; Every programming mode should enter & start LSP, with which-key support
         (css-mode . lsp-mode) ;; Enter LSP mode
         (css-mode . lsp))      ;; Start LSP server
;; LSP for HTML + CSS:2 ends here

;; [[file:init.org::#CSS-Property-Argument-Information-in-the-Echo-Area][CSS Property Argument Information in the Echo Area:1]]
;; [USAGE] In a CSS file, place cursor anywhere after the colon (but before ‘;’)
;; in “columns: 0ch;” or in “columns: ” and look at the echo area for how
;; arguments to this property should look like.
(use-package css-eldoc
  :init (progn (require 'css-eldoc) (turn-on-css-eldoc)))
;; CSS Property Argument Information in the Echo Area:1 ends here

;; [[file:init.org::#Show-me-HTML-CSS-Changes-Live-as-I-Type][Show me HTML+CSS Changes /Live as I Type/!:1]]
(use-package impatient-mode)

(use-package web-mode
  :init (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))


;;     C-c C-v: Browse buffer within external browser.
;; C-u C-c C-v: Ensure impatient-mode is enabled for current buffer and browse it WITHIN Emacs.
;; [xwidget-webkit has some bugs; e.g., sometimes buttons that should redirect don't do anything.]
;; [The “C-u” option is useful when I want to “see” the resulting HTML change as I type; e.g., new content or styling.]
;; [Note the “angular” snippet above works beautifully /within/ Emacs; use “b/f” to move backward/forward in the browser.]
(bind-key "C-c C-v"
          (lambda (open-within-emacs) (interactive "P")
            (if (not open-within-emacs)
                (browse-url-of-buffer (current-buffer))
              (unless (process-status "httpd") (httpd-start))
              (unless impatient-mode (impatient-mode))
              (let ((browser (car (--filter (s-starts-with? "*xwidget" (buffer-name it)) (buffer-list))))
                    (file (buffer-name)))
                (when browser (switch-to-buffer browser) (let (kill-buffer-query-functions) (kill-buffer)))
                (split-window-below)
                (other-window -1)
                (xwidget-webkit-browse-url (concat "http://localhost:8080/imp/live/" file))
                (preview-it-mode -1) ;; Looks poor; and I don't need it when writing HTML.
                (other-window -1))))
          'web-mode-map)
;; Show me HTML+CSS Changes /Live as I Type/!:1 ends here

;; [[file:init.org::#Show-me-HTML-CSS-Changes-Live-as-I-Type][Show me HTML+CSS Changes /Live as I Type/!:2]]
(bind-key "M-q" #'sgml-pretty-print 'web-mode-map)
;; Show me HTML+CSS Changes /Live as I Type/!:2 ends here

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

;; [[file:init.org::#Modern-Browsing-within-Emacs][Modern Browsing within Emacs:2]]
(cl-defun internet (&optional (url (concat "https://www." (read-string "https://www."))))
  "Browse to URL using `xwidget-webkit-browse-url'; see also `browse-url'."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (xwidget-webkit-browse-url url))

(my/defhydra "C-c p" "Emacs Browser" gamepad
  :Internet
  ("m" (internet "https://mail.google.com/mail/u/0/#inbox") "gMail"  :exit t)
  ("c" (internet "https://calendar.google.com/calendar/u/0/r") "gCalendar"  :exit t)
  ("e" (internet "https://www.reddit.com/r/emacs/") "Emacs Forum"  :exit t)
  ("b" (internet) "Browse"  :exit t))
;; Modern Browsing within Emacs:2 ends here

;; [[file:init.org::#VueJS][VueJS:1]]
(use-package vue-mode)
;; VueJS:1 ends here

;; [[file:init.org::#Lisp-Helpers-Kill-all-buffers-that-are-not-associated-with-a-file][Lisp Helpers / Kill all buffers that are not associated with a file; also ~C-x ←/→~:1]]
(cl-defun my/clean-buffers ()
  "Kill all buffers that are not associated with a file.
  By convention, such files are named in *earmuffs* style."
  (interactive)
  (ignore-errors (mapcar #'kill-buffer (--filter (s-matches? "\\*.*\\*" it) (mapcar #'buffer-name (buffer-list))))))
;; Lisp Helpers / Kill all buffers that are not associated with a file; also ~C-x ←/→~:1 ends here

;; [[file:init.org::#Lisp-Helpers-Kill-all-buffers-that-are-not-associated-with-a-file][Lisp Helpers / Kill all buffers that are not associated with a file; also ~C-x ←/→~:2]]
(defun my/buffer-predicate (buffer)
  "Run `C-u 0 C-x C-e' on the following form to see all buffer names and find the
   ones annyoning you, then place those in the function body below

        (mapcar #'buffer-name (buffer-list))
"
  ;; First let's kill a bunch of buffers
  ;; (my/clean-buffers) ;; TODO: Bad idea?

  ;; Next let's filter out any remaining ones [Redundant?]
  (defvar my/ignore/buffer/name '("*Quail Completions*" "*Backtrace*" "*Help*" "*agda2*" "*sqls*" "*which-key*" "*Warnings*" "*Messages*" "Status of Services"))
  (defvar my/ignore/buffer/prefix '("*helm" "*Helm"  "*quelpa" "*lsp" "*Occur" "magit" "*Flymake" "*format" "*Shell" "*Async"
                                    "*org-src-fontification:" "*Server:"))
  (defvar my/ignore/buffer/suffix  '("stderr*" "log*" "-ls*"))

  (-let [name (buffer-name buffer)]
    (not (or (member name my/ignore/buffer/name)
             (--any? (s-starts-with? it name) my/ignore/buffer/prefix)
             (--any? (s-ends-with? it name) my/ignore/buffer/suffix)))))

(set-frame-parameter nil 'buffer-predicate 'my/buffer-predicate)
;; Lisp Helpers / Kill all buffers that are not associated with a file; also ~C-x ←/→~:2 ends here

;; [[file:init.org::#Cucumber][Cucumber:1]]
;; Emacs mode for editing Cucumber plain text stories
;; “.feature” files now open up with nice colouring.
(use-package feature-mode)
;;
;; C-c ,g	Go to step-definition under point (requires ruby_parser gem >= 3.14.2)
;;
;; TODO: Ruby specific; but the source could be edited to work for JS.
;; (use-package cucumber-goto-step)
;; Cucumber:1 ends here

;; [[file:init.org::#Let's-jump-to-a-current-Chrome-browser-tab-or-one-from-our-Chrome-history-from-within-Emacs][Let's jump to a current Chrome browser tab, or one from our Chrome history, from within Emacs.:1]]
;; M-x helm-chrome-history
;; [Your Chrome History SQLite database file: helm-chrome-history-file]
(use-package helm-chrome-history)
;; M-x helm-chrome-control
(use-package helm-chrome-control)
;; Let's jump to a current Chrome browser tab, or one from our Chrome history, from within Emacs.:1 ends here

;; [[file:init.org::#Get-Shell-history-within-Emacs-via-Completing-Read-with-Helm][Get Shell history within Emacs via Completing Read with Helm:1]]
;; Usage: M-x helm-shell-history
(use-package helm-shell-history
  :config
  (setq helm-shell-history-file "~/.zsh_history")
  (bind-key "M-r" #'helm-shell-history shell-mode-map))
;; Get Shell history within Emacs via Completing Read with Helm:1 ends here

;; [[file:init.org::#Launch-macOS-apps-with-Helm][Launch macOS apps with Helm:1]]
;; MacOS's default ⌘-SPC does not let us do either of the following scenarios:
;; Usage: M-x helm-osx-app RET preferences bat RET ⇒ See battery preferences settings
;; Another Usage: M-x helm-osx-app RET ⇒ See all apps, maybe we forgot about one of them from an install a long time ago, and open it
;; See https://www.alfredapp.com/ as an alternative (for non-Emacs users), which can do more.
(use-package helm-osx-app)
;; For non-MacOS, we can use [[https://github.com/d12frosted/counsel-osx-app][counsel-osx-app]], whose name is misleading.
;; Launch macOS apps with Helm:1 ends here

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

;; [[file:init.org::#Let's-make-working-with-Emacs-Lisp-even-better][Let's make working with Emacs Lisp even better!:1]]
(use-package elisp-demos
  :config
  ;; Show demos when I do a `C-h o'.
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
  ;; Show demos in tooltips when I pause to select a completion, in Emacs Lisp mode.
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1))
;; Let's make working with Emacs Lisp even better!:1 ends here

;; [[file:init.org::#https-github-com-alphapapa-bufler-el-A-butler-for-your-buffers-Group-buffers-into-workspaces-with-programmable-rules-and-easily-switch-to-and-manipulate-them][[[https://github.com/alphapapa/bufler.el][A butler for your buffers. Group buffers into workspaces with programmable rules, and easily switch to and manipulate them.]]:1]]
(use-package bufler
  :config (bind-key "C-x C-b" #'bufler-list))
;; I still prefer “C-x b” to be “helm-mini”, since when looking for a buffer it also shows me recently visited files.
;; [[https://github.com/alphapapa/bufler.el][A butler for your buffers. Group buffers into workspaces with programmable rules, and easily switch to and manipulate them.]]:1 ends here

;; [[file:init.org::#https-github-com-motform-stimmung-themes-Let's-try-out-this-dope-theme-and-https-github-com-qhga-shanty-themes-shanty-themes-light-this-one-too][[[https://github.com/motform/stimmung-themes][Let's try out this dope theme]] and [[https://github.com/qhga/shanty-themes#shanty-themes-light][this one too!]]:1]]
(unless noninteractive
  (use-package stimmung-themes
    :quelpa (stimmung-themes :fetcher github :repo "motform/stimmung-themes")
    :config (load-theme 'stimmung-themes-light))

  (use-package shanty-themes)
  (load-theme 'shanty-themes-light)

  (setq-default cursor-type 'bar))
;; [[https://github.com/motform/stimmung-themes][Let's try out this dope theme]] and [[https://github.com/qhga/shanty-themes#shanty-themes-light][this one too!]]:1 ends here

;; [[file:init.org::*Fontifying =#+begin_details= blocks as if they were Org blocks][Fontifying =#+begin_details= blocks as if they were Org blocks:1]]
  (defvar my/block-fontifications
        '(("details" . "org"))
      "A cons list of block type and language pairs.

      The intent is that the block types are fontified using the given language name.")

  (defvar osbe--original-match-string (symbol-function 'match-string))

  (cl-defun osbe--match-string (n &optional str)
          (let* ((block-type (string-remove-prefix "_" (funcall osbe--original-match-string 4 str)))
             (fontification (cdr (assoc block-type my/block-fontifications))))
        ;; (message "%s - %s -> %s" n block-type fontification) ;; For debugging.
        (if (and (equal n 7) fontification)
            fontification
          (funcall osbe--original-match-string n str))))

        (advice-add 'org-fontify-meta-lines-and-blocks
            :around (lambda (fontify &rest args)
                  (cl-letf (((symbol-function 'match-string) #'osbe--match-string))
                    (apply fontify args))))
;; Fontifying =#+begin_details= blocks as if they were Org blocks:1 ends here
