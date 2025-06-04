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

(setq use-package-compute-statistics t) ;; So that I can use M-x use-package-report to see how long things take to load.

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

(setq org--docs-from-libraries nil)
(cl-defun org-docs-load-libraries (&rest args) )
;; MA: this is failing for some reason
(when nil use-package exec-path-from-shell
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

;; Auto update buffers that change on disk.
;; Will be prompted if there are changes that could be lost.
(global-auto-revert-mode 1)
;; Auto refreshes every 2 seconds. Don’t forget to refresh the version control status as well.
(setq auto-revert-interval 2
      auto-revert-check-vc-info t
      global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

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
(require 'server)
(unless (server-running-p) (server-start))

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

;; [[file:init.org::#Buffer-defaults][Buffer default mode is org-mode:1]]
(setq-default major-mode 'org-mode)
;; Buffer default mode is org-mode:1 ends here

;; [[file:init.org::#Org-mode's-𝒳-Block-Expansions][Org-mode's ~<𝒳~ Block Expansions:1]]
(require 'org-tempo)
;; Org-mode's ~<𝒳~ Block Expansions:1 ends here

;; [[file:init.org::*No code evaluation upon export][No code evaluation upon export:1]]
;; Ignore all header arguments relating to “:eval”. Do not evaluate code when I export to HTML or LaTeX or anything else.
(setq org-export-use-babel nil)
;; No code evaluation upon export:1 ends here

;; [[file:init.org::#ELisp][ELisp:1]]
;; Evaluation Result OverlayS for Emacs Lisp
(use-package eros :init (eros-mode t))
;; ELisp:1 ends here

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
                 (nano-theme nano-light nano-dark)
                 (pink-bliss-uwu-theme pink-bliss-uwu)
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

(when my/personal-machine?

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
  (system-packages-ensure "font-ibm-plex")

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
  (ignore-errors (my/toggle-font "IBM Plex Mono 12")))

(unless noninteractive
  ;; Breaks Gerrit: (my/toggle-font "Roboto Mono Light 14")
  (my/toggle-theme 'solarized-gruvbox-light))

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
  :hook prog-mode)

(electric-pair-mode 1)

;; The ‘<’ and ‘>’ are not ‘parenthesis’, so give them no compleition.
(setq electric-pair-inhibit-predicate
      (lambda (c)
        (or (member c '(?< ?> ?~)) (electric-pair-default-inhibit c))))

;; Treat ‘<’ and ‘>’ as if they were words, instead of ‘parenthesis’.
(modify-syntax-entry ?< "w<")
(modify-syntax-entry ?> "w>")

(use-package bufler
  :config (bind-key "C-x C-b" #'bufler-list))
;; I still prefer “C-x b” to be “helm-mini”, since when looking for a buffer it also shows me recently visited files.

(use-package all-the-icons
  ;; Install fonts only if they're not already installed.
  ;; Source: https://github.com/domtronn/all-the-icons.el/issues/120#issuecomment-427172073
  :config (let ((font-dest (cl-case window-system
                             (x  (concat (or (getenv "XDG_DATA_HOME")            ;; Default Linux install directories
                                             (concat (getenv "HOME") "/.local/share"))
                                         "/fonts/"))
                             (mac (concat (getenv "HOME") "/Library/Fonts/" ))
                             (ns (concat (getenv "HOME") "/Library/Fonts/" )))))
            (unless (file-exists-p (concat font-dest "all-the-icons.ttf"))
              (all-the-icons-install-fonts 'install-without-asking))))

(defcustom  my/weather-refresh-interval 60
  "Number of minutes between refreshes of weather information."
  :type 'integer)
;; Use (cancel-timer my/weather--timer) to stop this.
(setq my/weather--timer
      (run-with-timer (* 60 5) (* 60  my/weather-refresh-interval)
                      (defun my/weather-update ()
                        (interactive)
                        (setq my/weather-brief (shell-command-to-string "bash -c 'curl -s wttr.in/Niagara+Falls+Canada?format=%c%C+%t'"))
                        ;; (setq my/weather-brief (shell-command-to-string "bash -c 'curl -s wttr.in/Toronto+Canada?format=%c%C+%t+and+windy:+%w'"))
                        (setq my/weather-full-details (shell-command-to-string "bash -c 'curl -s wttr.in/Niagara+Falls+Canada?T'"))
                        (force-mode-line-update))))
;;
(setq my/weather--indicator
      `(:eval
        (propertize (format " %s " my/weather-brief)
              'face 'mode-line-buffer-id
                    'help-echo (concat  ;; "Click to see full details"
                                (propertize "\n" 'face '(:height 0.4))
                                (propertize "[Click]" 'face `(bold (foreground-color . "green"))) " To see detailed weather report\n"
                                (propertize "[M-x my/weather-update]" 'face '(bold (foreground-color . "maroon"))) " To fetch latest weather data"
                                (propertize "\n " 'face '(:height 0.5)))
                    'local-map my/mode-line-weather-map
                    'mouse-face 'mode-line-highlight)))
;;
(add-to-list 'mode-line-misc-info my/weather--indicator)
;; (pop mode-line-misc-info) ;; To remove from the modeline.

(setq my/weather-posframe-visible nil)
(defvar my/mode-line-weather-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
                (lambda (_e)
                  (interactive "e")
                  (use-package posframe)
                  (if my/weather-posframe-visible
                      (posframe-delete "*my/weather-full-details-posframe-buffer*")
                    (posframe-show "*my/weather-full-details-posframe-buffer*"
                                   :string my/weather-full-details
                                   :position (point))
                    (message "Click on the weather modeline icon to close the posframe."))
                  (setq my/weather-posframe-visible (not my/weather-posframe-visible))))
    map))

;; [[file:init.org::*Why Emacs? Because of Org-agenda: /“Write fragmentarily, read collectively”/][Why Emacs? Because of Org-agenda: /“Write fragmentarily, read collectively”/:1]]
;; I like to write everything in one massive file, and the agenda should consult it.
(setq org-agenda-files (list "~/Dropbox/my-life.org"))
;; Why Emacs? Because of Org-agenda: /“Write fragmentarily, read collectively”/:1 ends here

;; [[file:init.org::*Why Emacs? Because of Org-agenda: /“Write fragmentarily, read collectively”/][Why Emacs? Because of Org-agenda: /“Write fragmentarily, read collectively”/:2]]
;; `org-ql' is a Lispy query language for Org files.  It allows you to find Org
;; entries matching certain criteria and return a list of them or perform
;; actions on them.
(use-package org-ql)
;; Why Emacs? Because of Org-agenda: /“Write fragmentarily, read collectively”/:2 ends here

;; [[file:init.org::*Timestamps and their uses][Timestamps and their uses:2]]
(defun my/agenda-for-day ()
  "Call this method, then enter say “-fri” to see tasks timestamped for last Friday."
  (interactive)
  (let* ((date (org-read-date))
         (org-agenda-buffer-tmp-name (format "*Org Agenda(a:%s)*" date))
         (org-agenda-sticky nil)
         (org-agenda-span 'day)
         ;; Putting the agenda in log mode, allows to see the tasks marked as DONE
         ;; at the corresponding time of closing. If, like me, you clock all your
         ;; working time, the task will appear also every time it was worked on.
         ;; This is great to get a sens of what was accomplished.
         (org-agenda-start-with-log-mode t))
    (org-agenda-list nil date nil)))
;; Timestamps and their uses:2 ends here

;; [[file:init.org::*My default ~org-agenda-custom-commands~][My default ~org-agenda-custom-commands~:1]]
;; For each block in my Agenda, only show appointments, and tasks, occurring
;; today. For this week, month, etc, press “v w” or “v m”.
(setq org-agenda-span 'day)

(setq org-agenda-sticky nil)
;; My default ~org-agenda-custom-commands~:1 ends here

;; [[file:init.org::*My default ~org-agenda-custom-commands~][My default ~org-agenda-custom-commands~:2]]
(setq org-agenda-custom-commands
      '(("t" "My list of all TODO entries" tags-todo "-recurring-someday+LEVEL=3"
         ((org-agenda-overriding-header "\nTODOs sorted by state, priority, effort")
          (org-agenda-sorting-strategy '(todo-state-down priority-down effort-up))
          (org-super-agenda-groups (progn
                                     (org-super-agenda-mode t)
                                     '((:name "Important" :and (:priority "A" :not (:todo ("DONE" "CANCELLED"))))
                                       (:name "Process your Inbox" :tag "inbox")
                                       (:name "Approved" :todo "APPROVED")
                                       (:name "Started" :todo "STARTED")
                                       (:name "Waiting" :todo "WAITING")
                                       (:name "Low Priority" :priority "C" :tag "maybe"))))))
        ("a" "Daily Agenda;    Productivity  ≈   ♯DONE / ♯TASKS"
         (
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; New things coming into my life                                         ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (org-ql-block
           '(tags "inbox")
           ((org-ql-block-header "\n📩 Process Inbox: “m” to mark then “B r” to refile marked items 📥\n")))


          ;; MA: Moved this to Weekly Review. ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; High priority 𝒚𝒆𝒕 unscheduled tasks                                           ;;
          ;;                                                                               ;;
          ;; Note to self: When I write this query, I thought “I doubt I have		   ;;
          ;; any hits, this is too silly”.  I was wrong. I had 300+ hits.		   ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; (org-ql-block
          ;;  '(and (priority "A") (not (scheduled)) (not (deadline)))
          ;;  ((org-ql-block-header "\n🔥 High priority 𝒚𝒆𝒕 unscheduled tasks 🚒\n")))


          ;; MA: Moved this to Weekly Review. ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Items to review: Mine for useful info then archieve or delete                 ;;
          ;;                                                                               ;;
          ;; `M-x org-copy` is your friend. Archive an entry as is, but copy the	   ;;
          ;; useful parts.  Archiving is useful for clocking reports from the		   ;;
          ;; past.									   ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; (org-ql-block
          ;;  `(and (done) (not (tags "Top")) (closed :to ,(- (calendar-day-of-week (calendar-current-date))))) ;; i.e.;  :to ,(org-read-date nil nil "-1d")
          ;;  ((org-ql-block-header (propertize "\n📜 Items to review: Mine for useful info then archieve or delete. ☑️\n"
          ;;                                    'help-echo "Press E to toggle seeing ~5 lines of each entry."
          ;;                                    ;; Reduce the number of DONE and archived headlines so agenda operations that skip over these can finish faster.
          ;;                                    ))))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Items explicitly marked as the top focus goals for the month           ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (org-ql-block
           '(tags-local "Top")
           ((org-ql-block-header "\n⚡ Top goals for the month ⚡\n")
            (org-agenda-remove-tags t)))


          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Tasks that would bring me joy                                          ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (org-ql-block
           '(and (tags-local "Happy") (or (scheduled -7) (deadline -7) (not (done))))
           ((org-ql-block-header "\n🤗 I'd be happy if I got the following done this week 🥰\n")
            (org-agenda-remove-tags t)))


          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; What “I've done so far” is all tasks closed this week.                 ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Note the definition of “done” includes all terminal states in my workflow.
          (org-ql-block
           `(and (not (tags "Recurring")) (done) (closed :from ,(- (calendar-day-of-week (calendar-current-date))) :to today)) ;; “start-of-week” /from today/
           ((org-ql-block-header (propertize "\n✅ What I've done so far this week 💯\n" 'help-echo "Press E to toggle seeing ~5 lines of each entry. \n If DONE, mine for useful info then archive or delete."))))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Deadlines: Upcoming tasks I should prepare for                                ;;
          ;;                                                                               ;;
          ;; NOTE: I don't want to use predicate (not (done)) since I want to		   ;;
          ;; see the DONE items as a reminder to myself to actually archive		   ;;
          ;; these tasks.								   ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (org-ql-block
           '(and (deadline auto) (not (tags "Top")))
           ((org-ql-block-header "\n🎯 Deadlines\n")))


          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Whoops, things that I've missed!                                       ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (org-ql-block
           '(and (not (habit)) (not (tags "Top")) (not (done)) (scheduled :to today) (not (scheduled :on today)))
           ((org-ql-block-header "\n📆 Overdue\n")))


          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Things loads into my cognitive memory that I should probably continue? ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (org-ql-block
           '(and
             (todo "STARTED")
             (level '> 1)
             (not (tags-local "Someday" "Top" "SocialCredit"))
             (not (scheduled :from today)))
           ((org-ql-block-header "\n🤡 Please 𝒓𝒆𝒅𝒖𝒄𝒆 the number of (unscheduled) open loops\n")))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Stuff I'd like to do today; but do I actually have the time to do so?  ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (org-ql-block
           '(and (scheduled :on today) (ts :with-time nil))
           ((org-ql-block-header "\n😵‍💫 ﴾ Any time ≈ No time﴿ Scheduled today, but not time-blocked\n")))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; What am I doing today, and when?                                              ;;
          ;;                                                                               ;;
          ;; TODO: Use “agenda*” ?  The agenda* view is the same as agenda		   ;;
          ;; except that it only considers appointments, i.e., scheduled and		   ;;
          ;; deadline items that have a time specification ‘[h]h:mm’ in their		   ;;
          ;; timestamps.								   ;;
          ;; https://orgmode.org/manual/Special-Agenda-Views.html#FOOT172		   ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (agenda ""
                  ((org-agenda-overriding-header
                    "\n🔎 Please focus on 𝒪𝓃𝓁𝓎 these tasks for the day!")
                   (org-agenda-format-date "")
                   (org-agenda-skip-function
                    (lambda nil (org-back-to-heading t)
                      (cl-letf*
                          (((symbol-function 'day)
                            (lambda (org-date-string)
                              (cl-fourth (org-parse-time-string org-date-string))))
                           ((symbol-function 'month)
                            (lambda (org-date-string)
                              (cl-fifth (org-parse-time-string org-date-string))))
                           ((symbol-function 'is-repeating)
                            (lambda (org-date-string)
                              (s-matches? "<[^ ]* [^ ]* [^ ]* [^ ]*\\(d\\|m\\)>"
                                          org-date-string)))
                           ((symbol-function 'before)
                            (lambda (x y)
                              (or (< (month x) (month y))
                                  (and (= (month x) (month y)) (<= (day x) (day y))))))
                           ((symbol-function 'at-least)
                            (lambda (x y) (or (equal y x) (before y x))))
                           ((symbol-function 'skip)
                            (lambda nil (save-excursion (org-end-of-subtree t) (point))))
                           (scheduled (org-entry-get (point) "SCHEDULED"))
                           (today (org-read-date nil nil "+0d"))
                           (non-habit?
                            (not
                             (equal "habit"
                                    (ignore-errors
                                      (downcase (org-entry-get (point) "STYLE"))))))
                           (deadline? (org-entry-get (point) "DEADLINE"))
                           (overdue? (not (or (not scheduled) (at-least scheduled today))))
                           (scheduled-without-time
                            (and scheduled (not (s-matches? "<.* .* .*:.*>" scheduled)))))
                        (when
                            (and non-habit?
                                 (or deadline? overdue? scheduled-without-time
                                     (and scheduled
                                          (or (s-matches? "<[^ ]* [^ ]*>" scheduled)
                                              (s-matches? "<[^ ]* [^ ]* [^ ]*\\(d\\|m\\)>"
                                                          scheduled)))))
                          (skip)))))
                   (org-agenda-current-time-string (s-join "\n\t\t\t\t" (-repeat 3 "⏰⟵⏰⟵⏰⟵⏰⟵⏰⟵⏰⟵⏰⟵⟨ 𝒩ℴ𝓌 ⟩⟶⏰⟶⏰⟶⏰⟶⏰⟶⏰⟶⏰⟶⏰")))
                   ;; :org-agenda-remove-tags t
                   ;; :org-agenda-time-grid nil
                   ;; :org-use-tag-inheritance nil
                   (org-agenda-span 'day)
                   ;; I want work items to automatically have a briefcase next to them.
                   (org-agenda-prefix-format " ○ %t % i%s") ;; The “%i” is the icon on the next line.
                   (org-agenda-category-icon-alist '(("\\`work\\'" ("💼") nil nil :ascent center)))
                   (org-agenda-time-grid
                    '((daily today require-timed) (800 1000 1200 1400 1600 1800 2000) "" ""))))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; A glimpse into later this week.                                               ;;
          ;;                                                                               ;;
          ;; What I've left to do is all incomplete tasks scheduled within the		   ;;
          ;; next 5-𝓃 days, where 𝓃 is the numeral of the current week day.		   ;;
          ;; Mon=1, ⋯, Thu=4, ⋯								   ;;
          ;;                                                                               ;;
          ;; NOTE: org-ql and org-agenda are two implementations of essentially		   ;;
          ;; the same idea.  As such, org-ql doesn't honour all of org-agenda's		   ;;
          ;; configurations.  E.g., org-agenda-sorting-strategy seems to be		   ;;
          ;; honoured when set to todo-state-up, but otherwise ignored.  See		   ;;
          ;; https://github.com/alphapapa/org-ql/issues/79, “Sort entries by due	   ;;
          ;; date when using org-ql-block #79”.  See also				   ;;
          ;; https://github.com/alphapapa/org-ql/issues/370, “Agenda entries are	   ;;
          ;; missing properties necessary for view filtering #370”.			   ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (org-ql-block
           (cl-letf (((symbol-function 'org-ql-view--add-todo-face) (lambda (ignored_todo_state))))
             `(and (not (tags "Recurring" "Happy")) (not (done)) (scheduled :from 1 :to ,(- 7 (calendar-day-of-week (calendar-current-date)))))) ;; “end-of-week” /from today/
           ((org-agenda-sorting-strategy '(timestamp-up)) ;; Sort by any timestamp, early first. ;; ⟵- Not yet honoured, see #79.
            (org-agenda-todo-keyword-format "")  ;; ⟵- Not yet honoured, see #79.
            (org-ql-block-header (propertize
                                  "\n🗯️ Incomplete tasks scheduled later this week\n"
                                  'help-echo "Be aware!"))))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Stuff I'm waiting on others to repond to.                                     ;;
          ;;                                                                               ;;
          ;; TODO: When I enter the WAITING state, add a property WAITING_SINCE		   ;;
          ;; with a timestamp.  Then this query here can inspect that timestamp		   ;;
          ;; and see if it's been over a week.						   ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (org-ql-block
           '(todo "WAITING")
           ((org-ql-block-header "\n💢 I've been waiting on these [for over a week?], send reminder!\n")))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Add more items here                                                    ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ))))



;; NOTE: I find these queries by playing with:  (org-ql-search org-agenda-files '(tags-local "Top"))

;; All entries with a timestamp that looks like “<2025-04-16 Wed 18:00-18:30 .+1w>”, the important part is “.+”.
;; (org-ql-search org-agenda-files '(and (ts) (regexp "<[[:digit:]]\\{4\\}-[[:digit:]]+-[[:digit:]]+ .*\\.\\+.*>")) :title "All of my recurring events")


;; From: https://orgmode.org/manual/Speeding-Up-Your-Agendas.html
(setq org-agenda-ignore-properties '(stats)) ;; This will disable parsing and updating statistic cookies.


;; org-ql-block is mostly a "nice to have" feature. It isn't a high priority to
;; make it 100% compatible with every Org Agenda feature (doing so would
;; practically go against the purpose of org-ql, which began as a
;; reimplementation). [Src:
;; https://github.com/alphapapa/org-ql/issues/79#issuecomment-2360241077]
;;
;;
(setq my/special-org-ql-header "\n🗯️ Incomplete tasks scheduled later this week\n")
;; Hide TODO keyword
(advice-add
 'org-ql-view--add-todo-face
 :around
 (defun my/org-ql-omit-todo-keyword-for-specific-header-block (orig-fn keyword)
   (unless (string-equal org-ql-block-header my/special-org-ql-header)
     (funcall orig-fn keyword))))
;; Hide PRIORITY
(advice-add
 'org-ql-view--add-priority-face
 :around
 (defun my/org-ql-omit-priority-for-specific-header-block (orig-fn keyword)
   (unless (string-equal org-ql-block-header my/special-org-ql-header)
     (funcall orig-fn keyword))))


;; My default sorting strategy
;; Sort by DATE, for any org-ql-block-header
(advice-add
 'org-ql-select :around
 (defun my/org-ql-select--sort-if-needed (orig-fn from query &rest args)
   "Advice around `org-ql-select` to inject :sort '(date) under special block headers."
   (apply orig-fn from query
          (if (and org-ql-block-header
                   (not (equal org-agenda-type 'agenda))
                   (not (equal org-agenda-overriding-header "\n🔎 Please focus on 𝒪𝓃𝓁𝓎 these tasks for the day!")))
              (apply #'plist-put args :sort '(date))
            args))))
;; My default ~org-agenda-custom-commands~:2 ends here

;; [[file:init.org::*My default ~org-agenda-custom-commands~][My default ~org-agenda-custom-commands~:3]]
;; ≡ Enable folding via indentation in normal agenda buffer ≡
;; So that I can easily “TAB” to toggle folding sections 😉
(use-package origami
  :bind (:map org-agenda-mode-map ("<tab>" . origami-toggle-node))
  :hook org-agenda-mode)
;;
;; Alternatives: my/auto-set-selective-display-mode or (set-selective-display 1)
;; My default ~org-agenda-custom-commands~:3 ends here

;; [[file:init.org::*Getting Started with org-agenda][Getting Started with org-agenda:1]]
;; Jump straight to my 𝓪genda dashboard ---no dispatch menu please!
(bind-key
 "C-c a"
 (defun my/org-agenda ()
   (interactive)
   (org-agenda nil "a")
   (when org-super-agenda-mode
     (org-super-agenda-mode -1)
     (org-agenda-redo))
   (beginning-of-buffer)))

;; I want the following to happen whenever I do “g” or “/” in the agenda.
(add-hook 'org-agenda-finalize-hook
          (defun my/agenda-look-nice ()
            (-let [fill-column 120]
              (olivetti-mode))
            (message "Press “/ -Work” to hide all :Work: entries.")))


;; Reduce unhelpful visual clutter.
;;
;; Everything in my agenda is something I need to do, so no need to show me the actual todo state.
;; The TODO state is useful for filters, but after the filter it's not useful to see.
;; (To hide priorities as well, maybe not advisble, see https://emacs.stackexchange.com/a/61451)
(setq org-agenda-todo-keyword-format "")
;; Getting Started with org-agenda:1 ends here

;; [[file:init.org::*Agenda Dashboard: Buttons & Random Quote][Agenda Dashboard: Buttons & Random Quote:1]]
;; For testing:
;; (aql "hola" :query [(= TODO "I don't care, just show me the dashboard please")] :interactive t)
;;
(cl-defun my/insert-button (label action &key (foreground "white") (background "blue") (echo "This is a custom button"))
  "Insert a styled button at point"
  ;; Example Use:
  ;; (my/insert-button "Hello" (lambda (pos) (message (format "Hello at %s" pos))) :foreground "cyan" :background nil :echo "Press me!!")
  (interactive)
  ;; TODO: Use lf-define instead
  (cl-assert (stringp label) t "my/agenda-button: First arg should be a string")
  (cl-assert (functionp action) t "my/agenda-button: Second arg should be a lambda")
  (let ((start (point)))
    (insert-text-button
     label
     'action action
     'follow-link t
     :type (define-button-type 'custom-button
             ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html
             'face (list :foreground foreground
                         :background background
                         :slant 'italic
                         :weight 'bold
                         :box '(:line-width 2 :style released-button))
             'help-echo echo))))

(add-to-list
 'org-agenda-finalize-hook
 (cl-defun my/extra-agenda-prose ()

   (goto-char 1) ;; Sometimes this is not honoured by org-agenda, e.g., when changing state of a task
   (when (= (point) 1) ;; i.e., only do this once, when the buffer was created.
     (my/insert-button "New Journal Entry"
                       (lambda (pos) (my/capture-journal-entry))
                       :foreground "cyan"
                       :background nil
                       :echo "I enjoy rereading my journal and reliving nice memories ᕦ( ᴼ ڡ ᴼ )ᕤ")
     ;; TODO:? “See all `:Work:` open loops” button?
     (insert "\t")
     (my/insert-button "Consume Content"
                       (lambda (pos) (my/consume-content))
                       :foreground "cyan" :background nil
                       :echo "Get a random subtree from “Consume Content”")
     (insert "\t")
     (my/insert-button "Tell me a funny!"
                       (lambda (pos) (message (dad-joke-get)))
                       :foreground "cyan"
                       :background nil
                       :echo "Smile, it's a form of charity!")
     (insert "\t")
     (my/insert-button "Random WikiShia"
                       (lambda (pos) (browse-url "https://en.wikishia.net/view/Special:Random"))
                       :foreground "cyan"
                       :background nil
                       :echo "Learn, grow!")
     (insert "\t")
     (my/insert-button "Search" ;; TODO: As I learn more agenda search features, I'll make this into a completing-read menu?
                       (lambda (pos) (org-agenda nil "t"))
                       :foreground "pink"
                       :background nil
                       :echo "All TODOs I'd like to actually work on, so as to have a meaningful life")
     ;;
     ;; NOTE: ‘someday’ things sometimes go into my quote system so that I run into them sometime; lol likewise for things I want to remember
     ;;
     (insert "\n")
     (setq my/quote-start (point))
     (setq my/quote-end nil)
     (insert-text-button
      " " ;; Populated via “ display ”; but must be non-empty.
      'action (lambda (&rest args)
                (read-only-mode -1)
                (put-text-property my/quote-start my/quote-end 'display (my/string-fill-column-and-center 70 (my/random-quote)))
                (read-only-mode +1))
      'display (format "\n%s\n" (my/string-fill-column-and-center 70 (my/random-quote)))
      'follow-link t
      :type (define-button-type 'custom-button
              ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html
              'face (list :foreground  "forest green" :slant 'italic)
              'help-echo "Click to see another random quote!"))
     (setq my/quote-end (point))
     (insert "\n\n"))))

(defun my/string-fill-column-and-center (width str)
  (with-temp-buffer
    (insert str)
    (-let [fill-column width] (fill-paragraph))
    (center-region (point-min) (point-max))
    (buffer-string)))
;;
;; Example usage:
(my/string-fill-column-and-center 27 "“We don't think about sinning as you don't think about eating rotten food.” ---Imam As-Sadiq")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MY RANDOM QUOTE                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: When :use_body: is present, get the body but also split the body by “\n\s*-----*”, a line of at least 5 dashes.
(defun my/org-get-title-or-body ()
  "Get the title of an Org heading, or its body if tagged with :use_body:."
  (interactive)
  (save-excursion
    (when (org-at-heading-p)
      (let ((tags (org-get-tags))
            (title (substring-no-properties (org-get-heading t t t t))) ;; Get clean title without keywords or tags
            (body (progn
                    (org-end-of-meta-data)
                    (buffer-substring-no-properties
                     (point)
                     (org-end-of-subtree t t)))))
        (if (member "use_body" tags)
            (string-trim body)
          title)))))


(defun org-get-random-leaf-headline-or-body ()
  "
+ Org headlines that have no nested items are called `leaf nodes'.
+ Nesting of sections does not matter.
+ Items marked :use_body: have their body returned instead of title.
+ Sections with children have only their children consulted, as such we can nest arbitrarily without any issues.
+ COMMENTED-out headlines will not be considered
"
  (interactive)
  (let ((headlines '()))
    (org-map-entries
     (lambda ()
       (unless (org-goto-first-child) ;; Check if the heading has no children
         (push (my/org-get-title-or-body) headlines)))
     nil       ; no tag search, default is nil
     nil       ; use entire buffer for search, default is nil
     'comment) ; skip commented headlines
    (seq-random-elt headlines)))


(defun my/random-quote ()
  (interactive)
  (save-excursion
    (save-restriction
      ;;
      (find-file "~/.emacs.d/quotes.org")
      (widen)
      (-let [quote (org-get-random-leaf-headline-or-body)]
        (kill-buffer)
        quote))))
;; Agenda Dashboard: Buttons & Random Quote:1 ends here

;; [[file:init.org::*Agenda Variables][Agenda Variables:1]]
;; When I clock into a tasg, a “:LOGBOOK:” drawer is created to hold the timing meta-data for the task.
;; When I make document something I've learned with “C-c C-z”, the resulting note should also go into “:LOGBOOK:”.
(setq org-log-into-drawer t)

(setq org-agenda-span 'day)

(setq  org-fold-catch-invisible-edits 'show-and-error ;; Avoid accidental edits to folded sections
       org-special-ctrl-a/e t ;; C-a/C-e know about leading “*” and ending :tags:
       ;; Agenda styling
       org-agenda-tags-column -80
       org-agenda-time-grid '((daily today require-timed)
                              (800 1000 1200 1400 1600 1800 2000)
                              " ───── " "───────────────")
       org-agenda-current-time-string "◀── now ─────────────────────────────────────────────────")
;; Agenda Variables:1 ends here

;; [[file:init.org::*How tasks look in org agenda][How tasks look in org agenda:1]]
;; Start each agenda item with ‘○’, then show me it's %timestamp and how many
;; times it's been re-%scheduled.
(setq org-agenda-prefix-format " ○ %?-12t%-6e%s ")

(setq org-agenda-deadline-leaders '("DUE:       " "In %3d d.: " "%2d d. ago:  "))

(setq org-agenda-scheduled-leaders
      '(""                ;; Don't say “Scheduled ⟨Task⟩”, just show “⟨Task⟩”.
        "Overdue%2dx "))  ;; If something's overdue, say “Overdue 𝓃× ⟨Task⟩”.
;; How tasks look in org agenda:1 ends here

;; [[file:init.org::*Show me the agenda when I've been idle for 10 minutes][Show me the agenda when I've been idle for 10 minutes:1]]
;; Stop this with:  (cancel-function-timers 'my/pop-up-agenda-timer)
(setq my/pop-up-agenda-timer (run-with-idle-timer (* 60 30) t 'my/org-agenda))
;; Show me the agenda when I've been idle for 10 minutes:1 ends here

;; [[file:init.org::*Get in-Emacs notifications of upcoming appointments by running (org-agenda-to-appt)][Get in-Emacs notifications of upcoming appointments by running (org-agenda-to-appt):1]]
(setq appt-display-duration 30) ;; Show reminder window for 30 seconds please

(setq appt-message-warning-time 12) ;; Show me a warning 12 minutes before an appointment

(setq appt-display-interval 3) ;; Display warning every 3 minutes

;; Ensure all of my Org entries are part of appt whenever it checks for an appointment
(advice-add 'appt-check :before (lambda (&rest args) (org-agenda-to-appt t)))

;; `appt-activate' eagerly runs every minute, slow it down to once every 10 minutes
;; Also, don't do anything when I save a file (namely, `appt-update-list').
(advice-add 'appt-activate :after
            (lambda (&rest args)
              (remove-hook 'write-file-functions #'appt-update-list)
              (timer-set-time appt-timer (current-time) 600)))
;; Get in-Emacs notifications of upcoming appointments by running (org-agenda-to-appt):1 ends here

;; [[file:init.org::*Holy Days & Holidays][Holy Days & Holidays:2]]
(defun my/holiday-islamic (month day event-title url-to-learn-more)
  "Make Islamic holidays clickable, and they open URL-TO-LEARN-MORE."
  (list 'holiday-islamic month day (propertize event-title
                                               'local-map
                                               (let ((keymap (make-sparse-keymap)))
  (define-key keymap (kbd "<down-mouse-1>")
              `(lambda() (interactive) (browse-url ,url-to-learn-more)))
  keymap))))

  (setq calendar-holidays
        `(
          ;; Islamic Holy Days :: https://www.webcal.guru/en/event_list/holidays_islamic_shia?year=2025

          ;; ﴾1﴿ Muharram, “The Sacred Month” ⨾⨾ ≈ July ’25, Mid-June ’26, June ’27
          ,(my/holiday-islamic 1 1  "💔🥀 Mourning of Muharram starts" "https://en.wikipedia.org/wiki/Mourning_of_Muharram")
          ,(my/holiday-islamic 1 2  "💔🥀 Arrival of Imam Husayn ibn Ali in Karbalā, 61 AH" "https://en.wikipedia.org/wiki/Husayn_ibn_Ali")
          ,(my/holiday-islamic 1 3  "💔🥀 Water supply to the camp of Husayn ibn Ali was stopped" "https://en.wikipedia.org/wiki/Husayn_ibn_Ali")
          ,(my/holiday-islamic 1 7  "💔🥀 Stored water in the tents of the camp of Husayn ibn Ali runs out" "https://en.wikipedia.org/wiki/Husayn_ibn_Ali")
          ,(my/holiday-islamic 1 10 "💔🥀 Day of Ashura" "https://ar.wikipedia.org/wiki/%D8%B9%D8%A7%D8%B4%D9%88%D8%B1%D8%A7%D8%A1")
          ,(my/holiday-islamic 1 12 "💔🥀 Burial of the martyrs of Karbala by Bani Asad" "https://en.wikipedia.org/wiki/Banu_Asad_ibn_Khuzaymah")
          ,(my/holiday-islamic 1 17 "🐘 Abraha attacked the Kaʿbah in the Year of the Elephant" "https://en.wikipedia.org/wiki/Abraha")
          ,(my/holiday-islamic 1 18 "🕌 Changing of the Qibla, the direction of prayer" "https://en.wikipedia.org/wiki/Qibla")
          ,(my/holiday-islamic 1 25 "💔🥀 Martyrdom of Imam Ali ibn Husayn Zayn al-Abidin, 95 AH 🖤" "https://en.wikipedia.org/wiki/Ali_ibn_Husayn_Zayn_al-Abidin")

          ;; ﴾2﴿ Safar, “Void”
          ,(my/holiday-islamic 2 1  "💔🥀 Prisoners of Karbalā reach Yazid's palace in Syria" "https://en.wikipedia.org/wiki/Battle_of_Karbala")
          ,(my/holiday-islamic 2 1  "⚔️ Battle of Siffin, 37 AH" "https://en.wikipedia.org/wiki/Battle_of_Siffin")
          ,(my/holiday-islamic 2 7  "🥳 Birth of Imam Musa al-Kadhim, 128 AH" "https://en.wikipedia.org/wiki/Musa_al-Kadhim")
          ,(my/holiday-islamic 2 10 "💔🥀 Martyrdom of Ruqayyah bint Husayn" "https://en.wikipedia.org/wiki/Ruqayyah_bint_Husayn")
          ,(my/holiday-islamic 2 10 "⚔️ Victory to Ali in the Battle of Nahrawan" "https://en.wikipedia.org/wiki/Battle_of_Nahrawan")
          ,(my/holiday-islamic 2 12 "🥳 Birth of Salman the Persian" "https://en.wikipedia.org/wiki/Salman_the_Persian")
          ,(my/holiday-islamic 2 17 "💔🥀 Martyrdom of Imam Ali ar-Ridha, 203 AH" "https://en.wikipedia.org/wiki/Ali_al-Ridha")
          ,(my/holiday-islamic 2 20 "💔🥀 Ar'baeen, 40th day after Ashura 🖤" "https://en.wikipedia.org/wiki/Arba%CA%BDeen")
          ,(my/holiday-islamic 2 28 "💔🥀 Martyrdom of Imam Hasan ibn Ali, 50 AH" "https://en.wikipedia.org/wiki/Hasan_ibn_Ali")
          ,(my/holiday-islamic 2 28 "💔🥀 Martyrdom of Prophet Muhammad, 11 AH" "https://en.wikipedia.org/wiki/Muhammad")

          ;; ﴾3﴿ Rabi' al-Awwal, “The First Spring”
          ,(my/holiday-islamic 3 4  "💔🥀 Martyrdom of Fatimah bint Musa" "https://en.wikipedia.org/wiki/Fatimah_bint_Musa")
          ,(my/holiday-islamic 3 8  "💔🥀 Martyrdom of Imam Hasan al-Askari, 260 AH" "https://en.wikipedia.org/wiki/Hasan_al-Askari")
          ,(my/holiday-islamic 3 9  "🥳 Eid-e-Zahra" "https://en.wikipedia.org/wiki/Eid-e-Shuja%27")
          ,(my/holiday-islamic 3 17 "🥳 Birth of Imam Ja'far al-Sadiq, 83 AH" "https://en.wikipedia.org/wiki/Ja%27far_al-Sadiq")
          ,(my/holiday-islamic 3 17 "🥳 Mulad-al-Nabi: Birth of Prophet Muhammad, 53 BH" "https://en.wikipedia.org/wiki/Mawlid")
          ,(my/holiday-islamic 3 18 "🥳 Birth of Umm Kulthum bint Ali" "https://en.wikipedia.org/wiki/Umm_Kulthum_bint_Ali")

          ;; ﴾4﴿  Rabi' al-Thani, “The Second Spring”
          ,(my/holiday-islamic 4 18 "🥳 Birth of Imam Hasan al-Askari, 232 AH" "https://en.wikipedia.org/wiki/Hasan_al-Askari")

          ;; ﴾5﴿ Jumada al-Awwal, “The first of parched land”
          ,(my/holiday-islamic 5 10 "⚔️ Battle of the Camel" "https://en.wikipedia.org/wiki/Battle_of_the_Camel")
          ,(my/holiday-islamic 5 13 "💔🥀 Martyrdom of Sayedda Fatimah bint Muhammad, 11 AH" "https://en.wikipedia.org/wiki/Fatimah")

          ;; ﴾6﴿ Jumada al-Thani, “The second of parched land”
          ,(my/holiday-islamic 6 13 "💔🥀 Death of Umm ul-Banin (mother of Abbas ibn Ali)" "https://en.wikipedia.org/wiki/Umm_al-Banin")
          ,(my/holiday-islamic 6 20 "🥳 Birth of Sayedda Fatimah bint Muhammad, 8 BH" "https://en.wikipedia.org/wiki/Fatimah")
          ,(my/holiday-islamic 6 26 "💔🥀 Martyrdom of Imam Ali al-Hadi" "https://en.wikipedia.org/wiki/Ali_al-Hadi")

          ;; ﴾7﴿ Rajab, “Respect”
          ,(my/holiday-islamic 7 1  "🥳 Birth of Imam Muhammad al-Baqir, 57 AH" "https://en.wikipedia.org/wiki/Muhammad_al-Baqir")
          ,(my/holiday-islamic 7 10 "🥳 Birth of Imam Muhammad al-Taqi, 195 AH" "https://en.wikipedia.org/wiki/Muhammad_al-Jawad")
          ,(my/holiday-islamic 7 13 "🥳 Birth of Imam Ali ibn Abi Talib, 23 BH" "https://en.wikipedia.org/wiki/Ali")
          ,(my/holiday-islamic 7 15 "💔🥀 Martyrdom of Imam Ja'far al-Sadiq" "https://en.wikipedia.org/wiki/Ja%27far_al-Sadiq")
          ,(my/holiday-islamic 7 18 "💔🥀 Death of Prophet Abraham" "https://en.wikipedia.org/wiki/Abraham")
          ,(my/holiday-islamic 7 20 "🥳 Birth of Sukaynah bint Husayn" "https://en.wikipedia.org/wiki/Ruqayyah_bint_Husayn")
          ,(my/holiday-islamic 7 24 "🥳 Birth of Ali al-Asghar ibn Husayn" "https://en.wikipedia.org/wiki/Ali_al-Asghar_ibn_Husayn")
          ,(my/holiday-islamic 7 25 "💔🥀 Martyrdom of Imam Musa al-Kadhim" "https://en.wikipedia.org/wiki/Musa_al-Kadhim")
          ,(my/holiday-islamic 7 26 "💔🥀 Martyrdom of Imam Abu Talib" "https://en.wikipedia.org/wiki/Abu_Talib")
          ,(my/holiday-islamic 7 27 "🌟 Miʻrāj & day of Mabʻath" "https://en.wikipedia.org/wiki/Isra_and_Mi%27raj")
          ,(my/holiday-islamic 7 28 "💔🥀 Husayn ibn ‘Alī started his journey to Karbalā from Madinah in 60 AH" "https://en.wikipedia.org/wiki/Husayn_ibn_Ali")

          ;; ﴾8﴿ Sha'aban, “Scattered”
          ,(my/holiday-islamic 8 1  "🥳 Birth of Zaynab bint Ali, 6 AH" "https://en.wikipedia.org/wiki/Zaynab_bint_Ali")
          ,(my/holiday-islamic 8 3  "🥳 Birth of Imam Husayn ibn Ali, 4 AH" "https://en.wikipedia.org/wiki/Husayn_ibn_Ali")
          ,(my/holiday-islamic 8 4  "🥳 Birth of Abbas ibn Ali, 36 AH" "https://en.wikipedia.org/wiki/Abbas_ibn_Ali")
          ,(my/holiday-islamic 8 5  "🥳 Birth of Imam Ali ibn Husayn Zayn al-Abidin, 37 AH" "https://en.wikipedia.org/wiki/Ali_ibn_Husayn_Zayn_al-Abidin")
          ,(my/holiday-islamic 8 11 "🥳 Birth of Ali al-Akbar ibn Husayn" "https://en.wikipedia.org/wiki/Ali_al-Akbar_ibn_Husayn")
          ,(my/holiday-islamic 8 14 "🥳 Birth of Qasim ibn Hasan" "https://en.wikipedia.org/wiki/Qasim_ibn_Hasan")
          ,(my/holiday-islamic 8 14 "🌟 Laylat al-Bara'at" "https://en.wikipedia.org/wiki/Mid-Sha%27ban")
          ,(my/holiday-islamic 8 14 "🌟 Shab-e-barat" "https://en.wikipedia.org/wiki/Shab-e-barat")
          ,(my/holiday-islamic 8 15 "🥳 Birth of Imam Muhammad al-Mahdi" "https://en.wikipedia.org/wiki/Muhammad_al-Mahdi")

          ;; ﴾9﴿ Ramadan, “Burning Heat; The Month of Fasting”
          ,(my/holiday-islamic 9 4  "📜 Descending of the Torah" "https://en.wikipedia.org/wiki/Torah")
          ,(my/holiday-islamic 9 10 "💔🥀 Death of Khadijah bint Khuwaylid" "https://en.wikipedia.org/wiki/Khadijah_bint_Khuwaylid")
          ,(my/holiday-islamic 9 12 "📜 Descending of the Gospel" "https://en.wikipedia.org/wiki/Gospel")
          ,(my/holiday-islamic 9 14 "💔🥀 Martyrdom of Mukhtar ibn Abi Ubayd Al-Thaqafi" "https://en.wikipedia.org/wiki/Mukhtar_al-Thaqafi")
          ,(my/holiday-islamic 9 15 "🥳 Birth of Imam Hasan ibn Ali" "https://en.wikipedia.org/wiki/Hasan_ibn_Ali")
          ,(my/holiday-islamic 9 17 "⚔️ Battle of Badr" "https://en.wikipedia.org/wiki/Battle_of_Badr")
          ,(my/holiday-islamic 9 18 "📜 Descending of the Psalms" "https://en.wikipedia.org/wiki/Psalms")
          ,(my/holiday-islamic 9 19 "📜 1st night of Laylat al-Qadr" "https://en.wikipedia.org/wiki/Qadr_Night")
          ,(my/holiday-islamic 9 20 "🌟 Victorious Conquest of Mecca" "https://en.wikipedia.org/wiki/Conquest_of_Mecca")
          ,(my/holiday-islamic 9 21 "📜 2nd night of Laylat al-Qadr" "https://en.wikipedia.org/wiki/Qadr_Night")
          ,(my/holiday-islamic 9 23 "📜 3rd night of Laylat al-Qadr" "https://en.wikipedia.org/wiki/Qadr_Night")
          ,(my/holiday-islamic 9 28 "🌟 Jumu'atul-Wida" "https://en.wikipedia.org/wiki/Jumu%27atul-Wida")

          ;; ﴾10﴿ Shawwal, “Raised”
          ,(my/holiday-islamic 10 1  "🥳 Eid al-Fitr" "https://en.wikipedia.org/wiki/Eid_al-Fitr")
          ,(my/holiday-islamic 10 2  "⚔️ Battle of the Trench" "https://en.wikipedia.org/wiki/Battle_of_the_Trench")
          ,(my/holiday-islamic 10 8  "💔🥀 Day of Sorrow" "https://en.wikipedia.org/wiki/Day_of_Sorrow")
          ,(my/holiday-islamic 10 9  "🥳 Marriage of Khadijah bint Khuwaylid to Muhammad" "https://en.wikipedia.org/wiki/Khadija_bint_Khuwaylid")
          ,(my/holiday-islamic 10 10 "🌟 Major Occultation of Muhammad al-Mahdi begins" "https://en.wikipedia.org/wiki/Major_Occultation")
          ,(my/holiday-islamic 10 15 "⚔️ Martyrdom of Hamzah in the Battle of Uhud, 3 AH" "https://en.wikipedia.org/wiki/Hamza_ibn_%E2%80%98Abd_al-Muttalib")
          ,(my/holiday-islamic 10 29 "🥳 Birth of Abu Talib" "https://en.wikipedia.org/wiki/Abu_Talib_ibn_%E2%80%98Abd_al-Muttalib")

          ;; ﴾11﴿ Dhu al-Qi'dah, “The Month of Truce”
          ,(my/holiday-islamic 11 1  "🥳 Birth of Fatimah bint Musa" "https://en.wikipedia.org/wiki/Fatimah_bint_Musa")
          ,(my/holiday-islamic 11 6  "🥳 Treaty of Hudaybiyyah was executed, 6 AH" "https://en.wikipedia.org/wiki/Treaty_of_Hudaybiyyah")
          ,(my/holiday-islamic 11 11 "🥳 Birth of Imam Ali ar-Ridha, 148 AH" "https://en.wikipedia.org/wiki/Ali_ar-Ridha")
          ,(my/holiday-islamic 11 25 "🌟 Dahwul Ardh" "https://en.wikishia.net/view/Dahw_al-Ard")
          ,(my/holiday-islamic 11 29 "💔🥀 Martyrdom of Muhammad al-Taqī, 220 AH" "https://en.wikipedia.org/wiki/Muhammad_al-Taq%C4%AB")

          ;; ﴾12﴿ Dhu al-Hijjah, “The Month of Pilgrimage”
          ,(my/holiday-islamic 12 1  "🥳 Marriage of Sayedda Fatimah bint Muhammad to Imam Ali, 2 BH" "https://en.wikipedia.org/wiki/Fatimah_bint_Muhammad")
          ,(my/holiday-islamic 12 3  "🥳 Renunciation of Adam accepted" "https://en.wikipedia.org/wiki/Adam_in_Islam")
          ,(my/holiday-islamic 12 7  "💔🥀 Martyrdom of Imam Muhammad al-Baqir, 114 AH" "https://en.wikipedia.org/wiki/Muhammad_al-Baqir")
          ,(my/holiday-islamic 12 8  "💔🥀 Imam Husayn ibn Ali leaves Makkah for Karbala, 60 AH" "https://en.wikipedia.org/wiki/Husayn_ibn_Ali")
          ,(my/holiday-islamic 12 9  "🌟 Day of Arafah" "https://en.wikipedia.org/wiki/Day_of_Arafat")
          ,(my/holiday-islamic 12 9  "💔🥀 Martyrdom of Muslim ibn Aqeel & Hani ibn Urwa in Kufa, 60 AH" "https://en.wikipedia.org/wiki/Muslim_ibn_Aqeel")
          ,(my/holiday-islamic 12 10 "🥳 Eid al-Adha" "https://en.wikipedia.org/wiki/Eid_al-Adha")
          ,(my/holiday-islamic 12 15 "🥳 Birth of Imam Ali al-Hadi, 212 AH" "https://en.wikipedia.org/wiki/Ali_al-Hadi")
          ,(my/holiday-islamic 12 16 "💔🥀 Martyrdom of Sayedda Zaynab bint Ali" "https://en.wikipedia.org/wiki/Zaynab_bint_Ali")
          ,(my/holiday-islamic 12 18 "🥳 Eid al-Ghadeer" "https://en.wikipedia.org/wiki/Event_of_Ghadir_Khumm")
          ,(my/holiday-islamic 12 23 "💔🥀 Martyrdom of the children of Muslim ibn Aqeel, 60 AH" "https://en.wikipedia.org/wiki/Muslim_ibn_Aqeel")
          ,(my/holiday-islamic 12 24 "🥳 Eid al-Mubahalah" "https://en.wikipedia.org/wiki/Event_of_Mubahala")
          ,(my/holiday-islamic 12 27 "💔🥀 Martyrdom of Maytham al-Tammar, 60 AH" "https://en.wikipedia.org/wiki/Maytham_al-Tammar")

          ;; Canadian Holidays; https://www.canada.ca/en/revenue-agency/services/tax/public-holidays.html
          (holiday-fixed 1 1 "🇨🇦 New Year's Day 🇺🇸")
          (holiday-float 2 1 3 "🇨🇦 Family Day") ;; Third Monday in February
          (holiday-easter-etc) ;; Good Friday and Easter Monday
          (holiday-float 5 1 -1 "🇨🇦 Victoria Day" 25) ;; Monday preceding May 25th
          (holiday-fixed 6 1 "🇨🇦 Canada Day")
          (holiday-float 8 1 1 "🇨🇦 Civic Holiday") ;; First Monday in August
          (holiday-float 9 1 1 "🇨🇦 Labour Day 🇺🇸") ;;	First Monday of Septembe
          (holiday-fixed 9 30 "🇨🇦 National Day for Truth and Reconciliation")
          (holiday-float 10 1 2 "🇨🇦 Canadian Thanksgiving")	;; Second Monday in October
          (holiday-fixed 12 25 "🇨🇦 Christmas Day 🇺🇸")
          (holiday-fixed 12 26 "🇨🇦 Boxing Day / Day After Christmas 🇺🇸")

          ;; California Holidays
          (holiday-fixed 1 20 "🇺🇸 Martin Luther King Jr. Day")
          (holiday-fixed 2 17 "🇺🇸 Presidents’ Day")
          (holiday-fixed 5 26 "🇺🇸 Memorial Day")
          (holiday-fixed 7 4  "🇺🇸 Independence Day")
          (holiday-fixed 11 11 "🇺🇸 Veterans Day")
          (holiday-fixed 11 27 "🇺🇸 American Thanksgiving")
          (holiday-fixed 11 28 "🇺🇸 Day after Thanksgiving")

          ;; Misc
          (holiday-fixed 2 14 "💕 Valentine's Day")
          (holiday-float 5 0 2 "🧕 Mother's Day")
          (holiday-float 6 0 3 "👴 Father's Day")
          (holiday-fixed 10 31 "👻 Halloween")
          (holiday-islamic-new-year)
          (solar-equinoxes-solstices)
          (holiday-sexp calendar-daylight-savings-starts
                        (format "Daylight Saving Time Begins %s"
                                (solar-time-string
                                 (/ calendar-daylight-savings-starts-time (float 60))
                                 calendar-standard-time-zone-name)))
          (holiday-sexp calendar-daylight-savings-ends
                        (format "Daylight Saving Time Ends %s"
                                (solar-time-string
                                 (/ calendar-daylight-savings-ends-time (float 60))
                                 calendar-daylight-time-zone-name)))))
;; Holy Days & Holidays:2 ends here

;; [[file:init.org::*Holy Days & Holidays][Holy Days & Holidays:3]]
(defun my/today-and-future (sexpr)
  "Returns a list of SEXPR for today and the next `org-deadline-warning-days' days.

If you would like to see upcoming anniversary SEXPR with a bit of
forewarning, you can use the `%%(my/today-and-future SEXPR)`
instead. That will give you `org-deadline-warning-days' days’ warning:
on the anniversary date itself and the `org-deadline-warning-days' many
days prior.


SEXPR should be an expression you normally use in a Diary Sexpr Entry.

The result should be used in Diary Sexpr entries:
It expects a DATE argument to be in scope.

E.g.,

    %%(org-calendar-holiday)

Can be replaced by

    %%(my/today-and-future '(org-calendar-holiday))
"
  (-let [today date
               ;; '(month day year)
               ;; For testing:
               ;; (calendar-gregorian-from-absolute
               ;; (org-time-string-to-absolute (org-read-date nil nil "today")))
               ]
    (thread-last
      (cl-loop for date
               from (calendar-absolute-from-gregorian today)
               to (+ (calendar-absolute-from-gregorian today) org-deadline-warning-days)
               collect (calendar-gregorian-from-absolute date))
      (--map
       (let* ((agenda-date it)
              (org-agenda-current-date it)
              ;; Rebind 'date' so that SEXPR will
              ;; be fooled into giving us the list for the given
              ;; date and then annotate the descriptions for that
              ;; date.
              (date it)
              (headline (when-let ((matching-date (eval sexpr))) (or (if (s-blank? (s-trim entry)) nil entry) matching-date))))
         ;; Note `org-agenda` strips out text styles, so `propertize` is ignored. Sigh.
         (when headline (format "%s   ⟪%s⟫" headline (org-bbdb-anniversary-description today date)))))
      (--filter it))))


(defun org-bbdb-anniversary-description (agenda-date anniv-date)
  "Return a string used to incorporate into an agenda anniversary entry.

The calculation of the anniversary description string is based on
the difference between the anniversary date, given as ANNIV-DATE,
and the date on which the entry appears in the agenda, given as
AGENDA-DATE.  This makes it possible to have different entries
for the same event depending on if it occurs in the next few days
or far away in the future.

AGENDA-DATE and ANNIV-DATE are both Gregorian; i.e., of the form '(month day year)."
  (let ((delta (- (calendar-absolute-from-gregorian anniv-date)
                  (calendar-absolute-from-gregorian agenda-date))))
    (cond
     ((= delta 0) "Today")
     ((= delta 1) "Tomorrow")
     ((< delta org-deadline-warning-days) (format (format "In %d days" delta)))
     (t (eval `(format "%02d-%02d-%02d" ,@anniv-date))))))




(cl-defun dates (&key from to)
  "Return a list of (month day year) dates from FROM to TO.
FROM and TO are Org-style date strings like \"today\", \"+4d\", \"2025-04-30\"."
  (cl-loop for date
               from (org-time-string-to-absolute (org-read-date nil nil from))
               to (org-time-string-to-absolute (org-read-date nil nil to))
               collect (calendar-gregorian-from-absolute date)))
;;
;; (should (equal (dates :from "today" :to "+3d") '((4 22 2025) (4 23 2025) (4 24 2025) (4 25 2025))))

;; Example: Gets a (month day year) date.
;; (calendar-gregorian-from-absolute (org-time-string-to-absolute (org-read-date nil nil "now")))
;; Holy Days & Holidays:3 ends here

;; [[file:init.org::*Holy Days & Holidays][Holy Days & Holidays:4]]
(cl-defun date= (month day &optional year)
"To be used in a sexp, so assumes a `date' in scope."
  (-let [(m d y) date]
    (and (= m month) (= d day) (if year (= y year) t))))

(cl-defun birthday (month day)
  "To be used in a sexp, so assumes a `date' and `entry' in scope."
  (-let [entry (format "🥳 %s Birthday! 🎂" entry)]
    (my/today-and-future `(date= ,month ,day))))
;; Holy Days & Holidays:4 ends here

;; [[file:init.org::*Display times in agenda in 12-hour AM/PM format][Display times in agenda in 12-hour AM/PM format:1]]
(define-advice org-agenda-format-item (:around (orig &rest args) my/org-agenda-am-pm-time-format)
  "Display times in agenda in 12-hour AM/PM format."
  (let ((str (apply orig args)))
    (replace-regexp-in-string
     "\\b\\([0-2]?[0-9]\\):\\([0-5][0-9]\\)\\b"
     (lambda (match)
       (let* ((hour (string-to-number (match-string 1 match)))
              (minute (match-string 2 match))
              (ampm (if (>= hour 12) "PM" "AM"))
              (hour12 (cond ((= hour 0) 12)
                            ((> hour 12) (- hour 12))
                            (t hour))))
         (format "%d:%s %s" hour12 minute ampm)))
     str)))
;; Display times in agenda in 12-hour AM/PM format:1 ends here

;; [[file:init.org::*Implementation][Implementation:1]]
(setq org-use-fast-todo-selection t) ;; We can also change through states using Shift-⇄.
(setq org-imenu-depth 7)
;; Attempting to move a task to a DONE state is blocked if the has a child task that is not marked as DONE
(setq org-enforce-todo-dependencies t)
;; Implementation:1 ends here

;; [[file:init.org::*Implementation][Implementation:2]]
(use-package lf)
;; TODO: Add the line “(declare (indent defun))” right after the docstring of “lf-define”,
;; so that Emacs indents it like a “defun”.
;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/Indenting-Macros.html
;;
;; Until then, use the following incantation:
(lf-define (get 'lf-define 'lisp-indent-function) 'defun)

;; (MA: Eventually this function can itself become a small albeit useful MELPA ELisp Package ♥‿♥)
(lf-define my/declare-workflow-states (states)
  [:requires  (listp states) :ensures (stringp result)]
  "Declare STATES as todo-states. STATES is a list of (name on-entry on-exit color) lists.

       :on-entry and :on-exit can have the values:
       ⇒ note       ≈ Request the user to add a timestamped note when entering this state.
       ⇒ timestamp  ≈ Automatically log the time that this state was entered.
       ⇒ unschedule ≈ Unschedule the task when entering this state.
"
  ;; Declare the keywords and whether they have a note or timestamp on state change
  (setq org-todo-keywords  (list (cons 'sequence
                                       (cl-loop for (state . props) in states
                                                for first-letter = (downcase (substring state 0 1))
                                                for on-entry = (--when-let (plist-get props :on-entry) (if (listp it) it (list it)))
                                                for on-exit = (--when-let (plist-get props :on-exit) (if (listp it) it (list it)))
                                                ;; Assert: on-entry is a list, on-exit is a list
                                                for entry = (cond ((member 'note on-entry) "@") ((member 'timestamp on-entry) "!") (t ""))
                                                for exit = (cond ((member 'note on-exit) "/@") ((member 'timestamp on-exit) "/!") (t ""))
                                                collect (if (equal state "|") state (format "%s(%s%s%s)" state first-letter entry exit))))))
  ;; Colour the keywords
  (setq org-todo-keyword-faces
        (cl-loop for (state . props) in states
                 collect (list state :foreground (plist-get props :foreground) :weight 'bold)))
  ;; Account for ‘unschedule’ action
  ;; When a task goes into the state STATE, please remove its schedule
  ;; so that it does not appear in my agenda. (However, it's still not “done” and so appears when I do “C-c a t” for example.)
  ;; Source: https://emacs.stackexchange.com/a/2760
 (cl-loop for (state . props) in states
  do
  (when (member 'unschedule (plist-get props :on-entry))
       ;; TODO: Need to remove-hook “my/remove-schedule--⟨STATE⟩” to account for running this my/declare-workflow multiple times and expecting these hooks to not be present, say the second time around!
    (add-hook 'org-after-todo-state-change-hook
              (eval `(defun ,(intern (concat "my/remove-schedule--" state)) ()
                 "Remove SCHEDULED-cookie when switching state to STATE."
                 (save-excursion
                   (and (equal (org-get-todo-state) ,state)
                        (org-get-scheduled-time (point))
                        (when (search-forward-regexp org-scheduled-time-regexp nil t)
                          (or (delete-region (match-beginning 0) (match-end 0)) t))
                        (get-buffer "*Org Agenda*")
                        (with-current-buffer "*Org Agenda*"
                          (org-agenda-redo)))))))))
  ;; End
  "✔ Invoke “org-mode-restart” in existing Org buffers for this to take effect.")
;; Implementation:2 ends here

;; [[file:init.org::*Implementation][Implementation:3]]
(my/declare-workflow-states
 ;; Transitions: TODO → INVESTIGATED → STARTED ⟷ {AWAITING_REVIEW | PAUSED} → {DONE | CANCELLED}
 ;; (M-q via (setq fill-column 95) and M-x my/comment-box)
 '(
   ("TODO" :foreground "red") ;; A task was captured into my inbox.
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Tasks that are not started and not planned. They could be the backlogs or the GTD’s          ;;
   ;; someday/maybe. These tasks could be converted to NEXT INVESTIGATED during a weekly           ;;
   ;; review.                                                                                      ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


   ("INVESTIGATED"  :foreground "dark orange") ;; Tasks that are *ready* to be started next.
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; These are tasks that are not started but I plan to do as soon as I can.  When there is no     ;;
   ;; actionable STARTED task, I start one of the INVESTIGATED and convert it to STARTED.           ;;
   ;;                                                                                               ;;
   ;; The problem with most todo-lists is that you get overwhelmed by the amount of stuff to be     ;;
   ;; done. But in reality, most actions don't need or can't be progressed. So rather than look     ;;
   ;; at all the TODO tasks, we can limit our view to only the INVESTIGATED tasks.                  ;;
   ;; 💡 Instead of massive lists of things you'd like to do some day, have concrete NEXT           ;;
   ;; actions to undertake to achieve your goals. This way, only a subset of realistic              ;;
   ;; activities is in your consciousness. 💡                                                       ;;
   ;;                                                                                               ;;
   ;; Ideally, a task moves from TODO to NEXT only when I've actually split the task into small     ;;
   ;; achievable chunks; ie i've done some investigation into the task and thought about            ;;
   ;; what steps I need to do to actually get the task done. With this planning in place, I         ;;
   ;; can then ensure I allocate sufficient timeblocks to work on the subtasks of this              ;;
   ;; task. The investigation stage is about clarifying:                                            ;;
   ;; - What “done” means for the task,                                                             ;;
   ;; - /why/ am I doing this task; i.e., what are the benefits of the task;                        ;;
   ;; - Including background information; e.g., external links or a paragraph;                      ;;
   ;; - Splitting the task into achievable sub-tasks, so that it's not nebulous                     ;;
   ;; and so too daunting to start.                                                                 ;;
   ;;                                                                                               ;;
   ;; 🤔 Tip: Use my 1/1 time with my manager/peers to review my INVESTIGATED/NEXT/READY            ;;
   ;; findings for the tickets of the current sprint. I just want to make sure I'm on the right     ;;
   ;; track, before starting to work on them. Or, if a ticket is not investigated, do that with     ;;
   ;; my manager.  I find that while it might take me half an hour or more, it'll take like 5       ;;
   ;; minutes with him since he's familiar with the code-base.                                      ;;
   ;;                                                                                               ;;
   ;; (Aside: Some people use a “NEXT” state to denote “this task is the /next/ immediate task      ;;
   ;;         to do in this project”.  Since my tasks are ordered, the NEXT task would be the first ;;
   ;;         task that is “TODO” or “INVESTIGATED”.)                                               ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


   ("STARTED" :foreground "blue") ;; I've begun this task, and it's loaded into my short-term memory.
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; These are tasks that are working in progress (“open loops”). I work on these tasks before    ;;
   ;; starting another NEXT task to avoid too many open loops at any moment.                       ;;
   ;;                                                                                              ;;
   ;; Whenever I clock-into a task, it is automatically transitioned into this state. As such,     ;;
   ;; if I want know when I first started on a task, I simply look at the CLOCK entry it has.      ;;
   ;;                                                                                              ;;
   ;; Note: “:on-entry timestamp” is not ideal for my current use since my tasks tend to           ;;
   ;; ping-pong between STARTED ⟷ WAITING.                                                         ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


   ("|")
   ;; All states after this special marker are “terminal” and so not shown in the org-agenda:
   ;; (setq org-agenda-skip-scheduled-if-done t)
   ;;
   ;; Moreover, they are counted as “done” in the statistics “ [96%] ” count of a parent heading.
   ;;
   ;; Finally, I get a “CLOSED: <timestamp>” property on these tasks when
   ;; (setq org-log-done 'time). This is desirable, since it tells me when I entered these ‘terminal’
   ;; states: E.g., how long ago a task entered the WAITING state.
   ;;
   ;; Also, the definition of “done” in Org-QL includes all terminal states in my workflow, yay.


   ("PAUSED" :on-entry 'note :on-exit 'timestamp :foreground "magenta") ;; I'm not actively working on this task anymore. I may return to it later.
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; I am choosing to stop work on this task for some reason; e.g., it is no longer a priority    ;;
   ;; (i.e., I'm not interested in it, or I realised it does not contribute to my long-term        ;;
   ;; life goals).                                                                                 ;;
   ;;                                                                                              ;;
   ;; When I enter this state, record when & why the task is paused.                               ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


   ("WAITING" :on-entry 'unschedule :foreground "red2") ;; I'm waiting on feedback from others before I can continue.
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; When a task goes into WAITING, I've finished the task as much as possible and now need to      ;;
   ;; rely on someone else; e.g., for feedback. I'm waiting on *someone else*.                       ;;
   ;;                                                                                                ;;
   ;; ⇒ I don't want to see these scheduled tasks in my Agenda.                                      ;;
   ;; ⇒ If I'm waiting on an email, I can make an Org task to track the fact I'm waiting on it.      ;;
   ;; ⇒ In the Weekly Review, I will take time to look at my WAITING tasks                           ;;
   ;; and if they've been waiting ~2 weeks, then I should message the relevant people to unblock it. ;;
   ;;                                                                                                ;;
   ;; Waiting = “In progress, but BLOCKED by others”                                                 ;;
   ;;                                                                                                ;;
   ;; 🤔 Consider adding a WAITING_SINCE property whenever a task enters this state, so that I       ;;
   ;; can reach out to others and say “I've been waiting on you since Jan 1, please unblock          ;;
   ;; me!”                                                                                           ;;
   ;;                                                                                                ;;
   ;; “:on-entry timestamp” is not ideal for my current use since my tasks tend to ping-pong         ;;
   ;; between STARTED ⟷ WAITING.                                                                     ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


   ("APPROVED" :foreground "magenta") ;; Almost done, just needs a quick review.
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; I'm actively working on this task, and not waiting on any one else,                         ;;
   ;; but there is some blocker; e.g., a merge dependency issue or a conflict to resolve.         ;;
   ;; Sometimes these are quick to dispatch; other times they're blocked by PAUSED/WAITING tasks. ;;
   ;;                                                                                             ;;
   ;; This is essentially DONE, but there's some blocker.                                         ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


   ("DONE"  :foreground "forest green") ;; This task is DONE; mine it for useful LOG comments and archive it.


   ("CANCELLED" :on-entry 'note :foreground "saddle brown")  ;; Why was this task cancelled? (Notes come with a timestamp!)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; I've started on this task and now realised /I do not want/ continue this approach, and so    ;;
   ;; will ABANDON it.  This is useful at work to keep track of *why* we have decided against      ;;
   ;; doing a task; in my Org file there may be more, private, reasons not mentioned in the        ;;
   ;; company's Jira file. (For example).                                                          ;;
   ;;                                                                                              ;;
   ;; Instead of just deleting the task, I document it so that if I have the same idea/task        ;;
   ;; again then I can see my notes to figure out /why/ I didn't do it last time. Also useful      ;;
   ;; for when doing Annual Reviews: Why did I quit these tasks.                                   ;;
   ;;                                                                                              ;;
   ;; The logging is helpful if others at work want to know why some approach was abandoned, so    ;;
   ;; TAKE A MINUTE TO FLESH OUT THE REASONS FOR CANCELLATION.                                     ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ))
;; Implementation:3 ends here

;; [[file:init.org::*Also, logging][Also, logging:1]]
;; Since DONE is a terminal state, it has no exit-action.
;; Let's explicitly indicate time should be noted.
(setq org-log-done 'time)
;; → Take a look at org-log-done and org-log-into-drawer. These will tell org to log the date and time when an item's status is changed (you can specify).
;; → When the agenda is built you can show all these logged items on the date they were completed with org-agenda-log-mode, org-agenda-log-mode-items, and org-agenda-start-with-log-mode.
;; → This allows me to place TODO items anywhere I want (my logbook, notes, and a scratch list) and as I complete them through the week they're all shown in the agenda according to when I completed each.
;; → I use org-clock-in to the task I'm working on, then a simple clocktable with some dates will show me exactly what I worked on.
;; → [Weekly Review] which creates a day-by-day summary of the time I worked on what tasks. On Friday, I can look at this and see what I did over the week.
;;
;; clocking-in is about the best way to answer that dreaded Friday afternoon question: "WTF did I do with my time this week/month/etc!?"

;; I prefer to log TODO creation also
(setq org-treat-insert-todo-heading-as-state-change t)

;; Use S-⇆ as a convenient way to select a TODO state and bypass any logging associated with that.
;; E.g., a task is STARTED and pressing S-→ moves it to PAUSED /without/ having to add a note for why it's now paused.
(setq org-treat-S-cursor-todo-selection-as-state-change nil)
;; Also, logging:1 ends here

;; [[file:init.org::*Propagate =STARTED= to parent tasks][Propagate =STARTED= to parent tasks:1]]
(add-hook 'org-after-todo-state-change-hook
          (defun my/if-I-am-STARTED-task-so-is-my-project-parent ()
            (when (equal (org-get-todo-state) "STARTED")
              (save-excursion
                ;; Change all parents /that/ have a todo keyword
                (while (org-up-heading-safe)
                  (when (org-get-todo-state) (org-todo "STARTED")))))))
;; Propagate =STARTED= to parent tasks:1 ends here

;; [[file:init.org::*Capture: Now that I know how to query my agenda, how do I get things into it efficiently?][Capture: Now that I know how to query my agenda, how do I get things into it efficiently?:1]]
(cl-defmacro def-capture (name location template &rest forms)
  "Creates a method “my/capture-NAME”, which opens a capture buffer named NAME showing TEMPLATE.
When you press `C-c C-c`, the note is saved as an entry (ie TEMPLATE should start with “* ”.)
in `org-default-notes-file' section named LOCATION.

+ NAME, LOCATION, TEMPLATE are all strings that may contain spaces.
  ⇒ If you want to evaluate a function in TEMPLATE and have its results be inlined, use the syntax “%(f args)”.
    See https://stackoverflow.com/a/69331239 for an example.
  ⇒ See the docs of `org-capture-templates', half-way down, for supported %-escapes in the template.
+ FORMS is an optional collection of Elisp forms to execute after a capture template session has been initiated;
  e.g., to programmatically add content to a template, say a quote or the results of a shell command.
+ Example:  (def-capture \"Friends Info\" \"Journal\" \"* %t\" (message \"Well-done! Stay in touch!\"))
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
     (let (;; Temporarily pause any clocking hooks
           (org-clock-in-hook nil)
           (org-clock-out-hook nil)
           (org-capture-templates
            ;; Capture mode now handles automatically clocking in and out of a capture task.
            ;; When I start a capture mode task the task is clocked in as specified by :clock-in t
            ;; and when the task is filed with C-c C-c the clock resumes on the original clocking task.
            `(("𝒞" ,,name entry (file+headline "~/Dropbox/my-life.org" ,,location) ,,template :clock-in t :clock-resume t))))
       (org-capture (list prefix) "𝒞")
       (unless (> prefix 1) (rename-buffer ,name))
       ,@forms)))
(bind-key* "C-c c" (def-capture "Inbox Entry 📩" "Inbox 📩 \t\t\t:inbox:" "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"))
;; Capture: Now that I know how to query my agenda, how do I get things into it efficiently?:1 ends here

;; [[file:init.org::*Adding New *Tasks/Notes* Quickly Without Disturbing The Current Task Content][Adding New *Tasks/Notes* Quickly Without Disturbing The Current Task Content:1]]
(add-hook
 'org-insert-heading-hook
 (defun my/insert-CREATED-property ()
   "Insert :CREATED: property upon C-[S]-RET and M-[S]-RET, unless C-u is pressed first."
   (unless current-prefix-arg
     (org-set-property "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]")))))

;; I also have a short cut key defined to invoke this function on demand so that
;; I can insert the inactive timestamp anywhere on demand.
;;
;; I use TAB and S-TAB for cycling - I don't need c and C as well.
;; Instead use “c” to add the ‘C’reated property.
(setf (cdr (assoc "c" org-speed-commands)) #'my/insert-CREATED-property)
;; Adding New *Tasks/Notes* Quickly Without Disturbing The Current Task Content:1 ends here

;; [[file:init.org::*Adding New *Tasks/Notes* Quickly Without Disturbing The Current Task Content][Adding New *Tasks/Notes* Quickly Without Disturbing The Current Task Content:2]]
(add-hook 'org-mode-hook '(lambda ()
   (local-set-key (kbd "<return>") 'org-return-indent)) ;; Newline with indentation
   (local-set-key (kbd "C-M-<return>") 'electric-indent-just-newline)) ;; Newline without indentation
;; Adding New *Tasks/Notes* Quickly Without Disturbing The Current Task Content:2 ends here

;; [[file:init.org::*Hide blank lines /between/ headlines][Hide blank lines /between/ headlines:1]]
(setq org-cycle-separator-lines 0)

(setq org-blank-before-new-entry '((heading) (plain-list-item . auto)))
;; Hide blank lines /between/ headlines:1 ends here

;; [[file:init.org::#drag-and-drop-images-into-org-mode][“Smart Paste”: Drag and Drop Images/(Any File!) into Org-Mode:1]]
(add-hook
 'org-mode-hook
 (cl-defun my/setup-smart-paste ()
   (bind-key "s-v"
             (cl-defun my/dwim-paste ()
               "Call `yank-media', with first possible media-type; ie no prompting.

With “C-u” prefix, or when in minibuffer, this is traditional paste.

Otherwise, if clipboard contains:
+ Rich Text (i.e., copy of HTML text), then it's pasted as Org Markup.
+ Image, then it's attached to the current Org note, via `org-attach', and an “attachement:” link is pasted
  and the image link is displayed inline.
+ If it's a Gerrit link, “12345: [tags] Fix parsing bug | <url>”, then produce “[[<url>][Fix parsing bug]]”.
+ Otherwise, default string plaintext pasting occurs.
  - If string is a URL, then insert an Org link whose description is HTML title of that URL webpage.
    * If a region is selected when the yank is performed, then that region becomes the description of the link.


TODO:
⇒ If text is selected when I past an image, then use the text as #+caption, maybe?
⇒ Define “attachement:” using org-special-block-extras so that it gives me keymap options like
   delete attachement, rename attachement, open attachement externally, open in emacs. Low priority.
"
               (interactive)
               (if (or (minibufferp) current-prefix-arg (null yank-media--registered-handlers))
                   (yank)
                 (flet ((length= (&rest _args) 'assume-equality-check-is-true-in-<yank-media>-implementation))
                   (-let [start (point)]
                     (ignore-errors (yank-media))
                     ;; If no media to yank, then do plain old yank
                     (when (equal start (point)) (yank)))))))


   ;; Added to: yank-media--registered-handlers.
   (yank-media-handler
    "text/html"
    (cl-defun my/yank-html-media (_media-type contents)
      (insert (s-replace " " " " (shell-command-to-string
                                  (-let [delimiter "EOF"] ;; A unique “here-document delimiter”, unlikely to be part of ‘contents’
                                    ;; NOTE: `shell-quote-argument` does too much here; e.g., copied code blocks wont paste nicely.
                                    ;; For now, I want $FOO to be pasted as itself, and not interpreted as a Shell variable.
                                    (format "pandoc -f html -t org <<%s\n%s\n%s" delimiter (s-replace "$" "\\$" contents) delimiter)))))))

   (yank-media-handler
    "STRING"
    (cl-defun my/yank-plaintext-media (_media-type contents)
      (let* ((url (when (string-match-p "^https?://" contents) contents))
             (region-content (when (region-active-p)
                               (buffer-substring-no-properties (region-beginning)
                                                               (region-end)))))
        (cond ((and region-content url)
               (delete-region (region-beginning) (region-end))
               (insert (org-make-link-string url region-content)))
              (url
               ;; Insert Org link to URL using title of HTML page at URL.
               (org-web-tools-insert-link-for-url url))
              (:otherwise
               (-let [ (gerrit-title gerrit-url) (my/extract-gerrit-title-and-url contents) ]
                 (if (and gerrit-title gerrit-url)
                     (insert (org-make-link-string gerrit-url gerrit-title))
                   (insert contents))))))))
   ;;
   (defun my/extract-gerrit-title-and-url (str)
     "Extract the title and URL from the given string STR, omitting the leading number and tags in brackets."
     (if (string-match "^\\(?:[0-9]+: \\)?\\(?:\\[[^]]+\\] \\)?\\(.*?\\) | \\(https?://[^ \"]+\\) *$" str)
         (let ((title (match-string 1 str))
               (url (match-string 2 str)))
           (list title url))))
   ;; example usage
   (when nil
     (let ((input "12345: [foo, bar] OCaml Syntax: Fix foo bar baz | https://gerrit.local.company.com/c/abc/+/12345"))
       (-let [ (title url) (my/extract-gerrit-title-and-url input) ]
         (message "Title: %s\nURL: %s" title url))))


   (yank-media-handler
    "image/.*"
    (cl-defun my/yank-image-media (_media-type image)
      (org--image-yank-media-handler 'image/png image)
      (my/toggle-display-inline-image-at-point)))

   ;; Used by org--image-yank-media-handler
   (defun org-yank-image-autogen-filename ()
     "Autogenerate filename for image in clipboard."
     (format-time-string "%Y-%m-%d_%I:%M:%S%p"))


   (cl-defun my/toggle-display-inline-image-at-point ()
     "My single notes file is huge, so toggling all images takes a while. Whence this method."
     (interactive)
     (-let [inhibit-message t]
       (beginning-of-line)
       (push-mark)
       (end-of-line)
       (org-toggle-inline-images nil (region-beginning) (region-end))))

   ))

;; I was surprised I needed this; perhaps a system restart will show I don't need it.
(add-hook 'org-capture-mode-hook #'my/setup-smart-paste)
;; “Smart Paste”: Drag and Drop Images/(Any File!) into Org-Mode:1 ends here

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

;; [[file:init.org::#Proportional-fonts-for-Headlines][Proportional fonts for Headlines:1]]
(set-face-attribute 'org-document-title nil :height 2.0)
;; (set-face-attribute 'org-level-1 nil :height 1.0)
;; Remaining org-level-𝒾 have default height 1.0, for 𝒾 : 1..8.
;;
;; E.g., reset org-level-1 to default.
;; (custom-set-faces '(org-level-1 nil))
;; Proportional fonts for Headlines:1 ends here

;; [[file:init.org::#Pretty-Lists-Markers][Pretty Lists Markers:1]]
;; (x y z) ≈ (existing-item replacement-item positivity-of-preceding-spaces)
(cl-loop for (x y z) in '(("+" "◦" *)
                       ("-" "•" *)
                       ("*" "⋆" +))
      do (font-lock-add-keywords 'org-mode
                                 `((,(format "^ %s\\([%s]\\) " z x)
                                    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) ,y)))))))
;; Pretty Lists Markers:1 ends here

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
;; Making Block Delimiters Less Intrusive:3 ends here

;; [[file:init.org::#Making-Block-Delimiters-Less-Intrusive][Making Block Delimiters Less Intrusive:4]]
;; Un-disguise a symbol when cursour is inside it or at the right-edge of it.
(setq prettify-symbols-unprettify-at-point 'right-edge)
;; Making Block Delimiters Less Intrusive:4 ends here

;; [[file:init.org::#Hiding-Emphasise-Markers-Inlining-Images-and-LaTeX-as-PNG][Hiding Emphasis Markers, Inlining Images, and LaTeX-as-PNG:1]]
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
;; Hiding Emphasis Markers, Inlining Images, and LaTeX-as-PNG:1 ends here

;; [[file:init.org::#Hiding-Emphasise-Markers-Inlining-Images-and-LaTeX-as-PNG][Hiding Emphasis Markers, Inlining Images, and LaTeX-as-PNG:2]]
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :init (setq org-appear-autoemphasis  t
              org-appear-autolinks nil
              org-appear-autosubmarkers nil))
;; Hiding Emphasis Markers, Inlining Images, and LaTeX-as-PNG:2 ends here

;; [[file:init.org::#Hiding-Emphasise-Markers-Inlining-Images-and-LaTeX-as-PNG][Hiding Emphasis Markers, Inlining Images, and LaTeX-as-PNG:4]]
;; Automatically toggle LaTeX previews when cursour enters/leaves them
(use-package org-fragtog
  :disabled t
  :hook (org-mode . org-fragtog-mode))
;; Hiding Emphasis Markers, Inlining Images, and LaTeX-as-PNG:4 ends here

;; [[file:init.org::#Hiding-Emphasise-Markers-Inlining-Images-and-LaTeX-as-PNG][Hiding Emphasis Markers, Inlining Images, and LaTeX-as-PNG:7]]
;; Support “latex-as-png” src blocks, which show LaTeX as PNGs
(use-package ob-latex-as-png :disabled t)
;; Hiding Emphasis Markers, Inlining Images, and LaTeX-as-PNG:7 ends here

;; [[file:init.org::#Hiding-Emphasise-Markers-Inlining-Images-and-LaTeX-as-PNG][Hiding Emphasis Markers, Inlining Images, and LaTeX-as-PNG:8]]
;; Use the “#+name” the user provides, instead of generating label identifiers.
(setq org-latex-prefer-user-labels t)
;; Hiding Emphasis Markers, Inlining Images, and LaTeX-as-PNG:8 ends here

;; [[file:init.org::*Now C-c C-x C-v shows remote images inline, neato!][Now C-c C-x C-v shows remote images inline, neato!:1]]
  (quelpa '(org-remoteimg :fetcher github :repo "gaoDean/org-remoteimg"))
  (require 'org-remoteimg)
  (setq url-cache-directory "~/emacs.d/.cache/")
  (setq org-display-remote-inline-images 'cache)
;; Now C-c C-x C-v shows remote images inline, neato!:1 ends here

;; [[file:init.org::#Draw-pretty-unicode-tables-in-org-mode][Draw pretty unicode tables in org-mode:1]]
(quelpa '(org-pretty-table
         :repo "Fuco1/org-pretty-table"
         :fetcher github))

(add-hook 'org-mode-hook 'org-pretty-table-mode)
;; Draw pretty unicode tables in org-mode:1 ends here

;; [[file:init.org::*Use backtick as an alternative to “~” for code font][Use backtick as an alternative to “~” for code font:1]]
;; Working with others: `This is a piece of code`
(add-hook 'org-font-lock-set-keywords-hook
          (defun my/backticks-denote-code ()
            (add-to-list 'org-font-lock-extra-keywords
                         '("\\(`\\)\\(.*\\)\\(`\\)"
                           (1 '(face nil invisible t))
                           (3 '(face nil invisible t))
                           ;; (2 '(face code))
                           (2  '(:box t :foreground "#AAF"))))))
;; Use backtick as an alternative to “~” for code font:1 ends here

;; [[file:init.org::*\[Conversely,\] Rich Copy Commands :: =my/copy-as-{jira, slack, reddit, confluence}=][[Conversely,] Rich Copy Commands :: =my/copy-as-{jira, slack, reddit, confluence}=:1]]
(cl-defun my/copy-as-jira ()
  "Export region to Jira markdown, and copy to the kill ring for pasting into other programs."
  (interactive)
  (use-package ox-jira)
  (message "Converting to Jira Markdown...")
  (kill-new (org-export-as 'jira))
  (message "Copied as Jira Markdown!"))


(cl-defun my/copy-as-reddit ()
  "Export region to Reddit (i.e., Github Flavoured Markdown), and copy to the kill ring for pasting into other programs.

   “Mode for Reddit”: Conversely, to read Reddit within Emacs,
   (use-package md4rd :config (setq md4rd-subs-active '(emacs clojure shia)))
   then “M-x md4rd”."
  (interactive)
  (use-package ox-gfm)
  (message "Converting to Reddit Markdown...")
  (kill-new (org-export-as 'gfm))
  (message "Copied as Reddit Markdown!"))


(cl-defun my/copy-as-slack ()
  "Export region to slack, and copy to the kill ring for pasting into other programs."
  (interactive)
  (use-package ox-slack)
  (message "Converting to Slack Markdown...")
  (kill-new (org-export-as 'slack))
  (message "Copied as Slack Markdown! In Slack press “⌘ Shift F” to apply the formatting."))


(cl-defun my/copy-as-confluence ()
  "Export region to Confluence, and copy to the kill ring for pasting into other programs.

More precisely, export the selected region to HTML and copy it to the clipboard.

Note: Confluence has no markup language, it's a WYSIWYG editor,
but it does recognise formatted text, such as what you copy off a webpage."
  (interactive)
  (message "Converting to Confluence Markdown...")
  ;; NOTE: Consider using https://git.sr.ht/~bzg/org-contrib/blob/master/lisp/ox-confluence.el
  (kill-new (org-export-as 'html))
  (message "Copied as Confluence Markdown!"))
;; [Conversely,] Rich Copy Commands :: =my/copy-as-{jira, slack, reddit, confluence}=:1 ends here

;; [[file:init.org::*my/dired-copy-image-to-clipboard][my/dired-copy-image-to-clipboard:1]]
(defun my/dired-copy-image-to-clipboard ()
  "Copy the currently selected image file in dired to the clipboard as image data, so that I can paste it with ⌘-v into Slack or other programs.

   Note,
   1. In dired, “C-u 0 w” copies the absolute file path at point.
   2. In MacOS finder, press “⌘+Shift+g” and type in or paste a path.
      (Equivalently, in Finder look at the “Go” menu bar item, then select “Go to folder”.)

"
  (interactive)
  (let* ((file (dired-get-file-for-visit))
         (escaped-file (replace-regexp-in-string "\"" "\\\\\"" file))
         (script (format "osascript -e 'set the clipboard to (read (POSIX file \"%s\") as {«class PNGf»})'" escaped-file)))
    (if (and (file-exists-p file)
             (string-match-p (image-file-name-regexp) file))
        (progn
          (shell-command script)
          (message "Copied image to clipboard: %s" file))
      (message "Not an image file: %s" file))))
;; my/dired-copy-image-to-clipboard:1 ends here

;; [[file:init.org::*\[Super Low Priority\] Orphaned attachments --Part of the Weekly Review][[Super Low Priority] Orphaned attachments --Part of the Weekly Review:1]]
(defun my/find-orphaned-attachments ()
  "Shows buffer of clickable “Orphaned Attachment ❓ Verify unused ❌ Delete” rows.

Check for orphaned files in the Org attachment directory.

   NOTE: This implemention assumes all of my attachements are uniquely named,
   which is not necessairly true! As such, I've added a ‘verify’ button.
"
  (interactive)
  (when (derived-mode-p 'org-mode)
    (let* ((attach-dir (or (org-attach-dir) ;; try entry-specific attach dir
                           (when (boundp 'org-attach-id-dir) org-attach-id-dir)
                           (expand-file-name "data/" (file-name-directory (buffer-file-name))))) ;; fallback default
           (all-attachments (when (file-directory-p attach-dir)
                              (directory-files-recursively attach-dir ".*" nil)))
           ;; Collect all attachment links in all Org files
           (linked-attachments (-flatten (loop for org-file in (directory-files-recursively default-directory "\\.org$")
                                               unless (or (s-contains-p ".emacs.d/elpa" org-file) (s-contains-p ".emacs.d/quelpa" org-file))
                                               collect (with-temp-buffer
                                                         (insert-file-contents org-file)
                                                         (org-mode)
                                                         (org-element-map (org-element-parse-buffer) 'link
                                                           (lambda (link)
                                                             (when (string= (org-element-property :type link) "attachment")
                                                               (org-element-property :path link))))))))
           ;; Now check for unreferenced files
           (results (loop for attachment in all-attachments
                          for att =  (f-base attachment)
                          unless (member att (--map (f-base it) linked-attachments))
                          collect (format "- [[file:%s][%s]] ❓ [[elisp:(rgrep \"%s\" \"*.org\")][Verify unused]] ❌  [[elisp:(progn (ignore-errors (f-delete \"%s\")) (kill-whole-line))][Delete!]]"
                                          attachment att att   (concat default-directory attachment)))))
      ;; Create and display a new buffer
      (with-current-buffer (get-buffer-create "*Orphaned Attachments*")
        (erase-buffer)
        (insert (string-join results "\n"))
        (org-mode)
        (goto-char (point-min))
        (pop-to-buffer (current-buffer))))))
;; [Super Low Priority] Orphaned attachments --Part of the Weekly Review:1 ends here

;; [[file:init.org::*Org-rifle][Org-rifle:1]]
;; M-x helm-org-rifle ⇒ Search all open Org files, results are shown with
;; /context/, not just line-based. ⚠️ This respects narrowing.
(use-package helm-org-rifle)
;; Org-rifle:1 ends here

;; [[file:init.org::*Mitigate accidental deletion of large regions of text][Mitigate accidental deletion of large regions of text:1]]
;; Note: TAB on a heading is great to cycle, but when there's lots of text /before/ subheadings,
;; it can be hard to see the outline. Instead on a heading press “C-c C-k” to ‘kill’ (i.e., hide!) such
;; notes and only show the outline. Very nice.

(setq org-ctrl-k-protect-subtree :please-ask-me-when-deleting-a-collapsed-subtree-with-C-k)

;; When an org heading is folded and I press “C-k”, then only operate on the headline, not the contents!
;; Pressing “C-k” deletes tags iff cursour is at the end of the headline text and before tags.
(setq org-special-ctrl-k :please-make-C-k-consider-headline-and-tags-only)

;; Likewise the “k” speed key should also confirm before doing anything.
(map-put org-speed-commands "k" nil) ;; Orginally: org-cut-subtree


;; For any key press, the DELETE key, M-S-RET for creation of new headings, etc.
;; Note “being next to an invisible region” means cursor is immediately *after* such a region or immediately *before*.
;; As such, if you press M-DELETE at the end of the line of a folded heading, it's considered an invisible edit.
;; However, if you press M-DELETE at the start of the line *after* a folded heading, it's not considered an invisible edit! 😲
;; Instead you end-up deleting some invisible text! This is because of how the method org-fold-check-before-invisible-edit is defined.
(setq org-catch-invisible-edits 'show-and-error)

;; Require confirmation for large region deletion
(url-copy-file "https://www.emacswiki.org/emacs/download/wimpy-del.el" "~/.emacs.d/elpa/wimpy-del.el" :ok-if-already-exists)
(load-file "~/.emacs.d/elpa/wimpy-del.el")
(bind-key* "C-w" #'kill-region-wimpy)
(setq wimpy-delete-size 3000)

;; Note: This has no impact if you select a region, then just press DELETE. Let's fix that:
;;
(defun my/confirm-big-deletion (orig-fun &rest args)
"Ask for confirmation if deleting more than `wimpy-delete-size' characters in `org-delete-backward-char`."
;; If deletion should not be done, show wimpy msg, otherwise do the deletion.
(if (and (region-active-p)
         (-let [size (- (region-end) (region-beginning))]
           (and (> size wimpy-delete-size) (not (yes-or-no-p (format "Really delete %d characters? " size))))))
    (message wimpy-delete-dopey-message)
  (apply orig-fun args)))
;;
(advice-add 'org-delete-backward-char :around #'my/confirm-big-deletion) ;; For Org-mode
(advice-add 'backward-delete-char-untabify :around #'my/confirm-big-deletion) ;; For everywhere else
;; Mitigate accidental deletion of large regions of text:1 ends here

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

;; [[file:init.org::*Press M-SPC so all adjacent blank lines are also removed][Press M-SPC so all adjacent blank lines are also removed:1]]
;; Now when I press M-SPC all adjacent blank lines are also removed.
(advice-add 'cycle-spacing :after
            (defun my/cycle-spacing-then-delete-blank-lines (&rest _args)
  "Run `delete-blank-lines` after `cycle-spacing`."
  (delete-blank-lines)))
;; Press M-SPC so all adjacent blank lines are also removed:1 ends here

;; [[file:init.org::*Add more line padding for readability][Add more line padding for readability:1]]
(setq-default line-spacing 0.2)
;; Add more line padding for readability:1 ends here

;; [[file:init.org::*Olivetti: A clean writing environment][Olivetti: A clean writing environment:1]]
(use-package olivetti)
(setq olivetti-body-width 100)
;; Olivetti: A clean writing environment:1 ends here

;; [[file:init.org::*✨ Make Org properties look nice -- pretty emphasis markers][✨ Make Org properties look nice -- pretty emphasis markers:1]]
(add-hook
 #'org-mode-hook
 (defun my/make-properties-look-nice ()
   "Make it nice for me to place scheduled markers in an Org heading.
⇒ Shift-↑↓ continue to work to change dates, and dates are clickable to open the agenda.
⇒ Create them with “C-c C-s”, get them with “ (org-entry-get (point) \"SCHEDULE\") ”."
   ;; See changes in buffer: Replace the 2ⁿᵈ arg “ 'org-mode ” with “ nil ”, then “ C-x C-e C-x x f ”.
   ;; Useful: (pop font-lock-keywords)
   (font-lock-add-keywords
    nil ;; 'org-mode
    `(
      ;; I don't need to see the year, thanks. Also, use icons for these words.
      ("\\(CLOSED: *\\[[[:digit:]]\\{4\\}-\\)\\([^]]+\\)\\(\\]\\)"
       (1 '(face (:inherit (bold)) display "☺️ "))
       (2 '(face (:weight semi-bold :height 120 :background "SpringGreen1" :family "Source Code Pro Light 14")  help-echo "Well-done, buddo!"))
       (3 '(face (:inherit (bold)) display "")))
      ("\\(SCHEDULED: *<[[:digit:]]\\{4\\}-\\)\\([^>]+\\)\\(>\\)"
       (1 '(face (:inherit (bold)) display "📆 "))
       (2 '(face (:weight semi-bold :height 120 :background "ivory1" :family "Source Code Pro Light 14")  help-echo "Make progress, buddo!"))
       (3 '(face (:inherit (bold)) display "")))
      ("\\(DEADLINE: *<[[:digit:]]\\{4\\}-\\)\\([^>]+\\)\\(>\\)"
       (1 '(face (:inherit (bold)) display "🎯 "))
       (2 '(face (:weight semi-bold :height 120 :background "RosyBrown1" :family "Source Code Pro Light 14")  help-echo "Focus, buddo!"))
       (3 '(face (:inherit (bold)) display "")))
      ;; Make ALL “ :keyword: ” at the start of the line have their colons be invisible
      ("^ *\\(:\\)\\([^:]+\\)\\(: \\)"
       (1 '(face nil display ""))
       (2 '(face (:foreground  "LightPink1" :height 0.8) help-echo "😉 “C-c C-x p” to set a new property"))
       (3 '(face nil display " ")))
      ;; Consider: "\\(:CREATED:\\)" ↦ "📝"; "\\(:LOGBOOK:\\)" ↦ "🪵"
      ;; Clocking info is great, but it's meta-data useful for org-agenda, not for my naked eyes.
      ("\\(CLOCK: \\[[[:digit:]]\\{4\\}-\\)[^]]*\\(.*\\)"
       (1 '(face (:inherit (bold)) display "⏰ "))
       (2 '(face (:inherit (bold)) display "")))
      ;; Likewise, I want to see a note, via C-c C-z, but don't care to see it's (important) meta-data.
      (,(format "^- \\(Note taken on \\)?%s *\\\\*\n *" (org-re-timestamp 'inactive))
       (0 '(face nil display "📝 ")))))

   ;; [Posterity] Make key-value property names look like pressed buttons? Neat, but no thanks.
   ;; (set-face-attribute 'org-special-keyword nil :inverse-video nil)
   ;; (set-face-attribute 'org-special-keyword nil :box '(:line-width (2 . 1) :color "grey75" :style released-button))

   ;; I prefer the following via prettify-symbols-mode so that when my cursour is beside them, the original text disappears.
   (push (cons ":PROPERTIES:" ? ) prettify-symbols-alist)
   (push (cons ":END:" ? ) prettify-symbols-alist)
   (push (cons ":LOGBOOK:" ? ) prettify-symbols-alist)
   ;; Make “ ∶PROPERTIES∶ , ∶LOG∶ , ∶END∶ ” all look signficinatly different from the surrounding text.
   ;; These are meta-tokens, not intended for editing by my hands.
   (set-face-attribute 'org-drawer nil :foreground   "midnight blue")
   (set-face-attribute 'org-drawer nil :weight 'bold)
   (set-face-attribute 'org-drawer nil :height  1)
   (set-face-attribute 'org-drawer nil :slant 'normal)
   ;; (set-face-attribute 'org-drawer nil  :family  "Savoye LET")
   ;; (set-face-attribute 'org-drawer nil  :family  "Noteworthy")
   ;; (set-face-attribute 'org-drawer nil  :family  "Courier New")
   ;; (set-face-attribute 'org-drawer nil  :family  "Chalkduster")
   ;; (set-face-attribute 'org-drawer nil  :family  "Bradley Hand")
   ;; (set-face-attribute 'org-drawer nil  :family  "Papyrus")
   (set-face-attribute 'org-drawer nil  :family  "Input Mono")
   ;; [Something to consider] Maybe keep the word present but change height to like 0.5?
   ;; ❌ (font-lock-add-keywords nil '(("\\(^ *:PROPERTIES: *\\)" 1 '(face nil display "▽"))) t)
   ;; ❌ (font-lock-add-keywords nil '(("\\(:LOGBOOK:\\)" 1 '(face nil display "▽"))) t)
   ;; ❌ (font-lock-add-keywords nil '(("\\(^ *:END: *\\)" 1 '(face nil display "△"))) t)

   ;; Note: (add-to-list 'font-lock-extra-managed-props 'display)
   ))
;; ✨ Make Org properties look nice -- pretty emphasis markers:1 ends here

;; [[file:init.org::*✨ Make Org properties look nice -- pretty emphasis markers][✨ Make Org properties look nice -- pretty emphasis markers:2]]
(org-indent-mode +1)

;; useful for [link] and *formatted text*.
;; Make invisible parts of Org elements appear visible.
(unless my/personal-machine?
  (use-package org-appear))
;; ✨ Make Org properties look nice -- pretty emphasis markers:2 ends here

;; [[file:init.org::*Colourise clocking tasks with a block][Colourise clocking tasks with a block:1]]
;; work with org-agenda dispatcher [c] "Today Clocked Tasks" to view today's clocked tasks.
(add-hook 'org-agenda-finalize-hook
          (defun org-agenda-log-mode-colorize-block ()
            "Set different line spacing based on clock time duration.

   all the time below 30 minutes is displayed in a normal line, and the line height
   is proportionally expanded for more than 30 minutes.

   Making the log look better is mainly to stimulate my interest and motivation to
   record my times.
"
            (save-excursion
              ;; automatic color selection based on whether the theme background is black or white.
              (let* ((colors (cl-case (alist-get 'background-mode (frame-parameters))
                               ('light
                                (list "#F6B1C3" "#FFFF9D" "#BEEB9F" "#ADD5F7"))
                               ('dark
                                (list "#aa557f" "DarkGreen" "DarkSlateGray" "DarkSlateBlue"))))
                     pos
                     duration)
                (nconc colors colors)
                (goto-char (point-min))
                (while (setq pos (next-single-property-change (point) 'duration))
                  (goto-char pos)
                  (when (and (not (equal pos (point-at-eol)))
                             (setq duration (org-get-at-bol 'duration)))
                    ;; larger duration bar height
                    ;;
                    ;; This means the height is 1 unit within half an hour, and if it exceeds half an hour, it will increase by 0.5 units for every full hour, right?
                    (let ((line-height (if (< duration 15) 1.0 (+ 0.5 (/ duration 30))))
                          (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
                      (overlay-put ov 'face `(:background ,(car colors) :foreground "black"))
                      (setq colors (cdr colors))
                      (overlay-put ov 'line-height line-height)
                      (overlay-put ov 'line-spacing (1- line-height)))))))
            (olivetti-mode -1)))
;; Colourise clocking tasks with a block:1 ends here

;; [[file:init.org::*Automatically toggle timestamp prettifications][Automatically toggle timestamp prettifications:1]]
(add-hook
 'org-mode-hook
 (defun my/setup-toggle-timestamp-fontification ()
   ;; Disable with: (remove-hook 'post-command-hook #'my/toggle-timestamp-fontification)
   (add-hook
    'post-command-hook
    (defun my/toggle-timestamp-fontification ()
      "Hook function to check cursor entry/exit into Org timestamps & toggle fontification.

       Note: Since I'm using `org-element-context', this feature will be enabled if there's
       any timestamp near my point _regardless_ of whitespace. So to “leave”, say, a CLOSED:
       timestamp, the cursor needs to be on any NON-WHITESPACE outside the timestamp, such as
       any other text, or introduce a new text.
"
      (defvar my/last-ts nil)
      (defvar my/last-ts-visible nil)
      (when (member (buffer-file-name) (org-agenda-files))
      (let ((current-ts (my/cursor-in-timestamp-p)))
        (cond
         ((and current-ts (equal current-ts my/last-ts))
          ;; (message-box "Nothing new")
          (unless my/last-ts-visible
            (goto-char (org-element-property :begin my/last-ts))
            (my/toggle-line-fontification)
            (setq my/last-ts-visible nil)))
         ;; If I'm looking at a timestamp, and it's different from the last one, then show it.
         ((and current-ts (not (equal current-ts my/last-ts)))
          ;; (message-box "Welcome!")
          (save-excursion
            (goto-char (org-element-property :begin current-ts))
            (setq my/last-ts current-ts)
            (setq my/last-ts-visible t)
            (my/toggle-line-fontification)))
         ;; If I'm not looking at a timestamp, ensure the last one is fontified.
         (my/last-ts
          ;; (message-box "Bye!")
          (save-excursion
                       (goto-char (org-element-property :begin my/last-ts))
                       (my/toggle-line-fontification)
                       (setq my/last-ts nil)
                       (setq my/last-ts-visible nil))))))))

   (defun my/cursor-in-timestamp-p ()
     "Return Org timestamp if point is inside an Org timestamp; else null."
     (let ((element (org-element-context)))
       (when (member (org-element-type element) '(timestamp planning))
         element)))))
;; Automatically toggle timestamp prettifications:1 ends here

;; [[file:init.org::*Alternative approach to prettify planning keywords][Alternative approach to prettify planning keywords:1]]
(defun my/prettify-symbols-setup ()
  "Beautify keywords"
  (setq prettify-symbols-alist
        ;; Use both upcase and lowercase variations
		(mapcan (lambda (pair) (list pair (cons (upcase (car pair)) (cdr pair))))
				'(;; Org headers
				  ("#+title:"  . "")
				  ("#+author:" . "")
                  ("#+date:"   . "")
                  ;; Checkboxes
				  ("[ ]" . "") ;; TODO
                  ("[-]" . "") ;;  STARTED
				  ("[X]" . "") ;;  DONE
                  ;; Blocks
				  ("#+begin_src"   . "")
				  ("#+end_src"     . "")
				  ("#+begin_QUOTE" . "‟")
				  ("#+begin_QUOTE" . "”")
                  ;; Drawers
                  ;;    ⚙️
				  (":properties:" . "")
                  ;; Agenda scheduling
				  ("SCHEDULED:"   . "🕘")
				  ("DEADLINE:"    . "⏰")
                  ;; Agenda tags
				  (":project:"  . "☕")
				  (":work:"       . "🚀")
				  (":inbox:"     . "✉️")
				  (":goal:"       . "🎯")
				  (":task:"       . "📋")
				  (":thesis:"     . "📝")
				  (":uio:"        . "🏛️")
				  (":emacs:"      . "")
				  (":learn:"      . "🌱")
				  (":code:"       . "💻")
				  (":fix:"        . "🛠️")
				  (":bug:"        . "🚩")
				  (":read:"       . "📚")
				  ("#+filetags:"  . "📎")
				  (":wip:"        . "🏗️")
				  (":ct:"         . "😸") ;; Category Theory
                  (":verb:"       . "🌐") ;; HTTP Requests in Org mode
				  )))
  (prettify-symbols-mode))

(add-hook 'org-mode-hook        #'my/prettify-symbols-setup)
(add-hook 'org-agenda-mode-hook #'my/prettify-symbols-setup)
;; Alternative approach to prettify planning keywords:1 ends here

;; [[file:init.org::*Org-superstar][Org-superstar:1]]
(use-package org-superstar
  :after org
  :config
  (setq org-superstar-leading-bullet " ")
  (setq org-superstar-headline-bullets-list '("◆" "◇" "•" "⚬" "●" "○"))
  (setq org-superstar-special-todo-items t) ;; Makes TODO header bullets into boxes
  (setq org-superstar-todo-bullet-alist '(("TODO"     . 9744)
                                          ("PROG"     . 9744)
                                          ("NEXT"     . 9744)
                                          ("WAIT"     . 9744)
                                          ("DROP"     . 9744)
                                          ("QUESTION" . 9744)
                                          ("DONE"     . 9745)))
  :hook (org-mode . org-superstar-mode))
;; Org-superstar:1 ends here

;; [[file:init.org::*Example use of doc:consult--read][Example use of doc:consult--read:1]]
(use-package consult) ;; To get `consult--read'
;; Example use of doc:consult--read:1 ends here

;; [[file:init.org::*Questionnaire setup][Questionnaire setup:1]]
(require 'eieio)

;; See https://alhassy.com/ElispCheatSheet/#org71dcb45 for info on “defstruct”
(defstruct my/option
  "An option for use with my End of Day Review."
  label score description)

(defun assoc-by-label (options label)
  "Find the first `my/option' value in a list OPTIONS whose `my/option-label' is LABEL."
  (cl-find label options :key #'my/option-label :test #'string=))


(cl-defmethod pretty-print ((it my/option))
  (format "%d  --  %s  --  %s" (my/option-score it) (my/option-label it) (my/option-description it)))

(cl-defgeneric my-method (it)) ;; Need this to dispatch against primitive types, like “string” and “number”
(cl-defmethod  make-my/option-from-string ((it string))
  "Parse a “⟨score⟩ -- ⟨label⟩ -- ⟨description⟩” string into a `my/option' value."
  (-let [(score label description) (s-split "--"  it)]
    (make-my/option :score (string-to-number (s-trim score))
                    :label (s-trim label)
                    :description (s-trim description))))


(lf-documentation
 'my/daily-review-questionnaire
 'variable
 "
 Entries are of the form (headline . options)
 → HEADLINE is a string of the shape “⟨Org Property⟩:⟨Prompt⟩”.
 → OPTIONS are strings of the shape “⟨Numeric Score⟩ -- ⟨Label⟩ -- ⟨Note⟩”.
   ⇒ When omitted, we have an open-ended question.
   ⇒ If ⟨Label⟩ ends in “…”, then when it is chosen, a follow-up prompt starts to allow
     me to provide an alternate ⟨Note⟩ value. The entire option, including the new ⟨Note⟩,
    is then written as the value of ⟨Org Property⟩ in an Org heading.

A special entry is “ :random ”. All entries after it are considered optional
and 2 of them are randomly selected as part of the daily review.

⚠️ ⟨Org Property⟩ parts should be unique!
")


;; NOTE: Consider using an Org file as a data source.
(setq my/daily-review-questionnaire
      '(
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Mandatory questions asked each day                                       ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ("Happiness: Am I at peace with where I am right now?"
         ;; Am I happy? To find what's not making me happy and to prioritize what I should do because everything emanates from me and my internal state
         ;; Consider adding note follow-ups via “…” to some of these options. In the future, after I've used this often enough!
         "-1  --  Abysmal Low     --  I hate life."
         " 0  --  Low             --  What am I doing with my life?"
         " 1  --  Medium          --  Things are OK."
         " 2  --  High            --  I love my life ᕦ( ᴼ ڡ ᴼ )ᕤ"
         " 3  --  Extremely High  --  I'm king of the world!")
        ("Stress: How high are the demands upon me? Am I managing everything well?"
         " 2  --  Low             --  Things are chill; I'm gonna spend the day with my kids"
         " 1  --  Medium          --  Things are OK. It's just another day."
         " 0  --  High            --  People are getting on my nerves."
         "-1  --  Extremely High  --  I have so much to do; I'm freaking out!")
        ("Energy: How high is my capacity to do work? To be around others? Around myself?"
         "-1 --  Abysmal Low / Drained / Lacking Motivation  --  I need coffee and sleep."
         "0  --  Low / Sluggish                              --  I need coffee"
         "1  --  Medium / Calm                               --  I'm chill, doing my thing."
         "2  --  High / Enthusiastic                         --  I'm king of the world!")
        ("HoursSlept: How was my sleep last night?"
         "0  --  I slept                                     --  Man, I need to get my life together!"
         "1  --  I slept before midnight and awoke at ~7am   --  Good, but I can do better!"
         "2  --  I slept around 10pm and awoke at ~5am       --  Nice! Living the best life! Getting things done!" )
        ("HowISlept: How did I fall asleep last night?"
         "0  --  On my phone till exhaustion                 --  Man, I need to get my life together!"
         "1  --  My phone was on the other side of the room  --  Good, but I can do better!"
         "2  --  Cuddling my wife                            --  Nice! Living the best life!")
        ("Accomplished: I feel like I got done today what I set out to do?"
         "0  --  Nope…     --  Review my schedule in the morning and ensure it's a doable day!"
         "1  --  Almost…   --  Focus on the important tasks"
         "2  --  Yup…      --  Nice! Living the best life! Getting things done!"
         )
        ("Coffee: How many cups of coffee did I drink?"
         " 0  --   Zero   --  Nice! Exercise gives me energy!"
         "-1  --   One    --  I want to get things done."
         "-2  --   Two    --  I didn't eat well today, nor drink enough water."
         "-3  --   Three  --   Man, I need to get my life together!")

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Open ended questions (i.e., no options)                                  ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ("Relaxation: The most relaxing thing I did was …")
        ("Motivation: Why was I or wasn't motivated for something today?")


        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        :random  ;; 2 questions randomly chosen and asked each day                  ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; “Thematic” prompts: Each property acts as a theme.

        ("Time: Am I happy with how I am spending my time?") ;; Did I use most of my time wisely?

        ("Service: Was there anything I could easily do for someone else that I didn't do? Why not?")
        ("Service₂: Have I done anything to help someone in any way? Because a life lived only for oneself is only partly fulfilling.")
        ("Service₃: Did I wrong anyone who I owe amends to?")

        ("Values: What things are most important to me? (both things achieved and not yet achieved)")
        ("Value: Did it (whatever thing happened during the day) matter? To identify recurring things that either need to be dropped or addressed to better facilitate mental health.")
        ("Value₂: Did today matter? i.e., if I had slept all day, would anything really be any different?")

        ("GoalGetting: What did I do today to help achieve the things I have not yet achieved?")
        ("GoalGetting₂: What will I do tomorrow to further my achievement of things most important to me?")

        ;; ⇒ more positive thoughts, unlocked :)
        ("Gratitude: What am I grateful for today?") ;;  Makes you look at the big picture while appreciating something small that may be otherwise taken for granted.
        ("Anxiety: What problem is still on your mind, and what needs to be true for you to feel that this problem is resolved?")
        ("Worry: What am I worried about?") ;; Really helps clarify what to prioritize the next day, and gets the worries out of my head and onto paper right so I don't have to think about them in bed.

        ("Approval: What did I do today that I approve of?") ;; gets you out of all or nothing thinking. Do you approve of getting out of bed? Drinking water?
        ("Approval₂: What did I do well today? What did I do poorly today? What am I most grateful for? What is my goal?")

        ("Change: What is 1 thing I will do differently tomorrow?")
        ("Betterment: What can I do to be better tomorrow than I was today?")
        ("Improvement: How can anything I'm doing be improved upon? So that I can grow as a person and have more effectiveness in things I do.")

        ("Direction: Where am i going? What did i learn? What did i do that i liked? What can i do better?")
        ("Growth: What did I learn today?")
        ("Growth₂: Who do I need to be in order to master the day I had today. And how can I challenge myself to be that tomorrow.") ;; It helps with perspective, integrity and accountability.

        ("Annoyance: Of the things that happened to me today, what made me go “what the fuck?”")
        ("Joy: Of the things that happened to me today, what made me go “fuck yeah!”")
        ("Stress: Of the things that happened to me today, what made me go “oh fuck!”")

        ("Discomfort: What’s the most uncomfortable thing you encountered today?")
        ("Authenticity: What’s the most uncomfortable truth you said out loud today?")
        ("Inauthenticity: What did you mentally push aside today instead of thinking it through and openly saying your conclusions?")
        ("Rumination: What did you think about most today?")

        ("Competence:  What problems did I solve?")

        ("SelfCare: Did I put myself last today?") ;; Did I do myself justice today? If not, what will I do differently tomorrow?
        ))


(defun my/read-daily-review-properties ()
  "Returns a list of (PROPERTY . VALUE) pairs that could be `org-set-property' on a headline.

Makes use of `my/daily-review-questionnaire'.

At any time, press `C-.' to toggle adding a customised explanatory note to go along with a selection."
  (-let [(mandatory-questions random-questions) (-split-on :random my/daily-review-questionnaire)]
    (let* ((max-possible-score 0)
           (properties
            ;; Consider mandatory questions and 2 optional questions, chosen at random
            (cl-loop for (heading . option-strings) in (-concat mandatory-questions (-take 2 (--sort (< (random 2) 1) random-questions)))
                     for heading-info = (s-split ":" heading)
                     for property = (cl-first heading-info)
                     for prompt₀ = (cl-second heading-info)
                     for prompt = (if (s-ends-with? " " prompt₀) prompt₀ (concat prompt₀ " "))
                     for options = (--map (make-my/option-from-string it) option-strings)
                     for is-open-ended? = (null options)
                     collect
                     (cons property
                           (if is-open-ended?
                               (read-from-minibuffer prompt)
                             (cl-incf max-possible-score (apply #'max (mapcar #'my/option-score options)))
                             ;; If the user presses “C-.” they toggle on “note entry”.
                             (let (note-has-been-requested
                                   (my/note-map (make-sparse-keymap)))
                               (define-key my/note-map (kbd "C-.")
                                           (lambda () (interactive)
                                             (setq note-has-been-requested (not note-has-been-requested))
                                             (message (if note-has-been-requested "[You can enter a note after making a selection!]"
                                                        "[No entry note will be requested after selection.]"))))
                               (set-keymap-parent my/note-map minibuffer-local-map) ;; So that ⟨ENTER⟩ finalises the minibuffer, and not a literal new line!
                               (minibuffer-with-setup-hook
                                   (lambda () (use-local-map (copy-keymap my/note-map)))
                                 (consult--read (--map (my/option-label it) options)
                                                :prompt prompt
                                                :require-match t
                                                :annotate (lambda (label)
                                                            (format "\t ⟨ %s ⟩" (my/option-description (assoc-by-label options label))))
                                                :lookup (lambda (label _ _ _)
                                                          (-let [option (assoc-by-label options label)]
                                                            ;; If label ends in “…” or “C-.” pressed, prompt for a note.
                                                            (when (or (s-ends-with? "…" label) note-has-been-requested)
                                                              (-let [note (s-trim (read-from-minibuffer "Enter an explanatory note [ENTER to skip] "))]
                                                                (unless (s-blank? note)
                                                                  (setf (my/option-description option) note))))
                                                            (pretty-print option)))))))))))
      ;; Prepend a computed “daily score” property. Hopefully this value increases with time.
      (cons
       (thread-last properties
                    ;; The “ignore-errors” is here since some values are open ended, and so have no score.
                    (--map (or (ignore-errors (my/option-score (make-my/option-from-string (cdr it)))) 0))
                    (apply #'+)
                    float
                    ;; Note:  (thread-last x (/ max) (/ 100)) = (/ 100 (/ max x)) = (* 100 (/ x max))
                    (/ max-possible-score)
                    (/ 100)
                    (format "%.2f%%")
                    (cons "DailyScore"))
       properties))))



(defun my/org-align-property-values ()
  "Align Org property drawer by property name, then a digit, then on “--” markers.

Further reading:
→ https://pragmaticemacs.wordpress.com/2016/01/16/aligning-text/
→ https://blog.lambda.cx/posts/emacs-align-columns/
"
  (interactive)
  (save-excursion
    ;; Restrict to active region or current drawer
    (let* ((beg (if (use-region-p)
                    (region-beginning)
                  (save-excursion
                    (re-search-backward "^:PROPERTIES:" nil t)
                    (point))))
           (end (if (use-region-p)
                    (region-end)
                  (save-excursion
                    (re-search-forward "^:END:" nil t)
                    (point)))))
      ;; Pass 0: Align on property key, ie according to the first space
      (align-regexp beg end " " 0) ;; This works in general, to align Org properties: “M-x align-regexp ⟨RET⟩ ⟨SPACE⟩ ⟨RET⟩”
      ;; Pass 1: Align on the first ‘score value’: The first possibly negative number after a colon and whitespace.
      (execute-kbd-macro (kbd "C-u M-x align-regexp RET :\\(\\s-*\\) [-]?[0-9]+ RET RET RET n"))
      (align-regexp beg end ":\\(\\s-*\\) [-]?[0-9]+")
      ;; Pass 2: Align on all `--`
      (execute-kbd-macro (kbd "C-u M-x align-regexp RET \\(\\s-*\\)-- RET RET RET y")))))
;; Questionnaire setup:1 ends here

;; [[file:init.org::*Capture method][Capture method:1]]
(cl-defun my/insert-with-bg-colour (colour &rest text)
  "Inserts all of TEXT with background color COLOUR.

Example use: (my/insert-with-bg-colour \"pink\" \"Hello\\n\" (upcase \"world\"))

😉 If you want the colouring to continue to the end of the line, have a final \"\\n\".
💡 Use “M-x helm-colours” for inspiration."
  (-let [start (point)]
    (mapc #'insert text)
    (overlay-put (make-overlay start (point)) 'face `(background-color . ,colour))))


(cl-defun my/insert-with-fg-colour (colour &rest text)
  "Inserts all of TEXT with foreground color COLOUR.

Example use: (my/insert-with-fg-colour \"pink\" \"Hello\\n\" (upcase \"world\"))"
  (-let [start (point)]
    (mapc #'insert text)
    (overlay-put (make-overlay start (point)) 'face `(foreground-color . ,colour))))


;; “r”eview for the “d”ay
(bind-key*
 "C-c r d"
 (def-capture "🔄 Daily Review 😊"
              "🌿 Reviews 🌱"
              ;; Note: I prefer %T so that I get an active timestamp and so can see my review in an agenda
              ;; that looks at that day. That is, my review are personal appointments.
              "* :Daily:Review: \n:PROPERTIES:\n:CREATED: %T\n:END:\n"
              ;; Insert fancy date in header
              (defun my/fancy-date-string ()
                "Return a string like 'Saturday, May 17, 2025 – Day 137 of the year'."
                (let ((today (current-time)))
                  (format "%s – Day %d of the year"
                          (format-time-string "%A, %B %e, %Y" today)
                          (string-to-number (format-time-string "%j" today)))))
              (beginning-of-buffer)
              (org-beginning-of-line)

              (insert (my/fancy-date-string) " ")
              ;; Let's add some properties by prompting the user, me.
              (-let [properties (my/read-daily-review-properties)]
                (cl-loop for (property . value) in properties
                         do (org-set-property property value))
                ;; Add Daily Score to the start of the headline
                (beginning-of-buffer)
                (org-beginning-of-line)
                (insert (format "﴾%s﴿ " (cdr (assoc "DailyScore" properties)))))
              ;; Let's align them
              (my/org-align-property-values)
              ;; Let's insert a quote
              (progn
                (end-of-buffer)
                (my/insert-with-fg-colour "grey" "\n\n#+begin_quote_of_the_day\n")
                (my/insert-with-bg-colour "pink" (my/string-fill-column-and-center 70 (my/random-quote)) "\n")
                (my/insert-with-fg-colour "grey" "#+end_quote_of_the_day\n\n"))
              ;; Let's see some stats
              (progn
                (my/insert-with-fg-colour "grey" "\n#+begin_stats_of_the_day\n")
                ;; Randomise the order of stats, to keep things interesting.
                (--map (my/insert-with-bg-colour "aquamarine" (eval it) "\n")
                       (--sort (< (random 2) 1)
                               '((my/age-in-days-weeks-years)
                                 (my/percentage-of-life-spent)
                                 (my/git-commit-count)
                                 (my/how-long-I-have-been-at-my-job)
                                 my/weather-brief
                                 (pp-current-islamic-date)
                                 ;; Prayer Times
                                 (progn
                                   (ignore-errors  (my/update-prayer-times-task))
                                   (format "📿 الفجر %s ∣ الظهر %s ∣ المغرب %s"
                                           my/dawn-prayer-time my/noon-prayer-time my/sunset-prayer-time))
                                 ;; Emacs & OS up times
                                 (let* ((emacs (car (s-split " " (emacs-uptime))))
                                        (os-uptime (shell-command-to-string "uptime"))
                                        (os (when (string-match "\\([0-9]+\\) days" os-uptime)
                                              (match-string 1 os-uptime))))
                                   (format "🆙 Emacs up for %s days; OS up for %s days" emacs os))
                                 ;; Journal Line Count
                                 ;; If it's much smaller, look at git diff to figure out what happened!
                                 (-let [♯lines
                                        (with-current-buffer "my-life.org"
                                          (save-restriction
                                            (widen)
                                            (cl-format nil "~:d" (count-lines (point-min) (point-max)))))]
                                   (format "✍️ my-life.org has %s lines" ♯lines)))))
                (my/insert-with-fg-colour "grey" "#+end_stats_of_the_day\n\n"))
              
              (my/show-life-purpose-statement-then-remove-it-after-I-read-it)

              (progn
                (my/insert-with-fg-colour "grey" "\n#+begin_word_of_the_day\n")
                (my/insert-with-bg-colour "RosyBrown1" "💬 " (my/word-of-the-day) "\n")
                (org-fill-paragraph)
                (my/insert-with-fg-colour "grey" "#+end_word_of_the_day\n\n"))

  (my/insert-with-bg-colour "chartreuse" (lf-string "
                     ** Clean your inboxes!      [0%]
                     
                     Empty all your physical and digital workspaces. Move things to their
                     place and delete everything that can distract work in the upcoming week.
                     
                     
                                    /A cluttered workspace leads to an anxious mind!/
                     
                     
                     1. [ ] 🍽️🪑📚 *Clean Desk*. Clear off your desk from clutter and papers, receipts,
                        and miscellaneous paper-based materials. Wipe down your desk if you want.
                     
                     2. [ ] 🗑️ *Disable Youtube & Chrome* :: They needlessly suck-up my time. 📱
                     
                     3. [ ] *Empty Desktop & Download folders.* Move files to their appropriate
                        location. Both locations should be empty when you're done.
                        - Consider attaching files to Org headlines.
                          
                     4. [ ] 📭  💬 🌐 *Empty out inboxes: Clear Email, Org, Slack Read Later, Browser Tabs*
                        - [ ] 📧 Convert all emails to tasks. /Do not reply to emails right /now/.
                        - [ ] ✉️ Open unopened letters & make tasks for required follow-ups, and discard old ones; finances as well.
                        - [ ] Convert all Slack “read later” bookmarks and all open browser tabs into
                          tasks marked ~:ConsumeContent:~, then schedule the 𝓃-th article 𝓃 Tuesday's
                          from now, so that I'm making progress on them if I need to be doing so;
                          otherwise they can be reference matter.
                        - [ ] Do a search for “unsubscribe” in my email and unsubscribe from newsletters I don't read anymore. [MONTHLY?]
                           + it’s looking for any email that contains the word “unsubscribe,” which is required of all emails that are sent from a mailing list
                     
                     5. [ ] 💼 *Clean up Work Notes.* Look at my =Work= headline in Org and ensure it's not
                        messy; e.g., references are in the right place.
                     
                        🧹 Visit each of my buckets and clean it out: Ensure things are hierarchical,
                        archive done things, move useful notes to References. Reduce anxiety from
                        mess so that each headline is nice and tidy and useful.
                     
                     
                           /“Be regular and orderly in your life so that you may be violent and
                                       original in your work.” /--- Gustav Flaubert
                      "))

              ;; ⇒ 🤔 What did I do today? ⇐
               (when nil save-excursion
              (insert 
               ;; TODO: Make `my/what-did-i-work-on-today' tag an optional arg to just retrive the string instead of putting it in a buffer
               (save-excursion
                 (let (result)
                   (my/what-did-i-work-on-today)
                   (setq result (buffer-substring-no-properties (point-min) (point-max)))
                   (kill-buffer)
                   result))))
               
               (insert
"
Write a short story for the day.

                     # Use Clock-info to see where I clocked-in.
                     #                    [[elisp:(save-restriction (widen) (my/what-did-i-work-on-this-week))][⇒ 🤔 What did I do today? ⇐]]
                     # Also consider looking at the “log view”, via the C-c C-z notes #
 "               )

              ;; I think it'd be neat to insert my clocked-in / logs of the day here.
              ;; Look at what I clocked into this day/week! Get a great idea of what I've done with my time, in detail. Also, see ~C-c a v L~.
              (save-excursion ;; I want cursor to stay here.
                (let (todays-agenda org-agenda-finalize-hook)
                  (org-agenda-list 1)
                  (org-agenda-log-mode '(4))
                  (setq todays-agenda (buffer-string))
                  (org-agenda-quit)
                  (my/insert-with-fg-colour "grey"  "\n\n#+begin_agenda_for_the_day\n")
                  (my/insert-with-bg-colour "LightBlue1" todays-agenda)                
                  (my/insert-with-fg-colour "grey" "#+end_agenda_for_the_day")
                  (insert "\n⟨🤔 Did I get everything I wanted done? Perhaps, I underestimated time for things? 🗯️⟩")
                  (insert "\n~C-c a w v c~ to check for time gaps and review time for the past week. And to see what I worked on, and where I spent too much time or too little.")))

              
              (insert "\nSay, “Today, my purpose was to have fun and do a good job at work! I did it! (｡◕‿◕｡)”")

              ;;
              ;; MA: Consider adding other journal prompts here, whose replies may be long-form.
              ;; E.g., pick one, or two, random prompts.
              (message "To journal is to live; congratulations on another entry!")))






(cl-defun my/show-life-purpose-statement-then-remove-it-after-I-read-it ()
  (set-mark-command nil)
  (-let [purpose "                   When people say “What are you doing?”,
                        You say ⟪“Things that please me.”⟫
                          They say “Toward what end?”,
                            and you say ⟪“Pleasure.”⟫
                They say “But really, what are you working on?”
                         You say ⟪“Having a good time!”⟫
"]
    ;; NOTE: explore more faces via M-x highlight-phrase.
    (insert (propertize purpose 'font-lock-face 'hi-green)))
  (while (not (equal "yes"
                     (consult--read '("yes" "no") :prompt "Read “Life Purpose”?"
                                    :annotate (lambda (it) (format " ⟨%s⟩"
                                                              (if (equal it "no")
                                                                  "C'mon man, read it"
                                                                "That's right, live the good life!")))))))
  (backward-delete-char 1))



(defun my/word-of-the-day ()
  (let (result)
    (org-web-tools-read-url-as-org "https://www.merriam-webster.com/word-of-the-day")
    (setq result (format
                  "%s\n%s"
                  (substring-no-properties (org-get-heading t t t t))
                  (progn
                    (org-next-visible-heading 2)
                    (end-of-line)
                    (thread-last
                      (buffer-substring-no-properties (point) (progn (org-next-visible-heading 1) (point)))
                      (s-replace-regexp ".*See the entry.*" "")
                      (s-replace-regexp "^//" "Example: ")
                      s-trim))))
    (kill-buffer)
    result))


(require 'calendar)
(require 'cal-islam)
(defun pp-current-islamic-date ()
  "Return the current Islamic (Hijri) date as a readable string (e.g., \"21 Ramadan 1445\")."
  (require 'calendar)
  (let* ((today (calendar-current-date))          ; Gregorian date (MONTH DAY YEAR)
         (abs-date (calendar-absolute-from-gregorian today))) ; Convert to absolute days
    (-let [(month day year) (calendar-islamic-from-absolute abs-date)]  ; Convert to Islamic date
      (let ((month-name (aref calendar-islamic-month-name-array (1- month))))
        (format "🌙 %d %s %d ⟨Islamic Date⟩" day month-name year)))))


(defun my/get-prayer-times ()
  "Fetch and display prayer times for the given LAT and LON using AlAdhan API."
  ;;  Get (LAT . LON) based on IP geolocation via ipinfo.io.
  (let* ((json-object-type 'alist)
         (json (json-read-from-string
                (shell-command-to-string "curl -s https://ipinfo.io/json")))
         (loc (alist-get 'loc json)) ; loc is "LAT,LON"
         (parts (split-string loc ","))
         (lat (car parts))
         (lon (cadr parts))
         (url (format "http://api.aladhan.com/v1/timings?latitude=%s&longitude=%s&method=0"
                      lat lon)))
    (with-current-buffer (url-retrieve-synchronously url t t 5)
      (goto-char url-http-end-of-headers)
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (json-key-type 'symbol)
             (data (json-read))
             (timings (alist-get 'timings (alist-get 'data data))))
        (kill-buffer) ;; clean up the temp buffer
        (setq my/dawn-prayer-time (alist-get 'Fajr timings)
              my/noon-prayer-time (alist-get 'Dhuhr timings)
              my/sunset-prayer-time (alist-get 'Maghrib timings))))))
;;
(defun my/update-prayer-times-task ()
  (my/get-prayer-times)
  (save-excursion
    (with-current-buffer "my-life.org"
      ;; (org-id-goto "prayer-times") ⟵ Does not honour with-current-buffer
      (save-restriction
        (widen)
        (beginning-of-buffer)
        (re-search-forward ":ID: prayer-times")
        (while (re-search-forward "^<%%" nil t)
          (beginning-of-line)
          (kill-line))
        (thread-last
          (list my/dawn-prayer-time my/noon-prayer-time my/sunset-prayer-time)
          (--map (format "<%%%%(progn 'everyday \"%s\")>" it))
          (s-join "\n")
          insert)))))



;; MA: Maybe add another arg to denote flip a coin, and either emit poetically or else plainly.
(when nil cl-defun my/age-in-days-weeks-years (&optional (birthdate my\birthday))
  "Prompt for birthdate (YYYY-MM-DD) and display age in days, weeks, and years — poetically."
  (interactive)
  (let* ((birth-time (date-to-time (concat birthdate " 00:00:00")))
         (now (current-time))
         (days-old (/ (float-time (time-subtract now birth-time)) 86400))
         (weeks-old (/ days-old 7))
         (years-old (/ days-old 365.25)) ;; Approximate with leap years
         (message
          (format (concat
                   "🌞 Since the moment of your arrival on this plane:\n\n"
                   "🗓️  You have walked the earth for %d days\n"
                   "📅  Which is roughly %d weeks of stories\n"
                   "🌀  Or %.1f full solar revolutions\n\n"
                   "⏳ Time has shaped you across %d sunsets and %d moonrises.\n"
                   "✨ You are the sum of every breath taken since then.")
                  (floor days-old)
                  (floor weeks-old)
                  years-old
                  (floor days-old)
                  (floor (* 12.37 years-old))))) ;; Roughly average moonrises per year
    (message "%s" message)))

;; (my/age-in-days-weeks-years)

(when nil
  (cl-defun my/age-in-days-weeks-years (&optional (birthdate my\birthday))
    "Reveal the user's age with poetic, symbolic flair and mystic metaphors."
    (interactive)
    (let* ((birth-time (date-to-time (concat birthdate " 00:00:00")))
           (now (current-time))
           (days-old (/ (float-time (time-subtract now birth-time)) 86400))
           (weeks-old (/ days-old 7))
           (years-old (/ days-old 365.25))
           (sun-orbits (floor years-old))
           (moon-dances (floor (* 12.37 years-old)))
           (heartbeats-est (floor (* years-old 365.25 24 60 72))) ;; ~72 BPM
           (blinks-est (floor (* years-old 365.25 1440 15))) ;; ~15 blinks/min
           (soul-animal (seq-random-elt '("a moth with a crystal spine"
                                          "an eel made of memory"
                                          "a fox that speaks in riddles"
                                          "a library built from your dreams"
                                          "a crow carrying your name in code"
                                          "a jellyfish who remembers you"
                                          "a mirror that looks back differently each year")))
           (cipher (char-to-string (+ 9472 (random 40)))))
      (message
       (format (concat
                "━━━━━━━━━━━━━━━ 🕰️  𝒯𝒽𝑒 𝒞𝒽𝓇𝑜𝓃𝑜-𝒜𝒸𝒸𝑜𝓊𝓃𝓉 ━━━━━━━━━━━━━━━

☼ You have orbited the sun %d times.
🌘 The moon has danced overhead %d nights.
💓 Your heart has whispered its rhythm ~%d times.
👁️ Your eyes have blinked through ~%d moments.

🦴 Time has carved %d days into your bones,
   etched %d weeks across your skin.

🧬 Your soul currently takes the form of: %s

✶ Secret cipher for today: [%s]
   (Deciphering it may unlock an ancient memory.)

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
               sun-orbits
               moon-dances
               heartbeats-est
               blinks-est
               (floor days-old)
               (floor weeks-old)
               soul-animal
               cipher))))

  )

(load-file "~/Dropbox/private.el") ;; Loads “my\⋯” variables
;;
(cl-defun my/age-in-days-weeks-years (&optional (birthdate my\birthday))
  "Prompt for birthdate (YYYY-MM-DD) and display age in days, weeks, months, and years."
  (interactive)
  (let* ((birth-time (date-to-time (concat birthdate " 00:00:00")))
         (now (current-time))
         (days-old (/ (float-time (time-subtract now birth-time)) 86400))
         (weeks-old (/ days-old 7))
         (months-old (/ days-old 30.44)) ;; average month length
         (years-old (/ days-old 365.25))) ;; approximate year with leap years
    (cl-format nil "🥳 I am now ~:d days old; which is ~:d weeks old; which is ~:d months old; which is ~,1f years old."
               (floor days-old) (floor weeks-old) (floor months-old) years-old)))
;;
;; Elisp's “format” is not as capable as Common Lisp's “format”.
;; E.g., there's no equivalent of (cl-format nil "~:d" 1000000)
;; which prints numbers with comma separators.
;; See https://gigamonkeys.com/book/a-few-format-recipes for more uses.
(use-package cl-format)

(defun my/git-commit-count ()
  (thread-last user-full-name
               (format "cd %s; git log --author='%s' --pretty=oneline | wc -l" my\work-dir)
               shell-command-to-string
               string-to-number
               (cl-format nil "🤖 I have made ~:d commits at work")))


(defun my/how-long-I-have-been-at-my-job ()
  "Show how long ago I've been at my job, based on when I made my first Git commit."
  (interactive)
  (let* ((author (string-trim (shell-command-to-string "git config user.name")))
         (first-date-str
          (string-trim
           (shell-command-to-string
            (format "cd %s; git log --author='%s' --reverse --pretty='%%ad' --date=iso | head -n 1" my\work-dir author))))
         (first-time (date-to-time first-date-str))
         (now (current-time))
         (diff (time-subtract now first-time))
         (days (/ (float-time diff) 86400))
         (years (floor (/ days 365.25)))
         (months (floor (/ (- days (* years 365.25)) 30.44)))) ; approximate months
    (message "💼 I've been at my job for %d year%s and %d month%s. I joined %s."
             years (if (= years 1) "" "s")
             months (if (= months 1) "" "s")
             (substring first-date-str 0 10))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/percentage-of-life-spent ()
  (format "⏳ 〔%s life ∣ %s year ∣ %s month ∣ %s week ∣ %s day〕 elapsed"
          (my/percentage-of-life-elapsed)
          (my/percentage-of-year-elapsed)
          (my/percentage-of-month-elapsed)
          (my/percentage-of-week-elapsed)
          (my/percentage-of-waking-day-elapsed)))

(defun my/percentage-of-year-elapsed ()
  "Calculate percentage of current year that has elapsed.
Returns a float between 0 and 100."
  ;; (calendar-day-of-year-string) ⇒ "Day 143 of 2025; 222 days remaining in the year"
  (let* ((date (calendar-current-date))
         (year (calendar-extract-year date))
         (day-in-year (calendar-day-number date))
         (total-days-in-year (calendar-day-number (list 12 31 year)))
         (days-remaining (- total-days-in-year day-in-year)))
    (format "%.1f%%" (* 100 (/ (float day-in-year) total-days-in-year)))))


(defun my/percentage-of-month-elapsed ()
  "Calculate percentage of current month that has elapsed.
Returns a float between 0 and 100."
  (-let [(month day year) (calendar-current-date)]
    (format "%.1f%%" (* 100 (/ (float day) (calendar-last-day-of-month month year))))))


(defun my/percentage-of-week-elapsed ()
  "Calculate percentage of current week that has elapsed.
Returns a float between 0 and 100."
  (format "%.1f%%" (thread-first (calendar-current-date) calendar-day-of-week float (/ 7) (* 100))))


(defun my/percentage-of-waking-day-elapsed ()
  "Calculate percentage of current (waking) day that has elapsed.
Returns a float between 0 and 100."
  (let* ((now (decode-time))
         ;; (current-hour (nth 2 now))
         (current-awake-hour (- (nth 2 now) 8))
         (current-minute (nth 1 now))
         (current-second (nth 0 now))
         ;; (total-day-seconds (* 24 3600))
         (total-day-awake-seconds (* 16 3600))
         (elapsed-seconds (+ (* current-awake-hour 3600)
                             (* current-minute 60)
                             current-second)))
    (format "%.1f%%" (* 100 (/ (float elapsed-seconds) total-day-awake-seconds)))))


(defun my/percentage-of-life-elapsed ()
  "Calculate percentage of life elapsed based on birth date (January 1, 1990).
Returns a float between 0 and 100."
  (interactive)
  (let* ((birth-time (date-to-time (concat my\birthday " 00:00:00")))
         (now (current-time))
         (days-old (/ (float-time (time-subtract now birth-time)) 86400))
         (years-old (/ days-old 365.25))) ;; approximate year with leap years)
    (format "%.2f%%" (* 100 (/ years-old 70)))))
;; Capture method:1 ends here

;; [[file:init.org::*Implementation][Implementation:1]]
;; “r”eview for the “w”ay
;; 
;; Reflect on what went well and what could have gone better. Update your
;; to-do and projects list. Remove unimportant tasks and update your
;; calendar with any new relevant information.
;; 
;;  Prepend a new section to “Weekly Log” listing what I've done in the
;;                        past week; useful for standups, syncs, and performance reviews.
(bind-key*
 "C-c r w"
 (def-capture "🔄 Weekly Review 😊"
              "🌿 Reviews 🌱"
              ;; tldr on “ts.el”:
              ;; Today           = (ts-format) ;; ⇒ "2025-05-26 16:46:52 -0400"
              ;; Today + 10years = (ts-format (ts-adjust 'year 10 (ts-now))) 
              ;; Day of the week 2 days ago = (ts-day-name (ts-dec 'day 2 (ts-now))) ;; ⇒ "Saturday"
              ;; “What day was 2 days ago, Saturday? What day will it be in 10 years and 3 months?”
              ;; See https://github.com/alphapapa/ts.el, which has excellent examples.
              ;; 😲 Nice human formatting functions too!
              (-let [week♯ (ts-week-of-year (ts-now))]
                (-let [month-name (ts-month-name (ts-now))]
                  ;; Note: I prefer %T so that I get an active timestamp and so can see my review in an agenda
                  ;; that looks at that day. That is, my review are personal appointments.
                  (format "* Weekly Review ♯%s ---/“go from chaos to clarity”!/ [/] :%s:Weekly:Review: \n:PROPERTIES:\n:CREATED: %%T\n:END:\n"
                          week♯
                          month-name)))

              ;; Let's add some properties by prompting the user, me.
              (-let
                  [my/daily-review-questionnaire
                   '(
                     ("SocialScore: Did I see family *and* did I call or message a friend?"
                      " 0  --  No             --  What am I doing with my life?"
                      " 1  --  Yes…           --  Sweet, who was it? What did you do?")
                     ("FaithScore: Did I go to this Mosque this week? Or read passages from the Quran?"
                      " 0  --  No             --  What am I doing with my life?"
                      " 1  --  Yes…           --  Sweet, what did you do?")
                     ("MarriageScore: How are things with my wife?"
                      "-1  --  Abysmal Low     --  I hate life."
                      " 0  --  Low             --  What am I doing with my life?"
                      " 1  --  Medium          --  Things are OK."
                      " 2  --  High            --  I love my life ᕦ( ᴼ ڡ ᴼ )ᕤ"
                      " 3  --  Extremely High  --  I'm king of the world!")
                     ("HealthScore: Did I go for a run or do a workout this week?"
                      " 0  --  No             --  What am I doing with my life?"
                      " 1  --  Yes…           --  Sweet, what did you do?"))]
                ;; 📊 Other metrics to consider keeping track of:
                ;; ⇒ Hours worked
                ;; ⇒ Tasks completed

                (-let [properties (my/read-daily-review-properties)]
                  (cl-loop for (property . value) in properties
                           do (org-set-property property value))
                  ;; TODO: Make my/read-daily-review-⋯ attach a WeeklyScore, not a
                  ;; DailyScore? Maybe keep the latter for easy reference? E.g.,
                  ;; next line is copy/pasted from Daily Review.
                  ;;
                  ;; Add Daily Score to the start of the headline
                  (beginning-of-buffer)
                  (org-beginning-of-line)
                  (insert (format "﴾%s﴿ " (cdr (assoc "DailyScore" properties)))))
                ;; Let's align them
                (my/org-align-property-values))

              (org-set-property "WHY" "Ensure everything is on track! Be proactive, not reactive! Be in control of my life! 😌 Have a sense of closure and wrap-up before the weekend! ☺️")
              
              (end-of-buffer)


              (insert
               (let* ((♯lines (save-restriction (widen) (count-lines (point-min) (point-max))))
                      (last-time (s-trim (shell-command-to-string "git log -1 --pretty=%s")))
                      ;; Note that there's no `git push' since this is local ---Github may expose my notes to AI, no thank-you.
                      (save-incantation (format "git add .; git commit -m 'Weekly Review Start, save %s lines on %s'" ♯lines  (ts-format))))
                 (lf-string "
                     ***** Closing up last week

                     ****** TODO ⟨0⟩ Commit last week         [0%]

                     + [ ] Commit & push all the changes before the review

                       \t 🤖 Last time ∷ “ ${last-time} ”
                       \t 🛋️ my-life.org line count ∷ ${♯lines}
                       \t   # If it's significantly less, then look at diff to ensure I didn't lose anything important.
                       \t ⁉️ [[elisp:(shell-command-to-string \"${save-incantation}\")][Click to commit!]]

                     + [ ] ⁉️ [[elisp:(progn (widen) (funcall-interactively #'org-lint))][Lint my-life.org]]

                       \t Importantly this mitigates [[https://en.wikipedia.org/wiki/Link_rot][link rot]]
                       \t and ensures that when I do bulk find-replace actions that I haven't
                       \t royally messed things up.

                     ****** TODO ⟨1⟩ Friday Recap: What's the story of the past week?  [0%] :Standups:
                     :PROPERTIES:
                     :WHY: Recognise accomplishments, express self-gratitude, and debug!
                     :END:
                     

                     1️⃣ First, write a short story recapping the week.
                     


                     2️⃣ Reflect on what went well and what could have gone better. 
                     Answer [[https://nesslabs.com/plus-minus-next][the following 3 questions]]:
                     # (♯1 & ♯2 useful for bragging about yourself when it's time to do performance reviews!)
                     
                     1. [ ] ➕ Wins: What went well and why? ✅
                        # What could have caused things to go so well? Maybe I can duplicate this next week!
                        # Re-read [[https://jvns.ca/blog/brag-documents/][Get your work recognized: write a brag document]]



                     2. [ ] ➖ Challenges: What didn't go well? ⚠️
                        # How can I improve to mitigate bad weeks?


                     3. [ ] 🔁 Trends: Are there any recurring patterns (positive or negative)?
                        # Consider looking at previous Weekly Review if you need insight.
                        # [[elisp:(org-ql-search org-agenda-files '(and (tags \"Weekly\") (tags \"Review\") (ts :from -30)))][🤖 WRs of the month]]


                     4. [ ] 🔀 Super briefly: What will you focus on next week?
                        # Did I get any “$10k” tasks done? Why or why not?
                                         

                     # Use Clock-info to see where I clocked-in.
                     #                    [[elisp:(save-restriction (widen) (my/what-did-i-work-on-this-week))][⇒ 🤔 What did I do in the past 7 days? ⇐]]
                     # Also consider looking at the “log view”, via the C-c C-z notes
                     # Look at where all my time went, and think about whether things could’ve gone better.


                    3️⃣ One More Question to reflect on...
"))) ;; TODO: 

              (insert "\n + [ ] "
                      (seq-random-elt
                       '(
                         "😄 What was the most enjoyable work activity of the last week?"
                         "🤦‍♂️ What were some frustrating or boring moments you had? How can you avoid that going forward?"
                         "🔧 Adjustments: What to stop? What to start? What to continue?"
                         "🟢 What should I continue doing?"
                         "🔴 What should I stop or change?"
                         "🧪 What's one small experiment to try next week?" 
                         "😁 What are your biggest and most exciting challenges for the week to come?
                          What do you need to get there?"
                         "💭 Thoughts for the week to come: What are you thinking about for next week?"
                         "🖼️ Memories of the Amazing, Interesting and Unique:
              Share a photo, quote, line, or something from the
              previous week."
                         "🎯 Did you accomplish your goals? Which ones and how
              did it go? Share your goal tracking and record you
              progress"
                         "🐾 How did I feel this week overall?"
                         "🕰️ Was my time aligned with my goals?"
                         "👀 What distracted me?"
                         "🎇 What energized me?"
                         "📚 What did I learn?"                         
                         "🪨 What are my biggest challenges (or “boulders”)
              for the week to come? Think about which tasks will
              have the highest value in me reaching my potential
              and being successful.")))              
              
              (insert              "
****** TODO ⟨2⟩ Archive completed and cancelled tasks      [0%]

1. [ ] Look through the ~:LOG:~ for useful information to file away into my
   References.
   - If there's useful info, capture it with ~C-c C-c~, then archive the original
     tree for clocking purposes.
   - If clocking purposes do not matter, say for personal or trivial tasks, then just delete the tree.


     TODO: Agenda view: The list of completed and cancelled tasks to archive
     [[org-ql-search:(and (done) (not (tags \"Top\")) (closed :to ,(- (calendar-day-of-week (calendar-current-date)))))][📜 Items to review: Mine for useful info then archieve or delete. ☑️]]

***** Looking forward to next week ---/mentally try to see where I should be going/

****** TODO ⟨3⟩ Prioritize and schedule!    [0%]

0. [ ] For the “Waiting” list, have others completed their tasks?

   - Agenda view: The list of to-do or waiting tasks without SCHEDULED or DEADLINE


1. [ ] *Check Calendar*. Look at company calendar for the upcoming 2 weeks;
   add items to your todo list if needed.

2. [ ] Find relevant tasks: What are my “sprint goals” and “quarterly goals”?
   - What is assigned to me in Jira /for this sprint/?
   - Any upcoming deadlines?
   - Look at your “Someday/Maybe” list to see if there's anything worth doing.
   - get an overview about what you want to achieve in near future: your time
     and energy are finite, tasks are not
     - /Getting a sight of the forest can be very energizing and inspiring too,
       while the endless trees can feel overwhelming or pointless./
     - Relook some of the more recently processed tasks. Task priorities may change
       as time progresses, and it is possible that with a huge onslaught of new
       tasks, some important, older tasks may be left incubating. It’s also possible
       that some tasks are no longer necessary.


3. [ ] Assign a [[https://radreads.co/10k-work/][dollar value]] to your work: $10 (low skill, not important), $100, $1k, and $10k (high skill, prized effort).
   (MA: Aside: Change priorities to be render like this?)

   The Zen To Done system recommends scheduling your Most Important Tasks ($10k)
   on your calendar each week so that by the end of the week you have completed
   something of significance. It ensures that the more important things get
   done.


4. [ ] To ensure the week is doable, add efforts to tasks for the week
   then ensure the effort estimate is actually realistic. Also check each
   day and ensure it's realistic!
 
   # You can also add that to the column-mode (org-columns) to get a quick overview for a file (leave with org-columns-quit). Customize:
   # (org-columns-default-format \"%25ITEM %TODO %3PRIORITY %TAGS %17Effort(Estimated Effort){:} %CLOCKSUM\")

   - [ ] Look at daily agenda view for the next week and make sure it'd doable and not
         overloaded! I don't want to keep pushing things since my days are unrealistic!


5. [ ] Decide your tasks for the week and time block your calendar.

     *Focus on important tasks! Not low priority no-one-cares ‘fun’ efforts!*

     *Embrace trade-offs.* You can't do it all. Realize that when you're
      choosing to do one task, you're saying “no” to many other tasks. And
                              that's a good thing

     Finally, schedule time on your calendar to work on your tasks. This is
       called [[https://dansilvestre.com/time-blocking/][time blocking]]. Set alerts for critical tasks.
       - Look at all items with a deadline in the next month:
         Are they realistically broken down into doable tasks and scheduled?


6. [ ] Study the next week’s agenda: look at any important scheduled tasks or
      deadlines, and decide whether any preparatory work will need to be done.


 /By [[https://dansilvestre.com/weekly-planning/][planning your week]] in advance, you prevent distractions from ruining your
             day. You remain focused on your most important tasks./

****** TODO ⟨4⟩ Commit planning    [0%]

+ [ ] ⁉️ [[elisp:(shell-command-to-string \"git add .; git commit -m 'End Weekly Review'\")][Click to commit all the changes /after/ the review]]

****** TODO ⟨5⟩ Am I getting happier? [0%]

")
              (-let [start (point)]
                (insert (my/get-table-of-daily-scores-for-last-week))
                (center-region start (point)))
              (insert "\n" (my/percentage-of-life-spent) "\n")
              (org-update-statistics-cookies 'all)
              (insert "\nSay, “Today, my purpose was to have fun and do a good job at work! I did it! (｡◕‿◕｡)”")
              ;; Show todo sparse tree. (See also: org-ql-sparse-tree)
              (execute-kbd-macro (kbd "C-c / t"))
              (message "To journal is to live; congratulations on another entry!")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Looping through Org headlines; i.e., iterating over my notes
;;
;; I got a lot of Org-Mode headings and I want to modify their properties (add,
;; remove, edit). Is there a function to do the same modifications on each
;; heading?
;;
;; \=>
;;
;; Adds an Edited_Date property to org headings matching the given condition.
;; Alternatively one could use `org-map-entries', however the filtering syntax is less than ideal.
(when nil

  ;; Do a modification
  (org-ql-query
    ;; Called with point at the start of each note (ie Org headline).
    ;; SELECT is a function which is called on each matching entry with point at the beginning of its heading.  
    :select (lambda () (org-set-property "Edited_Date" (ts-format)))
    :from org-agenda-files
    ;; Daily Reviews Created in Last Week
    :where   '(and (tags "Daily") (tags "Review") (ts :from -5)))

  ;; See the changes.
  (org-ql-search org-agenda-files '(and (tags "Daily") (tags "Review") (ts :from -5)))

  ;; The org-ql-query function iterates through all the headings meeting the WHERE criteria in the determined FROM scope, and then calls the specified function SELECT function at each of those headings.

  ;; The SELECT function accepts no arguments and is called at the beginning of each Org heading.
  ;; If the optional WHERE argument is present, the headings will first be filtered based on it and then the SELECT will be called on only those.

  ;; Get all titles
  (org-ql-query
    ;; :select (lambda () (thing-at-point 'line)) ;; Also, OK.
    :select (lambda () (org-get-heading 'no-tags 'no-todo))
    :from org-agenda-files)

  )  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/get-table-of-daily-scores-for-last-week ()
  (thread-last      
    (org-ql-query
      ;; Called with point at the start of each note (ie Org headline).
      :select (lambda ()
                (list
                 (ts-format "%A %Y-%m-%d" (ts-parse-org (org-entry-get nil "CREATED")))
                 (org-entry-get nil "DailyScore")))
      :from org-agenda-files
      ;; Daily Reviews Created in Last Week
      :where   '(and (tags "Daily") (tags "Review") (ts :from -5)))
    (cons '("Date" "Daily Score"))
    my/pp-list-of-lists-as-table
    (concat "Daily Scores for last week \n\n")))


(defun my/pp-list-of-lists-as-table (lol &optional justify min-cell-width columns)
  "Convert a list of lists to a pretty `table.el' table, editable with ⌘-e."
  (let ((buf (get-buffer-create "*org-tb*")))
    (with-current-buffer buf
      (erase-buffer)
      (mapcar (lambda (x)
                (mapcar (lambda (y) (insert (format "%s&" y))) x)(insert "\n")) lol)
      (table-capture 1 (point-max) "&" "\n" justify min-cell-width columns)
      (buffer-substring-no-properties (point-min) (point-max)))))
;;
;; (my/pp-list-of-lists-as-table '((Abc Def "xyz") (1 1 X1) (2 4 X2) (3 9 X3) (4 16 X4)))
;; Implementation:1 ends here

;; [[file:init.org::*M-x org-lint][M-x org-lint:1]]
;; Checkers I'm interested in, for my review.
(require 'org-lint)
(setq org-lint--checkers
       (--filter
        (member (org-lint-checker-name it)
                '(planning-inactive
                  timestamp-syntax
                  spurious-colons
                  incomplete-drawer
                  misplaced-planning-info
                  mismatched-planning-repeaters
                  invalid-id-property
                  invalid-effort-property
                  ;; obsolete-properties-drawer
                  special-property-in-properties-drawer
                  ;; link-to-local-file
                  duplicate-custom-id
                  misplaced-heading))
 org-lint--checkers))
;; M-x org-lint:1 ends here

;; [[file:init.org::*Other benefits of clocking are …][Other benefits of clocking are …:2]]
  (setq org-clock-sound "~/.emacs.d/school-bell.wav")
;; Other benefits of clocking are …:2 ends here

;; [[file:init.org::*Basic Implementation][Basic Implementation:1]]
;; In case my hands slip and I press “C-x C-c”, which saves all buffers and quits Emacs; confirm that's my intention.
(setq confirm-kill-emacs 'yes-or-no-p)


;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)


;; Change task state to STARTED when clocking in, if in an agenda file.
;; This means I can clock-into tasks in my init.org or into captures “C-c c”
;; without having them marked STARTED.
(setq org-clock-in-switch-to-state
      (defun my/clock-in-to-STARTED-if-in-an-agenda-file (&rest args)
        (when (member (buffer-file-name) (org-agenda-files))
          "STARTED")))


;; At first, I used to record a note on what was accomplished when clocking out of an item.
;; Now, I use “C-c C-z” to make notes about insights or blockers or ideas or what to do next time, as I work on a task.
(setq org-log-note-clock-out nil)


;; Show lot of clocking history so it's easy to resume recently clocked items, using the “C-u C-c SPC” clock history list.
;;
;; For example, I'm working on task 𝒜, then I need to context-switch to task ℬ (e.g., “meeting”)
;; so I clock-out of 𝒜 and clock-into ℬ; then when ℬ is done, I press “C-u C-c SPC” and see a list
;; of tasks I've recently clocked-into, with 𝒜 being at the top, so I select it and now I'm back into 𝓐.
;;
;; The clock history is a nice way to quickly see what you've been working on lately.
(setq org-clock-history-length 23)


;; Resume clocking task when emacs is restarted: Save the running clock and all CLOCK HISTORY when exiting Emacs, load it on startup
(org-clock-persistence-insinuate)
(setq org-clock-persist t)
(setq org-clock-persist-query-resume nil) ;; Do not prompt to resume an active clock

;; If I clock into a task, then move to something else before a minute's elapsed, don't keep track of a 0:00 duration.
;; This is helpful when I capture a note quickly with “C-c c”.
(setq org-clock-out-remove-zero-time-clocks t)


;; When a task enters the DONE state, I don't want to automatically clock-out
;; because I may not have started another task and don't want to “lose a few
;; minutes” finding a sibling task to start. Such minutes add up; especially if
;; I'm taking the time to write good notes.
(setq org-clock-out-when-done nil)
;; Alternatively, make this “t” and add the following hook; then even when I
;; clock-out of a task, I don't lose unlogged minutes before logging into
;; another task.
(when nil
  (add-hook 'org-clock-out-hook
            (defun my/clock-into-PLANNING-task ()
              "Look for the task with property “:ID: planning” and clock into it.

              The “** Planning” task is intended for miscellaneous clock time:
              Reading email, clearing my inbox, reorganising my notes, etc.
             "
              (interactive)
              (org-with-point-at (org-id-find "planning" 'marker)
                (org-clock-in '(16))))))


;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)
;; Basic Implementation:1 ends here

;; [[file:init.org::*How should the clock in the modeline look?][How should the clock in the modeline look?:1]]
(set-face-attribute
 'org-mode-line-clock
  nil
  :background "grey75"
  :foreground "red"
  :box '(:line-width -1 :style released-button))

;; Conversely, the getter:
(face-attribute  'org-mode-line-clock :foreground) ;; ⇒ "red"
;; How should the clock in the modeline look?:1 ends here

;; [[file:init.org::*Say out loud “I begin this task in the name of God, the most gracious, the most merciful” and “ ﷽ ”, so that God may bless the task I'm undertaking.][Say out loud “I begin this task in the name of God, the most gracious, the most merciful” and “ ﷽ ”, so that God may bless the task I'm undertaking.:1]]
(add-hook 'org-clock-in-hook
          (defun my/say-bismillah-on-clock-in ()
            (unless org-capture-mode
              (alert " \nI begin this task in the name of God, the most gracious, the most merciful.\n\nGod please bless the task I'm undertaking."
                     :title "Bismi Allah ar-rahaman ar-raheem")
              (async-shell-command "say \"I begin this task in the name of God, the most gracious, the most merciful\""))))

(add-hook 'org-clock-out-hook
          (defun my/say-alhamudlilah-on-clock-out ()
            (unless org-capture-mode
              (async-shell-command "say \"Alhamudllah; praise be to God who has blessed me\""))))
;; Say out loud “I begin this task in the name of God, the most gracious, the most merciful” and “ ﷽ ”, so that God may bless the task I'm undertaking.:1 ends here

;; [[file:init.org::*org-clock-clocktable-default-properties and “my/what-did-i-work-on-this-week”][org-clock-clocktable-default-properties and “my/what-did-i-work-on-this-week”:1]]
;; This only works well if my tasks have timestamps; ie are scheduled ^_^
(setq org-clock-clocktable-default-properties
      '(:scope ("./my-life.org")       ;; Consider the current file
               :hidefiles t      ;; Hide the file column when multiple files are used to produced the table.
               :maxlevel 5       ;; Consider sub-sub-sections
               :block lastweek   ;; Only show me what I did last week
               ;; Other values: 2024-04, lastmonth, yesterday, thisyear, 2024
               ;; :tstart "<-2w>" :tend "<now>"   ;; Show me the past two weeks
               ;; :tstart "<2006-08-10 Thu 10:00>" :tend "<2006-08-10 Thu 12:00>" ;; Show me a particular range
               :step day         ;; Split the report into daily chunks
               :formula %        ;; Show me the percentage of time a task took relative to my day
               :link t           ;; Link the item headlines in the table to their origins
               :narrow 55!       ;; Limit the width of the headline column in the Org table
               :tcolumns 1       ;; Show only 1 column ---not multiple, for the sections and subsections and etc.
               :timestamp t      ;; Show the timestamp, if any
               :tags t        ;; Do not show task tags in a column; I use mostly hierarchies for my tasks right now.
               ;; :match "billable|notes-travel" ;; ⇒ Includes tags “billable” and “notes”, excludes tag “travel”
               ;; More examples at: https://orgmode.org/manual/Matching-tags-and-properties.html#Matching-tags-and-properties
               ;; For example, we can match tags & priority & todo states & property drawer values; “/DONE” matches all tasks with state being ‘DONE’.
               :match "-Personal" ;; Exclude tasks tagged “OOO”, which means “Out of Office” ---e.g., taking a break, or praying, or out for an appointment.
               ;; https://stackoverflow.com/a/53684091 for an accessible example use of :formatter
               ;; :formatter org-clocktable-write-default ;; This is the default way to print a table; it looks very long and annoying to change.
               ;; :sort (3 . ?N)              ;; Sort descendingly on time. Not ideal, since I'm very hierarchical.
               :properties ("Effort")
               ))
;; More properties at:  https://orgmode.org/manual/The-clock-table.html
;; [Low Priority] Figure out whether I've underworked or overworked? ⇒ https://www.erichgrunewald.com/posts/how-i-track-my-hour-balance-with-a-custom-org-mode-clock-table/


;; I prefer “27h” over “1d 3h”.
(setq org-duration-format 'h:mm)


;; In Org-agenda, press “v r” to view the clock report
;; (This method is invoked when you hit “v r” in Org-agenda.)
(defalias 'org-agenda-clockreport-mode 'my/what-did-i-work-on-this-week)



(defun my/what-did-i-work-on-this-week ()
  "Show in a dedicated buffer a clock report of the past 7 days."
  (interactive)
  (my/what-did-i-work-on
   "*What Did I Work On This Week?*"
   org-clock-clocktable-default-properties))



(defun my/what-did-i-work-on-today ()
  (interactive)
  (my/what-did-i-work-on
   "*What Did I Work on Today?*"
   (-snoc org-clock-clocktable-default-properties :block 'today)))



(defun my/what-did-i-work-on (buffer-title clocktable-properties)
  (switch-to-buffer buffer-title)
  (org-mode)
  ;; (org-modern-mode) Tables don't look good with the unicode/timestamp svg
  (-let [org-clock-clocktable-default-properties clocktable-properties]
    (org-clock-report))
  ;; For some reason, some of org-modern carries from the current buffer to the new buffer.
  (org-modern-mode +1)
  (org-modern-mode -1)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\\\_" nil t)
      (replace-match "⇒")
      (org-table-align))))
;; org-clock-clocktable-default-properties and “my/what-did-i-work-on-this-week”:1 ends here

;; [[file:init.org::*Nicer clocktable layout for nested entries][Nicer clocktable layout for nested entries:1]]
(advice-add 'org-clocktable-indent-string :override
 (defun my/org-clocktable-indent-string (level)
  (if (= level 1)
      ""
    (let ((str "└"))
      (while (> level 2)
        (setq level (1- level)
              str (concat str "──")))
      (concat str "─> ")))))
;; Nicer clocktable layout for nested entries:1 ends here

;; [[file:init.org::*~C-c SPC~: Quick key to go to the currently clocked-in entry, or to the most recently clocked one][~C-c SPC~: Quick key to go to the currently clocked-in entry, or to the most recently clocked one:1]]
(bind-key*
 "C-c SPC"
 (defun my/jump-to-clocked-task (&optional prefix)
   "Jump to the currently clocked-in entry, or to the most recently clocked one.

With prefix arg, offer recently clocked tasks for selection."
   (interactive "P")
   (org-clock-goto prefix)
   (org-narrow-to-subtree)))
;; ~C-c SPC~: Quick key to go to the currently clocked-in entry, or to the most recently clocked one:1 ends here

;; [[file:init.org::*🤔 \[Planning/Review\] When I hover over a task, tell me how long ago it was created! 😼 Also, show me my “WHY” so I remain motivated.][🤔 [Planning/Review] When I hover over a task, tell me how long ago it was created! 😼 Also, show me my “WHY” so I remain motivated.:1]]
(advice-add 'org-eldoc-get-breadcrumb :around
            (defun my-org-eldoc-get-breadcrumb-with-created (orig-fun &rest args)
              "Enhance `org-eldoc-get-breadcrumb` to also include how many days ago the entry was created, and how much “real work time” I've spent on it!"
              (let ((breadcrumb (or (apply orig-fun args) (org-get-heading t t t t)))) ;; Call the original function, which is non-nil only when cursor is on heading
                (if-let ((created (org-entry-get (point) "CREATED"))) ;; Get the :CREATED: property
                    (let* ((created-time (date-to-time created)) ;; Convert :CREATED: to a time value
                           (days-ago (floor (time-to-number-of-days (time-subtract (current-time) created-time))))  ;; Calculate days
                           (WHY  (org-entry-get (point) "WHY")) ;; If I have a specific reason, WHY, that I'd like to be echoed, then echo it!
                           (♯children (length (org-map-entries t (format "LEVEL=%s" (1+ (org-current-level))) 'tree))))
                      (concat breadcrumb " | Created " (number-to-string days-ago) " days ago" ;; Append days ago
                              (if (> ♯children 0) (format " | %s children" ♯children) "")
                              ;; face options: https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html
                              (if WHY
                                  (org-add-props (format "\n \n \n﴾%s﴿" WHY) nil 'face '(:slant italic :foreground "SpringGreen4" :weight bold))
                                (org-add-props "\n \n \n﴾No clue why I'm doing this task! 💢 Just wasting my life? ⏳﴿" nil 'face '(:slant italic :foreground "firebrick1" :weight bold)))
                              "\n\n"
                              (my/get-real-work-time-for-task-at-point)))
                  breadcrumb)))) ;; Return breadcrumb unchanged if :CREATED: is not found
;; 🤔 [Planning/Review] When I hover over a task, tell me how long ago it was created! 😼 Also, show me my “WHY” so I remain motivated.:1 ends here

;; [[file:init.org::*🤔 \[Planning/Review\] When I hover over a task, tell me how long ago it was created! 😼 Also, show me my “WHY” so I remain motivated.][🤔 [Planning/Review] When I hover over a task, tell me how long ago it was created! 😼 Also, show me my “WHY” so I remain motivated.:2]]
;; Suppose I run  (org-duration-from-minutes  (org-clock-sum))  on a task and it reports
;; 73:20 hours. It doesn't sound like a whole lot, but how much “work time” is that really?
;;
;; ⟨Axiom-1⟩ Suppose I work 6 straight hours per work day, with +2hrs for lunch and minor meetings.
;;           As such, a “work day” is 360=(* 6 60) minutes.
;;
;; ⟨Axiom-2⟩ A work-month is 30 days less the weekends, so it's 22 days.
;;          As such, a “work month” is 7920=(* 22 6 60) minutes.
;;
;; Hence, 73:20  hours is 12=(/ 73 6) work days and 1=(% 73 6) hours and 20mins.
;;
;; Let's get Org to show me this “real work time” when I pass by a task.
(defun my/get-real-work-time-for-task-at-point ()
  (let* (  ;; (total-minutes-worked (+ (* 73 60) 20)) ;; For testing purposes
               (total-minutes-worked  (org-clock-sum))
               ;; (total-minutes-worked 4500) ;; For testing purposes, ie 75 hours
               ;; “𝒽 hours and 𝓂 minutes”
               (total-hours-worked (/ total-minutes-worked 60)) ;; 𝒽
               (mins-worked-0 (% total-minutes-worked 60))     ;; 𝓂

               ;; “𝒹 days, 𝒽 hours, and 𝓂 minutes”
               (work-day-in-minutes (* 6 60))
               (total-days-worked (/ total-minutes-worked work-day-in-minutes))  ;; 𝒹
           ;; (hours-worked (/ (% total-minutes-worked work-day-in-minutes) 60)) ;; 𝒽
           ;; (mins-worked (% (% total-minutes-worked work-day-in-minutes) 60)) ;; 𝓂

               ;; “𝓜 months”
               (work-month-in-minutes (* 22 6 60)) ;; 22 days of 60 minutes each.
               (months-worked (/ total-minutes-worked work-month-in-minutes))
               (days-worked-in-minutes (% total-minutes-worked work-month-in-minutes))
               (days-worked (/ days-worked-in-minutes work-day-in-minutes))
               (hours-worked (/ (% days-worked-in-minutes work-day-in-minutes) 60)) ;; 𝒽
               (mins-worked (% (% days-worked-in-minutes work-day-in-minutes) 60))) ;; 𝓂

    (format "Worked %s%s%s%s mins \n (i.e., %s:%s hours)"
                (if (zerop months-worked) "" (format "%s months, " months-worked))
            (if (zerop days-worked) "" (format "%s days, " days-worked))
            (if (zerop hours-worked) "" (format "%s hours, " hours-worked))
                mins-worked
                total-hours-worked
                mins-worked-0)))
;; 🤔 [Planning/Review] When I hover over a task, tell me how long ago it was created! 😼 Also, show me my “WHY” so I remain motivated.:2 ends here

;; [[file:init.org::*Eldoc for org-mode][Eldoc for org-mode:1]]
(url-copy-file "https://git.sr.ht/~bzg/org-contrib/blob/master/lisp/org-eldoc.el" "~/.emacs.d/elpa/org-eldoc.el" :ok-if-already-exists)
(load-file "~/.emacs.d/elpa/org-eldoc.el")
(add-hook 'org-mode-hook 'eldoc-mode)
(add-hook 'org-mode-hook 'eldoc-box-hover-mode)
;; Eldoc for org-mode:1 ends here

;; [[file:init.org::*Done!][Done!:1]]
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(message-box "Enjoy life (｡◕‿◕｡))")
;; Done!:1 ends here

;; [[file:init.org::*Bookmarks: Quick naviagation to commonly visited locations][Bookmarks: Quick naviagation to commonly visited locations:1]]
;; Show me file locations alongside named bookmarks, when I press “C-x r b”. (Press C-z to take further actions on a bookmark, such as editing, deleting, etc).
;; Note: Set a bookmark with “C-x r m”.
(setq helm-bookmark-show-location t)
(setq bookmark-save-flag 1)  ;; save bookmarks to “ bookmark-default-file ” after each entry

;; Have “C-x r b” widen when I jump, otherwise it does not actually jump.
;; Finally, advise Emacs `C-x r b` to clone-indirect-buffer whenever a universal argument is provided.
;; That is, “C-u C-x r b” jumps to a bookmark in a *new* buffer.
(bind-key*
 "C-x r b"
 (defun my/bookmark-jump-widen (bookmark)
   "Jump to a bookmark and widen; when a universal arg is provided, jump in another window.

Adapted from the source of `bookmark-jump'. I intially advised it, but
the advice did not have access to `current-prefix-arg', so I made my own
method."
   (interactive
    (list (bookmark-completing-read "Jump to bookmark"
                                                    bookmark-current-bookmark)))
   (unless bookmark
     (error "No bookmark specified"))
   (bookmark-maybe-historicize-string bookmark)
   ;; First, go the relevant file
   (bookmark--jump-via bookmark (if current-prefix-arg #'switch-to-buffer-other-window #'pop-to-buffer-same-window ))
   ;; Then widen
   (widen)
   ;; Then actually go to the desired bookmark
   (bookmark--jump-via bookmark #'pop-to-buffer-same-window)))
;; Bookmarks: Quick naviagation to commonly visited locations:1 ends here

;; [[file:init.org::*Bookmarks: Quick naviagation to commonly visited locations][Bookmarks: Quick naviagation to commonly visited locations:2]]
;; Save/mark a location with “C-u M-m”, jump back to it with “M-m”.
(bind-key* "M-m"
           (lambda ()
             (interactive)
             (if (not current-prefix-arg)
                 (helm-mark-ring)
               (push-mark)
               (message "[To return to this location, press M-m] ∷ %s"
                        (s-trim (substring-no-properties (thing-at-point 'line)))))))
;; Bookmarks: Quick naviagation to commonly visited locations:2 ends here

;; [[file:init.org::*Working with massive files: my-life∙org][Working with massive files: my-life∙org:1]]
;; I ran M-x profiler-start then did a save (C-x C-x) then did M-x profiler-report and noticed that
;; whitespace-cleanup was taking a long time on a file with 96k lines. At first I thought this was
;; an Emacs limitiation, but I opened the file with “emacs -Q” and saw no issues.
;;
;; (setq after-save-hook nil  before-save-hook nil)
;;
(set-default 'before-save-hook (--remove (equal it 'whitespace-cleanup) before-save-hook))
;; Working with massive files: my-life∙org:1 ends here

;; [[file:init.org::#Show-off-screen-heading-at-the-top-of-the-window][Show off-screen heading at the top of the window:1]]
 (use-package org-sticky-header
  :hook (org-mode . org-sticky-header-mode)
  :config
  (setq-default
   org-sticky-header-full-path 'full
   ;; Child and parent headings are seperated by a /.
   org-sticky-header-outline-path-separator " ▷ "))
;; Show off-screen heading at the top of the window:1 ends here

;; [[file:init.org::#Never-lose-the-cursor][Never lose the cursor:1]]
;; Make it very easy to see the line with the cursor.
(global-hl-line-mode t)
;; Never lose the cursor:1 ends here

;; [[file:init.org::#Never-lose-the-cursor][Never lose the cursor:2]]
(use-package beacon
  :config (setq beacon-color "#666600")
  :hook   ((org-mode text-mode) . beacon-mode))
;; Never lose the cursor:2 ends here

;; [[file:init.org::*Automatically Exposing Knowledge Elsewhere ---Hyperbole: “DWIM at point”][Automatically Exposing Knowledge Elsewhere ---Hyperbole: “DWIM at point”:1]]
(use-package hyperbole :demand t)
(hyperbole-mode +1)
(setq hsys-org-enable-smart-keys t)
;; Automatically Exposing Knowledge Elsewhere ---Hyperbole: “DWIM at point”:1 ends here

;; [[file:init.org::*Automatically Exposing Knowledge Elsewhere ---Hyperbole: “DWIM at point”][Automatically Exposing Knowledge Elsewhere ---Hyperbole: “DWIM at point”:3]]
(advice-add 'hkey-either :around
(defun my/M-RET-in-enumeration-means-new-item (orig-fn &rest args)
  "In an Org enumeration, M-[S]-RET anywhere in an item should create a new item.

   However, Hyperbole belives being at the end of the line means M-RET should
   scroll down a screenful similar to `C-v' and `M-v'. Let's avoid this."
  (if (and (derived-mode-p 'org-mode) (save-excursion (beginning-of-line) (looking-at "\\([0-9]+\\|[a-zA-Z]\\)[.)].*")))
        (org-insert-item)
    (apply orig-fn args))))
;; Automatically Exposing Knowledge Elsewhere ---Hyperbole: “DWIM at point”:3 ends here

;; [[file:init.org::*~M-RET~ on an Org line sets tags, and on ~∶PROPERTIES∶~ adds a new property][~M-RET~ on an Org line sets tags, and on ~∶PROPERTIES∶~ adds a new property:1]]
(defib my/property-button ()
  "Clicking at the start of the :PROPERTIES: line prompts for adding a new Org property."
  (let ((case-fold-search t))
    (when (looking-at org-property-drawer-re)
      (ibut:label-set (match-string-no-properties 0)
                      (match-beginning 0)
                      (match-end 0))
      (hact (lambda (_current-props) (org-set-property nil nil)) (match-string-no-properties 0)))))


(defib my/tag-button ()
  "Clicking at the start of an Org heading means add a new Org tag."
  (let ((case-fold-search t))
    (when (looking-at org-tag-line-re)
      (ibut:label-set (match-string-no-properties 1)
                      (match-beginning 1)
                      (match-end 1))
      (hact (lambda (_current-tags) (org-set-tags-command)) (match-string-no-properties 1)))))
;; ~M-RET~ on an Org line sets tags, and on ~∶PROPERTIES∶~ adds a new property:1 ends here

;; [[file:init.org::*~MyModule::72~ means “find the file named ~MyModule~, somewhere, and jump to line 72”][~MyModule::72~ means “find the file named ~MyModule~, somewhere, and jump to line 72”:1]]
(defun my/open-::-file-path (path)
  "PATH is something like FooModule::72 or FooModule::interface_bar"
  (-let [(name regex) (s-split "::" path)]
    ;; brew install fd
    ;; NOTE: fd is fast!
    (-let [file (car (s-split "\n" (shell-command-to-string (format "fd \"^%s\\..*$\" %s" name my\work-dir))))]
      (if (s-blank? file)
          (message "😲 There's no file named “%s”; perhaps you're talking about a class/record/interface with that name?" name)
      (find-file file)
      (-let [line (string-to-number regex)]
        (if (= 0 line)
            (progn (beginning-of-buffer) ;; In case file already open
                   (re-search-forward (s-replace "_" " " regex) nil t))
          (goto-line line)))))))


(require 'hbut)


(defib my/::-file-paths ()
  "Find the file whose name is at point and jump to the given regex or line number."
  (let ((case-fold-search t)
        (path-id nil)
        (my-regex "\\b\\(\\w+::[^ \n]+\\)"))
    (if (or (looking-at my-regex)
            (save-excursion
              (my/move-to-::-phrase-start)
              (looking-at my-regex)))
        (progn (setq path-id (match-string-no-properties 1))
               (ibut:label-set path-id
                               (match-beginning 1)
                               (match-end 1))
               (hact 'my/open-::-file-path path-id)))))


(defun my/move-to-::-phrase-start ()
  "Move cursor to the start of a :: phrase, like Foo::bar, if point is inside one."
  (interactive)
  (let ((case-fold-search t)
        (pattern "\\b\\(\\w+::[^ \n]+\\)")
        (max-lookback 20))
      (catch 'found
        ;; First check if we're already inside a match
        (when (looking-at pattern)
          (goto-char (match-beginning 0))
          (throw 'found t))

        ;; If not at start of match, look backward
        (let ((pos (point)))
          (while (and (> pos (point-min))
                     (<= (- pos (point)) max-lookback))
            (goto-char pos)
            (when (looking-at " ") (throw 'found nil)) ;; It'd be nice if I depended only on PATTERN.
            (when (looking-at pattern)
              (goto-char (match-beginning 0))
              (throw 'found t))
            (setq pos (1- pos)))))))


;; Some highlighting so I'm prompted to use “M-RET”
(font-lock-add-keywords
 'org-mode
 '(("\\b[^ ]*::[^ \n]*" 0 'highlight prepend))
 t)
;; ~MyModule::72~ means “find the file named ~MyModule~, somewhere, and jump to line 72”:1 ends here

;; [[file:init.org::*Fontify Org Radio Targets /everywhere/ and have ~M-RET~ Jump to Them /from anywhere/][Fontify Org Radio Targets /everywhere/ and have ~M-RET~ Jump to Them /from anywhere/:1]]
(defun get-radio-targets ()
  "Extract all radio targets from the current Org buffer"
  (interactive)
  (let ((targets nil)
        (case-fold-search t))
    (cl-loop for file in (cons "~/.emacs.d/init.org" org-agenda-files)
             do (save-excursion
                  (find-file file)
                  (save-restriction
                    (widen)
                    (goto-char (point-min))
                    (while (re-search-forward "<<<\\(.*?\\)>>>" nil t)
                      (push (list (downcase (substring-no-properties (match-string 1))) file (line-number-at-pos)) targets)))))
    targets))

(setq my/radio-targets (get-radio-targets))
(setq my/radio-regex (eval `(rx (or ,@(mapcar #'cl-first my/radio-targets)))))

(font-lock-add-keywords
 'org-mode
 (--map (list (format "\\b%s\\b" (cl-first it)) 0 ''highlight 'prepend) my/radio-targets)
 t)

;; In programming modes, just show an underline.
(add-hook
 'prog-mode-hook
 (lambda ()
   (font-lock-add-keywords
    nil
    (--map (list (format "\\b%s\\b" (cl-first it)) 0 ''(:underline t) 'prepend) my/radio-targets)
    t)))


(defun my/jump-to-radio (radio)
  "RADIO is a downcased name."
  (-let [(name file line) (assoc radio my/radio-targets)]
    (find-file file)
    (goto-line line)
    ;; Make sure point and context are visible.
    (org-fold-show-context)))

(defib my/radio-target ()
  "Jump to the definition of this word, as an Org radio target"
  (let ((case-fold-search t)
        (radio nil))
    (if (or (looking-at my/radio-regex)
            (save-excursion
              (re-search-backward "\\b")
              (looking-at my/radio-regex)))
        (progn (setq radio (downcase (match-string-no-properties 0)))
               (ibut:label-set radio
                               (match-beginning 0)
                               (match-end 0))
               (hact 'my/jump-to-radio radio)))))
;; Fontify Org Radio Targets /everywhere/ and have ~M-RET~ Jump to Them /from anywhere/:1 ends here

;; [[file:init.org::*Testing that things are as they should be][Testing that things are as they should be:1]]
(progn
  ;; TODO: Move the next bunch of lines up somewhere
  (add-hook 'prog-mode-hook 'company-mode) 
  (add-hook 'org-mode-hook 'company-mode)
  (add-hook 'org-mode-hook (lambda ()
                             (org-eldoc-load)
                             (eldoc-mode)
                             (eldoc-box-hover-mode)
                             (require 'org-capture)))
  (setq browse-url-browser-function 'browse-url-default-browser)


  (switch-to-buffer "*test*")
  (require 'cl) ;; get `assert` macro
  (require 'ert) ;; get `should` macro


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                                                                                ;;
  ;; When programming, indentation is automatic & I have auto-complete              ;;
  ;;                                                                                ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (emacs-lisp-mode)
  (assert aggressive-indent-mode)
  (assert company-mode)
  (assert eldoc-mode)
  (assert color-identifiers-mode) ;; Semantic colouring


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                                                                                ;;
  ;; When I'm note-taking, I have a nice environment                                ;;
  ;;                                                                                ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (org-mode)
  ;; TODO (assert eldoc-mode)
  (assert company-mode t "Whoops, Org mode does not have Company enabled!")
  (assert eldoc-mode)
  (assert eldoc-box-hover-mode)

  ;; C-RET makes a new org heading with a pink :CREATED: property
  (execute-kbd-macro (kbd "C-<return>"))
  (insert "My neato notes")
  ;;
  ;; TODO assert fancy bullets are being used
  ;; Checks that CREATED equals today's date, and not something arbitrary.
  (should (s-matches? (format-time-string "\\[%Y-%m-%d %a %H:%M\\]")
                      (org-entry-get (point) "CREATED")))
  ;; TODO Assert that the :CREATED: property is a nice pink
  ;; (assert (progn (beginning-of-buffer) (re-search-forward ":PROPERTIES:") (beginning-of-line)
  ;;                (should (equal (get-text-property (point) 'face) 'org-drawer))
  ;;                (should (should (equal (face-attribute 'org-drawer :foreground)  "LightPink1")))
  ;;                (should (equal (face-attribute 'org-drawer :height) 0.9)))
  ;;         (should (equal (face-attribute 'org-drawer :slant) 'italic)))
  ;; TODO assert PROPERTIES, LOGBOOK, word and :END: words are invisible?

  ;; Let's add a schedule and deadline
  (execute-kbd-macro (kbd "C-c C-s <return>"))
  (execute-kbd-macro (kbd "C-c C-d <return>"))
  ;; Assert the second line, the SCHEDULED: and DEADLINE: have nice icons
  (should (org-entry-get (point) "SCHEDULED"))
  (should (org-entry-get (point) "DEADLINE"))
  ;; TODO (progn (beginning-of-buffer) (re-search-forward "SCHEDULED:") (assert (string-equal (get-text-property (point) 'display) "📆 ")))
  ;; TODO (progn (beginning-of-buffer) (re-search-forward "DEADLINE:")  (assert (string-equal (get-text-property (point) 'display) "🎯 ")))

  ;; Speed keys are enabled, so we can start by pressing “t”
  (assert org-use-speed-commands)
  (beginning-of-buffer)
  (execute-kbd-macro (kbd "t i")) ;; Enter “INVESTIGATED” state
  (should (string-equal (org-entry-get (point) "TODO") "INVESTIGATED"))

  ;; check all of my workflow states are present
  (should (equal org-todo-keywords '((sequence "TODO(t)" "INVESTIGATED(i)" "STARTED(s)" "|" "PAUSED(p@/!)" "WAITING(w)" "APPROVED(a)" "DONE(d)" "CANCELLED(c@)"))))
  (should (equal org-log-done 'time))

  ;; TODO When I “Clock-In” to a task, I'm in the STARTED state.
  (execute-kbd-macro (kbd "C-c C-x C-i"))
  ;; TODO (should (string-equal (org-entry-get (point) "TODO") "STARTED"))

  ;; TODO switch to scratch buffer, then press C-c SPC, then confirm I'm at the currently clocked-in task
  (scratch-buffer)
  (should (equal (buffer-name) "*scratch*"))
  (execute-kbd-macro (kbd "C-c SPC"))
  (should (equal (buffer-name) "*test*"))
  (should (equal (org-get-heading) "INVESTIGATED My neato notes"))

  ;; TODO Make a note, via C-c C-z, and confirm the note icon looks pretty.
  ;; TODO Finally, clock out and check that the clock icon looks pretty.
  ;; (progn (beginning-of-buffer) (re-search-forward "CLOCK:") (should (equal (get-text-property (point) 'display) "⏰ ")))
  (execute-kbd-macro (kbd "C-c C-x C-o"))

  ;; Enter “DONE” state, should now have a CLOSED property
  (execute-kbd-macro (kbd "t d"))
  (org-todo "DONE")
  (should (org-entry-get (point) "CLOSED"))
  ;; TODO (progn (beginning-of-buffer) (re-search-forward "CLOSED:") (should (equal (get-text-property (point) 'display) "☺️ ")))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                                                                                                          ;;
  ;; Smart Paste: Gerrit Links, Arbitrary URLs, Arbitrary Image Attachements, Arbitrary Rich Text, Plain Text ;;
  ;;                                                                                                          ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (execute-kbd-macro (kbd "s-a DEL"))
  (kill-new "Plain text is pasted as is")
  (execute-kbd-macro (kbd "s-v"))
  (should (equal (thing-at-point 'line :no-properties) "Plain text is pasted as is"))

  (execute-kbd-macro (kbd "s-a DEL"))
  (kill-new "12345: [foo, bar] OCaml Syntax: Fix foo bar baz | https://gerrit.local.company.com/c/abc/+/12345")
  (execute-kbd-macro (kbd "s-v"))
  (should (equal (thing-at-point 'line :no-properties)
                 "[[https://gerrit.local.company.com/c/abc/+/12345][OCaml Syntax: Fix foo bar baz]]"))

  (execute-kbd-macro (kbd "s-a DEL"))
  (kill-new "https://github.com/alphapapa/org-ql/tree/master")
  (execute-kbd-macro (kbd "s-v"))
  (should (equal (thing-at-point 'line :no-properties)
                 (format "[[%s][%s]]"
                         "https://github.com/alphapapa/org-ql/tree/master"
                         "GitHub - alphapapa/org-ql: A searching tool for Org-mode, including custom query languages, commands, saved searches and agenda-like views, etc.")))

  ;; Pressing “C-u ⌘-v” does a plain old paste
  (execute-kbd-macro (kbd "s-a DEL"))
  (kill-new "https://github.com/alphapapa/org-ql/tree/master")
  (execute-kbd-macro (kbd "C-u s-v"))
  (should (equal (thing-at-point 'line :no-properties) "https://github.com/alphapapa/org-ql/tree/master"))

  (when nil ;; If the above smart paste assertions pass, then likely so do the following (time-consuming!) ones.

    ;; Copy an image from somewhere
    (ignore-errors ;; This works when run in init.org, but fails when run programmatically? Perhaps Org hook is not setup correctly?
      (execute-kbd-macro (kbd "s-a DEL"))
      (insert "* Example Image\n")
      (shell-command-to-string "curl -sL https://alhassy.com/images/musa_pink.jpg -o /tmp/musa.jpg \
       && osascript -e 'set the clipboard to (read (POSIX file \"/tmp/musa.jpg\") as «class JPEG»)'")
      (execute-kbd-macro (kbd "s-v"))
      (should (s-matches? "\[\[attachment:.*.png\]\]" (thing-at-point 'line :no-properties))))

    ;; Check that copy-pasting rich html contents actually produces rich Org markup
    (ignore-errors ;; This works, but consider using xwidget within emacs instead
      (execute-kbd-macro (kbd "s-a DEL"))
      (message "Retrieving webpage test data...")
      (shell-command-to-string
       "osascript -e 'tell application \"Google Chrome\" to open location \"https://alhassy.com/about\"' \
        && sleep 0.5 \
        && osascript -e 'tell application \"System Events\" to keystroke \"a\" using command down' \
        && osascript -e 'tell application \"System Events\" to keystroke \"c\" using command down' \
        && osascript -e 'quit app \"Google Chrome\"' \
        && sleep 0.5 \
        && osascript -e 'tell application \"Emacs\" to activate' \
        ")
      (execute-kbd-macro (kbd "s-v"))
      ;; Confirm we have a few Org headings corresponding to the HTML headings we copied, also note /italics/ and other markup are rendered nicely.
      (should (equal
               (org-map-entries (lambda () (substring-no-properties (org-get-heading t t t t))))
               '("Musa Al-hassy\\\\" "[[javascript:window.scrollTo(0,0)][Ξ]]" "Quick Facts" "Character" "Goals" "/What do?/")))))

  (kill-buffer "*scratch*") ;; clean up
  (kill-buffer "*test*") ;; clean up

  ;; Finally, open my agenda.
  (execute-kbd-macro (kbd "C-c a"))

  )
;; Testing that things are as they should be:1 ends here
