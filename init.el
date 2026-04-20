;; [[file:init.org::#Personal-instructions-for-a-new-machine][Personal instructions for a new machine:4]]
    (setq org-image-actual-width nil)
;; Personal instructions for a new machine:4 ends here

;; [[file:init.org::#Personal-instructions-for-a-new-machine][Personal instructions for a new machine:5]]
    ;; Day-to-day, open URLs in the system browser (Arc, Chrome, etc.).
    ;; When blogging, we can temporarily switch to xwidget-webkit-browse-url.
    (setq browse-url-browser-function 'browse-url-default-browser)

    ;; (use-package xwwp) ;; Enhance the Emacs xwidget-webkit browser
;; Personal instructions for a new machine:5 ends here

;; Disable custom-file - all settings managed in init.el/init.org
(setq custom-file (make-temp-file "emacs-custom-"))

;; Kill noisy buffers that accumulate during init.
(add-hook 'emacs-startup-hook
  (lambda ()
    (dolist (buf '("*Shell Command Output*" "*Quail Completions*" "*Compile-Log*"))
      (when (get-buffer buf)
        (kill-buffer buf)))))

(load "~/Dropbox/private.el" 'no-error 'no-message)

(setq user-full-name    "Musa Al-hassy"
      user-mail-address "alhassy@gmail.com")

(defmacro 😴 (&rest sexp)
  "Defer any sexp.

If you have a call `(f x y)' then `(😴 f x y)' behaves the same but is run
when Emacs has been idle for 10 seconds.

E.g., (setq hi 12) defines a variable `hi', so `M-: hi' shows a value.
Whereas (😴 setq hello 12) does not immediately define a variable: `M-: hello' yields an error
when run immediately, but yields a value when Emacs is idle for 2 seconds.

Save the name of this macro by highlighting it and pressing `C-x r s z', then use it with `C-x r i z'.

================================================================================

Using `macrostep-expand' we can verify the following approximations:

⇒ (use-package foo) ≈ (require 'foo)
⇒ (😴 use-package foo) ≈ (use-pacakge foo :defer 10)
⇒ (use-pacakge foo :defer t) ≈ nil ;; It doesn't load the package!
  ⭆ Of-course, since I have “:ensure t” implicitly, such declaration ensures
    package foo is installed.

================================================================================

This should be used as a last resort. Instead prefer `use-pacakge' lazy loading instead.

❌ Avoid :preface, :config, and :init since they unconditionally load the package immediately.
   ⇒ Favour :custom over :init; i.e., replace (use-package foo :init (setq x y)) with (use-pacakge foo :custom (x y)).
   ⇒ If you must use :init, add a “:defer t” clause as well, to load it when it's needed.
   ⇒ If your :config only sets keybindings, then prefer :bind or :bind-keymap.
✅ Prefer auto-loading keywords :bind, :hook, and :mode since they defer loading a package until it's needed.
  ⇒ These all imply “:defer t”.
✅ Prefer loading modes only after Emacs’ initialisation has finished.
  ⇒ Replace (use-package foo :config (foo-mode + 1)) with (use-package foo :hook after-init)
  ⇒ Note (use-package foo :defer t :config (foo-mode + 1)) isn't lazy even though there's a :defer, since
    the :config clause forces the mode to load.
"
  `(run-with-idle-timer 10 nil (lambda nil ,sexp)))

;; ;; (defmacro when-idle (&rest body)
;;    ;; `(run-with-idle-timer 20 nil (lambda () ,@body)))
;; ;;
;;
;; ;; first this,
;; (setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
;;       gc-cons-percentage 0.6)
;; ;; then
;; (😴 load-file "~/.emacs.d/deferred-init.el")
;; ;; finally [[the following should really be at the end of deferred-init.el]]
;; (add-hook 'emacs-startup-hook
;;   (lambda ()
;;     (setq gc-cons-threshold 16777216 ; 16mb
;;           gc-cons-percentage 0.1)))

;; Make all commands of the “package” module present.
(require 'package)

;; Internet repositories for new packages.
(setq package-archives '(("gnu"    . "http://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa"  . "http://melpa.org/packages/")))

;; Update local list of available packages:
;; Get descriptions of all configured ELPA packages,
;; and make them available for download.
(😴 package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(setq use-package-always-ensure t)

;; “C-h e” to see how long it took to load each package with use-package.
(setq use-package-verbose t)

 ;; So that I can use M-x ‘use-package-report’ to see how long things take to load.
(setq use-package-compute-statistics t)

;; Avoid garbage collection during Emacs Startup phase
(use-package gcmh :config (gcmh-mode 1)) ;; “the Garbage Collector Magic Hack”

(use-package auto-package-update
  :custom ((auto-package-update-delete-old-versions t) ;; Delete residual old versions
           (auto-package-update-hide-results t)) ;; Do not bother me when updates have taken place.
  ;; Update installed packages at startup if there is an update pending.
  :hook (after-init . auto-package-update-maybe))

(eval-and-compile (require 'dash nil t)) ;; Needed at compile-time for anaphoric macros
(use-package dash :demand t) ;; "A modern list library for Emacs"
(use-package s :demand t)    ;; "The long lost Emacs string manipulation library".
(use-package f :demand t)    ;; Library for working with system files; ;; e.g., f-delete, f-mkdir, f-move, f-exists?, f-hidden?

(require 's nil t) ;; Ensure s is loaded before using s-collapse-whitespace
(declare-function s-collapse-whitespace "s" (s))

;; Lightweight undo visualiser — replaces undo-tree (which caused
;; corrupt history files and recursive-timer errors).
(use-package vundo
  :bind ("C-x u" . vundo)
  :custom (vundo-glyph-alist vundo-unicode-symbols))

;; Auto installing OS system packages
(use-package system-packages :defer t)

;; Ensure our operating system is always up to date.
;; This is run whenever we open Emacs & so wont take long if we're up to date.
;; It happens in the background ^_^
;;
;; After 5 minutes of being idle (i.e., you're likely away or reading).
;; Kill the output buffer once the update finishes — no stale *system-packages* lying around.
(run-with-idle-timer
 300
 nil
 (lambda ()
   (system-packages-update)
   (when-let ((buf (get-buffer "*system-packages*")))
     (when-let ((proc (get-buffer-process buf)))
       (set-process-sentinel proc
                             (lambda (_proc _event)
                               (when (get-buffer "*system-packages*")
                                 (kill-buffer "*system-packages*"))))))))


;; I seldom need more than the default 64KB response read size, but if I use
;; LSP/Eglot (which send multi-megabyte responses) then I'd like Emacs to reduce
;; the number of reads. A slight performance increase!
(setq read-process-output-max (* 4 1024 1024)) ; 4MB


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
;; Idempotent install check — no need to block startup for it.
(😴 system-packages-ensure "licecap") ;; Use: ⌘-SPACE licecap

;; Pack, ship and run any application as a lightweight container
;; (system-packages-ensure "docker")
;; Free universal database tool and SQL client
;; (system-packages-ensure "dbeaver-community")
;; Kubernetes IDE
;; (system-packages-ensure "lens")
;; Platform built on V8 to build network applications
;; Also known as: node.js, node@16, nodejs, npm
;; Idempotent install check — no need to block startup for it.
(😴 system-packages-ensure "node") ;; https://nodejs.org/
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

;; Provides only the command “restart-emacs”.
(use-package restart-emacs
  ;; If I ever close Emacs, it's likely because I want to restart it.
  :bind ("C-x C-c" . restart-emacs))
  ;; Let's define an alias so there's no need to remember the order.
  ;; :config (defalias 'emacs-restart #'restart-emacs)

;; Remember where I am in each file and jump back there when I reopen it.
(use-package emacs :hook (after-init . save-place-mode))
(setq save-place-file "~/.emacs.d/etc/saveplace")
;; When I reopen a file, plz recenter the view.
(advice-add 'save-place-find-file-hook :after
            (lambda (&rest _)
              (when buffer-file-name (ignore-errors (recenter)))))

;; In programming modes, M-g on a pop-up candidate to peek at its source code.
;; (Also, M-h to show its docs. Should be seldom needed, since docs pop-up by default.)
;;
;; Moreover, I also get completion for the special minibuffer sessions “M-:” or “M-&” *with* pop-up docs!
(use-package corfu
  :bind (:map corfu-map
              ("<escape>" . corfu-quit)
              ;; If there's a candidate you don't see (e.g., writing “corfu-” but
              ;; looking for the face symbol “corfu-bar” then “. s” will expose it)
              ;; or if you want to restrict the list of candidates (e.g., to see only
              ;; dictionary items use “.+ w” and to see only file paths use “. f”;
              ;; or to limit to emojis use “. :”.)
              ;; Finally, use “. l” to complete the current line based on /previous/ lines.
              ("C-." .  cape-prefix-map))
  :custom
  (corfu-auto t "Automatically enable corfu pop-ups")
  (corfu-auto-prefix 1 "Show me the completions asap")
  (corfu-auto-delay 0.02 "ASAP!")
  (corfu-popupinfo-delay 0.01 "Show extra info (e.g., docs) immediately")
  (corfu-min-width 15)
  (corfu-max-width corfu-min-width "Always have the same width")
  (corfu-count 14 "Show me 14 items in completion; ↑↓ keys or C-n/p let me navigate")
  (corfu-cycle nil "Do not cycle when I reach the end/start")
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode)
         ;; corfu-history-mode remembers selected candidates and sorts the candidates by their history position and frequency.
         ;; In order to save the history across Emacs sessions, enable `savehist-mode'.
         (global-corfu-mode . corfu-history-mode)
         (corfu-history-mode . savehist-mode)))


;; Add CAPFs to buffers where you want them
(add-hook 'prog-mode-hook  #'my/extra-completion-candidates)
(add-hook 'text-mode-hook  #'my/extra-completion-candidates)
(defun my/extra-completion-candidates ()
  "Add extra CAPFs in this buffer, after existing CAPFs."
  ;; Completion At Point Extensions (This is the “.” key mentioned above.)
  (use-package cape)
  ;; Append, don’t clobber. Order matters: first CAPF that returns wins; and the later Capfs may not get a chance to run.
  ;;
  (setq-local completion-at-point-functions
              (append completion-at-point-functions
                      (list
                       ;; Whenever I type “~/” or “./”, complete a file path
                       #'cape-file
                       ;; Fuzzy-ish words from this / other buffers.
                       ;; At the end, if there's no match, look around and see if there's similar words lying around.
                       ;; E.g., in an Elisp comment, typing “comm” should finish to “comment”.
                       #'cape-dabbrev
                       ;; Abbreviation expansions
                       #'cape-abbrev
                       ;; Eventually, give up and try the dictionary
                       #'cape-dict))))


;; Add icons to the completions
(use-package kind-icon
  :disabled "Cute, but will ignore for now. It does look nice when completing paths."
  :after corfu
  :custom
  (kind-icon-blend-background nil)
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

  ;; We cannot just do: (add-hook 'helm-cleanup-hook (lambda () (kill-matching-buffers "^\\*helm" nil t)))
  ;; Since the hook fires on C-g but by then Helm's already tears it down so
  ;; it fails buffer-live-p.
  (with-eval-after-load 'helm
  (defun my/helm-kill-session-buffers-safe ()
    "Kill stray *helm…* buffers after a Helm session ends."
    ;; Running the work via run-at-time 0 lets Helm finish its own teardown first, avoiding the “kill a thing that Helm just killed” race.
    (run-at-time 5 nil                   ; defer until after Helm cleanup
                 (lambda ()
                   (unless (minibufferp) ;; If I'm typing at a prompt, delete nothing!
                   (let ((kill-buffer-query-functions nil))
                     (dolist (b (buffer-list))
                       (when (and (buffer-live-p b)
                                  (string-match-p "\\`\\*helm" (buffer-name b)))
                         (with-demoted-errors "helm-kill: %S"
                           (kill-buffer b))))))))))
  (add-hook 'helm-cleanup-hook #'my/helm-kill-session-buffers-safe)

(use-package helm
  :hook ((after-init . helm-mode) ;; Enable Helm completion for common Emacs commands.
         (after-init . helm-autoresize-mode))
  :custom ((helm-completion-style 'emacs)
           (completion-styles '(basic partial-completion emacs22 initials flex))
           (helm-M-x-show-short-doc t)) ;; Show docstrings when I press “M-x”
  ;; Helm also has optimized Helm completions for some commands, so let's use those instead of the defaults:
  :bind* (("M-x"     . helm-M-x)
          ;; ⇒ After “M-x” press “C-h m” to see how Helm improves the default M-x command.
          ;; ⇒ For example, “M-x C-]” toggles docstrings to appear.
          ;; ⇒ Or, “RET” on any candidate after “M-x” to see entire docstrings! 💝
          ("C-x C-f" . helm-find-files)
          ;; Do “C-x b C-h m” to learn more about what Helm's “C-x b” can do for you!
          ;; ⟨📚 In general, after any Helm command, do “C-h m” to get rich info about what you can do!⟩
          ("C-x b"   . helm-mini)     ;; See buffers & recent files & bookmarks; more useful.
          ("C-x r b" . helm-filtered-bookmarks)
          ("C-x C-r" . helm-recentf)  ;; Search for recently edited files
          ("C-c i"   . helm-imenu) ;; C.f. M-x imenu-list 👀
          ;; Look at what was cut recently & paste it in.
          ("M-y" . helm-show-kill-ring)
          ;; “C-x r s 𝓍” temporarily save regions of text to character 𝓍 and paste them with “C-x r i”
          ("C-x r i" . helm-register)
          ;; When I run one-off ELisp expressions, show me function signatures and get immediate /live/ results
          ("M-:" . helm-eval-expression-with-eldoc)
          :map helm-map
          ;; We can list ‘actions’ on the currently selected item by C-z.
          ("C-z" . helm-select-action)
          ;; A “persistent action” is an action that you use in a Helm session that does not quit the session.
          ;; E.g., “M-x TAB” shows the docstring ---a persistent action-- of the currently selected command.
          ("TAB"   . helm-execute-persistent-action)
          ("<tab>" . helm-execute-persistent-action)))

;; Note M-x `helm-packages' is a browser for packages to install or upgrade.

;; Show me nice file icons when using, say, “C-x C-f” or “C-x b”
(use-package all-the-icons-completion
  :custom (helm-x-icons-provider 'all-the-icons)
  :hook (after-init . helm-ff-icon-mode))

;; When I want to see the TOC of an Org file, show me down to 3 subheadings.
(setq org-imenu-depth 7)


;; More Clipboard (“Kill ring”) Settings
;;
;; If I copy something while in another program, then kill some text in Emacs
;; via “C-k” then please ensure whatever I copied is first saved to the kill
;; ring and so accessible via “M-y”. I also use the app “Flycut” so that I get
;; to “keep a history of everything I've copied” so that I can easily
;; copy/paste/modify text ---which is surprisingly more often than you'd think!
;; (Emacs' kill-ring does not replace Flycut since Emacs' only saves copied text
;; from other apps before I perform an Emacs kill command, whereas FlyCut saves
;; every copy from anywhere.)
(setq save-interprogram-paste-before-kill t)

;; Kill the same line 3 times and you'll see (-take 3 kill-ring) has the same
;; line repeated 3 times. Let's avoid such waste!  (Note: This avoids adjacent
;; duplicates; it does not turn the kill-ring into a “set”.  The structure where
;; adjacent *sublists*, not items, is known as a Idempotent List, see §4.6 of
;; “The Boom Hierarchy”
;; https://web.archive.org/web/20150322051225/http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.49.3252&rep=rep1&type=pdf
;; )
(setq kill-do-not-save-duplicates t)

;; Persist the Kill Ring Across Sessions
;;
;; The built-in savehist-mode can be used to save any variable to survive across
;; restarts; let's save the kill ring ---and clocking history so that I know
;; what I was working on previously.

(use-package emacs :hook (after-init . savehist-mode))

(setq savehist-additional-variables
      '(search-ring regexp-search-ring kill-ring org-clock-history))

;; However, let's miniify the kill-ring before saving by removing text properties.
(add-hook 'savehist-save-hook
          (lambda ()
            (setq kill-ring
                  (mapcar #'substring-no-properties
                          (cl-remove-if-not #'stringp kill-ring)))))

(setq helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-recentf
                                    helm-source-bookmarks
                                    helm-source-bookmark-set
                                    helm-source-buffer-not-found))

(use-package helm
  :bind  (("C-s"     . #'helm-occur)           ;; search current buffer
          ("C-M-s"   . 'helm-multi-occur-from-isearch))) ;; Search all buffer

;; Idempotent install check — ag is already on the system; defer the verification.
(😴 system-packages-ensure "ag")

(use-package emacs
    :ensure org-contrib)

;; ox-extra is only needed during Org export — the `ignore-headlines'
;; feature strips headings tagged :ignore: at export time.  Loading it
;; eagerly pulls in org-contrib and Org itself (~0.9s).  Deferring to
;; first export is safe since no startup code depends on it.
(with-eval-after-load 'ox
  (require 'ox-extra)
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

;; C-n, next line, inserts newlines when at the end of the buffer
(setq next-line-add-newlines t)

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Make RETURN key act the same way as “y” key for “y-or-n” prompts.
;; E.g., (y-or-n-p "Happy?") accepts RETURN as “yes”.
(define-key y-or-n-p-map [return] 'act)

;; Enable all ‘possibly confusing commands’ such as helpful but
;; initially-worrisome “narrow-to-region”, C-x n n.
(setq-default disabled-command-function nil)

(defvar running-tests noninteractive "Code executed in headless Github CI.")

;; Shell with a nearly universal compatibility with terminal applications 💝
(use-package vterm
  :if (not running-tests) ;; Avoid vterm package initialization when in headless github CI
  ;; Easily paste in VTerm
  :bind (("s-v" . vterm-yank)
         ("M-y" . vterm-yank)))

;; "Intelligent" switching to vterm; eg creates it if it's not open, non-intrusive windowing, saves window setup, etc.
(use-package vterm-toggle
    :bind* ("C-t" . vterm-toggle))

;; Be default, Emacs please use zsh
;; E.g., M-x shell
(unless running-tests (setq shell-file-name "/bin/zsh"))

;; Idempotent install check — defer since tldr is never needed at startup.
(😴 system-packages-ensure "tldr")

;; Idempotent install check — defer since hr is a shell utility, not needed at startup.
(😴 system-packages-ensure "hr") ;; ≈ brew install hr

(advice-add #'view-echo-area-messages :after (lambda (&rest _) (other-window 1)))

;; Auto update buffers that change on disk.
;; Will be prompted if there are changes that could be lost.
(global-auto-revert-mode 1)
;; Auto refreshes every 2 seconds. Don’t forget to refresh the version control status as well.
(setq auto-revert-interval 2
      auto-revert-check-vc-info t
      global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(bind-key* "M-i" (lambda ()
                   (interactive)
                   (if current-prefix-arg
                       (load-file "~/.emacs.d/init.el")
                     (find-file "~/.emacs.d/init.org"))))

;; [[file:init.org::#Undo-tree-Very-Local-Version-Control][Undo-tree: Very Local Version Control:2]]
;; By default C-z is suspend-frame, i.e., minimise, which I seldom use.
(global-set-key (kbd "C-z") #'vundo)
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
;; Idempotent install check — magit-delta is lazy-loaded via :hook anyway.
(😴 system-packages-ensure "git-delta")
(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

;; Don't forget to copy/paste the delta config into the global ~/.gitconfig file.
;; Copy/paste this: https://github.com/dandavison/delta#get-started
;; Intro:2 ends here

;; [[file:init.org::#Credentials-I-am-who-I-am][Credentials: I am who I am:1]]
;; Only set these creds up if there is no Git email set up ---ie at work I have an email set up, so don't
;; override it with my personal creds.
;;
;; Deferred: These write to ~/.gitconfig which persists across sessions,
;; so the check is almost always a no-op.  No reason to block startup.
;;
;; See here for a short & useful tutorial:
;; https://alvinalexander.com/git/git-show-change-username-email-address
(😴 when (equal "" (shell-command-to-string "git config user.email "))
  (shell-command (format "git config --global user.name \"%s\"" user-full-name))
  (shell-command (format "git config --global user.email \"%s\"" user-mail-address)))
;; Credentials: I am who I am:1 ends here

;; [[file:init.org::#Credentials-I-am-who-I-am][Credentials: I am who I am:2]]
;; We want to reuse an existing Emacs process from the command line
;; E.g.,  emacsclient --eval '(+ 1 2)'    # ⇒ 3
(require 'server)
(unless (server-running-p) (server-start))

;; Or use it whenever we are editing a git message from the terminal.
;; Deferred: This is idempotent (same value every time) and persists
;; in ~/.gitconfig, so it only matters on a fresh machine.
;; Diabled: (😴 shell-command "git config --global core.editor 'emacsclient -t -a=\\\"\\\"'")
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

  ;; (use-package magit-repos) ;; Gets us the magit-repository-directories variable.
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
(use-package git-timemachine :defer t)
;; Gotta love that time machine:1 ends here

;; [[file:init.org::#Highlighting-TODO-s-Showing-them-in-Magit][Highlighting TODO-s & Showing them in Magit:1]]
;; NOTE that the highlighting works even in comments.
(use-package hl-todo
  :custom (hl-todo-keyword-faces
           '(("MA" . "#dc8cc3")
             ("TEST" . "#dc8cc3")
             ("HOLD" . "#d0bf8f")
             ("TODO" . "#cc9393")
             ("NEXT" . "#dca3a3")
             ("FAIL" . "#8c5353")
             ("DONE" . "#afd8af")
             ("NOTE" . "#d0bf8f")
             ("HACK" . "#d0bf8f")
             ("TEMP" . "#d0bf8f")
             ("FIXME" . "#cc9393")
             ("XXXX*" . "#cc9393")))
  ;; Enable it everywhere: I want todo-words highlighted in prose, not just in code fragments.
  :hook (after-init . global-hl-todo-mode))
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
  (setq magit-todos-keywords (list "TODO" "FIXME" "MA" "WK" "JC"))
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
  :hook (after-init . beginend-global-mode))
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
(use-package org-tempo :defer t :ensure emacs)
;; Org-mode's ~<𝒳~ Block Expansions:1 ends here

;; [[file:init.org::*No code evaluation upon export][No code evaluation upon export:1]]
;; Ignore all header arguments relating to “:eval”. Do not evaluate code when I export to HTML or LaTeX or anything else.
(setq org-export-use-babel nil)
;; No code evaluation upon export:1 ends here

;; [[file:init.org::#ELisp][ELisp:1]]
;; Evaluation Result OverlayS for Emacs Lisp
(use-package eros :hook (after-init . eros-mode))
;; ELisp:1 ends here

;; [[file:init.org::#e-Edit-Everything-in-a-separate-buffer][⌘-e: Edit Everything in a separate buffer:1]]
(use-package separedit :defer t)
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

;; Make "⌘-e" toggle editing string literals / select region / [Org/markdown] code block / comment block when programming.
(when (fboundp '--map)
  (--map (bind-key "s-e" #'separedit it)
         '(prog-mode-map minibuffer-local-map help-mode-map))) ;; TODO: helpful-mode-map
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
  :defer t
  :bind (:map edit-indirect-mode-map
         ("s-e" . (lambda ()
                    (interactive)
                    (or (ignore-errors (call-interactively #'separedit))
                        (call-interactively #'edit-indirect-commit))))))

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
(use-package language-detection :defer t)
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
  :defer t
  :bind ("s-r" . er/expand-region))
;; ⌘-r, ⌘-i, ⌘-o: Sleek Semantic Selection:1 ends here

;; [[file:init.org::#Editor-Documentation-with-Contextual-Information][Editor Documentation with Contextual Information:1]]
(use-package helpful
  :commands (helpful-callable helpful-symbol)
  :bind (("C-h k" . #'helpful-key)
         ("C-h o" . #'my/describe-symbol)))

(defun my/describe-symbol (symbol)
  "A “C-h o” replacement using “helpful”.

If there's a thing at point, offer that as default search item.

If a prefix is provided, i.e., “C-u C-h o” then the built-in
“describe-symbol” command is used.

⇨ Pretty docstrings, with links and highlighting.
⇨ Source code of symbol.
⇨ Callers of function symbol.
⇨ Key bindings for function symbol.
⇨ Aliases.
⇨ Options to enable tracing, dissable, and forget/unbind the symbol!"
  (interactive "P")
  (let* ((sym-at-pt (symbol-at-point))
         (default   (and (symbolp sym-at-pt) (symbol-name sym-at-pt)))
         (prompt    (if default
                        (format "Describe symbol (default %s): " default)
                      "Describe symbol: "))
         (pred      (lambda (sym)  ; SYM is a symbol when COLLECTION is an obarray
                      (cl-some (lambda (x) (funcall (cadr x) sym))
                               describe-symbol-backends)))
         (name      (completing-read prompt
                                     obarray         ; ← use obarray directly
                                     pred
                                     t               ; require-match
                                     nil nil
                                     default))       ; ← default goes here
         (sym (intern name)))
    (if current-prefix-arg
        (describe-symbol sym)        ; C-u C-h o → built-in
      (if (or (functionp sym) (macrop sym) (commandp sym))
          (helpful-callable sym)
        (helpful-symbol sym)))))
;; Editor Documentation with Contextual Information:1 ends here

;; [[file:init.org::#Let's-make-working-with-Emacs-Lisp-even-better][[[https://github.com/xuchunyang/elisp-demos][Append existing ELisp docstrings with example use and actual output.]]:1]]
(use-package elisp-demos
  :defer t
  :init
  ;; Advice wrappers load elisp-demos on first C-h o / describe-function.
  (with-eval-after-load 'helpful
    (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))
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
(use-package banner-comment :defer t)
;; Comment-boxes up to the fill-column ---or banner instead?:2 ends here

;; [[file:init.org::#Aggressive-Indentation][Aggressive Indentation:1]]
;; Always stay indented: Automatically have blocks reindented after every change.
(use-package aggressive-indent
  :hook (after-init . global-aggressive-indent-mode))

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
  :hook (after-init . global-color-identifiers-mode))

;; Sometimes just invoke: M-x color-identifiers:refresh
;; Coding with a Fruit Salad: Semantic Highlighting:1 ends here

;; [[file:init.org::#highlight-quoted-symbols][Highlight Quoted Symbols:1]]
(use-package highlight-quoted
  :hook (emacs-lisp-mode . highlight-quoted-mode))

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
  :init
  ;; Colour the escapes as if they were builtin keywords.
  ;; Plain `put` — no package load needed.
  (put 'hes-escape-backslash-face 'face-alias 'font-lock-builtin-face)
  (put 'hes-escape-sequence-face 'face-alias 'font-lock-builtin-face))


;; The use-package :hook declarations above already autoload these
;; packages when prog-mode activates — no manual load-file needed.

;; If the above two worked fine, then you should see \n and 3 highlighted below
(when nil "Look: 1 and \\ and \n 2" (setq three 3))
;; Highlighting Numbers and Escape Characters:1 ends here

;; [[file:init.org::#Highlight-defined-Lisp-symbols][Highlight /defined/ Lisp symbols:1]]
;; Emacs Lisp specific
(use-package highlight-defined :hook emacs-lisp-mode)
;; Highlight /defined/ Lisp symbols:1 ends here

;; [[file:init.org::*interactive macro-expander][interactive macro-expander:1]]
(use-package macrostep
  :bind (:map emacs-lisp-mode-map ("C-c e" . macrostep-expand)))
;; interactive macro-expander:1 ends here

;; [[file:init.org::*Smart jumping to definitions][Smart jumping to definitions:1]]
(😴 progn
(use-package elisp-def)
(bind-key*  "M-." #'elisp-def emacs-lisp-mode-map)

;; Example usage:
(when nil
  (let ((foo 1))
    (setq foo 2))) ;; “M-.” on this “foo” will now take us to the start of the let-clause.
)
;; Smart jumping to definitions:1 ends here

;; [[file:init.org::#Project-management-navigation][Projectile: Project management & navigation:2]]
(defun my/copy-current-file-path ()
  "Add current file path to kill ring."
  (interactive)
  (kill-new buffer-file-name)
  (message "Copied path “%s” to clipboard" buffer-file-name))
;; Projectile: Project management & navigation:2 ends here

;; [[file:init.org::*Tree cmd, also treemacs for navigating a new repository][Tree cmd, also treemacs for navigating a new repository:1]]
(defun my/explore-directory ()
  "Run “tree | less” for `default-directory'."
  (interactive)
  (vterm-toggle)
  (vterm-send-string "tree|less")
  (vterm-send-return)
  (message "Press “/ 𝒳” to narrow to files with 𝒳 in their path."))
;; Tree cmd, also treemacs for navigating a new repository:1 ends here

;; [[file:init.org::#Jump-between-windows-using-Cmd-Arrow-between-recent-buffers-with-Meta-Tab][Jump between windows using Cmd+Arrow & between recent buffers with Meta-Tab:1]]
;; Disabled: s-←/→/↑/↓ window switching — not actively used.
;; Re-enable by uncommenting:
;; (use-package windmove
;;   :bind (("s-<up>" . windmove-up)
;;          ("s-<down>" . windmove-down)
;;          ("s-<left>" . windmove-left)
;;          ("s-<right>" . windmove-right))
;;   :init (setq windmove-wrap-around t))
;; Jump between windows using Cmd+Arrow & between recent buffers with Meta-Tab:1 ends here

;; [[file:init.org::*\[C-u\]-M-TAB to move between buffers][[C-u]-M-TAB to move between buffers:1]]
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
;; [C-u]-M-TAB to move between buffers:1 ends here

;; [[file:init.org::*Refactoring: Rename fun, inline fun, extract fun, extract constant, etc][Refactoring: Rename fun, inline fun, extract fun, extract constant, etc:1]]
(use-package emr ;; 𝑬𝑴acs 𝑹refactor
  ;; Press “⌘-ENTER” to get a drop-down of relevant refactors. Select a region or else use thing at point.
  ;; NOTE: With hyperbole enabled in prog-mode, M-RET is “/smart/ jump to definition”:
  ;; This is like “M-.” but is smart enough to realize 'foo should jump to the form (defun foo …).
  :bind (:map prog-mode-map
              ("s-<return>" . emr-show-refactor-menu)))

;; Useful refactors
;; ⇒ delete unused let binding form
;; ⇒ eval and replace
;; ⇒ extract constant ★
;; ⇒ extract function / inline function
;; ⇒ extract to let ★★ / inline let variable
;; ⇒ extract variable / inline variable
;; ⇒ implement function ★
;; ⇒ comment / uncomment region

;; ★ Some refactors need to be rigged via M-x; e.g., on a symbol run “M-x emr
;; impl” to get a template to implement it as a function.
;; ★★ Some need to be against a form: Either before the starting paren, or else a selected region.

(😴 progn
    ;; It seems easy to add more refactorings:
    ;; https://github.com/Wilfred/emacs-refactor?tab=readme-ov-file#user-content-extension
    ;;
    ;; Let's add one!
    ;;
    (emr-declare-command 'emr-el-tidy-requires
      :title "tidy"
      :description "require"
      :modes 'emacs-lisp-mode
      :predicate (lambda ()
                   (thing-at-point-looking-at
                    (rx bol (* space) "(require " (* nonl)))))

    (defun emr-el-tidy-requires ()
      "Consolidate and reorder requires in the current buffer.

Order requires alphabetically and remove duplicates."
      (interactive "*")
      (let (requires
            (rx-require (rx bol (* space) "(require" (+ space)
                            (group-n 1 (+ (not space)))
                            ")")))
        (save-excursion
          (when (emr-el:goto-first-match rx-require)
            (beginning-of-line)
            (forward-line)

            ;; Collect requires in buffer.
            (save-excursion
              (goto-char (point-min))
              (while (search-forward-regexp rx-require nil t)
                (push (substring-no-properties (match-string 1))
                      requires)
                (replace-match "")
                (when (emr-blank-line?)
                  (ignore-errors
                    (kill-line)))))

            (->> (emr-el:sort-requires requires)
                 (s-join "\n")
                 (s-append "\n")
                 (insert))))))

    (defun emr-el:sort-requires (requires)
      (->> (sort requires (lambda (L R) (string< L R)))
           (-distinct)
           (--map (s-trim it))
           (--map (format "(require %s)" it))))

    ;; Let's provide an alias for a useful method.
    (defalias 'emr-el-rename-in-file 'emr-iedit-global)

    ;; More Lisp-specific refactoring ideas ---such as changing “if” to “cond”---
    ;; can be found by looking at RedShank:
    ;; https://github.com/emacsattic/redshank/blob/d059c5841044aa163664f8bf87c1d981bf0a04fe/redshank.el#L745
    ;; WAIT, it seems that these work fine with Emacs Lisp, not just Common Lisp! 😲

    ;; NOTE: Lispy already includes some common refactorings like extract function and cond<->if out of the box.
    )
;; Refactoring: Rename fun, inline fun, extract fun, extract constant, etc:1 ends here

;; [[file:init.org::*Finally, for authoring to MELPA][Finally, for authoring to MELPA:1]]
(use-package flycheck :defer t)

;; Checker for ELisp package meta-data.
;; This makes use of “package-lint”, which is a prerequisite for MELPA.
(use-package flycheck-package
  :hook (emacs-lisp . my/enable-flycheck-when-I-am-authoring-for-MELPA))

(cl-defun my/enable-flycheck-when-I-am-authoring-for-MELPA ()
  (unless (or (s-starts-with? "*Org Src init.org[ emacs-lisp ]*" (buffer-name))
              (equal default-directory (f-expand user-emacs-directory)))
    (flycheck-package-setup)
    (flycheck-mode +1)))
;; Finally, for authoring to MELPA:1 ends here

;; Get org-headers to look pretty! E.g., * → ⊙, ** ↦ ◯, *** ↦ ★
;; https://github.com/emacsorphanage/org-bullets
(use-package org-bullets :hook (org-mode . org-bullets-mode))

(if (member "Apple Color Emoji" (font-family-list))
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)
  (message-box "Musa: Install the font!"))
;; E.g., Download font such as https://fonts.google.com/noto/specimen/Noto+Color+Emoji
;; Double-click on the ttf file then select “install” to have it installed on your system
;; (Note: Noto does not work on my personal machine.)


;; Disabled: We use native Unicode emoji via Apple Color Emoji fontset above.
;; No need for ASCII→emoji conversion overhead on every buffer.
;; (use-package emojify)
;; (global-emojify-mode 1)

;; Silence the usual message: Get more info using the about page via C-h C-a.
(setq inhibit-startup-message t)

(setq org-default-notes-file "~/Dropbox/my-life.org")
(setq org-agenda-files (list org-default-notes-file))

;; Open my-life.org on startup.
(setq initial-buffer-choice
      (lambda ()
        (let ((buf (find-file-noselect org-default-notes-file)))
          (with-current-buffer buf
            (org-superstar-mode +1)
            (font-lock-update))
          buf)))

(defun display-startup-echo-area-message ()
  "The message that is shown after ‘user-init-file’ is loaded."
  (message
   (concat "Welcome "      user-full-name
           "! Emacs "      emacs-version
           "; Org-mode "   org-version
           "; System "     (symbol-name system-type)
           "/"             (system-name)
           "; Time "       (emacs-init-time))))

;; Ensure I'm notified if startup suddenly gets slower!
(unless running-tests
  (cl-assert (< (string-to-number (emacs-init-time "%d")) 5)
             'show-args
             "Whoops, things got slower! 😲 Or is this a fresh install?"))

;; Keep self motivated!
(setq frame-title-format '("" "%b - Living The Dream (•̀ᴗ•́)و"))

;; I have symlinks for various things, just follow them, do not ask me.
(setq vc-follow-symlinks t)

;; After my settings have been loaded, e.g., fancy priorities
;; and cosmetics, then open my notes files.
(when nil add-hook 'emacs-startup-hook
          (lambda ()
            (-let [my-life.el (getenv "MY_LIFE_ELISP")]
              (unless org-default-notes-file
                (error "Add to .zshrc “ export MY_LIFE_ELISP=\"/full/path/to/my-life.el\" ”, then load my-life.el"))
              (load-file my-life.el))))

;; The modeline looks really nice with doom-themes, e.g., doom-solarised-light.
;; doom-modeline must load eagerly — deferring via :hook after-init
;; triggers a SIGSEGV in face_for_font (Emacs 30.1 bug: the font
;; subsystem isn't ready when after-init hooks fire).
(use-package doom-modeline
  :config (doom-modeline-mode)
  (setq doom-modeline-height 21)
  (setq doom-modeline-buffer-state-icon nil)
  (setq doom-modeline-hud t)
  (setq doom-modeline-bar-width 1)
  (setq doom-modeline-checker-simple-format nil)
  (setq doom-modeline-buffer-encoding nil))

  ;; Inactive buffers' modeline is greyed out.
  ;; (let ((it "Source Code Pro Light" ))
  ;;   (set-face-attribute 'mode-line nil :family it :height 100)
  ;;   (set-face-attribute 'mode-line-inactive nil :family it :height 100))

  ;; A quick hacky way to add stuff to doom-modeline is to add to the mode-line-process list.
  ;; E.g.:  (add-to-list 'mode-line-process '(:eval (format "%s" (count-words (point-min) (point-max)))))
  ;; We likely want to add this locally, to hooks on major modes.

(use-package minions
  :custom (doom-modeline-minor-modes t)
  :hook (after-init . minions-mode))

;; If not for doom-modeline, we'd need to use fancy-battery-mode.el.
(display-battery-mode +1)

;; Show date and time as well.

;; [Simple Approach]
;; (setq display-time-day-and-date t)
;; (display-time)

;; [More Controlled Approach: Set date&time format]
;; a ≈ weekday; b ≈ month; d ≈ numeric day, R ≈ 24hr:minute.
(setq display-time-format "%a %b %d ╱ %r") ;; E.g.,:  Fri Mar 04 ╱ 03:42:08 pm
(setq display-time-interval 60) ;; Once per minute suffices; every-second redraws waste CPU.
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
;; We skip theme setup in batch mode (e.g., CI) — no display, no point.
(defvar my/theme-packages
  '(doom-themes solarized-theme stimmung-themes shanty-themes
    apropospriate-theme tao-theme leuven-theme material-theme
    moe-theme organic-green-theme tango-plus-theme minimal-theme
    espresso-theme nano-theme pink-bliss-uwu-theme modus-themes)
  "Packages that provide the themes in `my/themes'.")

(unless running-tests
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
                 ;; Build the list but do NOT call package-install here.
                 append theme-variants-I-like))

  (setcdr (last my/themes) my/themes)

  ;; Idempotent install checks — these 17 calls each spawn a subprocess
  ;; just to confirm the package is already present.  Deferring is safe
  ;; because the themes are already on disk; this only matters on a
  ;; fresh machine.  Run (my/ensure-theme-packages) manually if needed.
  (defun my/ensure-theme-packages ()
    “Ensure all theme packages in `my/theme-packages' are installed.”
    (interactive)
    (dolist (pkg my/theme-packages)
      (package-install pkg)))
  (😴 my/ensure-theme-packages))

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

;; Startup theme is set further below (currently solarized-gruvbox-light).

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

;; Idempotent install checks — fonts are already on the system; these 9
;; subprocess calls just verify presence.  Safe to defer since the default
;; font is set separately (below) and Emacs falls back gracefully.
(😴 progn
    ;; NOTE: No longer needed - fonts are now available directly in homebrew/cask
    ;; (shell-command "brew tap homebrew/cask-fonts")
    (system-packages-ensure "svn") ;; Required for the following font installs
    ;; No thanks! (system-packages-ensure "font-roboto-mono") ;; Makes Gerrit in Chrome look like Gibberish!
    (system-packages-ensure "font-input")
    (system-packages-ensure "font-source-code-pro")
    (system-packages-ensure "font-fira-mono")
    (system-packages-ensure "font-mononoki")
    (system-packages-ensure "font-monoid")
    (system-packages-ensure "font-menlo-for-powerline")
    (system-packages-ensure "font-fantasque-sans-mono")
    (system-packages-ensure "font-ibm-plex"))

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
  (when (display-graphic-p) ;; i.e., do nothing when run in CI for github verifications
    (let* ((all-fonts (append (fontset-list) (x-list-fonts "*" nil)))
           (font (if current-prefix-arg
                     (nth (random (length all-fonts)) all-fonts)
                   new-font)))
      (set-face-font 'default font)
      (message "Font: %s" font))))

(global-set-key "\C-c\ F" 'my/toggle-font)

;; Default font; the “ignore-⋯” is for users who may not have the font.
(my/toggle-font "Fantasque Sans Mono 12")
;; (my/toggle-font "Source Code Pro Light 14")
;; (my/toggle-font "IBM Plex Mono 12")

(unless running-tests
  ;; Breaks Gerrit: (my/toggle-font "Roboto Mono Light 14")
  (my/toggle-theme 'solarized-gruvbox-light))

(use-package dimmer
  :hook after-init)

;; (setq visible-bell 1) ;; On MacOS, this shows a caution symbol ^_^

;; The doom themes package comes with a function to make the mode line flash on error.
;; (use-package doom-themes)
;; (require 'doom-themes-ext-visual-bell)
;; (doom-themes-visual-bell-config)

(blink-cursor-mode 1)

;; Phantom cursors are a minor visual annoyance, but they also have a small rendering cost.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(unless running-tests
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

(use-package all-the-icons
  :defer t
  :config
  ;; Idempotent font install — runs when the package eventually loads
  ;; (e.g., via doom-modeline). Only matters on a fresh machine.
  (let ((font-dest (cl-case window-system
                     (x  (concat (or (getenv "XDG_DATA_HOME")
                                     (concat (getenv "HOME") "/.local/share"))
                                 "/fonts/"))
                     (mac (concat (getenv "HOME") "/Library/Fonts/" ))
                     (ns (concat (getenv "HOME") "/Library/Fonts/" )))))
    (unless (file-exists-p (concat font-dest "all-the-icons.ttf"))
      (all-the-icons-install-fonts 'install-without-asking))))

;; [[file:init.org::*Show me the diff!][Show me the diff!:1]]
(defun my/show-string-diff (string1 string2)
  "Show the difference between STRING1 and STRING2 using `diff` in Emacs."
  (interactive "sEnter first string: \nsEnter second string: ")
  (let ((temp-buffer1 (get-buffer-create "*Diff String 1*"))
        (temp-buffer2 (get-buffer-create "*Diff String 2*")))
    ;; Fill the first buffer with string1
    (with-current-buffer temp-buffer1
      (erase-buffer)
      (insert string1))
    ;; Fill the second buffer with string2
    (with-current-buffer temp-buffer2
      (erase-buffer)
      (insert string2))
    ;; Call the diff command on the two buffers
    (diff temp-buffer1 temp-buffer2)))
;; Show me the diff!:1 ends here

;; [[file:init.org::*Why Emacs? Because of Org-agenda: /“Write fragmentarily, read collectively”/][Why Emacs? Because of Org-agenda: /“Write fragmentarily, read collectively”/:1]]
;; I like to write everything in one massive file, and the agenda should consult it.
(setq org-agenda-files (list org-default-notes-file))
;; Why Emacs? Because of Org-agenda: /“Write fragmentarily, read collectively”/:1 ends here

;; [[file:init.org::*Why Emacs? Because of Org-agenda: /“Write fragmentarily, read collectively”/][Why Emacs? Because of Org-agenda: /“Write fragmentarily, read collectively”/:2]]
;; `org-ql' is a Lispy query language for Org files.  It allows you to find Org
;; entries matching certain criteria and return a list of them or perform
;; actions on them.
(use-package org-ql :defer t)
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
(setq org-agenda-block-separator "") ;; one blank line between blocks (no ugly horizontal rule)
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
           ((org-ql-block-header "📩 Process Inbox: 'm' to mark then 'B r' to refile marked items 📥")))


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
           ((org-ql-block-header "⚡ Top goals for the month ⚡")
            (org-agenda-remove-tags t)))


          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Tasks that would bring me joy                                          ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (org-ql-block
           '(and (tags-local "Happy") (or (scheduled -7) (deadline -7) (not (done))))
           ((org-ql-block-header "🤗 I'd be happy if I got the following done this week 🥰")
            (org-agenda-remove-tags t)))


          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; What “I've done so far” is all tasks closed this week.                 ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Note the definition of “done” includes all terminal states in my workflow.
          (org-ql-block
           `(and (not (tags "Recurring")) (done) (closed :from ,(- (calendar-day-of-week (calendar-current-date))) :to today)) ;; “start-of-week” /from today/
           ((org-ql-block-header (propertize "✅ What I've done so far this week 💯" 'help-echo "Press E to toggle seeing ~5 lines of each entry. \n If DONE, mine for useful info then archive or delete.")))
)

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Deadlines: Upcoming tasks I should prepare for                                ;;
          ;;                                                                               ;;
          ;; NOTE: I don't want to use predicate (not (done)) since I want to		   ;;
          ;; see the DONE items as a reminder to myself to actually archive		   ;;
          ;; these tasks.								   ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (org-ql-block
           '(and (deadline auto) (not (tags "Top")) (not (done)))
           ((org-ql-block-header "🎯 Deadlines")))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Whoops, things that I've missed!                                       ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (org-ql-block
           '(and (not (habit)) (not (tags "Top")) (not (done)) (scheduled :to today) (not (scheduled :on today)))
           ((org-ql-block-header "📆 Overdue")))


          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Things loads into my cognitive memory that I should probably continue? ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (org-ql-block
           '(and
             (todo "STARTED")
             (level '> 1)
             (not (tags-local "Someday" "Top" "SocialCredit"))
             (not (scheduled :from today)))
           ((org-ql-block-header "🤡 Please 𝒓𝒆𝒅𝒖𝒄𝒆 the number of (unscheduled) open loops")))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; Stuff I'd like to do today; but do I actually have the time to do so?  ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (org-ql-block
           '(and (scheduled :on today) (ts :with-time nil))
           ((org-ql-block-header "😵‍💫 ﴾ Any time ≈ No time﴿ Scheduled today, but not time-blocked")))

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ;; What am I doing today, and when?                                              ;;
          ;;                                                                               ;;
          ;; TODO: Use “agenda*” ?  The agenda* view is the same as agenda		   ;;
          ;; except that it only considers appointments, i.e., scheduled and		   ;;
          ;; deadline items that have a time specification '[h]h:mm’ in their		   ;;
          ;; timestamps.								   ;;
          ;; https://orgmode.org/manual/Special-Agenda-Views.html#FOOT172		   ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          (agenda ""
                  ((org-agenda-overriding-header
                    "🔎 Please focus on 𝒪𝓃𝓁𝓎 these tasks for the day!")
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
                                  "🗯️ Incomplete tasks scheduled later this week"
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
           ((org-ql-block-header "💢 I've been waiting on these [for over a week?], send reminder!")))

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
(setq my/special-org-ql-header "🗯️ Incomplete tasks scheduled later this week")
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
                   (not (equal org-agenda-overriding-header "🔎 Please focus on 𝒪𝓃𝓁𝓎 these tasks for the day!")))
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
              (perfect-margin-mode))
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
     (insert "\t")
     (my/insert-button "Refresh Work"
                       (lambda (pos) (my/agenda-work-refresh))
                       :foreground "orange"
                       :background nil
                       :echo "Re-fetch Gerrit/Jira data for the work sections below")
     ;;
     ;; NOTE: 'someday’ things sometimes go into my quote system so that I run into them sometime; lol likewise for things I want to remember
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
;; (my/string-fill-column-and-center 27 ""We don't think about sinning as you don't think about eating rotten food." ---Imam As-Sadiq")


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

;; [[file:init.org::*my/consume-content][my/consume-content:1]]
(defun my/consume-content ()
  "This agenda block should display 5 entries, selected uniformly at random
from those entries meeting your selection criteria. Refreshing the
agenda will reshuffle.

Note: Use “g” to refresh and see 5 (new) random entries;
or press “v l 30 RET r” to view 30 entries."
  (interactive)
  (find-file org-default-notes-file)
  (widen)
  (beginning-of-buffer)
  (search-forward  (car (org-ql-query
                          :select (lambda () (substring-no-properties (thing-at-point 'line)))
                          :from org-agenda-files
                          :where '(tags "ConsumeContent")
                          :order-by (lambda (x y) (pcase (random 3)
                                               (0 nil)
                                               (1 1)
                                               (2 -1)))
                          ;; PR open to get :limit support https://github.com/alphapapa/org-ql/pull/495
                          ;; Note to self: I used https://github.com/emacs-eldev/eldev to run the org-ql tests.
                          :limit 1))) ;; ⟵ Much faster since I only want 1.
  ;; (org-tree-to-indirect-buffer) (other-window 1)
  (org-narrow-to-subtree))
;; my/consume-content:1 ends here

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
;; Start each agenda item with '○’, then show me it's %timestamp and how many
;; times it's been re-%scheduled.
(setq org-agenda-prefix-format " ○ %?-12t%-6e%s ")

(setq org-agenda-deadline-leaders '("DUE:       " "In %3d d.: " "%2d d. ago:  "))

(setq org-agenda-scheduled-leaders
      '(""                ;; Don't say “Scheduled ⟨Task⟩”, just show “⟨Task⟩”.
        "Overdue%2dx "))  ;; If something's overdue, say “Overdue 𝓃× ⟨Task⟩”.
;; How tasks look in org agenda:1 ends here

;; [[file:init.org::*Configuration][Configuration:1]]
;; These defvars provide safe empty defaults.  The real values are
;; set by private.el (loaded eagerly at startup; see §Separating Secrets).

(defvar my\gerrit-ssh-command "/usr/bin/ssh"
  "Path to the SSH binary used for Gerrit queries.")

(defvar my\gerrit-ssh-host "gerrit"
  "SSH host alias for Gerrit, as defined in ~/.ssh/config.")

(defvar my\gerrit-base-url ""
  "Base URL for Gerrit web UI links.")

(defvar my\jira-base-url ""
  "Base URL for Jira ticket links (the /browse endpoint).")

(defvar my\gerrit-user user-login-name
  "Gerrit/Jira username for ownership queries.")

(defvar my\gerrit-project-path "/c/+/"
  "Gerrit URL path template between the base URL and the change number.
Used to construct clickable change
links, e.g. BASE-URL + PROJECT-PATH + CHANGE-NUMBER.")

(defvar my\jira-ticket-regex nil
  "Regex to extract Jira ticket IDs from Gerrit commit messages.
Must have one capture group (\\\\1) yielding the ticket ID string.
When nil, ticket extraction is disabled.")
;; Configuration:1 ends here

;; [[file:init.org::*Cache variables][Cache variables:1]]
(cl-defstruct (work-item (:constructor work-item-create))
  "A single unit of work displayed in the agenda.
Each instance represents either a Gerrit dependency stack or a
Jira ticket (possibly both — a Gerrit stack whose Jira ticket is
flagged urgent).  Created by `work-item-from-stack' or
`work-item-from-jira-issue'; rendered by
`work-item-to-string'.

The `urgent' flag is orthogonal to `status': an item can be both
`:urgent t' and `:status \\='wip', meaning it is urgent AND work-in-
progress.  Urgent items are rendered with a \"🔴 \" prefix and
appear in whichever status section they belong to.

Example:
  (work-item-create :jira \"BUG-1234\"
                    :title \"Fix frobnicate\"
                    :tip-url \"https://…/+/101\"
                    :reviewers \\='(\"Alice\" \"Bob\")
                    :author \"Musa\"
                    :status \\='my-needing-action
                    :urgent t)"
  (jira nil
        :documentation "Jira ticket ID string, eg `Bug-1234', or nil for one-off commits.")
  (title nil
         :documentation "Jira summary, or commit subject when no Jira ticket.")
  (tip-url nil
           :documentation "Gerrit URL for the topmost change in the stack, or nil for not-yet-started urgent Jira items.")
  (reviewers nil
             :documentation "List of reviewer name strings (humans from allReviewers + any negative Code-Review voters).")
  (reviewer-users nil
                  :documentation "Alist of (display-name . username) for each reviewer.
Used to construct per-reviewer Gerrit attention URLs.")
  (author nil
          :documentation "Display name of the stack owner.")
  ;; Whether this item is urgent, WIP, something for me to review, something I
  ;; should ping others to review, or if it's feedback I need to address on my
  ;; own work.
  (status nil
          :documentation "Symbol: todo | assigned | jira-active | wip | please-review | reviews-needed | my-needing-action.
`todo' means an urgent Jira ticket with no Gerrit work yet.
`assigned' means an unresolved ticket assigned to me (not urgent, no Gerrit work).
`jira-active' means Jira considers the ticket In Progress/In Review but no Gerrit changes exist — likely a #progress vs #resolve mistake.")
  (urgent nil
          :documentation "Non-nil if this item is flagged urgent by Jira (Urgency = 1 month or upcoming fixVersion).")
  (org-tree nil
            :documentation "Marker to the corresponding Org heading in
`org-default-notes-file', or nil.  Set by `work-item--resolve-org-trees'
during cache build.  Enables `org-marker'/`org-hd-marker' text properties
so standard agenda commands (RET, TAB, I, o) work on work-item lines.")
  (age nil
       :documentation "Unix epoch (number) of the Gerrit tip's `lastUpdated'.
Nil for `todo' items (Jira-only, no Gerrit change).  Rendered as
a human-readable staleness string by `work-item-to-string'.")
  (stack-size nil
              :documentation "Number of Gerrit changes in the dependency stack.
A rough proxy for effort size: 1 = atomic fix, 5+ = substantial feature.")
  (max-patchsets nil
                 :documentation "Highest patchset number across all changes in the stack.
A proxy for churn: > 4 suggests repeated rework cycles and possible
author/reviewer misalignment — consider a synchronous conversation.")
  (comment-count nil
                 :documentation "Total number of inline review comments across all changes
in the stack.  High counts relative to stack size signal a contentious
or unclear change — worth a synchronous conversation to align.")
  (avg-shirt-size nil
                  :documentation "Average Gerrit shirt size across changes in the stack.
A float derived from mapping each change's (insertions + deletions) to
the Gerrit size scale (XS=1, S=2, M=3, L=4, XL=5) and averaging.
Displayed as the nearest label in `work-item-help-echo'.")
  (ci-status nil
             :documentation "Symbol: `pass', `fail', or nil (no Verified vote).
Derived from the most recent Verified approval on the tip change.
`fail' is the highest-priority nudge: nothing else matters until CI is green.")
  (has-code-review nil
                   :documentation "Non-nil if the tip has a Code-Review +1 or +2 from a non-owner.
When CI passes and this is nil, the nudge is: request final approval.")
  (blocked-by-parent nil
                     :documentation "Non-nil if this work-item's tip depends on an unmerged parent.
The parent must land first; nudge is: unblock the dependency."))

(defvar my/agenda-work-data nil
  "Cached plist holding structured Gerrit/Jira agenda data.
Keys: `:items' (list of `work-item' instances), `:timestamp'.")

(defvar my/agenda-work-fetching nil
  "Non-nil while an async Gerrit/Jira fetch cycle is in progress.

Guards against double-fetching: `my/agenda-work-refresh' is a
no-op when this is non-nil.  Reset to nil by the coordinator
`my/agenda--format-and-cache' (or on error).")

(defvar my/agenda-work--on-complete nil
  "One-shot callback invoked after the work-item cache is populated.
Set before calling `my/agenda-work-refresh'; cleared after invocation.")

(defvar my/agenda--pending-queries 0
  "Number of async Gerrit SSH queries still in flight.

Set to 2 at the start of a refresh (attention + all-mine).
Each sentinel decrements it; when it hits zero, the coordinator
fires.")

(defvar my/agenda--attention-changes nil
  "Parsed change alists from the \"attention:self\" Gerrit query.

These are changes in my attention set — either others' changes
I need to review, or my own changes that need my response.")

(defvar my/agenda--all-mine-changes nil
  "Parsed change alists from the \"owner:self\" Gerrit query.

All my open, non-abandoned changes.  Combined with the attention
set to derive the \"please review\" and WIP sections (changes I
own that are NOT in my attention set).")
;; Cache variables:1 ends here

;; [[file:init.org::*Resolving work-items to Org headings][Resolving work-items to Org headings:1]]
(defun work-item--resolve-org-trees (items)
  "Populate the `org-tree' slot for each item in ITEMS.
Pass 1: match Jira IDs against headings in `org-default-notes-file'.
Pass 2: match colleague names for reviews-needed items without Jira."
  (let ((resolved 0))
    ;; Pass 1: Jira-ID heading search
    (let ((jira-items (-filter #'work-item-jira items))
          (buf (find-file-noselect org-default-notes-file)))
      (when (and jira-items (buffer-live-p buf))
        (with-current-buffer buf
          (org-with-wide-buffer
           (dolist (it jira-items)
             (goto-char (point-min))
             (let ((re (concat "^\\*+ .*"
                               (regexp-quote (work-item-jira it)))))
               (if (re-search-forward re nil t)
                   (progn
                     (org-back-to-heading t)
                     (setf (work-item-org-tree it) (point-marker))
                     (cl-incf resolved))
                 (message "work-item--resolve: no heading found for %s"
                          (work-item-jira it)))))))))
    ;; Pass 2: colleague Org-ID lookup for reviews-needed sans Jira
    (when (boundp 'my\colleagues)
      (dolist (it items)
        (when (and (eq (work-item-status it) 'reviews-needed)
                   (not (work-item-org-tree it))
                   (work-item-author it))
          (let* ((first-name (downcase (car (split-string
                                             (work-item-author it)))))
                 (match (cl-find-if
                         (lambda (c)
                           (string-prefix-p first-name
                                            (downcase (symbol-name c))))
                         my\colleagues)))
            (when match
              (when-let ((loc (org-id-find (symbol-name match) 'marker)))
                (setf (work-item-org-tree it) loc)
                (cl-incf resolved)))))))
    (message "work-item--resolve: %d/%d items linked to Org headings"
             resolved (length items))))
;; Resolving work-items to Org headings:1 ends here

;; [[file:init.org::*Gerrit SSH query (synchronous)][Gerrit SSH query (synchronous):1]]
(defvar my/gerrit--bridge-max-depth 5
  "Maximum BFS depth when discovering intermediate bridge changes.
During stack grouping, BFS walks outward from known changes along
dependsOn/neededBy edges.  Each depth level fires a synchronous
SSH query to Gerrit for any unseen neighbors.  Setting this too
high can be slow; 5 covers the deepest stacks we encounter in
practice.")
(defvar my/gerrit--jira-title-cache (make-hash-table :test 'equal)
  "Hash-table mapping Jira ticket ID (string) → summary title (string).
Populated lazily by `my/gerrit--fetch-jira-titles' and read by
`my/gerrit--get-jira-title'.  Survives across agenda refreshes
so we do not re-fetch titles for tickets we have already seen.")

(cl-defun my/gerrit--query (query-string &optional extra-flags)
  "Run a synchronous Gerrit SSH query and return a list of change alists.
Calls `my\\gerrit-ssh-command' via `call-process', parsing each
line of the output as JSON.  The final stats line (with key `type')
is filtered out.  Temporarily binds `timer-idle-list' and
`timer-list' to nil to suppress idle timers — this prevents
recursive `call-process' errors when called from a `run-at-time'
callback.

QUERY-STRING is the Gerrit query, e.g.:
  \"status:open owner:self -is:abandoned\"

EXTRA-FLAGS overrides the default flags (--current-patch-set,
--dependencies, --commit-message, --comments).  By way of example,
bridge resolution passes \\='(\"--dependencies\") since it only needs edges.

Example:
  (my/gerrit--query \"change:123456\")
  → (((number . 123456) (subject . \"Fix frobnicate\") ...))"
  (let* ((default-flags '("--current-patch-set" "--dependencies"
                           "--commit-message" "--comments"))
         (flags (or extra-flags default-flags))
         (args `(,my\gerrit-ssh-host "gerrit" "query" "--format=JSON"
                 ,@flags "--" ,query-string))
         (timer-idle-list nil) (timer-list nil)
         (raw (with-output-to-string
                (with-current-buffer standard-output
                  (apply #'call-process
                         my\gerrit-ssh-command nil t nil args)))))
    (->> (s-lines raw)
         (-filter #'s-present?)
         (-map (lambda (line)
                 (ignore-errors (json-read-from-string line))))
         (-filter #'identity)
         (-filter (lambda (obj) (not (assoc 'type obj)))))))

(defun my/agenda--parse-gerrit-json (raw)
  "Parse RAW (a string of newline-delimited JSON) into change alists.
Each line from Gerrit SSH is a standalone JSON object.  The final
line is a stats summary (containing key `type') which we discard.
Lines that fail `json-read-from-string' are silently skipped.

This is the async counterpart to `my/gerrit--query': the latter
calls `call-process' directly, whereas this function operates on
a pre-collected output string from a process sentinel.

Example:
  (my/agenda--parse-gerrit-json
   \"{\\\"number\\\":42,\\\"subject\\\":\\\"Fix it\\\"}\\n{\\\"type\\\":\\\"stats\\\"}\\n\")
  → (((number . 42) (subject . \"Fix it\")))"
  (->> (s-lines raw)
       (-filter #'s-present?)
       (-map (lambda (line)
               (ignore-errors (json-read-from-string line))))
       (-filter #'identity)
       (-filter (lambda (obj) (not (assoc 'type obj))))))
;; Gerrit SSH query (synchronous):1 ends here

;; [[file:init.org::*Stack grouping (Union-Find + BFS)][Stack grouping (Union-Find + BFS):1]]
(cl-defun my/gerrit--collect-neighbor-numbers (change)
  "Return a deduplicated list of change numbers adjacent to CHANGE.
Walks both the `dependsOn' and `neededBy' fields of the Gerrit
change alist, extracting the `number' from each entry.

Example (given a change that depends on 111 and is needed by 222):
  (my/gerrit--collect-neighbor-numbers
   \\='((number . 100)
     (dependsOn . [((number . 111))])
     (neededBy  . [((number . 222))])))
  → (222 111)"
  (let (nums)
    (dolist (dep (append (alist-get 'dependsOn change) nil))
      (push (alist-get 'number dep) nums))
    (dolist (nb (append (alist-get 'neededBy change) nil))
      (push (alist-get 'number nb) nums))
    (-uniq nums)))

(cl-defun my/gerrit--build-full-edge-map (changes)
  "Build a complete edge map via BFS, discovering bridge changes.
Starting from CHANGES (the known result set), walk outward along
dependsOn/neededBy edges up to `my/gerrit--bridge-max-depth' hops.
At each depth, fire a synchronous Gerrit SSH query to fetch any
unseen neighbors — these are the \"bridges\" that connect two known
changes through an intermediate that was not in the original query.

Returns a two-element list (EDGES PRESENT) where:
  EDGES   — hash-table: change-number → list of neighbor numbers
  PRESENT — hash-table: change-number → t, for numbers in CHANGES

Only PRESENT numbers participate in Union-Find merging later; bridge
changes appear in EDGES but not in PRESENT."
  (let* ((present (make-hash-table :test 'eql))
         (edges   (make-hash-table :test 'eql))
         (visited (make-hash-table :test 'eql)))
    (dolist (c changes)
      (let ((n (alist-get 'number c)))
        (puthash n t present)
        (puthash n t visited)
        (puthash n (my/gerrit--collect-neighbor-numbers c) edges)))
    (cl-loop
     repeat my/gerrit--bridge-max-depth
     do (let ((frontier
               (let (nums)
                 (maphash (lambda (_n nbrs)
                            (dolist (nb nbrs) (push nb nums)))
                          edges)
                 (->> nums -uniq
                      (-filter (lambda (n)
                                 (not (gethash n visited))))))))
          (when (null frontier) (cl-return))
          (let ((intermediates
                 (my/gerrit--query
                  (s-join " OR "
                          (-map (lambda (n) (format "change:%d" n))
                                frontier))
                  '("--dependencies"))))
            (dolist (c intermediates)
              (let ((n (alist-get 'number c)))
                (puthash n t visited)
                (puthash n (my/gerrit--collect-neighbor-numbers c)
                         edges))))))
    (list edges present)))

(cl-defun my/gerrit--group-into-stacks (changes)
  "Group CHANGES into dependency stacks using Union-Find over BFS edges.
Two changes land in the same stack if they are transitively connected
via dependsOn/neededBy edges — even through intermediate \"bridge\"
changes not in CHANGES themselves.

Returns a list of stacks, each stack being a list of change alists
sorted base-first (ascending change number).  Stacks are ordered
by the tip's change number (most recent first), so the agenda
shows the freshest work at the top.

Example (conceptual):
  Given changes A→B→C (a linear chain) and D (standalone):
  (my/gerrit--group-into-stacks (list A B C D))
  → ((A B C) (D))"
  (if (null changes) nil
    (-let* (((edges present)
             (my/gerrit--build-full-edge-map changes))
            (parent (make-hash-table :test 'eql)))
      (dolist (c changes)
        (puthash (alist-get 'number c)
                 (alist-get 'number c) parent))
      (cl-labels
          ((find (x)
             (let ((p (gethash x parent x)))
               (if (eql p x) x
                 (let ((root (find p)))
                   (puthash x root parent) root))))
           (union (a b)
             (let ((ra (find a)) (rb (find b)))
               (unless (eql ra rb) (puthash ra rb parent)))))
        (dolist (c changes)
          (let* ((start (alist-get 'number c))
                 (queue (list start))
                 (seen (make-hash-table :test 'eql)))
            (puthash start t seen)
            (while queue
              (let* ((cur (pop queue))
                     (neighbors (gethash cur edges)))
                (dolist (nb neighbors)
                  (unless (gethash nb seen)
                    (puthash nb t seen)
                    (when (gethash nb present)
                      (union start nb))
                    (unless (gethash nb present)
                      (push nb queue))))))))
        (let ((groups (make-hash-table :test 'eql)))
          (dolist (c changes)
            (let* ((n (alist-get 'number c))
                   (root (find n)))
              (puthash root
                       (cons c (gethash root groups))
                       groups)))
          (let (stacks)
            (maphash
             (lambda (_root members)
               (push (my/gerrit--topo-sort-stack members)
                     stacks))
             groups)
            (sort stacks
                  (lambda (a b)
                    (> (alist-get 'number (-last-item a))
                       (alist-get 'number
                                  (-last-item b)))))))))))

(cl-defun my/gerrit--topo-sort-stack (changes &rest _ignored)
  "Sort CHANGES base-first by ascending change number.
In a Gerrit dependency chain, lower change numbers are created
first and sit at the base of the stack.  This ordering ensures
the agenda renders the chain from bottom to top — matching the
order you would cherry-pick or submit them."
  (sort (copy-sequence changes)
        (lambda (a b)
          (< (alist-get 'number a) (alist-get 'number b)))))
;; Stack grouping (Union-Find + BFS):1 ends here

;; [[file:init.org::*Jira helpers][Jira helpers:1]]
(cl-defun my/gerrit--extract-jira-tickets (change)
  "Extract the primary Jira ticket from CHANGE's commit message.
Uses `my\\jira-ticket-regex' (set in private.el) to scan the
commit message.  When multiple tickets are referenced, the last
one is taken — by convention, the final Jira tag is the primary
ticket being worked towards, and earlier ones are related context.

Returns a single-element list (for API compatibility with callers
that expect a list), or nil if no ticket matches."
  (when my\jira-ticket-regex
    (let ((msg (or (alist-get 'commitMessage change) "")))
      (-when-let (matches (->> (s-match-strings-all my\jira-ticket-regex msg)
                               (-map #'cadr)))
        (last matches)))))

(cl-defun my/gerrit--stack-jira-tickets (stack)
  "Return Jira tickets shared by every change in STACK.
We intersect (not union) because a ticket that appears in all
changes of a stack is the \"theme\" of that stack — suitable for
display as a prefix.  A ticket mentioned in only one change is
likely incidental and would clutter the summary.

When the intersection is empty — e.g. a dependent change
references a different ticket or none at all — we fall back to
the tip (last) change's tickets so the caller still gets
something useful.

Example:
  ;; Change A references BUG-1, BUG-2; change B references BUG-1, BUG-3
  (my/gerrit--stack-jira-tickets (list A B))
  → (\"BUG-1\")"
  (let ((all (-map #'my/gerrit--extract-jira-tickets stack)))
    (when all
      ;; Fall back to the tip's tickets when the intersection is empty.
      (or (-reduce-from
           (lambda (acc tix) (-intersection acc tix))
           (car all) (cdr all))
          (car (last all))))))

(cl-defun my/gerrit--format-jira-link (ticket)
  "Format TICKET as an Org-style bracketed link using `my\\jira-base-url'.

Example:
  (let ((my\\jira-base-url \"https://bugs.example.com/browse\"))
    (my/gerrit--format-jira-link \"BUG-101\"))
  → \"[[https://bugs.example.com/browse/BUG-101][BUG-101]]\""
  (format "[[%s/%s][%s]]" my\jira-base-url ticket ticket))

(cl-defun my/gerrit--jira-rest-base ()
  "Derive the Jira REST API base URL from `my\\jira-base-url'.
Strips the trailing \"/browse\" path component, since the REST API
lives at /rest/api/2/... on the same host.

Example:
  (let ((my\\jira-base-url \"https://bugs.example.com/browse\"))
    (my/gerrit--jira-rest-base))
  → \"https://bugs.example.com\""
  (replace-regexp-in-string "/browse\\'" "" my\jira-base-url))

(cl-defun my/gerrit--fetch-jira-titles (tickets)
  "Batch-fetch Jira summaries for TICKETS into `my/gerrit--jira-title-cache'.
Only fetches tickets not already cached, so repeated calls are cheap.
Uses `curl -sn' (silent, read ~/.netrc for auth) to hit the Jira
REST API with a JQL `key in (...)' query.

TICKETS is a list of ticket ID strings, e.g. (\"BUG-101\" \"BUG-202\").
After this call, `my/gerrit--get-jira-title' returns the summary
for each successfully fetched ticket."
  (let ((uncached (-filter
                   (lambda (tk)
                     (not (gethash tk my/gerrit--jira-title-cache)))
                   tickets)))
    (when uncached
      (let* ((jql (format "key in (%s)" (s-join "," uncached)))
             (url (format "%s/rest/api/2/search?jql=%s&fields=summary&maxResults=%d"
                          (my/gerrit--jira-rest-base)
                          (url-hexify-string jql)
                          (length uncached)))
             (timer-idle-list nil) (timer-list nil)
             (raw (with-output-to-string
                    (with-current-buffer standard-output
                      (call-process "curl" nil t nil "-sn" url))))
             (json-obj (ignore-errors (json-read-from-string raw))))
        (when json-obj
          (dolist (issue (append (alist-get 'issues json-obj) nil))
            (let ((key (alist-get 'key issue))
                  (summary (alist-get 'summary
                                      (alist-get 'fields issue))))
              (when (and key summary)
                (puthash key summary
                         my/gerrit--jira-title-cache)))))))))

(cl-defun my/gerrit--get-jira-title (ticket)
  "Return the cached Jira summary string for TICKET, or nil if unknown.
The cache is populated by `my/gerrit--fetch-jira-titles'.

Example:
  (my/gerrit--get-jira-title \"BUG-101\")
  → \"Optimize OSPF route computation\"  ; or nil if not yet fetched"
  (gethash ticket my/gerrit--jira-title-cache))

(cl-defun my/gerrit--query-jira (jql &optional fields)
  "Run a JQL query against Jira and return a list of issue alists.
Uses `curl -sn' (reading ~/.netrc for auth) to call the REST API.
FIELDS defaults to \"summary\"; pass a comma-separated string for
additional fields (e.g. \"summary,status,assignee\").  Returns up
to 50 results.

Example:
  (my/gerrit--query-jira
   \"assignee = musa AND resolution = Unresolved\"
   \"summary,priority\")
  → (((key . \"BUG-101\") (fields . ((summary . \"Fix it\") ...))) ...)"
  (let* ((url (format "%s/rest/api/2/search?jql=%s&fields=%s&maxResults=50"
                      (my/gerrit--jira-rest-base)
                      (url-hexify-string jql)
                      (or fields "summary")))
         (timer-idle-list nil) (timer-list nil)
         (raw (with-output-to-string
                (with-current-buffer standard-output
                  (call-process "curl" nil t nil "-sn" url))))
         (json-obj (ignore-errors (json-read-from-string raw))))
    (when json-obj
      (append (alist-get 'issues json-obj) nil))))

(cl-defun my/jira-post-comment (ticket comment)
  "Post COMMENT string as a comment on Jira TICKET via REST API.
Uses `curl -sn' (silent, read ~/.netrc for auth).  Returns the
comment ID string on success, nil on failure."
  (let* ((url (format "%s/rest/api/2/issue/%s/comment"
                      (my/gerrit--jira-rest-base) ticket))
         (payload (json-encode `((body . ,comment))))
         (timer-idle-list nil) (timer-list nil)
         (raw (with-output-to-string
                (with-current-buffer standard-output
                  (call-process "curl" nil t nil
                                "-sn" "-X" "POST"
                                "-H" "Content-Type: application/json"
                                "-d" payload url))))
         (json-obj (ignore-errors (json-read-from-string raw))))
    (and json-obj (alist-get 'id json-obj))))
;; Jira helpers:1 ends here

;; [[file:init.org::*Conversion: raw alists → =work-item= instances][Conversion: raw alists → =work-item= instances:1]]
(defun work-item--strip-commit-tags (subject)
  "Strip leading bracketed module tags from SUBJECT.
Commit subjects often begin with \"[module]\" or \"[a, b]\" prefixes
that are noise in the agenda context.

Example:
  (work-item--strip-commit-tags \"[gui, css] Align button spacing\")
  → \"Align button spacing\""
  (if (string-match "\\`\\[.*?\\] *" subject)
      (substring subject (match-end 0))
    subject))

(defun work-item--shirt-size-value (change)
  "Map a Gerrit CHANGE alist to a numeric shirt size (1–5).
Based on total lines changed (insertions + deletions):
  XS (≤10) → 1, S (≤50) → 2, M (≤250) → 3, L (≤1000) → 4, XL → 5.
Returns nil when size data is absent."
  (let* ((ps  (alist-get 'currentPatchSet change))
         (ins (alist-get 'sizeInsertions ps))
         (del (alist-get 'sizeDeletions ps)))
    (when (and ins del)
      (let ((total (+ ins del)))
        (cond ((<= total 10)   1)
              ((<= total 50)   2)
              ((<= total 250)  3)
              ((<= total 1000) 4)
              (t               5))))))

(defun work-item--shirt-size-label (value)
  "Convert a numeric shirt size VALUE (1–5) to a label string.
Rounds to the nearest integer first."
  (pcase (round value)
    (1 "XS") (2 "S") (3 "M") (4 "L") (_ "XL")))

(defun work-item--avg-shirt-size (stack)
  "Compute the average numeric shirt size across changes in STACK.
Returns nil when no change has size data."
  (let ((vals (-keep #'work-item--shirt-size-value stack)))
    (when vals
      (/ (-sum vals) (float (length vals))))))

(defun work-item--ci-status (change)
  "Derive CI status from the Verified approval on CHANGE.
Returns `pass' if the latest Verified vote is positive, `fail' if
negative, or nil if no Verified vote exists."
  (let* ((approvals (append (alist-get 'approvals
                                       (alist-get 'currentPatchSet change))
                            nil))
         (verified (-first (lambda (a)
                             (equal "Verified" (alist-get 'type a)))
                           approvals)))
    (when verified
      (let ((val (string-to-number (or (alist-get 'value verified) "0"))))
        (if (< val 0) 'fail 'pass)))))

(defun work-item--has-code-review (change owner-username)
  "Return non-nil if CHANGE has a positive Code-Review from a non-owner.
OWNER-USERNAME is the change owner's Gerrit username, excluded from
the check (self-approvals don't count)."
  (let ((approvals (append (alist-get 'approvals
                                      (alist-get 'currentPatchSet change))
                           nil)))
    (-any-p (lambda (a)
              (and (equal "Code-Review" (alist-get 'type a))
                   (> (string-to-number (or (alist-get 'value a) "0")) 0)
                   (not (equal owner-username
                               (alist-get 'username (alist-get 'by a))))))
            approvals)))

(defun work-item--blocked-by-parent (change stack)
  "Return non-nil if CHANGE depends on an unmerged parent in STACK.
A change is blocked when it has `dependsOn' entries whose change
numbers also appear in the stack (i.e., they are open, not yet merged)."
  (let ((deps (append (alist-get 'dependsOn change) nil))
        (stack-nums (-map (lambda (c) (alist-get 'number c)) stack)))
    (-any-p (lambda (d)
              (member (alist-get 'number d) stack-nums))
            deps)))

(defun work-item--format-age (epoch)
  "Format a Gerrit `lastUpdated' EPOCH into a human-readable age string.
Returns nil when EPOCH is nil.  Uses the coarsest unit that is ≥ 1:
years → months → weeks → days → \"today\"."
  (when epoch
    (let* ((seconds (- (float-time) epoch))
           (days (floor (/ seconds 86400))))
      (cond
       ((>= days 365) (format "%d year%s"
                              (/ days 365) (if (>= (/ days 365) 2) "s" "")))
       ((>= days 30)  (format "%d month%s"
                              (/ days 30) (if (>= (/ days 30) 2) "s" "")))
       ((>= days 7)   (format "%d week%s"
                              (/ days 7) (if (>= (/ days 7) 2) "s" "")))
       ((>= days 1)   (format "%d day%s"
                              days (if (>= days 2) "s" "")))
       (t "today")))))

(defun work-item-from-stack (stack status)
  "Convert a Gerrit dependency STACK into a `work-item' with STATUS.
STACK is a list of change alists (base-first, as produced by
`my/gerrit--group-into-stacks').  STATUS is one of the symbols:
  wip, please-review, reviews-needed, my-needing-action.

The Jira title cache must already be populated (via
`my/gerrit--fetch-jira-titles') before calling this function.

Example:
  (work-item-from-stack some-stack \\='my-needing-action)
  → #s(work-item :jira \"BUG-1234\" :title \"Fix it\" ...)"
  (let* ((tip (car (last stack)))
         (jira (car (my/gerrit--stack-jira-tickets stack)))
         (jira-title (when jira (my/gerrit--get-jira-title jira))))
    (work-item-create
     :jira jira
     :title (or jira-title
                (when-let ((subj (alist-get 'subject tip)))
                  (work-item--strip-commit-tags subj))
                "untitled")
     :tip-url (format "%s%s%s"
                      my\gerrit-base-url my\gerrit-project-path
                      (alist-get 'number tip))
     :reviewers (my/gerrit--extract-reviewer-names stack)
     :reviewer-users (my/gerrit--extract-reviewer-user-map stack)
     :author (alist-get 'name (alist-get 'owner tip))
     :status status
     :age (alist-get 'lastUpdated tip)
     :stack-size (length stack)
     :max-patchsets (when stack
                      (-max (-map (lambda (c)
                                    (or (alist-get 'number
                                                   (alist-get 'currentPatchSet c))
                                        1))
                                  stack)))
     :comment-count (-sum (-map (lambda (c)
                                  (length (alist-get 'comments c)))
                                stack))
     :avg-shirt-size (work-item--avg-shirt-size stack)
     :ci-status (work-item--ci-status tip)
     :has-code-review (work-item--has-code-review
                       tip (alist-get 'username (alist-get 'owner tip)))
     :blocked-by-parent (work-item--blocked-by-parent tip stack))))

(defun work-items-from-stack (stack status)
  "Split STACK by primary Jira ticket, one work-item per ticket.
Each group's sub-stack is passed to `work-item-from-stack'.

Nil-ticket changes are dropped when at least one change has a
ticket.  When NO change has a ticket, the entire stack is kept
as a single group so the item still appears in the agenda."
  (let* ((by-ticket
          (-group-by
           (lambda (c)
             (car (my/gerrit--extract-jira-tickets c)))
           stack))
         (non-nil-groups
          (-filter (lambda (g) (car g)) by-ticket))
         (groups (or non-nil-groups by-ticket)))
    (-map (lambda (group)
            (work-item-from-stack (cdr group) status))
          groups)))

(defun work-items-from-review-stack (stack self-username)
  "Split STACK by (Jira ticket × author), one work-item per group.
SELF-USERNAME is the current user's Gerrit username; their changes
are excluded (we don't review our own work).

Groups keyed by a nil Jira ticket are discarded when at least one
non-nil group exists — the jira-less changes are noise in a
multi-ticket stack.  When ALL groups have nil tickets (the regex
did not match, or the author genuinely omitted references), we
keep them so the review section still appears.

Each group's sub-stack is passed to `work-item-from-stack', which
picks up the correct tip, Jira ticket, reviewers, and author
automatically.  For a single-author, single-ticket stack this
returns a one-element list — identical to the old behaviour."
  (let* ((non-self
          (-filter
           (lambda (c)
             (not (equal self-username
                         (alist-get 'username (alist-get 'owner c)))))
           stack))
         (by-ticket-and-author
          (-group-by
           (lambda (c)
             (cons (car (my/gerrit--extract-jira-tickets c))
                   (alist-get 'username (alist-get 'owner c))))
           non-self))
         (non-nil-groups
          (-filter (lambda (g) (car (car g))) by-ticket-and-author))
         (groups (or non-nil-groups by-ticket-and-author)))
    (-map (lambda (group)
            (work-item-from-stack (cdr group) 'reviews-needed))
          groups)))

(defun work-item-from-jira-issue (issue)
  "Convert a Jira REST API ISSUE alist into a `work-item'.
Status is `todo' (no Gerrit work has started), and `urgent' is t.
ISSUE has the shape returned by `my/gerrit--query-jira':
  ((key . \"BUG-1234\") (fields . ((summary . \"Fix frobnicate\") ...)))

Example:
  (work-item-from-jira-issue
   \\='((key . \"BUG-99\") (fields . ((summary . \"Urgent thing\")))))
  → #s(work-item :jira \"BUG-99\" :title \"Urgent thing\"
                 :status todo :urgent t)"
  (let ((key (alist-get 'key issue))
        (summary (alist-get 'summary (alist-get 'fields issue))))
    (work-item-create
     :jira key
     :title (or summary "untitled")
     :tip-url nil
     :reviewers nil
     :author nil
     :status 'todo
     :urgent t)))
;; Conversion: raw alists → =work-item= instances:1 ends here

;; [[file:init.org::*Rendering =work-item= instances][Rendering =work-item= instances:1]]
(cl-defun my/gerrit--extract-reviewer-user-map (stack)
  "Extract an alist of (display-name . username) for reviewers in STACK.
Merges two sources:

1. Human names from `allReviewers' — kept when the name contains
   a space (heuristic: \"Ali Hassan\" is human, \"Jenkins\" is a
   bot) and the username is non-nil.  The change owner is excluded.
2. ANY reviewer (including bots) from `currentPatchSet.approvals'
   where type = \"Code-Review\" and value < 0.  A -1 from a bot
   is actionable feedback that should surface in the agenda.

The union is returned, deduplicated by username.

Example:
  (my/gerrit--extract-reviewer-user-map stack)
  → ((\"Ali ibn Abi Talib\" . \"ali\") (\"Hassan ibn Ali\" . \"hassan\"))"
  (let* ((owners (->> stack
                      (-map (lambda (c)
                              (alist-get 'username (alist-get 'owner c))))
                      (-filter #'identity)
                      -uniq))
         (human-reviewers
          (->> stack
               (-mapcat
                (lambda (c)
                  (->> (append (alist-get 'allReviewers c) nil)
                       (-filter
                        (lambda (rv)
                          (let ((name (alist-get 'name rv))
                                (uname (alist-get 'username rv)))
                            (and name uname
                                 (s-contains-p " " name)
                                 (not (member uname owners))))))
                       (-map (lambda (rv)
                               (cons (alist-get 'name rv)
                                     (alist-get 'username rv)))))))
               (-filter #'identity)))
         (negative-voters
          (->> stack
               (-mapcat
                (lambda (c)
                  (->> (append (alist-get 'approvals
                                          (alist-get 'currentPatchSet c))
                               nil)
                       (-filter
                        (lambda (a)
                          (and (equal "Code-Review" (alist-get 'type a))
                               (< (string-to-number
                                   (or (alist-get 'value a) "0"))
                                  0))))
                       (-map (lambda (a)
                               (let ((by (alist-get 'by a)))
                                 (when-let ((name (alist-get 'name by))
                                            (uname (alist-get 'username by)))
                                   (cons name uname))))))))
               (-filter #'identity))))
    (cl-remove-duplicates
     (append human-reviewers negative-voters)
     :key #'cdr :test #'equal)))

(cl-defun my/gerrit--extract-reviewer-names (stack)
  "Extract reviewer display names from STACK.
Thin wrapper around `my/gerrit--extract-reviewer-user-map' — returns
just the car (display name) of each pair, plus any name-only reviewers
whose username was nil (the user-map variant drops those).

Example:
  (my/gerrit--extract-reviewer-names stack)
  → (\"Ali ibn Abi Talib\" \"Hassan ibn Ali\")"
  (let ((with-usernames (-map #'car (my/gerrit--extract-reviewer-user-map stack)))
        ;; Catch reviewers that have a name but no username — the user-map
        ;; variant drops these, but the original names function kept them.
        (name-only
         (let ((owners (->> stack
                            (-map (lambda (c)
                                    (alist-get 'username (alist-get 'owner c))))
                            (-filter #'identity)
                            -uniq)))
           (->> stack
                (-mapcat
                 (lambda (c)
                   (->> (append (alist-get 'allReviewers c) nil)
                        (-filter
                         (lambda (rv)
                           (let ((name (alist-get 'name rv))
                                 (uname (alist-get 'username rv)))
                             (and name (not uname)
                                  (s-contains-p " " name)
                                  (not (member uname owners))))))
                        (-map (lambda (rv) (alist-get 'name rv))))))
                (-filter #'identity)
                -uniq))))
    (-uniq (append with-usernames name-only))))

(cl-defun my/gerrit--format-reviewer-names (names)
  "Format a list of NAMES into natural English with Oxford comma.

Examples:
  (my/gerrit--format-reviewer-names \\='(\"Alice\"))
  → \"Alice\"
  (my/gerrit--format-reviewer-names \\='(\"Alice\" \"Bob\"))
  → \"Alice and Bob\"
  (my/gerrit--format-reviewer-names \\='(\"Alice\" \"Bob\" \"Carol\"))
  → \"Alice, Bob, and Carol\""
  (pcase (length names)
    (1 (car names))
    (2 (format "%s and %s" (car names) (cadr names)))
    (_ (format "%s, and %s"
               (s-join ", " (butlast names))
               (car (last names))))))

(cl-defgeneric work-item-to-string (item)
  "Render ITEM as a single-line string for display in the agenda.")

(defun work-item--sort (items)
  "Sort ITEMS for agenda display: urgent first, sidequests last.
All three tiers are individually sorted stalest-first (oldest
`age' epoch).  Items with no `age' sort after those with one."
  (let (urgent normal sidequests)
    (dolist (it items)
      (cond
       ((work-item-urgent it)     (push it urgent))
       ((not (work-item-jira it)) (push it sidequests))
       (t                         (push it normal))))
    (cl-flet ((stalest-first (items)
                (sort items
                      (lambda (a b)
                        (let ((ea (or (work-item-age a) most-positive-fixnum))
                              (eb (or (work-item-age b) most-positive-fixnum)))
                          (< ea eb))))))
      (append (stalest-first urgent)
              (stalest-first normal)
              (stalest-first sidequests)))))

(cl-defmethod work-item-to-string ((item work-item))
  "Render a `work-item' ITEM as a single-line Org string for the agenda.
Dispatches on `work-item-status':
  todo              → JIRA-LINK + title (no Gerrit work yet)
  my-needing-action → Address latest feedback from REVIEWERS
  reviews-needed    → Review AUTHOR's latest efforts
  please-review     → REVIEWERS please review this work
  wip               → Resume or abandon this work
Each line is optionally prefixed with a Jira link + title when
the work-item has a non-nil `jira' slot.

The `urgent' flag prepends \"🔴 \" to the rendered string so the
item stands out in whichever status section it belongs to."
  (let* ((jira        (work-item-jira item))
         (title       (work-item-title item))
         (url         (work-item-tip-url item))
         (reviewers   (work-item-reviewers item))
         (author      (work-item-author item))
         (status      (work-item-status item))
         (urgent      (work-item-urgent item))
         (jira-link   (when jira (my/gerrit--format-jira-link jira)))
         (jira-prefix (cond
                       (jira-link
                        (if title
                            (format "%s %s ∷ " jira-link title)
                          (format "%s ∷ " jira-link)))
                       (title
                        (format "Sidequest{ %s } ∷ " title))))
         (age-str     (work-item--format-age (work-item-age item)))
         (rv-str      (when reviewers
                        (my/gerrit--format-reviewer-names reviewers)))
         (urgent-prefix (if urgent "🔴 " ""))
         (age-prefix  (if age-str (format "[%s] " age-str) "")))
    (pcase status
      ('todo
       (concat urgent-prefix age-prefix
               (if title (format "%s \"%s\"" (or jira-link "") title)
                 (or jira-link ""))))

      ('my-needing-action
       (concat urgent-prefix age-prefix jira-prefix
               (format "Address [[%s][latest feedback]]" url)
               (when rv-str (format " from %s" rv-str))))

      ('reviews-needed
       (concat urgent-prefix age-prefix jira-prefix
               (format "Review %s's [[%s][latest efforts]]"
                       (or author "someone") url)))

      ('please-review
       (if jira-prefix
           (format "%s%s%s%s please review this [[%s][work]]"
                   urgent-prefix age-prefix jira-prefix (or rv-str "Reviewers") url)
         (format "%s%s%s please review [[%s][%s]]"
                 urgent-prefix age-prefix (or rv-str "Reviewers") url title)))

      ('wip
       (if jira-prefix
           (concat urgent-prefix age-prefix
                   (format "%sResume or abandon this [[%s][work]]"
                           jira-prefix url) "?")
         (concat urgent-prefix age-prefix
                 (format "Resume or abandon [[%s][%s]]"
                         url title) "?")))

      ('assigned
       (concat age-prefix
               (if title (format "%s \"%s\"" (or jira-link "") title)
                 (or jira-link ""))))

      ('jira-active
       (concat urgent-prefix age-prefix
               (if title (format "%s \"%s\" — ⚠️ Jira active but no Gerrit changes"
                                 (or jira-link "") title)
                 (format "%s — ⚠️ Jira active but no Gerrit changes"
                         (or jira-link ""))))))))

(defun work-item-help-echo (item)
  "Return a minibuffer hint suggesting the next action for ITEM.
The hint has three layers, concatenated:

  ACTION — status-specific primary suggestion.
  META   — bracketed stats: changes, size, patchset, comments, urgency.
  NUDGE  — stat-based strategic advice (split, rebase, escalate, etc.).

Patchsets alone are not a contention signal — iterative improvement is
normal.  We only flag contention when patchsets AND comments are both
high, optionally combined with long duration for the strongest signal.

Stat-based nudges (highest priority wins):
  (i) 🔴 CI failed → fix CI before anything else.
  (j) 🔗 Blocked by parent → unblock the dependency first.
  (a) 🧱 Stuck: many ps + many comments + old → escalate / split / rethink.
  (f) Escalate: high ps + growing comments → schedule synchronous talk.
  (c) Abandon & re-open: very old + large + many ps → start fresh.
  (b) Split: large size (L/XL) → smaller stacks = happier reviewers.
  (e) Spin off follow-ups: many comments but low ps → optional cleanup.
  (g) Solo reviewer + stale → widen the reviewer pool or re-assign.
  (h) Many reviewers (> 2) → coordination overhead, narrow the pool.
  (k) ✅ CI green + no Code-Review → request final approval.
  (d) Rebase: > 7 days old → rebase to stay current with target branch."
  (let* ((status       (work-item-status item))
         (urgent       (work-item-urgent item))
         (age-epoch    (work-item-age item))
         (days-old     (when age-epoch
                         (floor (/ (- (float-time) age-epoch) 86400))))
         (stale        (and days-old (>= days-old 14)))
         (very-stale   (and days-old (>= days-old 30)))
         (rv-str       (when (work-item-reviewers item)
                         (my/gerrit--format-reviewer-names
                          (work-item-reviewers item))))
         (stack-size   (or (work-item-stack-size item) 1))
         (max-ps       (or (work-item-max-patchsets item) 1))
         (comments     (or (work-item-comment-count item) 0))
         (shirt-size   (work-item-avg-shirt-size item))
         (num-reviewers (length (work-item-reviewers item)))
         (ci            (work-item-ci-status item))
         (ci-red        (eq ci 'fail))
         (ci-green      (eq ci 'pass))
         (has-cr        (work-item-has-code-review item))
         (blocked       (work-item-blocked-by-parent item))
         (large-change (and shirt-size (>= shirt-size 4))) ;; L or XL
         (many-ps      (> max-ps 4))
         (many-comments (> comments (* 5 stack-size))) ;; > 5 comments/change
         (solo-reviewer (= num-reviewers 1))
         (many-reviewers (> num-reviewers 2))
         (needs-rebase (and days-old (> days-old 7)))
         ;; ── Layer 1: status-specific primary action ──
         (action
          (pcase status
            ('reviews-needed
             (cond
              (very-stale "This review is rotting — Slack nudge or offer to pair.")
              (stale      "Stale review — prioritise it today.")
              (t          "Open their change and leave a Code-Review vote.")))
            ('my-needing-action
             (cond
              (very-stale
               (format "Feedback from %s has waited a month — address or abandon."
                       (or rv-str "reviewers")))
              (stale
               (format "Feedback from %s is going stale — address it soon."
                       (or rv-str "reviewers")))
              (t "Address the feedback and re-publish.")))
            ('please-review
             (cond
              (very-stale
               (format "A month without review — escalate or re-assign %s."
                       (or rv-str "the reviewer")))
              (stale
               (format "Two weeks without review — send %s a Slack nudge."
                       (or rv-str "reviewers")))
              (t (format "Awaiting %s — patience, or a gentle ping."
                         (or rv-str "reviewers")))))
            ('wip
             (cond
              ;; (d) Very old + large + many ps → abandon and start fresh.
              ((and very-stale large-change many-ps)
               "Ancient, large, and churning — abandon and re-open as smaller pieces.")
              (very-stale "Dead weight? Abandon in Gerrit or commit to finishing it.")
              (stale
               "Idle for weeks — set it up for success: self-review, add context, then resume.")
              (t (concat "🌱 Is this actually ready? Do a self-review, "
                         "ensure the right reviewers, and keep stacks small."))))
            ('todo
             (if urgent
                 "Urgent and unstarted — create a Gerrit change today."
               "Not yet started — scope it, create a first patchset, small stacks = happy reviewers."))
            ('assigned
             "Assigned to you — decide: start working, or push back on scope.")
            ('jira-active
             "Jira says active but no Gerrit changes — stale #progress? Fix the status.")
            (_ "")))
         ;; ── Layer 2: bracketed metadata ──
         (meta
          (concat
           (when (> stack-size 1)
             (format " [%d changes]" stack-size))
           (when shirt-size
             (format " [size %s]" (work-item--shirt-size-label shirt-size)))
           (when (> max-ps 1)
             (format " [ps %d]" max-ps))
           (when (> comments 0)
             (format " [%d comment%s]" comments (if (= comments 1) "" "s")))
           (when (> num-reviewers 1)
             (format " [%d reviewers]" num-reviewers))
           (when ci-red " [CI ✗]")
           (when blocked " [blocked]")
           (when urgent " [URGENT]")))
         ;; ── Layer 3: stat-based strategic nudge (highest priority wins) ──
         (nudge
          (cond
           ;; (i) 🔴 CI failed → nothing else matters until CI is green.
           (ci-red
            " 🔴 CI is red — fix the build before anything else.")
           ;; (j) 🔗 Blocked by parent → unblock the dependency first.
           (blocked
            " 🔗 Blocked by an unmerged parent — land the dependency first.")
           ;; (a) 🧱 Stuck: many ps + many comments + stale → unblock strategically.
           ((and many-ps many-comments stale)
            " 🧱 Stuck — don't just iterate: schedule a sync discussion, split the change, or rethink the approach.")
           ;; (f) Escalate: high ps + growing comments (but not yet stale).
           ((and many-ps many-comments)
            " ⚠ High churn + heavy comments — escalate to a synchronous conversation before the next patchset.")
           ;; (c) Abandon & re-open: very old + large + many ps.
           ((and very-stale large-change many-ps)
            " ⚠ Old, large, and churning — consider abandoning and re-opening as smaller, focused pieces.")
           ;; (b) Split: large size (L/XL).
           (large-change
            (format " 💡 Size %s — split into a smaller stack; small changes = faster reviews."
                    (work-item--shirt-size-label shirt-size)))
           ;; (e) Spin off follow-ups: many comments but low ps → optional cleanup.
           ((and many-comments (not many-ps))
            " 💡 Lots of comments but few patchsets — spin off unrelated cleanup as follow-up work.")
           ;; (g) Solo reviewer + stale → widen the reviewer pool.
           ((and solo-reviewer stale)
            " 👥 Only one reviewer and going stale — add another reviewer or re-assign to get things moving.")
           ;; (h) Many reviewers → coordination overhead warning.
           (many-reviewers
            (format " 👥 %d reviewers — coordination overhead; consider narrowing to 1–2 primary reviewers."
                    num-reviewers))
           ;; (k) ✅ CI green + no Code-Review → request final approval.
           ((and ci-green (not has-cr))
            (format " ✅ CI green but no Code-Review — ask %s for final approval."
                    (or rv-str "a reviewer")))
           ;; (d) Rebase: > 7 days old.
           (needs-rebase
            " 🔄 Older than a week — rebase to stay current with the target branch."))))
    (concat action meta (or nudge ""))))
;; Rendering =work-item= instances:1 ends here

;; [[file:init.org::*Ticket collection helper][Ticket collection helper:1]]
(cl-defun my/gerrit--collect-all-tickets (stacks)
  "Collect all unique Jira ticket IDs across all STACKS.
Used before `my/gerrit--fetch-jira-titles' to batch-fetch every
ticket referenced by any change, regardless of which section it
will appear in.  Returns a flat deduplicated list of ID strings."
  (->> stacks
       (-mapcat (lambda (s)
                  (-mapcat #'my/gerrit--extract-jira-tickets s)))
       -uniq))
;; Ticket collection helper:1 ends here

;; [[file:init.org::*Async fetch machinery][Async fetch machinery:1]]
(defun my/agenda--async-gerrit-query (query-string store-var)
  "Run Gerrit QUERY-STRING asynchronously via `start-process'.
When the SSH process exits, the sentinel parses the JSON output
and stores the resulting list of change alists into STORE-VAR
(a symbol, e.g. \\='my/agenda--attention-changes).  The symbol is
attached to the process via `process-put' so the sentinel can
retrieve it — we cannot use closures since init.el has no
lexical binding.

Example:
  (my/agenda--async-gerrit-query
   \"attention:self status:open -is:abandoned\"
   \\='my/agenda--attention-changes)"
  (let* ((buf (generate-new-buffer " *gerrit-agenda-query*"))
         (args (list my\gerrit-ssh-host "gerrit" "query"
                     "--format=JSON"
                     "--current-patch-set" "--all-reviewers"
                     "--dependencies"
                     "--commit-message" "--" query-string))
         (proc (apply #'start-process "gerrit-agenda" buf
                      my\gerrit-ssh-command args)))
    (process-put proc :store-var store-var)
    (set-process-sentinel proc #'my/agenda--gerrit-sentinel)))

(defun my/agenda--gerrit-sentinel (proc _event)
  "Process sentinel for async Gerrit SSH queries.
Fires when PROC exits or is signalled.  Parses the accumulated
buffer output via `my/agenda--parse-gerrit-json', stores the
result into the symbol stashed in PROC's :store-var property,
and decrements `my/agenda--pending-queries'.  When the counter
hits zero (both queries done), schedules the coordinator
`my/agenda--format-and-cache' via `run-at-time 0' — escaping
sentinel context so that the coordinator's synchronous
`call-process' calls (bridge resolution, Jira fetching) are safe."
  (when (memq (process-status proc) '(exit signal))
    (let* ((buf (process-buffer proc))
           (raw (when (buffer-live-p buf)
                  (with-current-buffer buf (buffer-string))))
           (changes (when raw
                      (my/agenda--parse-gerrit-json raw)))
           (store-var (process-get proc :store-var))
           (exit-code (process-exit-status proc)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (set store-var changes)
      (message "Gerrit/Jira: query for %s done (exit=%d, %d changes, pending=%d)"
               store-var exit-code (length changes)
               (1- my/agenda--pending-queries))
      (cl-decf my/agenda--pending-queries)
      (when (zerop my/agenda--pending-queries)
        ;; Escape sentinel context before heavy sync processing
        (run-at-time 0 nil #'my/agenda--format-and-cache)))))

(cl-defun my/agenda-work-refresh ()
  "Kick off a full Gerrit/Jira refresh cycle (async).
Fires two parallel SSH queries to Gerrit (attention set + all my
open changes).  When both complete, the coordinator groups them
into stacks, fetches Jira titles, checks for urgent tickets, and
caches the rendered output.  If the agenda buffer is live, it
auto-refreshes to show the new data.

Guards against double-fetching: if a fetch is already in progress,
this is a no-op.

Can be called interactively via M-x or from the \"Refresh Work\"
agenda button."
  (interactive)
  (when my/agenda-work-fetching
    (message "Gerrit/Jira: fetch already in progress (fetching=%s)"
             my/agenda-work-fetching)
    (cl-return-from my/agenda-work-refresh))
  (when (s-blank? my\gerrit-base-url)
    (message "Gerrit/Jira: my\\gerrit-base-url is empty — check private.el")
    (cl-return-from my/agenda-work-refresh))
  (message "Fetching Gerrit/Jira data — you'll see a dialog box when this time-consuming task is done…")
  (setq my/agenda-work-fetching t
        my/agenda--pending-queries 2
        my/agenda--attention-changes nil
        my/agenda--all-mine-changes nil)
  (my/agenda--async-gerrit-query
   "attention:self status:open -is:abandoned"
   'my/agenda--attention-changes)
  (my/agenda--async-gerrit-query
   "status:open owner:self -is:abandoned"
   'my/agenda--all-mine-changes))
;; Async fetch machinery:1 ends here

;; [[file:init.org::*Coordinator][Coordinator:1]]
(defun my/agenda--format-and-cache ()
  "Coordinator: process fetched Gerrit/Jira data and update the cache.
Runs after both async SSH queries have completed.  Performs all
heavy synchronous work:
  1. Group changes into dependency stacks (may fire additional
     synchronous SSH queries for bridge resolution).
  2. Split attention-set stacks into mine vs. others.
  3. Batch-fetch Jira titles for all referenced tickets.
  4. Query for urgent Jira tickets (Urgency = 1 month / next release).
  5. Convert each stack to a `work-item' and store in `my/agenda-work-data'.
  6. Trigger `org-agenda-redo-all' if the agenda buffer is live.
Wrapped in `condition-case' so a failure does not leave
`my/agenda-work-fetching' stuck at t."
  (condition-case err
      (let* ((attention my/agenda--attention-changes)
             (all-mine  my/agenda--all-mine-changes)
             ;; Attention-set number lookup
             (attn-nums
              (let ((ht (make-hash-table :test 'eql)))
                (dolist (c attention)
                  (puthash (alist-get 'number c) t ht))
                ht))
             ;; Stale = my open changes NOT in the attention set
             (stale (-filter
                     (lambda (c)
                       (not (gethash (alist-get 'number c) attn-nums)))
                     all-mine))
             (wip     (-filter (lambda (c)
                                 (eq (alist-get 'wip c) t))
                               stale))
             (non-wip (-filter (lambda (c)
                                 (not (eq (alist-get 'wip c) t)))
                               stale))
             ;; Group into stacks (sync bridge resolution is safe
             ;; here --- we are in a run-at-time callback, not a
             ;; sentinel or idle timer)
             (_ (message "Gerrit/Jira: grouping %d + %d changes into stacks…"
                         (length attention) (length all-mine)))
             (stale-stacks (my/gerrit--group-into-stacks non-wip))
             (wip-stacks   (my/gerrit--group-into-stacks wip))
             (attn-stacks  (my/gerrit--group-into-stacks attention))
             ;; Split attention stacks by ownership
             (mine
              (-filter
               (lambda (s)
                 (-every-p
                  (lambda (c)
                    (equal my\gerrit-user
                           (alist-get 'username
                                      (alist-get 'owner c))))
                  s))
               attn-stacks))
             (others
              (-filter
               (lambda (s)
                 (not
                  (-every-p
                   (lambda (c)
                     (equal my\gerrit-user
                            (alist-get 'username
                                       (alist-get 'owner c))))
                   s)))
               attn-stacks))
             ;; Batch-fetch Jira titles for every ticket
             (all-stacks (append stale-stacks wip-stacks attn-stacks))
             (_ (let ((tix (my/gerrit--collect-all-tickets all-stacks)))
                  (when tix
                    (message "Gerrit/Jira: fetching titles for %d Jira tickets…"
                             (length tix))
                    (my/gerrit--fetch-jira-titles tix))))
             ;; Convert stacks → work-item instances
             (stale-items (-mapcat (lambda (s)
                                    (work-items-from-stack s 'please-review))
                                  stale-stacks))
             (wip-items   (-mapcat (lambda (s)
                                    (work-items-from-stack s 'wip))
                                  wip-stacks))
             (rv-items    (-mapcat (lambda (s)
                                     (work-items-from-review-stack
                                      s my\gerrit-user))
                                   others))
             (my-items    (-mapcat (lambda (s)
                                    (work-items-from-stack s 'my-needing-action))
                                  mine))
             (gerrit-items (append stale-items wip-items rv-items my-items))
             ;; Urgent Jira tickets (separate JQL query)
             (_ (message "Gerrit/Jira: checking for urgent Jira tickets…"))
             (urgent-jql
              (let* ((now (decode-time))
                     (mon (nth 4 now))
                     (yr  (mod (nth 5 now) 100))
                     (next-mon (1+ mon))
                     (next-yr  (if (> next-mon 12) (1+ yr) yr))
                     (next-mon (if (> next-mon 12) 1 next-mon))
                     (fix-ver  (format "%d.%d" next-yr next-mon)))
                (format (concat "(Urgency = \"1 month\""
                                " OR fixVersion = \"%s\")"
                                " AND resolution = Unresolved"
                                " AND assignee = %s")
                        fix-ver my\gerrit-user)))
             (urgent-issues (my/gerrit--query-jira urgent-jql))
             ;; Build set of urgent Jira ticket IDs
             (urgent-ids
              (let ((ht (make-hash-table :test 'equal)))
                (dolist (issue urgent-issues)
                  (puthash (alist-get 'key issue) t ht))
                ht))
             ;; Mark Gerrit work-items as urgent if their Jira ticket
             ;; appears in the urgent query results
             (_ (dolist (it gerrit-items)
                  (when (and (work-item-jira it)
                             (gethash (work-item-jira it) urgent-ids))
                    (setf (work-item-urgent it) t))))
             ;; Tickets that are urgent but have no Gerrit work yet
             (covered-ids
              (let ((ht (make-hash-table :test 'equal)))
                (dolist (it gerrit-items)
                  (when (work-item-jira it)
                    (puthash (work-item-jira it) t ht)))
                ht))
             (todo-items
              (-map #'work-item-from-jira-issue
                    (-filter (lambda (issue)
                               (not (gethash (alist-get 'key issue)
                                             covered-ids)))
                             urgent-issues)))
             ;; All unresolved tickets assigned to me — catches items
             ;; with no Gerrit work and no urgency flag (e.g., recently
             ;; assigned, or tickets with discussion activity).
             ;; Request "status" so we can detect Jira-active anomalies.
             (_ (message "Gerrit/Jira: checking for assigned tickets…"))
             (assigned-jql
              (format (concat "assignee = %s"
                              " AND resolution = Unresolved"
                              " AND updated >= -14d")
                      my\gerrit-user))
             (assigned-issues
              (condition-case nil
                  (my/gerrit--query-jira assigned-jql "summary,status")
                (error nil)))
             ;; Also fetch titles for these tickets.
             (_ (let ((tix (mapcar (lambda (i) (alist-get 'key i))
                                   assigned-issues)))
                  (when tix (my/gerrit--fetch-jira-titles tix))))
             ;; IDs already covered by Gerrit items or urgent TODO items
             (all-covered-ids
              (let ((ht (copy-hash-table covered-ids)))
                (dolist (it todo-items)
                  (when (work-item-jira it)
                    (puthash (work-item-jira it) t ht)))
                ht))
             ;; Split uncovered issues: Jira says "In Progress" or
             ;; "In Review" but no Gerrit changes exist → suspicious
             ;; (e.g., accidentally used #progress instead of #resolve).
             (uncovered-issues
              (-filter (lambda (issue)
                         (not (gethash (alist-get 'key issue)
                                       all-covered-ids)))
                       assigned-issues))
             (jira-active-statuses '("In Progress" "In Review"
                                     "Code Review" "Pending Review"))
             (jira-active-items
              (-map (lambda (issue)
                      (work-item-create
                       :jira (alist-get 'key issue)
                       :title (or (alist-get 'summary
                                             (alist-get 'fields issue))
                                  "untitled")
                       :status 'jira-active))
                    (-filter
                     (lambda (issue)
                       (member (alist-get 'name
                                          (alist-get 'status
                                                     (alist-get 'fields issue)))
                               jira-active-statuses))
                     uncovered-issues)))
             (jira-active-ids
              (let ((ht (make-hash-table :test 'equal)))
                (dolist (it jira-active-items)
                  (puthash (work-item-jira it) t ht))
                ht))
             (assigned-items
              (-map (lambda (issue)
                      (work-item-create
                       :jira (alist-get 'key issue)
                       :title (or (alist-get 'summary
                                             (alist-get 'fields issue))
                                  "untitled")
                       :status 'assigned))
                    (-filter (lambda (issue)
                               (not (gethash (alist-get 'key issue)
                                             jira-active-ids)))
                             uncovered-issues)))
             ;; Combine all items
             (all-items (append todo-items jira-active-items
                                assigned-items gerrit-items))
             ;; Resolve Org-tree markers for agenda navigation
             (_ (work-item--resolve-org-trees all-items)))
        (setq my/agenda-work-data
              (list :items all-items
                    :timestamp (current-time))
              my/agenda-work-fetching nil)
        (message "Gerrit/Jira: cached %d attn, %d mine, %d work-items"
                 (length attention) (length all-mine)
                 (length all-items))
        (non-blocking-message-box
         :title "Standup"
         :content "Gerrit/Jira ready for your consumption!")
        ;; Refresh agenda if the buffer exists
        (when-let ((buf (get-buffer "*Org Agenda*")))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (org-agenda-redo-all))))
        ;; Fire one-shot callback (e.g. my/standup opening the agenda)
        (when my/agenda-work--on-complete
          (let ((cb my/agenda-work--on-complete))
            (setq my/agenda-work--on-complete nil)
            (funcall cb))))
    (error
     (setq my/agenda-work-fetching nil
           my/agenda-work--on-complete nil)
     (message "Gerrit/Jira agenda update failed: %s"
              (error-message-string err)))))
;; Coordinator:1 ends here

;; [[file:init.org::*Link activation][Link activation:1]]
(defun my/agenda--activate-org-links (start end)
  "Make [[url][desc]] links clickable while preserving raw markup.
The agenda buffer is not in `org-mode', so raw bracket links would
display as literal text.  This post-processes the region between
START and END, adding text properties to each [[url][desc]] so that:

- Visually, only DESC is shown (via the `display' property).
- Clicking or pressing RET opens URL via `browse-url'.
- Hovering shows the full URL as a tooltip.
- Copy/paste preserves the raw [[url][desc]] Org link syntax,
  so pasting into an Org buffer yields a working link."
  (save-excursion
    (goto-char start)
    (let ((map (make-sparse-keymap)))
      (define-key map [mouse-1]
        (lambda (e) (interactive "e")
          (browse-url (get-text-property (posn-point (event-start e)) 'my-url))))
      (define-key map (kbd "RET")
        (lambda () (interactive)
          (browse-url (get-text-property (point) 'my-url))))
      (while (re-search-forward
              "\\[\\[\\(.+?\\)\\]\\[\\(.+?\\)\\]\\]" end t)
        (let ((url  (match-string 1))
              (desc (match-string 2))
              (beg  (match-beginning 0))
              (fin  (match-end 0)))
          (put-text-property beg fin 'display desc)
          (put-text-property beg fin 'face 'link)
          (put-text-property beg fin 'mouse-face 'highlight)
          (put-text-property beg fin 'help-echo url)
          (put-text-property beg fin 'my-url url)
          (put-text-property beg fin 'keymap map))))))
;; Link activation:1 ends here

;; [[file:init.org::*Finalize hook + once-daily auto-fetch][Finalize hook + once-daily auto-fetch:1]]
(defun my/agenda--cache-stale-p ()
  "Return non-nil if the cached work data is from a previous calendar day.
A nil cache (never fetched) is also considered stale."
  (if (null my/agenda-work-data) t
    (let* ((cached-time (plist-get my/agenda-work-data :timestamp))
           (cached-day  (format-time-string "%Y-%m-%d" cached-time))
           (today       (format-time-string "%Y-%m-%d")))
      (not (string= cached-day today)))))

(defun work-item-insert-as-agenda-section (heading items)
  "Insert HEADING and a numbered list of rendered ITEMS into the agenda buffer.
Each item is rendered via `work-item-to-string'.  Each line carries a
`help-echo' text property (via `work-item-help-echo') so that hovering
or resting the cursor on any item shows a context-sensitive action hint
in the minibuffer --- turning the dashboard from a passive report into
an active one that tells you what to do next.

When an item has a non-nil `org-tree' marker, `org-marker' and
`org-hd-marker' text properties are set on the line so that standard
org-agenda commands (RET, TAB, I, o, etc.) work."
  (when items
    (insert "\n"
            (propertize heading 'face 'org-agenda-structure)
            "\n")
    (let ((n 0))
      (dolist (it items)
        (cl-incf n)
        (let ((line-start (point)))
          (insert (format "%d. %s\n" n (work-item-to-string it)))
          (let ((end (1- (point))))
            ;; Action hint: displayed in minibuffer via help-at-pt.
            (put-text-property line-start end
                               'help-echo (work-item-help-echo it))
            ;; When the item has an Org tree, make the line behave like
            ;; a real agenda entry.
            (when-let ((marker (work-item-org-tree it)))
              (when (marker-buffer marker)
                (put-text-property line-start end 'org-marker marker)
                (put-text-property line-start end
                                   'org-hd-marker marker)))))))))

(defun my/agenda-insert-work-sections ()
  "Insert cached `work-item' sections at the end of the daily agenda.
Sections by status: reviews-needed, my-needing-action,
please-review, wip, todo.  Urgent items are rendered with a
\"🔴 \" prefix (via `work-item-to-string') and appear in
whichever status section they belong to.

On first agenda open each calendar day, automatically triggers a
background fetch.  Subsequent opens reuse the cache.  To force a
manual refresh at any time, use the \"Refresh Work\" button or press
\\='g' in the agenda buffer."
  (when (string-prefix-p "*Org Agenda" (buffer-name))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (if my/agenda-work-data
          (let* ((start (point))
                 (items (plist-get my/agenda-work-data :items))
                 (status-sections
                  '((reviews-needed    "👀 Reviews Needed (I'm blocking someone)")
                    (my-needing-action "🔧 My Changes Needing Action")
                    (please-review     "⏳ Please Review My Work")
                    (wip               "🚧 Work In Progress")
                    (todo              "📋 Urgent Tasks Not Yet Started")
                    (assigned          "📌 Assigned To Me")
                    (jira-active       "⚠️ Jira Active But No Gerrit Changes"))))
            (dolist (sec status-sections)
              (let* ((status (car sec))
                     (heading (cadr sec))
                     (matching (-filter
                                (lambda (it)
                                  (eq (work-item-status it) status))
                                items))
                     (sorted (work-item--sort matching)))
                (work-item-insert-as-agenda-section heading sorted)))
            (my/agenda--activate-org-links start (point))
            ;; If the cache is from yesterday, silently refresh in the background
            (when (and (my/agenda--cache-stale-p)
                       (not my/agenda-work-fetching))
              (my/agenda-work-refresh)))
        ;; No cache yet — placeholder + trigger fetch
        (insert "\n"
                (propertize
                 "⏳ Loading Gerrit/Jira data… press 'g' to refresh"
                 'face 'org-agenda-structure)
                "\n")
        (unless my/agenda-work-fetching
          (my/agenda-work-refresh))))))

(add-hook 'org-agenda-finalize-hook #'my/agenda-insert-work-sections)

;; Display help-echo text properties in the minibuffer when the cursor
;; idles on a work-item line.  This is what makes the agenda an *active*
;; dashboard — each line tells you what to do, not just what exists.
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (setq-local help-at-pt-display-when-idle t)
            (help-at-pt-set-timer)))
;; Finalize hook + once-daily auto-fetch:1 ends here

;; [[file:init.org::*Standup command][Standup command:1]]
(defun my/standup ()
  "Begin the workday: clock into standup, refresh work data, open agenda.
On Mondays, gate on the weekly review — you don't get to start the week
without reflecting on the last one."
  (interactive)
  ;; 0. Monday gate: force weekly review before the week begins.
  ;;    The review is a capture buffer; when the user presses C-c C-c to
  ;;    finalise, they call my/standup again to proceed normally.
  (if (and (= (nth 6 (decode-time)) 1) ;; Monday
           (fboundp 'my/weekly-review-done-this-week-p)
           (not (my/weekly-review-done-this-week-p)))
      (progn
        (message "🔄 It's Monday! Let's start the week with a review.")
        (when (eq system-type 'darwin)
          (non-blocking-message-box
           :title "Weekly Review"
           :content "Seven days without a review makes one weak! Start your week by reflecting on the last one."
           :buttons '(:OK nil)))
        (call-interactively #'my/capture-🔄-weekly-review-😊))
    ;; else: Normal standup flow
    (progn
      ;; 1. Open my-life.org and navigate to "Daily Standup Planning"
      (find-file org-default-notes-file)
      (widen)
      (goto-char (point-min))
      (unless (re-search-forward
               "^\\*\\* .*Daily Standup Planning" nil t)
        (error "Could not find '** ... Daily Standup Planning' heading"))
      ;; 2. Clock in at the ** Daily Standup Planning heading
      (beginning-of-line)
      (org-clock-in)
      (org-narrow-to-subtree)
      ;; 3. Move past metadata to the first child heading
      (org-end-of-meta-data t)
      (unless (re-search-forward "^\\*\\*\\* " nil t)
        (error "No child headings found under Daily Standup Planning"))
      (goto-char (match-beginning 0))
      ;; 4. Prepend today's entry; save a marker for async insertion.
      (insert (format-time-string "*** <%Y-%m-%d %a>\n\n"))
      (let ((insertion-marker (point-marker))
            (is-monday (= (nth 6 (decode-time)) 1)))
        (insert "\n")
        ;; 5. Refresh work-item cache; open agenda when done.
        (setq my/agenda-work--on-complete
              (lambda ()
                (unwind-protect (my/org-agenda)
                  ;; On Mondays (after weekly review): insert please-review
                  ;; items into the standup entry so they're front-of-mind.
                  (when is-monday
                    (my/standup--insert-review-items insertion-marker))
                  (my/progress-pulse--maybe-remind))))
        (my/agenda-work-refresh)))))
;; Standup command:1 ends here

;; [[file:init.org::*Colleague Meeting Shortcuts][Colleague Meeting Shortcuts:1]]
(cl-loop for colleague in (and (boundp 'my\colleagues) my\colleagues)
         for function-name = (intern (format "meet-with-%s" colleague))
         for colleague-str = (symbol-name colleague)
         for first-letter = (substring colleague-str 0 1)
         do (eval `(bind-key ,(format "C-c m %s" first-letter)
                             (defun ,function-name () (interactive)
                               (find-file org-default-notes-file)
                               (widen)
                               (org-id-goto ,colleague-str)
                               (org-clock-in)
                               (org-narrow-to-subtree)))))
;; Colleague Meeting Shortcuts:1 ends here

;; [[file:init.org::*Show me the agenda when I've been idle for 10 minutes][Show me the agenda when I've been idle for 10 minutes:1]]
;; Stop this with:  (cancel-function-timers 'my/pop-up-agenda-timer)
(setq my/pop-up-agenda-timer (run-with-idle-timer (* 60 30) t 'my/org-agenda))
;; Show me the agenda when I've been idle for 10 minutes:1 ends here

;; [[file:init.org::*Get in-Emacs notifications of upcoming appointments by running (org-agenda-to-appt)][Get in-Emacs notifications of upcoming appointments by running (org-agenda-to-appt):1]]
(require 'appt)

(setq appt-display-duration 30) ;; Show reminder window for 30 seconds please

(setq appt-message-warning-time 12) ;; Show me a warning 12 minutes before an appointment

(setq appt-display-interval 3) ;; Display warning every 3 minutes

;; Ensure all of my Org entries are part of appt whenever it checks for an appointment
(advice-add 'appt-check :before (lambda (&rest args) (org-agenda-to-appt t)))
;; Alterantively: (add-hook 'org-finalize-agenda-hook #'org-agenda-to-appt)

;; `appt-activate' eagerly runs every minute, slow it down to once every 60 minutes
;; Also, don't do anything when I save a file (namely, `appt-update-list').
(advice-add 'appt-activate :after
            (lambda (&rest args)
              (remove-hook 'write-file-functions #'appt-update-list)
              (timer-set-time appt-timer (current-time) (* 60 60))))
;; Get in-Emacs notifications of upcoming appointments by running (org-agenda-to-appt):1 ends here

;; [[file:init.org::*Get in-Emacs notifications of upcoming appointments by running (org-agenda-to-appt)][Get in-Emacs notifications of upcoming appointments by running (org-agenda-to-appt):2]]
;; (org-show-notification "Attention! You have a meeting in 5 minutes!") ;; Example use.
;; (org-notify "This message has a sound" t) ;; Show pop-up and play a beep.

;; With this, I now see a pop-up notification every `appt-display-interval' minutes
(setq appt-disp-window-function
      (lambda (remaining new-time msg)
        (org-show-notification (format "⟪In %s minutes⟫ %s" remaining msg))))
;; Get in-Emacs notifications of upcoming appointments by running (org-agenda-to-appt):2 ends here

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
          ,(my/holiday-islamic 7 28 "💔🥀 Husayn ibn 'Alī started his journey to Karbalā from Madinah in 60 AH" "https://en.wikipedia.org/wiki/Husayn_ibn_Ali")

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
(use-package lf :defer t)
;; TODO: Add the line “(declare (indent defun))” right after the docstring of “lf-define”,
;; so that Emacs indents it like a “defun”.
;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/Indenting-Macros.html
;;
;; Until then, use the following incantation:
(with-eval-after-load 'lf
  (lf-define (get 'lf-define 'lisp-indent-function) 'defun))

;; The `lf' package references `lf-unindent' in the `lf-string' docstring
;; but never factored it out.  We define it here: find the shortest
;; leading whitespace across all non-blank lines *after* line 1 (which
;; starts right after the opening quote and so always has zero indent),
;; then strip that many columns from every line.
(with-eval-after-load 'lf
  (defun lf-unindent (str)
    "Strip common leading whitespace from every line of STR.
The indent level is the minimum leading whitespace among non-blank
lines after the first (line 1 abuts the opening quote, so its
indent is not representative)."
    (let* ((lines (s-lines str))
           (rest-non-blank (--filter (not (s-blank-p it)) (cdr lines)))
           (min-indent
            (if rest-non-blank
                (apply #'min (--map (length (car (s-match "^ *" it)))
                                    rest-non-blank))
              0)))
      (s-join "\n" (cons (car lines)
                         (--map (if (>= (length it) min-indent)
                                    (substring it min-indent)
                                  it)
                                (cdr lines)))))))

;; (MA: Eventually this function can itself become a small albeit useful MELPA ELisp Package ♥‿♥)
(with-eval-after-load 'lf
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
  "✔ Invoke `org-mode-restart` in existing Org buffers for this to take effect."))
;; Implementation:2 ends here

;; [[file:init.org::*Implementation][Implementation:3]]
;; Auto-load workflow states on first C-c C-t (org-todo) invocation:
;; We keep `lf' deferred, so the definition of `my/declare-workflow-states'
;; only exists after `lf' loads. This advice forces `lf' to load, declares
;; the states, restarts org-mode in the current buffer, then removes itself.
(defun my/ensure-workflow-states (&rest _)
  "Load `lf', declare workflow states, then self-destruct."
  (require 'lf)
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

   ("REFERENCE"  :foreground "purple2") ;; The task was to ditigize some info and keep it around

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
  (advice-remove 'org-todo #'my/ensure-workflow-states)
  ;; Refresh todo keyword data in all open Org buffers — lightweight
  ;; alternative to `org-mode-restart', which chokes on large files.
  (dolist (buf (org-buffer-list))
    (with-current-buffer buf
      (org-set-regexps-and-options))))
(advice-add 'org-todo :before #'my/ensure-workflow-states)
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
(eval-and-compile (require 's nil t)) ;; Needed for s-replace-regexp in def-capture macro
(declare-function s-replace-regexp "s" (regexp replacement s &optional literal))
(declare-function s-contains\? "s" (needle s &optional ignore-case))
(declare-function s-replace "s" (old new s))
(declare-function s-trim "s" (s))
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
  `(defun ,(intern (concat "my/capture-" (replace-regexp-in-string " " "-" (downcase name)))) (&optional prefix)
     (interactive "p")
     (let (;; Temporarily pause any clocking hooks
           (org-clock-in-hook nil)
           (org-clock-out-hook nil)
           (org-capture-templates
            ;; Capture mode now handles automatically clocking in and out of a capture task.
            ;; When I start a capture mode task the task is clocked in as specified by :clock-in t
            ;; and when the task is filed with C-c C-c the clock resumes on the original clocking task.
            `(("𝒞" ,,name entry (file+headline org-default-notes-file ,,location) ,,template :clock-in t :clock-resume t))))
       (org-capture (list prefix) "𝒞")
       (unless (> prefix 1) (rename-buffer ,name))
       ,@forms)))
(unless running-tests
  (eval '(bind-key* "C-c c" (def-capture "Inbox Entry 📩" "Inbox 📩 \t\t\t:inbox:" "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"))))
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

;; [[file:init.org::*Adding New *Tasks/Notes* Quickly Without Disturbing The Current Task Content][Adding New *Tasks/Notes* Quickly Without Disturbing The Current Task Content:3]]
(add-hook 'org-mode-hook '(lambda ()
   (local-set-key (kbd "<return>") 'org-return-indent)) ;; Newline with indentation
   (local-set-key (kbd "C-M-<return>") 'electric-indent-just-newline)) ;; Newline without indentation
;; Adding New *Tasks/Notes* Quickly Without Disturbing The Current Task Content:3 ends here

;; [[file:init.org::*Hide blank lines /between/ headlines][Hide blank lines /between/ headlines:1]]
(setq org-cycle-separator-lines 0)

(setq org-blank-before-new-entry '((heading) (plain-list-item . auto)))
;; Hide blank lines /between/ headlines:1 ends here

;; [[file:init.org::#drag-and-drop-images-into-org-mode][“Smart Paste”: Drag and Drop Images/(Any File!) into Org-Mode:1]]
(add-hook
 'org-mode-hook
 (cl-defun my/setup-smart-paste ()
   (interactive)
   (require 'cl)
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
                                    (format "pandoc -f html -t org <<%s\n%s\n%s" delimiter
                                            ;; Often at work people use backticks as code markers, so transform that to
                                            ;; ~org syntax~. This also avoids breaking the shell call, since backquote means
                                            ;; inline-eval in shell; e.g., echo `echo hello`world ⇒ helloworld
                                            ;; likewise:  echo `pwd`world ⇒ ⟨current directory⟩world
                                         (s-replace "`" "~"  (s-replace "$" "\\$" contents))
                                            delimiter)))))))

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
(use-package org-web-tools :defer t)
  ;; :config
  ;; Insert an Org-mode link to the URL in the clipboard or kill-ring. Downloads
  ;; the page to get the HTML title.
  ;; (bind-key* "C-c C-l" #'org-web-tools-insert-link-for-url) ;; Instead, see my/org-insert-link-dwim below.
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
(cl-loop for (x y z) in '(("+" "◦" "*")
                       ("-" "•" "*")
                       ("*" "⋆" "+"))
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
;; Note: org-mode-restart only works in org buffers, not at init time
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
  :hook org-mode
  :custom ((org-appear-autoemphasis  t)
           (org-appear-autolinks nil)
           (org-appear-autosubmarkers nil)))
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
(use-package org-remoteimg
  :vc (:url "https://github.com/gaoDean/org-remoteimg")
  :defer t
  :init
  ;; These are plain global vars — no package load needed.
  (setq url-cache-directory "~/emacs.d/.cache/")
  (setq url-automatic-caching t)
  (setq org-display-remote-inline-images 'cache))
;; Now C-c C-x C-v shows remote images inline, neato!:1 ends here

;; [[file:init.org::#Draw-pretty-unicode-tables-in-org-mode][Draw pretty unicode tables in org-mode:1]]
(use-package org-pretty-table
  :vc (:url "https://github.com/Fuco1/org-pretty-table")
  :hook org-mode)
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


(cl-defun my/org-links-stash (text)
  "Replace Org links in TEXT with opaque \\x01LINKn\\x01 tokens.
Returns (SANITISED-TEXT . ALIST) where each ALIST entry maps a
token to its Slack mrkdwn replacement:
  [[url][desc]] --> [desc](url)   (described link)
  [[url]]       --> url           (bare link)
The tokens are inert under every subsequent regex pass (italics,
bold, code) because \\x01 never appears in natural Org text.
Use `my/org-links-restore' to rehydrate the tokens afterwards."
  (let ((links '()) (counter 0))
    (while (string-match
            "\\[\\[\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" text)
      (cl-incf counter)
      (let ((token (format "\x01LINK%d\x01" counter)))
        (push (cons token
                    (format "[%s](%s)"
                            (match-string 2 text)
                            (match-string 1 text)))
              links)
        (setq text (replace-match token t t text))))
    (while (string-match "\\[\\[\\([^]]+\\)\\]\\]" text)
      (cl-incf counter)
      (let ((token (format "\x01LINK%d\x01" counter)))
        (push (cons token (match-string 1 text)) links)
        (setq text (replace-match token t t text))))
    (cons text links)))

(cl-defun my/org-links-restore (links text)
  "Replace opaque tokens in TEXT with their Slack mrkdwn forms.
LINKS is the alist returned by `my/org-links-stash' -- each
entry is (TOKEN . REPLACEMENT).  FIXEDCASE is t so Emacs does
not apply its case-adjustment heuristics on the replacement.
Argument order is (LINKS TEXT) so the function threads naturally
with `->>': the text flows in as the last argument."
  (cl-reduce (lambda (s pair)
               (replace-regexp-in-string
                (regexp-quote (car pair)) (cdr pair) s t t))
             links :initial-value text))

(cl-defun my/replace-names-with-slack-mentions (text)
  "Replace full names in TEXT with Slack @-mentions per `my\\slack-name-map'.
Falls back to TEXT unchanged when the map is unset (no private.el)."
  (if (boundp 'my\slack-name-map)
      (cl-reduce (lambda (s pair)
                   (replace-regexp-in-string
                    (regexp-quote (car pair)) (cdr pair) s t t))
                 my\slack-name-map :initial-value text)
    text))

(cl-defun my/copy-as-slack (&optional text)
  "Convert TEXT (or the region, or the buffer) from Org markup to Slack mrkdwn and copy it.
No external dependency -- handles the subset of Org that appears in
standup messages: inline markup, links, and lists.
Full names (from Gerrit) are replaced with @-names per
`my\\slack-name-map' (set in private.el)."
  (interactive)
  (let* ((text (or text
                   (buffer-substring-no-properties
                    (if (use-region-p) (region-beginning) (point-min))
                    (if (use-region-p) (region-end) (point-max))))))
    (-let [(text . links) (my/org-links-stash text)]
      (kill-new
       (->> text
            ;; Code: ~code~ or =code= -> `code`
            (replace-regexp-in-string
             "[~=]\\([^ ~=\n][^~=\n]*?\\)[~=]" "`\\1`")
            ;; Bold: *bold* -> protect from italics pass
            (replace-regexp-in-string
             "\\*\\([^ *\n][^*\n]*?\\)\\*" "\x03\\1\x03")
            ;; Italics: /italics/ -> _italics_
            (replace-regexp-in-string
             "/\\([^ /\n][^/\n]*?\\)/" "_\\1_")
            ;; Restore bold markers
            (replace-regexp-in-string "\x03" "*")
            ;; Restore stashed links
            (my/org-links-restore links)
            ;; Normalise ordered lists: "1) " -> "1. "
            (replace-regexp-in-string
             "^\\( *\\)\\([0-9]+\\)) " "\\1\\2. ")
            ;; Strip Org heading stars
            (replace-regexp-in-string "^\\*+ " "")
            ;; Full names -> Slack @names
            (my/replace-names-with-slack-mentions)))
      (message "Copied as Slack Markdown! In Slack press “⌘ Shift F” to apply the formatting."))))


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
(use-package helm-org-rifle :defer t)
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
(😴 -let [wimpy-del.el "~/.emacs.d/elpa/wimpy-del.el"]
    (unless (f-exists? wimpy-del.el)
      (url-copy-file "https://www.emacswiki.org/emacs/download/wimpy-del.el" wimpy-del.el))
    (load-file wimpy-del.el)
    (setq wimpy-delete-size 3000)
    (bind-key* "C-w" #'kill-region-wimpy))

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

;; Also, please avoid doing a bunch of work on every redisplay cycle for right-to-left languages!
;; So don't do the Bidirectional Parentheses Algorithm (“bpa”) to make redisplay faster!
(setq bidi-inhibit-bpa t)
(setq-default bidi-display-reordering 'left-to-right)
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
;; Idempotent install checks — flyspell is lazy-loaded via hooks,
;; so aspell/wordnet are never needed at startup.
(😴 system-packages-ensure "aspell")
(😴 system-packages-ensure "wordnet")
;; Fix spelling as you type ---thesaurus & dictionary too!:1 ends here

;; [[file:init.org::#Fix-spelling-as-you-type-thesaurus-dictionary-too][Fix spelling as you type ---thesaurus & dictionary too!:2]]
(use-package flyspell

  :hook ((prog-mode . flyspell-prog-mode)
         ((org-mode text-mode) . flyspell-mode)))
;; Fix spelling as you type ---thesaurus & dictionary too!:2 ends here

;; [[file:init.org::#Fix-spelling-as-you-type-thesaurus-dictionary-too][Fix spelling as you type ---thesaurus & dictionary too!:3]]
;; Defer ispell setup — the `which aspell` subprocess is ~50ms and
;; flyspell only activates on text/org/prog-mode hooks, never at startup.
(with-eval-after-load 'ispell
  (setq ispell-program-name (s-trim (shell-command-to-string "which aspell")))
  (setq ispell-dictionary "en_GB"))
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
(with-eval-after-load 'ispell
  (setq ispell-silently-savep t))
;; Fix spelling as you type ---thesaurus & dictionary too!:7 ends here

;; [[file:init.org::#Fix-spelling-as-you-type-thesaurus-dictionary-too][Fix spelling as you type ---thesaurus & dictionary too!:8]]
(with-eval-after-load 'ispell
  (setq ispell-personal-dictionary "~/.emacs.d/.aspell.en.pws"))
;; Fix spelling as you type ---thesaurus & dictionary too!:8 ends here

;; [[file:init.org::#Fix-spelling-as-you-type-thesaurus-dictionary-too][Fix spelling as you type ---thesaurus & dictionary too!:9]]
(add-hook          'c-mode-hook 'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
;; Fix spelling as you type ---thesaurus & dictionary too!:9 ends here

;; [[file:init.org::#Fix-spelling-as-you-type-thesaurus-dictionary-too][Fix spelling as you type ---thesaurus & dictionary too!:10]]
(use-package synosaurus
  :custom (synosaurus-choose-method 'popup)
  :bind* ("M-#" . synosaurus-choose-and-replace))
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
  :hook (text-mode org-mode) ;; Load this whenver I'm composing prose.
  :config (cons writegood-weasel-words
                '("it turns out that" ;; How does it turn out so? ;; ↯ What is the evidence of highighted phrase? ↯
                  "may have"
                  "experience shows"
                  "clearly"
                  "probably"
                  "easily"
                  "often"
                  "easy"
                  "simply"
                  "simple"
                  "some"
                  "many"
                  "various"
                  "very"
                  "fairly"
                  "several"
                  "extremely"
                  "exceedingly"
                  "quite"
                  "remarkably"
                  "few"
                  "surprisingly"
                  "mostly"
                  "largely"
                  "huge"
                  "tiny"
                  "are a number"
                  "is a number"
                  "excellent"
                  "interestingly"
                  "significantly"
                  "substantially"
                  "clearly"
                  "vast"
                  "relatively"
                  "completely"
                  "literally"
                  "not rocket science"
                  "outside the box"
                  "some"
                  "simple"
                  "simply"
                  "easy"
                  "often"
                  "easily"
                  "probably"
                  "clearly"               ;; Is the premise undeniably true?
                  "experience shows"      ;; Whose? What kind? How does it do so?
                  "may have")))           ;; It may also have not!
;; Lightweight Prose Proofchecking:1 ends here

;; [[file:init.org::#Placeholder-Text-For-Learning-Experimenting][Placeholder Text ---For Learning & Experimenting:1]]
(use-package lorem-ipsum :defer t)
;; Placeholder Text ---For Learning & Experimenting:1 ends here

;; [[file:init.org::#Some-text-to-make-us-smile][Some text to make us smile:1]]
(😴 use-package dad-joke
  :config (defun dad-joke () (interactive) (insert (dad-joke-get))))
;; Some text to make us smile:1 ends here

;; [[file:init.org::#Unicode-Input-via-Agda-Input][Unicode Input via Agda Input:1]]
;; (load (shell-command-to-string "agda-mode locate"))
;;
;; Seeing: One way to avoid seeing this warning is to make sure that agda2-include-dirs is not bound.
; (makunbound 'agda2-include-dirs)
;; Unicode Input via Agda Input:1 ends here

;; [[file:init.org::#Unicode-Input-via-Agda-Input][Unicode Input via Agda Input:5]]
;; TODO: Maybe don't bother installing Agda, and just get agda-input.el
;; from: https://github.com/agda/agda/blob/master/src/data/emacs-mode/agda-input.el
;; then loading that!
;; Defer agda-input loading — the load-file + quail/latin-ltx cascade
;; costs ~150ms.  The input method is only needed once we're actually
;; typing in a buffer, so we load the file lazily via the hook.
(-let [agda-input.el "~/.emacs.d/elpa/agda-input.el"]
  (unless (f-exists? agda-input.el)
    (url-copy-file "https://raw.githubusercontent.com/agda/agda/master/src/data/emacs-mode/agda-input.el" agda-input.el)))

;; MA: This results in "Package cl is deprecated" !?
(unless running-tests
  (setq default-input-method "Agda")
  (defun my/setup-agda-input ()
    "Load agda-input on first text/prog buffer, then activate it."
    (unless (featurep 'agda-input)
      (load-file "~/.emacs.d/elpa/agda-input.el"))
    (set-input-method "Agda"))
  (add-hook 'text-mode-hook #'my/setup-agda-input)
  (add-hook 'prog-mode-hook #'my/setup-agda-input))
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
(with-eval-after-load 'agda-input
  (add-to-list 'agda-input-user-translations '("set" "𝒮ℯ𝓉")))
;; Unicode Input via Agda Input:7 ends here

;; [[file:init.org::#Unicode-Input-via-Agda-Input][Unicode Input via Agda Input:8]]
(with-eval-after-load 'agda-input
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
(with-eval-after-load 'agda-input
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
(with-eval-after-load 'agda-input (agda-input-setup))
;; Unicode Input via Agda Input:10 ends here

;; [[file:init.org::#Increase-decrease-text-size][Increase/decrease text size:1]]
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
;; C-x C-0 restores the default font size
;; Increase/decrease text size:1 ends here

;; [[file:init.org::#Moving-Text-Around][Moving Text Around:1]]
;; M-↑,↓ moves line, or marked region; prefix is how many lines.
(use-package move-text
    :hook (after-init . move-text-default-bindings))
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
(use-package visual-regexp
  :defer t
  :bind ("M-%" . (lambda (&optional prefix)
                   "C-u M-% to do regexp replace, without querying."
                   (interactive "P")
                   (call-interactively (if prefix #'vr/replace #'vr/query-replace)))))
;; visual-regexp:1 ends here

;; [[file:init.org::#HTML-Org-mode][HTML ⇐ Org-mode:1]]
(use-package htmlize :defer t)
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

;; [[file:init.org::*Perfect-Margin: A clean writing environment][Perfect-Margin: A clean writing environment:1]]
(use-package perfect-margin
  :hook (after-init . perfect-margin-mode)
  :init
  (setq perfect-margin-visible-width 128)
  (setq perfect-margin-ignore-filters nil)
  (setq perfect-margin-ignore-regexps nil))

;; (use-package olivetti)
;; ;; NOTE: “setq-default” sets this buffer-local variable in all buffers
;; (setq-default olivetti-body-width 100)
;;
;; Olivetti is buffer-specific, but I want to be enabled for all buffers.
;; As such, I'm using perfect-margin.el.
;; Perfect-Margin: A clean writing environment:1 ends here

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
;; org-appear is already configured via use-package :hook org-mode above.
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
                               (light
                                (list "#F6B1C3" "#FFFF9D" "#BEEB9F" "#ADD5F7"))
                               (dark
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
            (perfect-margin-mode -1)))
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

;; On massive buffers (e.g., my-life.org at 145k lines / 7 MB),
;; org-superstar--fontify-buffer calls font-lock-ensure on the *entire*
;; widened buffer — bypassing jit-lock's lazy fontification.  The
;; combined font-lock regexps (nested groups + \\**?) overflow the
;; C-level regexp matcher stack.  On large buffers we call only
;; font-lock-flush (cheap: marks the buffer dirty) so jit-lock
;; re-fontifies the visible window lazily — skipping font-lock-ensure
;; (expensive: fontifies the entire widened buffer).
;;
;; If I do a ⟨Tab⟩ on an org heading, then that heading's content becomes visible and so JIT fontlocking
;; activites and so I get nice Org stars for that heading and its children! Lovely!
(define-advice org-superstar--fontify-buffer
    (:around (orig-fn &rest args) skip-large-buffers)
  "Skip eager fontification on buffers larger than 1 MB."
  (if (> (buffer-size) (* 1024 1024))
      (when font-lock-mode (font-lock-flush))
    (apply orig-fn args)))
;; Org-superstar:1 ends here

(😴 progn ;; Defer: Only needed when a review is performed (C-c r w/d/m).

(require 'eieio)

;; See https://alhassy.com/ElispCheatSheet/#org71dcb45 for info on “defstruct”
(cl-defstruct my/option
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

😲 ⟨Org Property⟩ names need not be unique:
If there are multiple entries sharing the same Org Property name,
then only one of them is randomly selected in the questionnaire.

😲 Moreover, ⟨Org Property⟩ names may be space-separated strings:
Spaces are replaced with a visual space “␣”. It seems Org Property
syntax allows some Unicode to appear as part of the name; but not
the Unicode space “ ”. For example, the following runs with no problem:

   (org-set-property  \"look-ma-∀␣p∈People␣•␣∃q␣•␣q␣𝑳𝒪𝓋𝐸𝔖␣p\" \"😲\")

")


;; NOTE: Consider using an Org file as a data source.
(setq my/daily-review-questionnaire
      '(
        ;; TODO: Make Weekly review get trend insights
        ;; ⇒ “Average Stress this week: +0.5”
        ;; ⇒ “Exercise streak: 3/5 days”
        ;; ⇒ Notes on emotional patterns?
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
         " 0  --  High            --  People are getting on my nerves. ⚠️ Stress ≡ Pot Belly!"
         "-1  --  Extremely High  --  I have so much to do; I'm freaking out!")
        ("Energy: How high is my capacity to do work? To be around others? Around myself?"
         "-1 --  Abysmal Low / Drained / Lacking Motivation  --  I need coffee and sleep."
         "0  --  Low / Sluggish                              --  I need coffee"
         "1  --  Medium / Calm                               --  I'm chill, doing my thing."
         "2  --  High / Enthusiastic                         --  I'm king of the world!")
        ("Energy Window: When was I most focused or energised?"
         "0  --  Morning   --  6am-12pm"
         "0  --  Afternoon --  12pm-3pm")
        ("Hours Slept: How was my sleep last night?"
         "0  --  I slept                                     --  Man, I need to get my life together! ⚠️ Poor Sleep ≡ Pot Belly!"
         "1  --  I slept before midnight and awoke at ~7am   --  Good, but I can do better!"
         "2  --  I slept around 10pm and awoke at ~5am       --  Nice! Living the best life! Getting things done!" )
        ("How I Slept: How did I fall asleep last night?"
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
         "-3  --   Three  --  Man, I need to get my life together!")
        ;; Exercise questions. TODO: Make Weekly Review remind me to increment exercise duration
        ("Water: Did I drink an entire bottle of water today?"
         "-1  --  Nope! Coffee, sugar, pop!     --  Fatigue! Low Energy! Headaches! Dry Mouth! Mood swings!"
         "1   --  Yup                           --  Optimal brain! Mood stability! Support weight management!")
        ("Running: Did I go for a  ﴾7-minute﴿  run today?"
         "-1  --  Nope    --  Why!? Don't you want to love your body! 💢 Be better, man!"
         "1   --  Yup     --  Nice! Lose the pot belly! 🫖")
        ("Push ups: Did I do  ﴾12﴿  push-ups today?"
         "-1  --  Nope    --  Why!? Don't you want to love your body! 💢 Be better, man!"
         "1   --  Yup     --  Nice! Lose the pot belly! 🫖")
        ("Jump Rope: Did I do  ﴾30﴿  skips today?"
         "-1  --  Nope    --  Why!? Don't you want to love your body! 💢 Be better, man!"
         "1   --  Yup     --  Nice! Lose the pot belly! 🫖")
        ("Bicep Curls: Did I do  ﴾10﴿  curls today?"
         "-1  --  Nope    --  Why!? Don't you want to love your body! 💢 Be better, man!"
         "1   --  Yup     --  Nice! Lose the pot belly! 🫖")
        ;; In due time: HIIT via Youtube; e.g., Crunches, Mountain Climbers, Russian Twists?
        ;; Also: Cycling? Karate Kata? Full Strength Training?
        ;; Did I stay within my calorie range?
        ;; Did I eat balanced meals (protein, fiber, healthy fats)?
        ;; Did I snack out of hunger, boredom, or habit?
        ;; +Did I emotionally eat today? If so, why?+
        ;;
        ;; /Consistency beats perfection!/
        ;; ("Calories: Did I stay within my calorie range?"
        ;;  "-1  --  Overeating / Snacking Mindlessly"
        ;;  " 0  --  Ate more than planned"
        ;;  " 1  --  Stayed within limits!"
        ;;  " 2  --  Tracked & ate healthy meals!")
        ("Nutrition: Did I eat balanced meals (protein, fiber, healthy fats)?"
         "-1  --  No idea / Ate junk -- Meal prep reduces unhealthy food reliance."
         " 0  --  Kinda tried -- ? "
         " 1  --  Well-balanced meals! -- ? ")
        ;; ("Snacking: Did I snack out of hunger, boredom, or habit?"
        ;;  "-1  --  Boredom / Emotion"
        ;;  " 0  --  Minor grazing"
        ;;  " 1  --  No snacking or only fruit/nuts!")
        ;; ("WaistMeasurement: What's my current wasit measure in cm?")
        ;; ("Fullness: Did I stop eating when satisfied?"
        ;;  "-1 -- Ate until overstuffed"
        ;;  " 0  -- Ate a bit much"
        ;;  " 1  -- Ate to satisfaction")

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Open ended questions (i.e., no options)                                  ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ("Relaxation: The most relaxing thing I did was …")
        ;; When did I feel drained, frustrated, or stressed?
        ("Motivation: Why was I or wasn't motivated for something today?")
        ("Tomorrow: What’s the one thing I must do tomorrow to feel accomplished?")



        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        :random  ;; 2 questions randomly chosen and asked each day                  ;;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; “Thematic” prompts: Each property acts as a theme.

        ("Time: Am I happy with how I am spending my time?") ;; Did I use most of my time wisely?
        ("Time: Where did most of my time go today?")
        ("Time: Did I control my schedule, or did my schedule control me?")

        ("Closure:  What can I let go of today?")
        ("Closure: What am I carrying into tomorrow?")

        ;; NOTE: These all use the “same” key, so only one of them will randomly be selected.
        ("Service: Was there anything I could easily do for someone else that I didn't do? Why not?")
        ("Service: Have I done anything to help someone in any way? Because a life lived only for oneself is only partly fulfilling.")
        ("Service: Did I wrong anyone who I owe amends to?")

        ("Values: What things are most important to me? (both things achieved and not yet achieved)")
        ("Values: Did it (whatever thing happened during the day) matter? To identify recurring things that either need to be dropped or addressed to better facilitate mental health.")
        ("Values: Did today matter? i.e., if I had slept all day, would anything really be any different?")

        ("Goal Getting: What did I do today to help achieve the things I have not yet achieved?")
        ("Goal Getting: What will I do tomorrow to further my achievement of things most important to me?")
        ("Goal Getting: Was I working on what truly matters, or just staying busy?")

        ;; ⇒ more positive thoughts, unlocked :)
        ("Gratitude: What am I grateful for today?") ;;  Makes you look at the big picture while appreciating something small that may be otherwise taken for granted.
        ("Anxiety: What problem is still on your mind, and what needs to be true for you to feel that this problem is resolved?")
        ("Worry: What am I worried about?") ;; Really helps clarify what to prioritize the next day, and gets the worries out of my head and onto paper right so I don't have to think about them in bed.

        ("Approval: What did I do today that I approve of?") ;; gets you out of all or nothing thinking. Do you approve of getting out of bed? Drinking water?
        ("Approval: What did I do well today? What did I do poorly today? What am I most grateful for? What is my goal?")
        ("Pride: What am I proud of?")

        ("Fall Short: Where did I fall short?")
        ("Distraction: What distracted me or slowed me down?")
        ("Procrastination: Was I procrastinating? Why?")
        ("Change: What is 1 thing I will do differently tomorrow?")
        ("Betterment: What can I do to be better tomorrow than I was today?")
        ("Improvement: How can anything I'm doing be improved upon? So that I can grow as a person and have more effectiveness in things I do.")

        ("Direction: Where am i going? What did i learn? What did i do that i liked? What can i do better?")
        ("Growth: What did I learn today?")
        ("Growth: Who do I need to be in order to master the day I had today. And how can I challenge myself to be that tomorrow.") ;; It helps with perspective, integrity and accountability.

        ("Annoyance: Of the things that happened to me today, what made me go “what the fuck?”")
        ("Joy: Of the things that happened to me today, what made me go “fuck yeah!”")
        ("Stress: Of the things that happened to me today, what made me go “oh fuck!”")

        ("Discomfort: What’s the most uncomfortable thing you encountered today?")
        ("Authenticity: What’s the most uncomfortable truth you said out loud today?")
        ("Inauthenticity: What did you mentally push aside today instead of thinking it through and openly saying your conclusions?")
        ("Rumination: What did you think about most today?")

        ("Competence:  What problems did I solve?")

        ("Self-Care: Did I put myself last today?") ;; Did I do myself justice today? If not, what will I do differently tomorrow?
        ))



(defun my/randomize (list)
  "Shuffle the elements in LIST."
  (--sort (< (random 2) 1) list))


(when nil
  ;; Sometimes I'll get the first "Approval" (an open-ended question) and other times I'll get the second one "Approval" (a choice-delimited question) 😁
  (my/read-daily-review-properties :questionnaire
                                   '(("Approval: What did I do today that I approve of?")
                                     ("Pride: What am I proud of?")
                                     ("Approval: What did I do well today? What did I do poorly today? What am I most grateful for? What is my goal?")
                                     ("Approval: Did I eat balanced meals (protein, fiber, healthy fats)?"
                                      "0  --  Nope -- Pro tip: Meal prep reduces unhealthy food reliance."
                                      "1  --  Yup  -- Yay, good job! ")
                                     ("Fall short: Where did I fall short?"))
                                   ;; :prop-val-action (lambda (property value) (progn (org-set-property property value) (save-buffer)))
                                   )

  )


;; DONE: Make this take 2 keyword args:
;; ⇒ :questions
;; ⇒ :action, a function to run on each property-value pair; e.g., (progn (org-set-property property value) (save-buffer))
;;            This is useful since then the Daily Review immediately adds a property whenever I've answered it, not at the very end.
;;            And the (save-buffer) part saves everything in-case a disaster occurs (eg one of my Lisp methods crashes).
;;            ↑ Make this the default, then I can change it when I write a test against this method.
(cl-defun my/read-daily-review-properties (&key (questionnaire my/daily-review-questionnaire)
                                                (prop-val-action #'cons))
  "Returns a list of (PROPERTY . VALUE) pairs that could be `org-set-property' on a headline.

Makes use of `my/daily-review-questionnaire'.

PROP-VAL-ACTION is run on each property-value pair, including the “Daily␣score”.
The list of such results is returned from this method.

At any time, press `C-.' to toggle adding a customised explanatory note to go along with a selection."
  (-let [(mandatory-questions random-questions) (-split-on :random questionnaire)]
    (let* ((max-possible-score 0)
           (daily-score 0)
           (properties
            (cl-loop
             ;; questionnaire may use the same property name for different questions, so avoid
             ;; prompting user for the same property value, since later ones override earlier
             ;; ones when added to an Org Heading. Note that this questions are chosen randomly
             ;; we do not always select the first “property: prompt” pair  in the questionnaire
             ;; when there are multiple such pairs having the same property name.
             with seen-properties = '()
             ;; Consider mandatory questions and 2 optional questions, chosen at random
             for (heading . option-strings) in (-concat
                                                (my/randomize mandatory-questions)
                                                (-take 2 (my/randomize random-questions)))
             for heading-info = (s-split ":" heading)
             ;; Org Properties cannot use Unicode space “ ” in their names, but it seems
             ;; that they can use the visual space “␣” in their name (among other Unicode).
             ;; E.g., "how i slept amigo" ⇒ "how␣i␣slept␣amigo"
             ;; NOTE: Org-ql with “ (regexp "\n.*amigo") ” will find such an entry.
             for property = (s-replace " " "␣" (cl-first heading-info))
             for prompt₀ = (cl-second heading-info)
             for prompt = (if (s-ends-with? " " prompt₀) prompt₀ (concat prompt₀ " "))
             for options = (--map (make-my/option-from-string it) option-strings)
             for is-open-ended? = (null options)
             unless (member property seen-properties)
             collect
             (funcall prop-val-action
                      (progn (push property seen-properties) property)
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
                          (setq my/show_C-._message t)
                          (minibuffer-with-setup-hook
                              (lambda () (use-local-map (copy-keymap my/note-map)))
                            (consult--read (--map (my/option-label it) options)
                                           :prompt prompt
                                           :require-match t
                                           :annotate (lambda (label)
                                                       (format "\t ⟨ %s ⟩" (my/option-description (assoc-by-label options label))))
                                           ;; Initial message shown in minibuffer: This is a message, not default input string.
                                           :state (lambda (action candidate)
                                                    (when my/show_C-._message
                                                      ;; Note: Did not work as expected: (equal action 'setup)
                                                      (message (concat
                                                                (propertize "Press" 'face `(bold (foreground-color . "grey")))
                                                                (propertize " C-. " 'face '(bold (foreground-color . "maroon")))
                                                                (propertize "to toggle entering a note after making a selection" 'face `(bold (foreground-color . "grey")))))))
                                           :lookup (lambda (label _ _ _)
                                                     (-let [option (assoc-by-label options label)]
                                                       (cl-incf daily-score (my/option-score option))
                                                       ;; If label ends in “…” or “C-.” pressed, prompt for a note.
                                                       (when (or (s-ends-with? "…" label) note-has-been-requested)
                                                         (setq my/show_C-._message nil)
                                                         (-let [note (s-trim (read-from-minibuffer "Enter an explanatory note [ENTER to skip] "))]
                                                           (unless (s-blank? note)
                                                             (setf (my/option-description option) note))))
                                                       (pretty-print option)))))))))))
      ;; Prepend a computed “daily score” property. Hopefully this value increases with time.
      (cons
       (funcall prop-val-action "Daily␣score"
                (thread-last daily-score
                             float
                             ;; Note:  (thread-last x (/ max) (/ 100)) = (/ 100 (/ max x)) = (* 100 (/ x max))
                             (/ max-possible-score)
                             (/ 100)
                             (format "%.2f%%")))
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
      (execute-kbd-macro (kbd "C-u M-x align-regexp RET \\(\\s-*\\)-- RET RET RET y"))

      (my/org-sort-properties))))

(defun my/org-sort-properties ()
  "To be used in my ‘review’ captures."
  (beginning-of-buffer)
  (search-forward ":CREATED:")
  (forward-line)
  (-let [start (point)]
    (search-forward ":END:")
    (beginning-of-line)
    (sort-lines nil start (point))))

) ;; End 😴 — deferred questionnaire infrastructure

;; ═══════════════════════════════════════════════════════════════════════
;; Weekly Review — Progressive Phases
;; ═══════════════════════════════════════════════════════════════════════
;;
;; Phase 1 (reviews 0–3): Starter — clock report, 3 reflections, L5
;;                         check, archive nudge. Just enough to build
;;                         the habit.
;; Phase 2 (reviews 4–7): Add life scores (Social, Faith, Marriage,
;;                         Health) and a random reflection question.
;; Phase 3 (reviews 8+):  Full template with forward planning,
;;                         commit steps, and daily score table.
;;
;; The counter persists via savehist so it survives restarts.

(defvar my/weekly-review-count 0
  "How many weekly reviews we’ve completed. Drives progressive unlocks.")

(defvar my/weekly-review-last-week nil
  "ISO week number of the last completed review, to avoid double-counting.")

;; Persist the counter across Emacs restarts.
(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'my/weekly-review-count)
  (add-to-list 'savehist-additional-variables 'my/weekly-review-last-week))

(defun my/weekly-review-phase ()
  "Return the current review phase (1, 2, or 3)."
  (cond ((< my/weekly-review-count 4) 1)
        ((< my/weekly-review-count 8) 2)
        (t 3)))

(defun my/weekly-review-done-this-week-p ()
  "Non-nil if a weekly review was already completed this ISO week."
  (equal my/weekly-review-last-week (ts-week-of-year (ts-now))))

(defun my/weekly-review--increment-counter ()
  "Bump the review counter and record the current week.
Also, at phase boundaries, nudge the user to unlock more."
  (let ((week♯ (ts-week-of-year (ts-now))))
    (unless (equal my/weekly-review-last-week week♯)
      (cl-incf my/weekly-review-count)
      (setq my/weekly-review-last-week week♯)
      ;; Phase-boundary nudges
      (when (eq system-type 'darwin)
        (cond
         ((= my/weekly-review-count 4)
          (non-blocking-message-box
           :title "Weekly Review"
           :content "You’ve completed 4 reviews — nice streak! Phase 2 unlocked: life scores (Social, Faith, Marriage, Health) will appear next week."
           :buttons '(:Sweet! nil)))
         ((= my/weekly-review-count 8)
          (non-blocking-message-box
           :title "Weekly Review"
           :content "8 reviews done! Phase 3 unlocked: the full review template with forward planning, commit steps, and daily score trends. You’ve built the habit."
           :buttons '(:Let’s\ go! nil))))))))


(😴 progn ;; Defer the keybinding and capture template — not needed at startup.

    ;; =======================================================================
    ;; Weekly Review -- extracted helper functions
    ;;
    ;; Each `my/weekly-review--insert-*' inserts its section at point.
    ;; Extracting them from the capture template keeps `def-capture' readable.
    ;; =======================================================================

    (defun my/weekly-review--heading () ;; From Chaos to Clarity!
      "Return the Org capture template string for this week's review entry.
This is used as the TEMPLATE argument to `def-capture', so it must
return a string — not insert anything.  `org-capture' expands %T
into an active timestamp."
      (format (concat "* Weekly Review & Weekly Resolution ♯%s :%s:Weekly:Review:\n"
                      ":PROPERTIES:\n"
                      ":CREATED: %%T\n"
                      ":WHY_PROMISE: A progress report is a promise report. Your word is on the line.\n"
                      ":WHY_REVIEW: Ensure everything is on track! Be proactive, not reactive!"
                      " Be in control of my life! 😌 Have a sense of closure for last week,"
                      " to start this week on a positive note! ☺️\n"
                      ":PHASE: %s\n"
                      ":END:\n\n"
                      "/Declare your goals. Confront your results. Adjust to living in reality./\n"
                      "/Be specific, tangible, and observable. Use numbers, names, and dates./\n"
                      "/Keep it to one page. Report only essentials — headlines, not a Sunday edition./\n")
              (ts-week-of-year (ts-now))
              (ts-month-name (ts-now))
              (my/weekly-review-phase)))



    (defun my/weekly-review--insert-life-scores ()
      "Phase 2+: prompt for Social, Faith, Marriage, Health scores.
       Inserts the composite daily score into the heading and aligns properties."
      (when (>= (my/weekly-review-phase) 2)
        (my/read-daily-review-properties
         :questionnaire '(("SocialScore: Did I see family *and* did I call or message a friend?"
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
                           " 3  --  Extremely High  --  I’m king of the world!")
                          ("HealthScore: Did I go for a run or do a workout this week?"
                           " 0  --  No             --  What am I doing with my life?"
                           " 1  --  Yes…           --  Sweet, what did you do?"))
                                        ; 📊 Other metrics to consider keeping track of:
                                        ; ⇒ Hours worked
                                        ; ⇒ Tasks completed
         :prop-val-action (lambda (property value) (org-set-property property value)))
        ;; Add Daily Score to the start of the headline
        (beginning-of-buffer)
        (org-beginning-of-line)
        (insert (format "﴾%s﴿ " (org-entry-get (point) "Daily␣score")))
        (my/org-align-property-values)))

    (defun my/weekly-review--insert-commit-lint ()
      "Phase 3: insert commit and lint checklist for closing out last week."
      (when (>= (my/weekly-review-phase) 3)
        (insert
         (let* ((♯lines (save-restriction (widen) (count-lines (point-min) (point-max))))
                (last-time (s-trim (shell-command-to-string "git log -1 --pretty=%s")))
                ;; Note that there's no `git push' since this is local ---Github may expose my notes to AI, no thank-you.
                (save-incantation (format "git add .; git commit -m 'Weekly Review Start, save %s lines on %s'" ♯lines  (ts-format))))
           (lf-string "
       ***** Closing up last week

       ****** TODO ⟨0⟩ Commit last week         [0%]

       + [ ] Commit & push all the changes before the review

         \t 🤖 Last time ∷ " ${last-time} "
         \t 🛋️ my-life.org line count ∷ ${♯lines}
         \t   # If it’s significantly less, then look at diff to ensure I didn’t lose anything important.
         \t ⁉️ [[elisp:(shell-command-to-string \"${save-incantation}\")][Click to commit!]]

       + [ ] ⁉️ [[elisp:(progn (widen) (funcall-interactively #'org-lint))][Lint my-life.org]]

         \t Importantly this mitigates [[https://en.wikipedia.org/wiki/Link_rot][link rot]]
         \t and ensures that when I do bulk find-replace actions that I haven’t
         \t royally messed things up.
")))))


    (defun my/weekly-review--insert-clock-visuals ()
      "Insert the clock report and pop open the chord/sankey diagram."
      ;; (save-restriction (widen)  (org-clock-report))
      (insert "For additional context, see browser for interactive diagram of where the time went! ⏰")
      ;; Pop open the chord/sankey diagram for a visual overview.
      (my/clock-diagram))

    (defun my/weekly-review--insert-git-commits ()
      "Insert a numbered list of git commits merged this week.
Each line links to magit-show-commit for the full diff."
      (-let [raw (let ((default-directory my\work-dir))
                   (string-trim
                    (shell-command-to-string
                     "git log --author=\"Musa\" --since=\"7 days ago\" --format=\"%h\t%s\" --no-merges")))]
        (insert "\n\n*Commits merged this week:*\n"
                (if (string-empty-p raw)
                    "/(None yet — keep shipping!)/\n"
                  (-let [n 0]
                    (concat
                     "#+begin_quote\n"
                     (mapconcat
                      (lambda (line)
                        (-let [(hash subject) (s-split-up-to "\t" line 1)]
                          (format "%d. %s 📚 [[elisp:(let ((default-directory my\\work-dir)) (magit-show-commit \"%s\"))][Details]]"
                                  (cl-incf n) subject hash)))
                      (s-lines raw) "\n")
                     "\n#+end_quote\n"))))))

    (defun my/weekly-review--insert-jira-tickets ()
      "Insert a summary of Jira tickets worked on this week.
Shows ticket ID, title, Gerrit status, and time spent."
      (let* ((end-date (format-time-string "%Y-%m-%d"))
             (start-date (format-time-string
                          "%Y-%m-%d"
                          (time-subtract (current-time)
                                         (days-to-time 7))))
             (tickets (my/jira-tickets-clocked-in-range start-date end-date)))
        (when tickets
          ;; Titles come from the work-item cache (populated by
          ;; my/agenda-work-refresh) or from clock headings — no
          ;; additional Jira API calls needed.
          (insert "\n\n*Jira tickets worked on this week:*\n#+begin_quote\n")
          (let ((total-minutes 0)
                (n 0))
            (dolist (entry tickets)
              (let* ((ticket (car entry))
                     (title (or (my/gerrit--get-jira-title ticket)
                                (alist-get 'heading (cdr entry))))
                     (minutes (alist-get 'minutes (cdr entry)))
                     (hours (/ minutes 60))
                     (mins (% minutes 60))
                     (status (my/progress-pulse--ticket-status ticket)))
                (cl-incf total-minutes minutes)
                (insert (format "%d. *%s* %s -- %s (%dh %02dm)\n"
                                (cl-incf n) ticket title status hours mins))))
            (insert (format "\nTotal: %dh %02dm across %d tickets\n"
                            (/ total-minutes 60) (% total-minutes 60) (length tickets))))
          (insert "#+end_quote\n"))))

    (defvar my/cowsay-figures
      '("actually" "alpaca" "beavis.zen" "blowfish" "bong" "bud-frogs"
        "bunny" "cheese" "cower" "cupcake" "daemon" "default" "dragon"
        "dragon-and-cow" "elephant" "elephant-in-snake" "eyes"
        "flaming-sheep" "fox" "ghostbusters" "head-in" "hellokitty"
        "kiss" "kitty" "koala" "kosh" "llama" "luke-koala"
        "mech-and-cow" "meow" "milk" "moofasa" "moose" "mutilated"
        "ren" "sheep" "skeleton" "small" "stegosaurus" "stimpy"
        "supermilker" "surgery" "sus" "three-eyes" "turkey" "turtle"
        "tux" "udder" "vader" "vader-koala" "www")
      "Cowsay figure files to choose from in the weekly review.")

    (defun my/cowsay-random-command ()
      "Return a shell command string for fortune piped to a random cowsay figure.
When the figure is `www', a random mood modifier (-b, -d, ...) may be appended."
      (let ((figure (seq-random-elt my/cowsay-figures)))
        (if (string= figure "www")
            (let ((modifier (seq-random-elt '("" "-b" "-d" "-g" "-p" "-s" "-t" "-w" "-y"))))
              (format "fortune | cowsay -W 60 -f www %s 2>/dev/null" modifier))
          (format "fortune | cowsay -f %s 2>/dev/null" figure))))

    (defun my/weekly-review--insert-social-reminder ()
      "Insert the #social visibility reminder with a fortune/cowsay quote.
Picks a random cowsay figure (and mood modifier for `www'),
copies the result as a Slack code block (wrapped in triple
backticks), opens the #social channel in the browser, and
shows a reminder dialog.

Implementation note: This could be done automatically via
webhooks, however we don't want this to seem like automation;
we want it to feel like genuine human connection!"
      (let* ((cmd (my/cowsay-random-command))
             (cowsay (string-trim (shell-command-to-string cmd))))
        (cl-assert (not (string-empty-p cowsay)))
        ;; Copy as a Slack code block (triple-backtick wrapped).
        (kill-new (concat "```\n" cowsay "\n```"))

        (when (eq system-type 'darwin)
          ;; /Implementation note: This could be done automatically via webhooks,/
          ;; /but we don't want automation here — we want genuine human connection!/
          (non-blocking-message-box
           :title "⟨1⟩ Post to #social — stay visible!"
           :content "ASCII art copied! Paste it into #social now for a human connection!

:WHY: Remote workers vanish from collective memory. A weekly post keeps you in the conversation.

Share something in *#social* this week — a joke, a link, a photo, a hot take.
Doesn't have to be profound; it just has to be /you/."
           ;; Open #social in the browser so posting is one paste away.
           :buttons '("Yeah, let's do it!" (browse-url my\social-channel-url))))))

    (defun my/weekly-review--promise-section-body ()
      "Return the body text of the current Org heading (up to the first child).
Skips the property drawer; strips Org comments."
      (save-excursion
        (org-end-of-meta-data t)
        (let* ((body-start (point))
               (body-end (save-excursion
                           (if (org-goto-first-child)
                               (point)
                             (org-end-of-subtree t)
                             (point))))
               (raw (buffer-substring-no-properties body-start body-end)))
          (->> raw
               ;; Axe Org comments (lines starting with #)
               (replace-regexp-in-string "^#.*\n?" "")
               (replace-regexp-in-string "\n\\{3,\\}" "\n\n")
               string-trim))))

    (defun my/weekly-review--promise-cleaned-text ()
      "Extract the Weekly Promise for Slack posting.
Walks the Org tree structurally: for each direct child of the
Weekly Promise heading, gathers the title (bolded) and body text
while skipping :private:-tagged children entirely."
      (save-excursion
        (goto-char (point-min))
        (let (sections)
          (when (org-goto-first-child)
            (while (progn
                     (unless (member "private" (org-get-tags nil t))
                       (let ((title (org-get-heading t t t t))
                             (body  (my/weekly-review--promise-section-body)))
                         (push (if (string-empty-p body)
                                   (format "*%s*" title)
                                 (format "*%s*\n%s" title body))
                               sections)))
                     (org-get-next-sibling))))
          (string-trim
           (mapconcat #'identity (nreverse sections) "\n\n")))))

    (defun my/weekly-review--monday-standup-body ()
      "Return the body text under the Monday Standup heading.
This is the content between the property drawer and the first
child heading (i.e., the user-edited standup text, not the
auto-generated sub-sections)."
      (save-excursion
        (goto-char (point-min))
        (unless (re-search-forward "^\\*+ Monday Standup" nil t)
          (user-error "No 'Monday Standup' heading found"))
        (my/weekly-review--promise-section-body)))

    (defun my/weekly-review--archive-standup-to-org ()
      "Copy the Monday Standup body into the Standups tree.
Prepends a dated entry under 'Daily Standup Planning', mirroring
what `my/standup' does for daily entries.  If today's entry already
exists, it is replaced with the (potentially updated) content."
      (let ((body (my/weekly-review--monday-standup-body))
            (today-heading (format-time-string "*** <%Y-%m-%d %a>")))
        (save-excursion
          (find-file org-default-notes-file)
          (widen)
          (goto-char (point-min))
          (unless (re-search-forward
                   "^\\*\\* .*Daily Standup Planning" nil t)
            (error "Could not find '** ... Daily Standup Planning' heading"))
          (org-narrow-to-subtree)
          (org-end-of-meta-data t)
          ;; Axe existing entry for today, if present.
          (when (re-search-forward
                 (concat "^" (regexp-quote today-heading)) nil t)
            (org-back-to-heading t)
            (let ((beg (point))
                  (end (save-excursion (org-end-of-subtree t t) (point))))
              (delete-region beg end)))
          ;; Insert at the top (after metadata, before first child).
          (org-end-of-meta-data t)
          (when (re-search-forward "^\\*\\*\\* " nil t)
            (goto-char (match-beginning 0)))
          (insert today-heading "\n\n" body "\n\n")
          (widen)
          (save-buffer))))

    (defun my/weekly-review--publish-promise ()
      "Publish the Weekly Promise: copy to clipboard, open #standups, archive to Org."
      (interactive)
      (let ((text (my/weekly-review--promise-cleaned-text)))
        ;; 1. Convert Org → Slack mrkdwn and copy to clipboard.
        (my/copy-as-slack text)
        ;; 2. Open #standups in the browser.
        (browse-url my\standups-channel-url)
        ;; 3. Archive the Monday Standup body into the Standups org tree.
        (my/weekly-review--archive-standup-to-org)
        (message "Weekly promise copied to clipboard & archived! Paste it into #standups.")))

    (defun my/weekly-review--week-ticket-data ()
      "Gather and partition this week’s clocked and cached work items.
Returns a plist with keys :in-progress-clocked, :todo-clocked,
:extra-wip, and :extra-todo.

All data comes from local sources: clock entries in Org and the
work-item cache (`my/agenda-work-data’) populated by
`my/agenda-work-refresh’.  No Jira API calls are made here."
      (let* ((end-date (format-time-string "%Y-%m-%d"))
             (start-date (format-time-string
                          "%Y-%m-%d"
                          (time-subtract (current-time)
                                         (days-to-time 7))))
             (clocked (my/jira-tickets-clocked-in-range start-date end-date))
             (clocked-ids (mapcar #'car clocked))
             ;; Partition clocked tickets by status.
             (in-progress-clocked
              (--filter (let ((s (my/progress-pulse--ticket-status (car it))))
                          (not (or (string= "MERGED" s) (string= "TODO" s))))
                        clocked))
             (todo-clocked
              (--filter (string= "TODO" (my/progress-pulse--ticket-status (car it)))
                        clocked))
             ;; Work items from the cache that weren’t clocked this week.
             ;; These are already fetched by my/agenda-work-refresh — no
             ;; additional Jira/Gerrit calls needed.
             (all-items (or (plist-get my/agenda-work-data :items) nil))
             (extra-items
              (--filter (and (work-item-jira it)
                             (not (member (work-item-jira it) clocked-ids)))
                        all-items))
             ;; Partition by work-item status.
             (extra-wip
              (--filter (memq (work-item-status it)
                              '(wip my-needing-action reviews-needed jira-active))
                        extra-items))
             (extra-todo
              (--filter (memq (work-item-status it)
                              '(todo assigned))
                        extra-items)))
        (list :in-progress-clocked in-progress-clocked
              :todo-clocked        todo-clocked
              :extra-wip           extra-wip
              :extra-todo          extra-todo)))


    (defun my/weekly-review--insert-numbered-items (items &optional counter)
      "Insert ITEMS as a numbered Org list, stripping the urgency prefix.
Each work-item is rendered via `work-item-to-string' with the \"🔴 \"
prefix removed (weekly review sections convey urgency structurally).
COUNTER, when non-nil, should be a cons cell whose `car' is the
current count — it is mutated in place so callers can continue
numbering across multiple calls.  Returns the final count."
      (let ((n (if counter (car counter) 0)))
        (dolist (item items)
          (insert (format "%d. %s\n"
                          (cl-incf n)
                          (string-remove-prefix "🔴 " (work-item-to-string item)))))
        (when counter (setcar counter n))
        n))

    ;; Usage:
    ;;   (my/org-insert-heading :child "Title" :WHY "reason" :body "Body text")
    ;;   (my/org-insert-heading :sibling "Title")
    ;;   (my/org-insert-heading :uncle "Title" :FOO "bar")
    ;;   (my/org-insert-heading :aunt "Title")  ;; same as :uncle
    ;;
    ;; The first keyword is the relation (:child, :sibling, :uncle, :aunt).
    ;; The string immediately after is the heading title.
    ;; Remaining keyword pairs become properties — except :body, whose
    ;; value is inserted as text after the property drawer.
    (defun my/org-insert-heading (relation title &rest props)
      "Insert an Org heading at RELATION (:child, :sibling, :uncle, :aunt).
TITLE is the heading text. Remaining PLIST keyword pairs become
properties, except :body whose value is inserted after the drawer.
CREATED is set automatically (suppressing `org-insert-heading-hook'
to avoid firing `my/insert-CREATED-property' in non-Org buffers)."
      (let ((org-insert-heading-hook nil)
            ;; Should org-insert-heading leave a blank line before new heading? Yes!
            (org-blank-before-new-entry '((heading . t))))
        (org-insert-heading))
      (pcase relation
        (:child   (org-demote))
        (:sibling nil)
        ((or :uncle :aunt) (org-promote)))
      (insert title)
      ;; Set CREATED (mirroring my/insert-CREATED-property) then caller props.
      (org-set-property "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]"))
      (map-do (lambda (prop val) (unless (equal prop :body) (org-set-property (substring (symbol-name prop) 1) val))) props)
      (re-search-forward ":END:")
      (insert "\n\n")
      (-when-let (body (plist-get props :body))(insert body) (insert "\n\n")))



    (defun my/weekly-review--insert-completed ()
      "Insert the Completed section: merged tickets with git-log cross-reference.
Tickets still active in the work-item cache (e.g., please-review,
wip) are excluded — even if git log contains their commits."
      (my/org-insert-heading :sibling "✅ Completed" :DESCRIPTION "what shipped this week")

      (let* ((end-date (format-time-string "%Y-%m-%d"))
             (start-date (format-time-string
                          "%Y-%m-%d"
                          (time-subtract (current-time)
                                         (days-to-time 7))))
             (tickets (my/jira-tickets-clocked-in-range start-date end-date))
             ;; Tickets absent from Gerrit default to "MERGED", but that
             ;; also catches meetings and syncs (which never had changes).
             ;; Cross-reference git log: only tickets with actual commits
             ;; count as shipped.
             (git-ticket-ids
              (let* ((default-directory my\work-dir)
                     (ticket-re (concat "\\(" (regexp-opt my\jira-project-prefixes) "-[0-9]+\\)"))
                     (log (shell-command-to-string
                           "git log --author=\"Musa\" --since=\"7 days ago\" --format=\"%B\" --no-merges"))
                     ids)
                (dolist (line (split-string log "\n" t) (delete-dups ids))
                  (when (string-match ticket-re line)
                    (push (match-string 1 line) ids)))))
             ;; Tickets still active in the cache are NOT completed —
             ;; they may have some merged commits but are still in flight
             ;; (e.g., waiting on review, needs action).
             (active-ids
              (when-let* ((items (plist-get my/agenda-work-data :items)))
                (mapcar #'work-item-jira items)))
             (n 0))
        (if (null tickets)
            (insert "- /(fill in)/\n")
          (dolist (entry tickets)
            (let* ((ticket (car entry))
                   (title (or (my/gerrit--get-jira-title ticket)
                              (alist-get 'heading (cdr entry))))
                   (status (my/progress-pulse--ticket-status ticket)))
              (when (and (string= status "MERGED")
                         (member ticket git-ticket-ids)
                         (not (member ticket active-ids)))
                (insert (format "%d. %s %s\n"
                                (cl-incf n)
                                (if ticket
                                    (my/gerrit--format-jira-link ticket)
                                  "")
                                (or title "")))))))
        (when (= n 0)
          (insert "- /(nothing merged this week?! Really?! 🫣 Edit as needed)/\n")))

      (my/org-insert-heading :child "What did I ship this week? :private:"
                             :WHY "Recognise accomplishments, express self-gratitude, and debug!")
      (my/weekly-review--insert-clock-visuals)
      (my/weekly-review--insert-git-commits)
      (my/weekly-review--insert-jira-tickets)
      (insert (format "\n\n[[%s][🤔 Update Sprint Doc]]\n\n" my\sprint-doc-url))
      (my/weekly-review--insert-archive))

    (defun my/weekly-review--insert-in-progress (ticket-data &optional blocked-ids)
      "Insert the In Progress section from TICKET-DATA plist.
Renders clocked WIP tickets and un-clocked Jira items that are
neither MERGED nor TODO.  Tickets in BLOCKED-IDS (from Impediments)
are excluded — they belong in Impediments, not here."
      (my/org-insert-heading :uncle "🔨 In Progress" :DESCRIPTION "actively working on")
      (let ((n 0))
        ;; Clocked and actively in progress
        (dolist (entry (plist-get ticket-data :in-progress-clocked))
          (let* ((ticket (car entry))
                 (title (or (my/gerrit--get-jira-title ticket)
                            (alist-get 'heading (cdr entry))))
                 (status (my/progress-pulse--ticket-status ticket)))
            (unless (member ticket blocked-ids)
              (insert (format "%d. %s %s — %s\n"
                              (cl-incf n)
                              (if ticket
                                  (my/gerrit--format-jira-link ticket)
                                "")
                              (or title "") status)))))
        ;; From work-item cache, not clocked, but actively in progress.
        (let ((counter (cons n nil))
              (extra (--filter (not (member (work-item-jira it) blocked-ids))
                               (plist-get ticket-data :extra-wip))))
          (my/weekly-review--insert-numbered-items extra counter)
          (setq n (car counter)))
        (when (= n 0)
          (insert "- /(None?! Really?! — Edit as needed) 🫣/\n")))
      (insert "# - /(...design docs, discussion threads, or other non-Jira work?)/\n")
      (my/org-insert-heading :child "Notes \t\t\t:private:"
                             :body "1. ➕ Wins: What went well and why? ✅
2. What could have caused things to go so well? Maybe I can duplicate this next week!
3. Re-read [[https://jvns.ca/blog/brag-documents/][Get your work recognized: write a brag document]]")
      (insert (format "\n\n[[%s][🤔 Update Sprint Doc]]\n\n" my\sprint-doc-url)))

    (defun my/weekly-review--insert-reviewer-overload (items)
      "Insert a reviewer-overload hint after the impediment ITEMS.
Aggregates how many impediment items each reviewer appears on, then
inserts a sentence like:

  These impediments may be due to reviewer overload: Ali has 5,
  Hassan has 3, and Hussain has 1 Gerrit items vying for their
  attention, respectively.

Each count is an Org link to the reviewer's Gerrit attention
dashboard (open, non-abandoned items awaiting their action)."
      (let ((user-counts (make-hash-table :test 'equal))
            (user-names  (make-hash-table :test 'equal)))
        (dolist (item items)
          (dolist (pair (work-item-reviewer-users item))
            (let ((name (car pair))
                  (uname (cdr pair)))
              (when uname
                (cl-incf (gethash uname user-counts 0))
                (puthash uname name user-names)))))
        (when (> (hash-table-count user-counts) 0)
          (let ((entries nil))
            (maphash
             (lambda (uname count)
               (push (list (gethash uname user-names) uname count)
                     entries))
             user-counts)
            ;; Sort descending by count so the busiest reviewer is first.
            (setq entries (--sort (> (nth 2 it) (nth 2 other)) entries))
            (let ((parts
                   (-map
                    (lambda (e)
                      (let* ((name  (car (s-split " " (nth 0 e))))
                             (uname (nth 1 e))
                             (count (nth 2 e))
                             (url (format
                                   "%s/q/attention:%s+status:open+-is:abandoned"
                                   my\gerrit-base-url uname)))
                        (format "%s has [[%s][%d]]" name url count)))
                    entries)))
              (insert (format
                       "\nThese impediments may be due to reviewer overload: %s Gerrit %s vying for their attention, respectively.\n"
                       (my/gerrit--format-reviewer-names parts)
                       (if (= (length entries) 1) "item" "items"))))))))

    (defun my/weekly-review--insert-impediments ()
      "Insert the Impediments section: outstanding reviews sorted by age."
      (my/org-insert-heading :uncle "🚧 Impediments" :DESCRIPTION "who must do what by when?")
      (let ((please-review
             (when-let* ((items (plist-get my/agenda-work-data :items)))
               (--sort (< (or (work-item-age it) most-positive-fixnum)
                          (or (work-item-age other) most-positive-fixnum))
                       (--filter (eq 'please-review (work-item-status it))
                                 items)))))
        (if (null please-review)
            (insert "# - /(none — no outstanding reviews!)/\n")
          (my/weekly-review--insert-numbered-items please-review)
          (my/weekly-review--insert-reviewer-overload please-review)))
      (insert "# - /(...design docs, discussion threads, or other non-Jira work?)/\n")

      (my/org-insert-heading :child "Notes \t\t\t:private:"
                             :body (concat "

0. Are you sure of these impediments are valid? E.g., maybe they're outdated sidequests?

1. ➖ Challenges: What didn’t go well? ⚠️
2. How can I improve to mitigate bad weeks?"
                                           (format "[[%s][Open Sprint Doc 🤔 ]] — ensure each item above is listed and assigned!\n\n"
                                                   my\sprint-doc-url))))


    (defun my/weekly-review--insert-planned (ticket-data)
      "Insert the Planned section from TICKET-DATA plist.
Renders TODO-status tickets from clock data and Jira."
      (my/org-insert-heading :uncle "📆 Planned" :DESCRIPTION "Aim for ~3 Jiras, since rework requires time!")
      (insert "\n # *Tasks marked Urgent from Jira.*\n")
      (let ((pn 0))
        (dolist (entry (plist-get ticket-data :todo-clocked))
          (let* ((ticket (car entry))
                 (title (or (my/gerrit--get-jira-title ticket)
                            (alist-get 'heading (cdr entry)))))
            (insert (format "%d. %s %s\n"
                            (cl-incf pn)
                            (if ticket
                                (my/gerrit--format-jira-link ticket)
                              "")
                            (or title "")))))
        (let ((counter (cons pn nil)))
          (my/weekly-review--insert-numbered-items
           (plist-get ticket-data :extra-todo) counter)
          (setq pn (car counter)))

        (my/org-insert-heading :child "Notes \t\t\t:private:"
                               :body "1. 🔀 What will I focus on this week?
2. What $10k tasks do I want done? Why or why not?

")
        (insert "\n*What's my manager interested in for this sprint?* ")
        (insert (format "[[%s][Open Sprint Doc 🤔 ]]" my\sprint-doc-url))

        (insert (format "\n*Tickets I reported (personal interest):* [[%s][Open in Jira 🎯]]\n"
                        my\jira-reported-by-me-url))

        (insert "\n*WIP agenda items — open Gerrit changes I should resume.*\n")
        (let ((counter (cons pn nil)))
          (my/weekly-review--insert-numbered-items
           (when-let* ((items (plist-get my/agenda-work-data :items)))
             (--filter (eq 'wip (work-item-status it)) items))
           counter)
          (setq pn (car counter)))
        (when (= pn 0)
          (insert "- /(edit as needed)/\n")))
      (insert "- \n")
      )


    (defun my/weekly-review--insert-promise-checklist ()
      "Insert the final edit/publish checklist for the Weekly Promise."
      (my/org-insert-heading :aunt "Wrap-up \t\t\t :private: "
                             :body (lf-string "

       1. Edit the sections above
       2. [[elisp:(my/weekly-review--publish-promise)][📤 Publish to #standups]]
       3. Press “C-x C-s” then “C-x g” and if that looks good, commit!
       4. Press “C-c C-c” to conclude this capture!
")))

    (defun my/weekly-review--insert-weekly-promise ()
      "Insert the Weekly Promise section: Completed, In Progress, Planned, Impediments.
Pre-populates Completed from merged tickets, In Progress from non-merged
clocked tickets plus unresolved Jira items, and Impediments from stale
outstanding reviews."

      (my/org-insert-heading :sibling "Monday Standup"
                             :DESCRIPTION "This will be automatically copy-pasted to '* Standups' upon publish."
                             :body "1. ? \n2. ? \n3. ?

\n/Below is a progress report since last week and what I intend to do this week./")
      (my/weekly-review--insert-completed)
      (let* ((ticket-data (my/weekly-review--week-ticket-data))
             ;; Collect ticket IDs that will appear in Impediments,
             ;; so In Progress can exclude them (blocked ≠ active).
             (blocked-ids
              (when-let* ((items (plist-get my/agenda-work-data :items)))
                (mapcar #'work-item-jira
                        (--filter (eq 'please-review (work-item-status it))
                                  items)))))
        (my/weekly-review--insert-in-progress ticket-data blocked-ids)
        (my/weekly-review--insert-impediments)
        (my/weekly-review--insert-planned ticket-data))
      (my/weekly-review--insert-promise-checklist))


    (defun my/weekly-review--insert-l5-check ()
      "Insert the L5 senior-engineer competency self-check."
      ;; ─── All phases: L5 Competency check ───────────────────────────
      ;; "Am I operating at L5?" — a gentle, structured self-check.
      (my/org-insert-heading :child "Am I growing as a senior engineer, L5? \t\t\t:private:"
                             :body "
1. 🏗️ *Ownership*: Did I take responsibility for something beyond
my immediate task? (unowned work, tech debt, test gaps, a
design doc, a code review for someone outside my team)


2. 🧑‍🏫 *Collaboration*: Did I help another engineer learn something,
or unblock them? (pairing, review feedback, a Slack explanation)


3. 📐 *Scope*: Am I independently driving a project or feature
through its full lifecycle? (design → implement → test → ship) \n"))


    (defun my/weekly-review--insert-random-reflection ()
      "Phase 2+: insert a randomly chosen reflection question."
      (when (>= (my/weekly-review-phase) 2)
        (insert "\n + [ ] "
                (seq-random-elt
                 '(
                   "😄 What was the most enjoyable work activity of the last week?"
                   "🤦‍♂️ What were some frustrating or boring moments you had? How can you avoid that going forward?"
                   "🔧 Adjustments: What to stop? What to start? What to continue?"
                   "🟢 What should I continue doing?"
                   "🔴 What should I stop or change?"
                   "🧪 What’s one small experiment to try next week?"
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
                   "🪨 What are my biggest challenges (or "boulders")
for the week to come? Think about which tasks will
have the highest value in me reaching my potential
and being successful.")))))

    (defun my/weekly-review--insert-archive ()
      "Insert the archive-completed-tasks checklist."
      ;; ─── All phases: Archive as closure ────────────────────────────
      ;; Archiving DONE tasks is a small ritual of /completion/. The task
      ;; moves from "active" to "history." Your task list gets shorter.
      ;; You can /see/ progress because the list shrinks. The problem
      ;; isn’t productivity — it’s that you never close the loop.
      (my/org-insert-heading
       :sibling "Archive completed and cancelled tasks \t\t\t :private:"
       :WHY "/Archiving is an act of closure. It says: this is finished./ /A clean task list is a calm mind./"
       :body "1. Look through the ~:LOG:~ for useful information to file away into my References.
- If there’s useful info, capture it with ~C-c C-c~, then archive the original tree for clocking purposes.
- If clocking purposes do not matter, say for personal or trivial tasks, then just delete the tree.

[[elisp:(org-ql-search org-agenda-files '(and (done) (not (tags \"Top\")) (closed :to (-
(calendar-day-of-week (calendar-current-date))))))][📜 Items to review: Mine for useful info then archieve or delete. ☑️]]
"))

    (defun my/weekly-review--insert-forward-planning ()
      "Phase 3: insert forward planning, commit, and happiness sections."
      (when (>= (my/weekly-review-phase) 3)
        (insert "
***** Looking forward to next week ---/mentally try to see where I should be going/

****** TODO Prioritize and schedule!    [0%]

0. [ ] For the Waiting list, have others completed their tasks?

- Agenda view: The list of to-do or waiting tasks without SCHEDULED or DEADLINE


1. [ ] *Check Calendar*. Look at company calendar for the upcoming 2 weeks;
add items to your todo list if needed.

2. [ ] Find relevant tasks: What are my sprint goals and quarterly goals?
- What is assigned to me in Jira /for this sprint/?
- Any upcoming deadlines?
- Look at your Someday/Maybe list to see if there’s anything worth doing.
- get an overview about what you want to achieve in near future: your time
and energy are finite, tasks are not
- /Getting a sight of the forest can be very energizing and inspiring too,
while the endless trees can feel overwhelming or pointless./
- Relook some of the more recently processed tasks. Task priorities may change
as time progresses, and it is possible that with a huge onslaught of new
tasks, some important, older tasks may be left incubating. It’s also possible
that some tasks are no longer necessary.


3. [ ] Assign a [[https://radreads.co/10k-work/][dollar value]] to your work: $10 (low skill, not important), $100, $1k, and $10k (high skill, prized effort).

The Zen To Done system recommends scheduling your Most Important Tasks ($10k)
on your calendar each week so that by the end of the week you have completed
something of significance. It ensures that the more important things get
done.


4. [ ] To ensure the week is doable, add efforts to tasks for the week
then ensure the effort estimate is actually realistic. Also check each
day and ensure it’s realistic!

- [ ] Look at daily agenda view for the next week and make sure it’d doable and not
overloaded! I don’t want to keep pushing things since my days are unrealistic!


5. [ ] Decide your tasks for the week and time block your calendar.

*Focus on important tasks! Not low priority no-one-cares 'fun' efforts!*

*Embrace trade-offs.* You can’t do it all. Realize that when you’re
choosing to do one task, you’re saying `no` to many other tasks. And
                that’s a good thing

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

****** TODO Am I getting happier? [0%]

")
        (-let [start (point)]
          (insert (my/get-table-of-daily-scores-for-last-week))
          (center-region start (point)))
        (insert "\n" (my/percentage-of-life-spent) "\n")))

    (defun my/weekly-review--wrap-up ()
      "Finalise the review: update statistics cookies, increment counter, celebrate."
      (org-update-statistics-cookies 'all)
      (insert "\nSay, \"Today, my purpose was to have fun and do a good job at work! I did it! (｡◕‿◕｡)\"")
      (my/weekly-review--increment-counter)
      (message "Review ♯%d complete! (Phase %d) 🎉 To journal is to live!"
               my/weekly-review-count (my/weekly-review-phase)))

    ;; =======================================================================
    ;; Weekly Review -- capture template (the readable orchestration)
    ;; =======================================================================

    ;; "r"eview for the "w"eek
    ;;
    ;; Reflect on what went well and what could have gone better. Update your
    ;; to-do and projects list. Remove unimportant tasks and update your
    ;; calendar with any new relevant information.
    ;;
    ;;  Prepend a new section to "Weekly Log" listing what I've done in the
    ;;                        past week; useful for standups, syncs, and performance reviews.
    (bind-key*
     "C-c r w"
     (def-capture "🔄 Weekly Review 😊"
                  "🌿 Reviews 🌱"
                  ;; tldr on "ts.el":
                  ;; Today           = (ts-format) ;; ⇒ "2025-05-26 16:46:52 -0400"
                  ;; Today + 10years = (ts-format (ts-adjust 'year 10 (ts-now)))
                  ;; Day of the week 2 days ago = (ts-day-name (ts-dec 'day 2 (ts-now))) ;; ⇒ "Saturday"
                  ;; "What day was 2 days ago, Saturday? What day will it be in 10 years and 3 months?"
                  ;; See https://github.com/alphapapa/ts.el, which has excellent examples.
                  ;; 😲 Nice human formatting functions too!
                  (my/weekly-review--heading) ;; ⟵ This first argument is a template STRING!
                  (my/weekly-review--insert-life-scores)
                  (end-of-buffer)
                  ;; Start with something light — post to #social before
                  ;; diving into the heavier reflection sections.
                  (my/weekly-review--insert-social-reminder)
                  (my/weekly-review--insert-commit-lint)
                  (my/weekly-review--insert-l5-check)
                  (my/weekly-review--insert-weekly-promise)
                  (my/weekly-review--insert-random-reflection)
                  (my/weekly-review--insert-forward-planning)
                  (my/weekly-review--wrap-up)
                  (beginning-of-buffer)
                  ;; See only overview
                  (org-cycle)
                  (org-cycle)))

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
    ;; Alternatively one could use `org-map-entries’, however the filtering syntax is less than ideal.
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
      "Convert a list of lists to a pretty `table.el’ table, editable with ⌘-e."
      (let ((buf (get-buffer-create "*org-tb*")))
        (with-current-buffer buf
          (erase-buffer)
          (mapcar (lambda (x)
                    (mapcar (lambda (y) (insert (format "%s&" y))) x)(insert "\n")) lol)
          (table-capture 1 (point-max) "&" "\n" justify min-cell-width columns)
          (buffer-substring-no-properties (point-min) (point-max)))))
    ;;
    ;; (my/pp-list-of-lists-as-table '((Abc Def "xyz") (1 1 X1) (2 4 X2) (3 9 X3) (4 16 X4)))

    ) ;; End 😴 — deferred weekly review keybinding and helpers

  (setq org-clock-sound "~/.emacs.d/school-bell.wav")

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

(set-face-attribute
 'org-mode-line-clock
  nil
  :background "grey75"
  :foreground "red"
  :box '(:line-width -1 :style released-button))

;; Conversely, the getter:
(face-attribute  'org-mode-line-clock :foreground) ;; ⇒ "red"

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

(😴 progn ;; Defer: clock report functions are only needed on demand.

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

)  ;; End 😴 — deferred clock report functions

(😴 progn ;; Defer: chord diagram functions are only needed on demand.

(defun my/clock-diagram--clean-heading (s)
  "Strip Org metadata from heading string S for display in diagrams.
Removes TODO states, priority cookies, link markup, and timestamps.
\"WAITING [[url][Weekly Sprint]]\" → \"Weekly Sprint\";
\"TODO [#A] Jasim 1/1 <2025-05-12 Mon 14:00-15:30 .+1w>\" → \"Jasim 1/1\"."
  (thread-last s
    ;; 1. Axe leading TODO state (uses the live keyword list).
    (replace-regexp-in-string
     (if (bound-and-true-p org-todo-keywords-1)
         (concat "\\`" (regexp-opt org-todo-keywords-1 'words) " *")
       "\\`\\'")                        ; match nothing
     "")
    ;; 2. Axe priority cookie, e.g. "[#A] ".
    (replace-regexp-in-string "\\`\\[#[A-Z]\\] *" "")
    ;; 3. Replace [[link][description]] with just description.
    (replace-regexp-in-string "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]" "\\1")
    ;; 4. Axe bare [[link]] with no description.
    (replace-regexp-in-string "\\[\\[.*?\\]\\]" "")
    ;; 5. Axe Org timestamps: <...> and [...]
    (replace-regexp-in-string " *<[0-9].*?>" "")
    (replace-regexp-in-string " *\\[[0-9].*?\\]" "")
    (string-trim)))


(defun my/clock-diagram-data (&optional days-back category)
  "Collect clock data from `org-default-notes-file' for the past DAYS-BACK days.
Returns a list of rows (DAY-NAME HEADING MINUTES &optional NOTE),
suitable for passing as the :matrix argument to
`my/visualize-weighted-graph'.

By default, HEADING is the leaf heading that owns the CLOCK entry
\(the task you actually clocked into\).  When CATEGORY is non-nil,
it is the top-level (level-1) ancestor — the project/category.

Log notes \(created with \\[org-add-note]\) whose timestamp falls on the
same date as a clock entry are collected and joined with bullet
separators into the NOTE field."
  (let* ((days-back (or days-back 7))
         (cutoff (time-subtract (current-time) (days-to-time days-back)))
         (day-names ["Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"])
         ;; (day-name . heading) → minutes
         (table (make-hash-table :test 'equal))
         ;; (day-name . heading) → list of note strings
         (notes (make-hash-table :test 'equal)))
    (with-current-buffer (find-file-noselect org-default-notes-file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward
               "CLOCK: \\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) \\([A-Za-z]\\{3\\}\\).*?=>[ \t]+\\([0-9]+\\):\\([0-9]+\\)" nil t)
         (let* ((date-str (match-string 1))
                (day-abbr (match-string 2))
                (hours (string-to-number (match-string 3)))
                (mins  (string-to-number (match-string 4)))
                (total-mins (+ (* hours 60) mins))
                (ts (encode-time (parse-time-string (concat date-str " 00:00:00"))))
                (day-idx (pcase day-abbr
                           ("Mon" 0) ("Tue" 1) ("Wed" 2) ("Thu" 3)
                           ("Fri" 4) ("Sat" 5) ("Sun" 6)))
                (heading (my/clock-diagram--clean-heading
                          (save-excursion
                            (if category
                                (progn (while (and (org-up-heading-safe)
                                                   (> (org-current-level) 1)))
                                       (substring-no-properties
                                        (org-get-heading t t t t)))
                              (org-back-to-heading t)
                              (substring-no-properties
                               (org-get-heading t t t t)))))))
           (when (and day-idx
                      (time-less-p cutoff ts)
                      (> total-mins 0)
                      (not (string-empty-p heading)))
             (let ((key (cons (aref day-names day-idx) heading)))
               (puthash key (+ (gethash key table 0) total-mins) table)
               ;; Collect log notes under this heading whose date matches.
               (save-excursion
                 (org-back-to-heading t)
                 (let ((bound (save-excursion (org-end-of-subtree t) (point))))
                   (while (re-search-forward
                           (concat "^- Note taken on \\["
                                   (regexp-quote date-str)
                                   ".*?\\] *\\\\\\\\$")
                           bound t)
                     (forward-line 1)
                     (let ((body-start (point))
                           (body-end (point)))
                       ;; Note body = contiguous indented lines.
                       (while (and (not (eobp))
                                   (looking-at "^  +[^ \n]"))
                         (forward-line 1)
                         (setq body-end (point)))
                       (when (> body-end body-start)
                         (let ((txt (replace-regexp-in-string
                                     "\n" "<br>"
                                     (string-trim
                                      (buffer-substring-no-properties
                                       body-start body-end)))))
                           (unless (string-empty-p txt)
                             (push txt (gethash key notes)))))))))))))))
    ;; Emit rows.
    (let (rows)
      (maphash
       (lambda (k mins)
         (let* ((day (car k))
                (heading (cdr k))
                (note-list (nreverse (gethash k notes)))
                (index 0)
                (note (when note-list
                        (mapconcat (lambda (s) (format "<br>﴾%s﴿ %s" (cl-incf index)
                                                  ;; Don't bother showing me Org markup, I can't use it in hovers!
                                                  (replace-regexp-in-string "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]" "\\1" s)))
                                   note-list "<br>"))))
           (push (if note
                     (list day heading mins note)
                   (list day heading mins))
                 rows)))
       table)
      rows)))


  (defun my/chord--render-js (container-sel matrix)
    "Return a JS IIFE that renders a D3 chord diagram from MATRIX rows.
CONTAINER-SEL is a D3 selector string, e.g., \"'#chart'\" or
\"'#chord-view'\".  Expects a <div class=\"tooltip\" id=\"tooltip\">
to exist in the page.

Each row of MATRIX is (SOURCE TARGET WEIGHT &optional NOTE) — a flat
edge list.  D3's chord layout expects a square symmetric adjacency
matrix indexed by node number.  This function bridges the two
representations: it derives left (unique sources) and right (unique
targets) node groups, builds the N×N numeric matrix D3 expects, and
encodes per-edge notes into a JS object keyed by \"srcIdx,tgtIdx\" so
the ribbon tooltip can display them.

When NOTE is non-nil it appears as an italicised line beneath the
default \"Source → Target: Xh Ym\" tooltip text."
    (let* ((left-labels (cl-remove-duplicates
                         (cl-loop for (src . _rest) in matrix collect src)
                         :test #'equal))
           (right-labels (cl-remove-duplicates
                          (cl-loop for (_src tgt . _rest) in matrix collect tgt)
                          :test #'equal))
           (left-count (length left-labels))
           (right-count (length right-labels))
           (edge-table (let ((ht (make-hash-table :test 'equal)))
                         (cl-loop for (src tgt weight . _rest) in matrix
                                  do (let ((key (cons src tgt)))
                                       (puthash key (+ (gethash key ht 0) weight) ht)))
                         ht))
           (note-table (let ((ht (make-hash-table :test 'equal)))
                         (cl-loop for (src tgt _w . rest) in matrix
                                  when (car rest)
                                  do (let ((key (cons src tgt))
                                           (note (car rest)))
                                       (puthash key
                                                (if (gethash key ht)
                                                    (concat (gethash key ht) "; " note)
                                                  note)
                                                ht)))
                         ht))
           (full-matrix
            (append
             (cl-loop for l in left-labels collect
                      (append (make-list left-count 0)
                              (cl-loop for r in right-labels
                                       collect (gethash (cons l r) edge-table 0))))
             (cl-loop for r in right-labels collect
                      (append (cl-loop for l in left-labels
                                       collect (gethash (cons l r) edge-table 0))
                              (make-list right-count 0)))))
           (all-nodes (append left-labels right-labels))
           (nodes-json (json-encode (vconcat all-nodes)))
           (matrix-json (json-encode (vconcat (mapcar #'vconcat full-matrix))))
           (left-count-json (number-to-string left-count))
           (notes-json
            (let ((ht (make-hash-table :test 'equal)))
              (maphash (lambda (k v)
                         (let ((si (cl-position (car k) all-nodes :test #'equal))
                               (ti (cl-position (cdr k) all-nodes :test #'equal)))
                           (when (and si ti)
                             (puthash (format "%d,%d" si ti) v ht))))
                       note-table)
              (json-encode ht))))
      (concat
       "(function(){
const nodes = " nodes-json ";
const matrix = " matrix-json ";
const leftCount = " left-count-json ";
const notes = " notes-json ";
const totalWidth = 720, totalHeight = 720;
const innerRadius = Math.min(totalWidth,totalHeight)*0.30, outerRadius = innerRadius + 28;
const leftPalette = d3.scaleOrdinal().domain(nodes.slice(0,leftCount))
                      .range(['#4e79a7','#f28e2b','#e15759','#76b7b2',
                              '#59a14f','#edc948','#b07aa1']);
const rightPalette = d3.scaleOrdinal().domain(nodes.slice(leftCount))
                       .range(d3.schemeTableau10);
const color = i => i < leftCount ? leftPalette(nodes[i]) : rightPalette(nodes[i]);
const svg = d3.select(" container-sel ").append('svg')
  .attr('width',totalWidth).attr('height',totalHeight)
  .attr('viewBox',[-totalWidth/2,-totalHeight/2,totalWidth,totalHeight]);
const chords = d3.chord().padAngle(0.04).sortSubgroups(d3.descending)(matrix);
const ribbon = d3.ribbon().radius(innerRadius);
const tooltip = document.getElementById('tooltip');
function shorten(s,max=40){
  s=s.replace(/^[A-Z]+-\\d+\\s+/,'');
  return s.length>max ? s.slice(0,max)+'\\u2026' : s;
}
const ribbons = svg.append('g').selectAll('path').data(chords).join('path')
  .attr('d',ribbon).attr('class','chord-ribbon')
  .attr('fill',d=>color(d.source.index))
  .attr('stroke',d=>d3.rgb(color(d.source.index)).darker(0.3))
  .on('mouseenter',function(_,d){
    d3.select(this).attr('fill-opacity',0.95);
    const s=nodes[d.source.index], t=nodes[d.target.index];
    const m=matrix[d.source.index][d.target.index];
    var txt=s+' \\u2192 '+t+': '+Math.floor(m/60)+'h '+m%60+'m';
    var note=notes[d.source.index+','+d.target.index];
    if(note) txt+='<br><em>'+note+'</em>';
    tooltip.innerHTML=txt;
    tooltip.style.opacity='1';})
  .on('mousemove',e=>{tooltip.style.left=e.pageX+12+'px';tooltip.style.top=e.pageY+12+'px';})
  .on('mouseleave',function(){
    d3.select(this).attr('fill-opacity',0.75);
    tooltip.style.opacity='0';});
const arc = d3.arc().innerRadius(innerRadius).outerRadius(outerRadius);
const group = svg.append('g').selectAll('g').data(chords.groups).join('g');
group.append('path')
  .attr('d',arc).attr('fill',(_,i)=>color(i))
  .attr('stroke',(_,i)=>d3.rgb(color(i)).darker(0.4))
  .on('mouseenter',(_,d)=>{
    ribbons.transition().duration(150)
      .style('opacity',c=>
        c.source.index===d.index||c.target.index===d.index ? 1 : 0.08);
    tooltip.innerHTML='<strong>'+nodes[d.index]+'</strong><br>Total: '
      +Math.floor(d.value/60)+'h '+d.value%60+'m';
    tooltip.style.opacity='1';})
  .on('mousemove',e=>{tooltip.style.left=e.pageX+12+'px';tooltip.style.top=e.pageY+12+'px';})
  .on('mouseleave',()=>{
    ribbons.transition().duration(150).style('opacity',1);
    tooltip.style.opacity='0';});
group.append('text')
  .each(d=>{d.angle=(d.startAngle+d.endAngle)/2;})
  .attr('dy','0.35em')
  .attr('transform',d=>'rotate('+(d.angle*180/Math.PI-90)+')'
    +' translate('+(outerRadius+14)+')'
    +(d.angle>Math.PI?' rotate(180)':''))
  .attr('text-anchor',d=>d.angle>Math.PI?'end':'start')
  .style('font-size','13px').style('font-weight','600')
  .style('fill',(_,i)=>color(i))
  .text((_,i)=>shorten(nodes[i]));
})();")))


  (defun my/sankey--render-js (container-id matrix width height)
    "Return a JS IIFE that renders a Plotly Sankey from MATRIX rows.
CONTAINER-ID is a JS string literal for the target element,
e.g., \"'myDiv'\" or \"'sankey-view'\".  WIDTH and HEIGHT are pixel
dimensions.

Each row of MATRIX is (SOURCE TARGET WEIGHT &optional NOTE) — a flat
edge list.  Plotly's Sankey trace expects five parallel arrays: label[]
for node names, source[] and target[] for integer indices into label[],
value[] for edge weights, and color[] for edge colours.  This function
bridges the two representations: it deduplicates labels in
first-appearance order, maps each (SOURCE TARGET WEIGHT) to the
corresponding index triple, and assigns node colours from a Tableau-10
palette.

When NOTE is non-nil it appears as an italicised line beneath the
default \"Source → Target: value\" hover text."
    (let* ((all-labels (let (result (seen (make-hash-table :test 'equal)))
                         (cl-loop for (src tgt . _rest) in matrix
                                  do (dolist (name (list src tgt))
                                       (unless (gethash name seen)
                                         (puthash name t seen)
                                         (push name result))))
                         (nreverse result)))
           (palette '("#4e79a7" "#f28e2b" "#e15759" "#76b7b2"
                      "#59a14f" "#edc948" "#b07aa1" "#ff9da7"
                      "#9c755f" "#bab0ac"))
           (labels-json (json-encode (vconcat all-labels)))
           (colors-json
            (json-encode
             (vconcat (cl-loop for i below (length all-labels)
                               collect (nth (mod i (length palette)) palette)))))
           (sources-json
            (json-encode
             (vconcat (cl-loop for (src . _rest) in matrix
                               collect (cl-position src all-labels :test #'equal)))))
           (targets-json
            (json-encode
             (vconcat (cl-loop for (_src tgt . _rest) in matrix
                               collect (cl-position tgt all-labels :test #'equal)))))
           (values-json
            (json-encode
             (vconcat (cl-loop for (_src _tgt weight . _rest) in matrix
                               collect weight))))
           (notes-json
            (json-encode
             (vconcat (cl-loop for (_src _tgt _w . rest) in matrix
                               collect (or (car rest) "")))))
           (width-str (number-to-string width))
           (height-str (number-to-string height)))
      (concat
       "(function(){
var labels = " labels-json ";
var nodeColors = " colors-json ";
var sources = " sources-json ";
var targets = " targets-json ";
var notes = " notes-json ";
var values = " values-json ";
function shorten(s, max) {
  max = max || 40;
  s = s.replace(/^[A-Z]+-\\d+\\s+/, '');
  return s.length > max ? s.slice(0, max) + '\\u2026' : s;
}
function hexToRgba(hex, a) {
  return 'rgba('+parseInt(hex.slice(1,3),16)+','
    +parseInt(hex.slice(3,5),16)+','
    +parseInt(hex.slice(5,7),16)+','+a+')';
}
var restColors = sources.map(function(i){ return hexToRgba(nodeColors[i], 0.45); });
var ctr = " container-id ";
Plotly.react(ctr, [{
  type: 'sankey', orientation: 'h',
  node: {
    pad: 20, thickness: 24,
    line: { color: '#333', width: 0.5 },
    label: labels.map(function(s){ return shorten(s); }),
    customdata: labels,
    color: nodeColors,
    hovertemplate: '%{customdata}<extra></extra>'
  },
  link: {
    source: sources,
    target: targets,
    value: values,
    color: restColors,
    customdata: sources.map(function(i,idx){
      var s = labels[i]+' \\u2192 '+labels[targets[idx]]+': '+values[idx];
      if(notes[idx]) s += '<br><em>'+notes[idx]+'</em>';
      return s; }),
    hovertemplate: '%{customdata}<extra></extra>'
  }
}], {
  font: { size: 13, family: \"-apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif\" },
  width: " width-str ", height: " height-str ",
  paper_bgcolor: '#fafafa', margin: { t: 10, l: 10, r: 10, b: 10 }
});
// Dim unrelated links on node hover (replicates d3-sankey behaviour).
// Skip restyle during drag to avoid lag from rapid hover/unhover events.
var el = document.getElementById(ctr);
var dimmed = false, dragging = false;
el.addEventListener('mousedown', function() { dragging = true; restore(); });
document.addEventListener('mouseup', function() { dragging = false; });
function restore() {
  if (!dimmed) return;
  dimmed = false;
  Plotly.restyle(ctr, {'link.color': [restColors]}, [0]);
}
el.on('plotly_hover', function(ev) {
  if (dragging) return;
  var pt = ev.points[0];
  if (pt.source !== undefined) { restore(); return; }
  if (typeof pt.pointNumber !== 'number') return;
  var ni = pt.pointNumber;
  dimmed = true;
  Plotly.restyle(ctr, {'link.color': [sources.map(function(s,i){
    return (s===ni||targets[i]===ni)
      ? hexToRgba(nodeColors[s], 0.7)
      : hexToRgba(nodeColors[s], 0.06);
  })]}, [0]);
});
el.on('plotly_unhover', function() { if (!dragging) restore(); });
el.addEventListener('mouseleave', restore);
})();")))


  (cl-defun my/visualize-weighted-graph (&key title subtitle matrix)
    "Render a labelled, weighted, directed graph as a [Chord | Sankey] toggle page.
Returns the HTML file path.

TITLE and SUBTITLE label the page.  MATRIX is the edge list of a
labelled weighted digraph: each row is (SOURCE TARGET WEIGHT &optional NOTE).
All weights must be positive.  SOURCE and TARGET are node labels
\(strings\); WEIGHT is the numeric edge weight; NOTE is an optional
human-readable annotation for the edge — when non-nil it appears as
italicised text in the hover tooltip, giving qualitative context that
raw weights lack.

Each render function \(`my/chord--render-js', `my/sankey--render-js'\)
handles its own data prep from the raw edge list.  Plotly handles
cycles gracefully \(unlike d3-sankey\), so flows like A→B and B→A
render correctly.

By way of example, a labelled code-review graph:

  (my/visualize-weighted-graph
   :title \"Code Reviews\"
   :subtitle \"Past 30 days\"
   :matrix \\='((\"Alice\" \"Carol\" 10 \"Reviewed the parser refactor\")
             (\"Alice\" \"Dan\"   5)
             (\"Bob\"   \"Carol\"  3 \"Urgent hotfix reviews\")
             (\"Bob\"   \"Dan\"   12)))

A time-allocation digraph with cyclic flows:

  (my/visualize-weighted-graph
   :title \"Time Allocation\"
   :subtitle \"This week\"
   :matrix \\='((\"Mon\" \"Search\" 120)
             (\"Mon\" \"Reviews\" 60)
             (\"Tue\" \"Search\" 90)
             (\"Search\" \"New\" 12)
             (\"New\" \"Search\" 12)))

Or tag co-occurrence — which Org tags appear together on the same
headings?  Collect rows like (\"emacs\" \"elisp\" 14) via
`org-map-entries' over your notes file.  The chord view reveals which
topics cluster; the Sankey view shows volume."
    (let* ((html-file (expand-file-name
                       (format "graph-%s.html"
                               (format-time-string "%Y%m%d-%H%M%S"))
                       temporary-file-directory))
           (html (concat
                  "<!DOCTYPE html>
<html lang=\"en\">
<head>
<meta charset=\"UTF-8\">
<title>" title "</title>
<script src=\"https://d3js.org/d3.v7.min.js\"></script>
<script src=\"https://cdn.plot.ly/plotly-3.4.0.min.js\"></script>
<style>
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
       display:flex; flex-direction:column; align-items:center;
       margin:2em; background:#fafafa; }
h2 { color:#333; margin-bottom:0; }
p.subtitle { color:#888; margin-top:4px; }
.toggle { display:flex; gap:0; margin:16px 0 8px; }
.toggle button { padding:8px 24px; font-size:14px; font-weight:600;
  border:2px solid #4e79a7; cursor:pointer; transition:all .15s;
  background:#fff; color:#4e79a7; }
.toggle button:first-child { border-radius:6px 0 0 6px; }
.toggle button:last-child  { border-radius:0 6px 6px 0; border-left:none; }
.toggle button.active { background:#4e79a7; color:#fff; }
.view { display:none; }
.view.active { display:block; }
.chord-ribbon { fill-opacity:0.75; stroke-width:0.5;
                transition:fill-opacity .15s, opacity .15s; }
.tooltip { position:absolute; background:rgba(0,0,0,.82); color:#fff;
           padding:6px 12px; border-radius:4px; font-size:13px;
           pointer-events:none; opacity:0; transition:opacity .15s; }
</style>
</head>
<body>
<h2>" title "</h2>
<p class=\"subtitle\">" subtitle "</p>
<div class=\"toggle\">
  <button id=\"btn-chord\" class=\"active\" onclick=\"show('chord')\">Chord</button>
  <button id=\"btn-sankey\" onclick=\"show('sankey')\">Sankey</button>
</div>
<div id=\"chord-view\" class=\"view active\"></div>
<div id=\"sankey-view\" class=\"view\"></div>
<div class=\"tooltip\" id=\"tooltip\"></div>
<script>
function show(which) {
  document.querySelectorAll('.view').forEach(v=>v.classList.remove('active'));
  document.querySelectorAll('.toggle button').forEach(b=>b.classList.remove('active'));
  document.getElementById(which+'-view').classList.add('active');
  document.getElementById('btn-'+which).classList.add('active');
  if (which==='sankey' && window.Plotly) Plotly.Plots.resize('sankey-view');
}

// ===== CHORD =====
" (my/chord--render-js "'#chord-view'" matrix) "

// ===== SANKEY (Plotly) =====
" (my/sankey--render-js "'sankey-view'" matrix 1280 500) "
</script>
</body>
</html>")))
      (with-temp-file html-file (insert html))
      (browse-url (concat "file://" html-file))
      (message "Weighted graph: %s" html-file)
      html-file))


  (defun my/clock-diagram (&optional days-back leaf)
    "Open a unified clock diagram page with [Chord | Sankey] toggle buttons.
Plain = leaf tasks (default), \\[universal-argument] = top-level categories,
numeric prefix = days back (leaf)."
    (interactive "P")
    (let* ((category (consp days-back))
           (days-back (if (or (null days-back) (consp days-back)) 7 days-back))
           (kind (if category "Category" "Task")))
      (my/visualize-weighted-graph
       :title (format "Clock Time × %s" kind)
       :subtitle (format "Past %d days — %s"
                         days-back (format-time-string "%b %d"))
       :matrix (my/clock-diagram-data days-back category))))

  ) ;; End 😴 — deferred chord diagram functions

(😴 advice-add 'org-clocktable-indent-string :override
 (defun my/org-clocktable-indent-string (level)
  (if (= level 1)
      ""
    (let ((str "└"))
      (while (> level 2)
        (setq level (1- level)
              str (concat str "──")))
      (concat str "─> ")))))

(defun my/jira-tickets-clocked-in-range (start-date end-date)
  "Return alist of (TICKET . ((heading . STR) (minutes . N) (entries . LINES))).
Scans `org-default-notes-file' for CLOCK entries whose date falls
between START-DATE and END-DATE (inclusive, both \"YYYY-MM-DD\"
strings).  Groups by Jira ticket ID extracted from ancestor
headings using `my\\jira-project-prefixes'.

ISO date strings compare lexicographically, so a simple
`string<' / `string=' pair gives correct date ordering."
  (let* ((prefix-alt (mapconcat #'identity my\jira-project-prefixes "\\|"))
         (jira-re (format "\\(%s\\)-[0-9]+" prefix-alt))
         ;; Match any CLOCK line and capture its date.
         (clock-re "CLOCK: \\[\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)")
         result)
    (with-current-buffer (find-file-noselect org-default-notes-file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward clock-re nil t)
         (let ((date (match-string 1)))
           (when (and (not (string< date start-date))
                      (not (string< end-date date)))
             ;; Grab the clock line before moving to the heading.
             (let ((clock-line (string-trim
                                (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position)))))
               (save-excursion
                 (org-back-to-heading t)
                 (let ((heading (org-get-heading t t t t))
                       ticket)
                   ;; Walk up ancestor headings until we find a Jira ID.
                   (while (and (not (string-match jira-re heading))
                               (org-up-heading-safe))
                     (setq heading (org-get-heading t t t t)))
                   (when (string-match jira-re heading)
                     (setq ticket (match-string 0 heading))
                     (let* ((minutes
                             (if (string-match "=> +\\([0-9]+\\):\\([0-9]+\\)" clock-line)
                                 (+ (* 60 (string-to-number (match-string 1 clock-line)))
                                    (string-to-number (match-string 2 clock-line)))
                               0))
                            (existing (assoc ticket result)))
                       (if existing
                           (progn
                             (cl-incf (alist-get 'minutes (cdr existing)) minutes)
                             (push clock-line (alist-get 'entries (cdr existing))))
                         (push (cons ticket
                                     `((heading . ,heading)
                                       (minutes . ,minutes)
                                       (entries . (,clock-line))))
                               result))))))))))))
    (nreverse result)))

(defun my/jira-tickets-clocked-today ()
  "Return today's Jira ticket clock data.  See `my/jira-tickets-clocked-in-range'."
  (let ((today (format-time-string "%Y-%m-%d")))
    (my/jira-tickets-clocked-in-range today today)))

(define-minor-mode my/end-of-day-publish-mode
  "Ephemeral minor mode active only in the *End-of-Day Comments* buffer.
Binds \\`C-c C-c' to `my/end-of-day--publish' without polluting
`org-mode-map'."
  :lighter " EOD"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") #'my/end-of-day--publish)
            map))

(defun my/end-of-day-jira-comments ()
  "Open a buffer to write end-of-day comments for today's Jira tickets.
Scans clock entries, shows each ticket with clock context, and
provides \\[my/end-of-day--publish] to publish all comments to Jira."
  (interactive)
  (let ((tickets (my/jira-tickets-clocked-today)))
    (if (null tickets)
        (message "No Jira tickets clocked today.")
      ;; Batch-fetch titles for display.
      (my/gerrit--fetch-jira-titles (mapcar #'car tickets))
      (let ((buf (get-buffer-create "*End-of-Day Comments*")))
        (switch-to-buffer buf)
        (erase-buffer)
        (org-mode)
        (insert (format "* End-of-Day Comments — %s\n"
                        (format-time-string "%A, %B %e %Y")))
        (insert "Press =C-c C-c= to publish all comments to Jira.\n\n")
        (dolist (entry tickets)
          (let* ((ticket (car entry))
                 (info (cdr entry))
                 (heading (alist-get 'heading info))
                 (minutes (alist-get 'minutes info))
                 (entries (alist-get 'entries info))
                 (title (or (my/gerrit--get-jira-title ticket) heading))
                 (hours (/ minutes 60))
                 (mins (% minutes 60)))
            (insert (format "** %s: %s\n" ticket title))
            (insert (format "Clock: %dh %02dm today\n\n" hours mins))
            (dolist (cl (reverse entries))
              (insert (format "- =%s=\n" cl)))
            (insert "\nYour comment below this line:\n\n\n")))
        ;; Buffer-local C-c C-c to publish.  We use a minor mode so the
        ;; binding lives and dies with this buffer — local-set-key would
        ;; pollute the shared org-mode-map.
        (my/end-of-day-publish-mode 1)
        ;; Position cursor at the first comment area.
        (goto-char (point-min))
        (re-search-forward "Your comment below this line:" nil t)
        (forward-line 1)))))

(defun my/end-of-day--publish ()
  "Parse the *End-of-Day Comments* buffer and post comments to Jira."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let (results
          (ticket-re "^\\*\\* \\([A-Z]+-[0-9]+\\):"))
      (while (re-search-forward ticket-re nil t)
        (let* ((ticket (match-string 1))
               (section-start (point))
               (section-end (or (save-excursion
                                  (and (re-search-forward ticket-re nil t)
                                       (line-beginning-position)))
                                (point-max)))
               (section-text (buffer-substring-no-properties
                              section-start section-end))
               (comment (when (string-match
                               "Your comment below this line:\n"
                               section-text)
                          (string-trim (substring section-text
                                                  (match-end 0))))))
          (when (and comment (not (string-empty-p comment)))
            (let ((ok (my/jira-post-comment ticket comment)))
              (push (cons ticket (if ok 'ok 'fail)) results)))))
      (if (null results)
          (message "No comments to publish — write something under each ticket first.")
        ;; Open each published ticket in the browser for visual confirmation.
        (dolist (r results)
          (when (eq (cdr r) 'ok)
            (browse-url (format "%s/%s" my\jira-base-url (car r)))))
        (message "Published: %s"
                 (mapconcat
                  (lambda (r)
                    (format "%s %s" (if (eq (cdr r) 'ok) "✓" "✗") (car r)))
                  (nreverse results) ", "))
        ;; Open the sprint doc and remind to update it — manager watches it!
        (when (boundp 'my\sprint-doc-url)
          (browse-url my\sprint-doc-url))
        (when (eq system-type 'darwin)
          (non-blocking-message-box
           :title "Sprint Doc"
           :content "Jira comments posted. Now update the sprint Google Doc!"
           :buttons '(:OK nil)))))))

;; ——— Persistent state ———————————————————————————————————————————

(defvar my/progress-pulse-last-shared nil
  "Timestamp (as from `current-time') of the last progress-pulse share.
Loaded from and saved to `my/progress-pulse--timestamp-file'.")

(defconst my/progress-pulse--timestamp-file
  (expand-file-name ".progress-pulse-timestamp" user-emacs-directory)
  "Dot-file storing the epoch of the last progress-pulse share.")

(defun my/progress-pulse--load-timestamp ()
  "Load the last-shared timestamp from disk, or nil if absent."
  (when (file-exists-p my/progress-pulse--timestamp-file)
    (condition-case nil
        (let ((epoch (string-to-number
                      (with-temp-buffer
                        (insert-file-contents my/progress-pulse--timestamp-file)
                        (string-trim (buffer-string))))))
          (when (> epoch 0)
            (setq my/progress-pulse-last-shared
                  (seconds-to-time epoch))))
      (error nil))))

(defun my/progress-pulse--save-timestamp ()
  "Persist the current time as the last-shared timestamp."
  (setq my/progress-pulse-last-shared (current-time))
  (with-temp-file my/progress-pulse--timestamp-file
    (insert (number-to-string (floor (float-time my/progress-pulse-last-shared))))))

;; Load on startup so the standup nudge has data.
(my/progress-pulse--load-timestamp)

;; ——— Status lookup ——————————————————————————————————————————————

(defun my/progress-pulse--ticket-status (ticket)
  "Return a display string for TICKET's Gerrit status.
Cross-references `my/agenda-work-data' for open changes, then
falls back to git log: tickets with commits are MERGED, tickets
with neither open changes nor commits are CLOCKED (e.g., syncs,
meetings, umbrella epics)."
  (if-let* ((items (plist-get my/agenda-work-data :items))
            (item (--first (equal ticket (work-item-jira it)) items)))
      (pcase (work-item-status item)
        ('wip             "IN PROGRESS")
        ('please-review   "IN REVIEW")
        ('reviews-needed  "REVIEWS NEEDED")
        ('my-needing-action "NEEDS ACTION")
        ('todo            "TODO")
        ('assigned        "ASSIGNED")
        ('jira-active     "JIRA ACTIVE")
        (_                "OPEN"))
    ;; No open Gerrit change — check git log for actual commits.
    (if (and ticket
             (not (string-empty-p
                   (string-trim
                    (shell-command-to-string
                     (format "git -C %s log --author=\"Musa\" --since=\"14 days ago\" --format=\"%%h\" --no-merges --grep=%s"
                             (shell-quote-argument (expand-file-name my\work-dir))
                             (shell-quote-argument ticket)))))))
        "MERGED"
      "CLOCKED")))

;; ——— Core command ———————————————————————————————————————————————

(defun my/progress-pulse (&optional days)
  "Generate a progress summary for the past DAYS days (default 7).
Shows tickets worked on with time, title, and Gerrit status.
Offers one-click copy-as-slack / copy-as-confluence / mark-shared."
  (interactive "P")
  (let* ((n (or days 7))
         (end-date (format-time-string "%Y-%m-%d"))
         (start-date (format-time-string
                      "%Y-%m-%d"
                      (time-subtract (current-time)
                                     (days-to-time n))))
         (tickets (my/jira-tickets-clocked-in-range start-date end-date)))
    (if (null tickets)
        (message "No Jira tickets clocked in the past %d days." n)
      ;; Batch-fetch titles.
      (my/gerrit--fetch-jira-titles (mapcar #'car tickets))
      (let ((buf (get-buffer-create "*Progress Pulse*"))
            (total-minutes 0))
        (switch-to-buffer buf)
        (erase-buffer)
        (org-mode)
        ;; Header
        (insert (format "* Progress Pulse -- %s to %s\n\n" start-date end-date))
        ;; Ticket table
        (insert "** Tickets worked on\n\n")
        (dolist (entry tickets)
          (let* ((ticket (car entry))
                 (info (cdr entry))
                 (title (or (my/gerrit--get-jira-title ticket)
                            (alist-get 'heading info)))
                 (minutes (alist-get 'minutes info))
                 (hours (/ minutes 60))
                 (mins (% minutes 60))
                 (status (my/progress-pulse--ticket-status ticket)))
            (cl-incf total-minutes minutes)
            (insert (format "- *%s* %s -- %s (%dh %02dm)\n"
                            ticket title status hours mins))))
        ;; Total
        (insert (format "\nTotal: %dh %02dm across %d tickets\n"
                        (/ total-minutes 60)
                        (% total-minutes 60)
                        (length tickets)))
        ;; Review activity (count items where we are reviewer, not author)
        (when-let* ((items (plist-get my/agenda-work-data :items))
                    (reviews (--filter
                              (eq 'reviews-needed (work-item-status it))
                              items)))
          (insert (format "\n** Review queue: %d changes awaiting my review\n"
                          (length reviews))))
        ;; Action links
        (insert "\n** Actions\n\n")
        (insert "[[elisp:(progn (mark-whole-buffer) (my/copy-as-slack) (deactivate-mark))][Copy as Slack]]")
        (insert "  |  ")
        (insert "[[elisp:(progn (mark-whole-buffer) (my/copy-as-confluence) (deactivate-mark))][Copy as Confluence]]")
        (insert "  |  ")
        (insert "[[elisp:(progn (my/progress-pulse--save-timestamp) (message \"Marked as shared.\"))][Mark as shared]]")
        (insert "\n")
        ;; Position at top
        (goto-char (point-min))))))

;; ——— Monday standup: please-review items ————————————————————————

(defun my/standup--insert-review-items (marker)
  "Insert please-review work items at MARKER in the standup buffer.
Highlights stale items (>5 days old) so they're front-of-mind
during the Monday standup after the weekly review."
  (when-let* ((items (plist-get my/agenda-work-data :items))
              (please-review (--filter
                              (eq 'please-review (work-item-status it))
                              items)))
    (let ((sorted (--sort
                   (< (or (work-item-age it) most-positive-fixnum)
                      (or (work-item-age other) most-positive-fixnum))
                   please-review)))
      (save-excursion
        (with-current-buffer (marker-buffer marker)
          (org-with-wide-buffer
           (goto-char marker)
           (insert "*Outstanding reviews (poke these people!):*\n")
           (dolist (item sorted)
             (let* ((age-str (work-item--format-age (work-item-age item)))
                    (stale (and (work-item-age item)
                                (> (- (float-time) (work-item-age item))
                                   (* 5 86400))))
                    (prefix (if stale "🔴 " "- "))
                    (line (work-item-to-string item)))
               (insert (format "%s%s\n" prefix line))))
           (insert "\n")))))))

;; ——— Standup nudge ——————————————————————————————————————————————

(defun my/progress-pulse--maybe-remind ()
  "Show a macOS dialog if progress has not been shared in >4 days.
Called after the agenda opens during `my/standup'."
  (when (eq system-type 'darwin)
    (let* ((last (or my/progress-pulse-last-shared '(0 0 0 0)))
           (elapsed-days (/ (float-time (time-subtract (current-time) last))
                            86400.0)))
      (when (> elapsed-days 4)
        (non-blocking-message-box
         :title "Progress Visibility"
         :content (format "Last sprint-doc update: %d days ago.\nRun C-c r p to generate a progress pulse."
                          (floor elapsed-days))
         :buttons '(:OK nil))))))

;; ——— Enhanced end-of-day ————————————————————————————————————————

;; my/end-of-day is now a thin alias — the progress pulse is a
;; separate command (C-c r p), not mixed into the Jira comment flow.
(defalias 'my/end-of-day #'my/end-of-day-jira-comments
  "End-of-day ritual: write and publish Jira comments for today's tickets.")

;; ——— Keybindings ————————————————————————————————————————————————

(bind-key* "C-c r p" #'my/progress-pulse)
;; Repurpose C-c r d: the enhanced end-of-day wraps the original
;; Jira commenting ritual with a Slack-ready summary appended.
(bind-key* "C-c r d" #'my/end-of-day)

(bind-key*
 "C-c SPC"
 (defun my/jump-to-clocked-task (&optional prefix)
   "Jump to the currently clocked-in entry, or to the most recently clocked one.

With prefix arg, offer recently clocked tasks for selection."
   (interactive "P")
   (org-clock-goto prefix)
   (org-narrow-to-subtree)))

(url-copy-file "https://git.sr.ht/~bzg/org-contrib/blob/master/lisp/org-eldoc.el" "~/.emacs.d/elpa/org-eldoc.el" :ok-if-already-exists)
(load-file "~/.emacs.d/elpa/org-eldoc.el")
;; NB: 'eldoc-mode (no arg) toggles — and global-eldoc-mode already
;; enables it, so the toggle would turn it OFF.  Use turn-on-* instead.
(add-hook 'org-mode-hook #'turn-on-eldoc-mode)
(add-hook 'org-mode-hook 'eldoc-box-hover-mode)

;; [[file:init.org::*Implementation][Implementation:1]]
(setq org-archive-default-command #'my/org-archive-subtree-hierarchically)

(defvar my/org-archive--hierarchical-p nil
  "Non-nil when the current archive should refile hierarchically.
Bound dynamically by `my/org-archive-subtree-hierarchically’;
read by the `org-archive-finalize-hook’.")

(defun my/org-archive-subtree-hierarchically (&optional prefix)
  "Archive subtree, then refile it under the original outline path."
  (interactive "P")
  (let ((my/org-archive--hierarchical-p
         (and (not prefix) (not (use-region-p)))))
    (org-archive-subtree prefix)))

;; The hook runs with point on the just-archived entry, inside the
;; archive buffer, BEFORE org-archive-subtree’s own save-buffer ---
;; so our modifications are persisted automatically.
(add-hook 'org-archive-finalize-hook #'my/org-archive--refile-hierarchically)

(defun my/org-archive--refile-hierarchically ()
  "Refile the just-archived entry under its original outline path.
Runs as an `org-archive-finalize-hook’."
  (when my/org-archive--hierarchical-p
    (let* ((olpath (org-entry-get (point) "ARCHIVE_OLPATH"))
           (path (and olpath (split-string olpath "/")))
           (level 1)
           tree-text)
      (when olpath
        (org-mark-subtree)
        (setq tree-text (buffer-substring (region-beginning) (region-end)))
        (let (this-command (inhibit-message t)) (org-cut-subtree))
        (goto-char (point-min))
        (save-restriction
          (widen)
          (-each path
            (lambda (heading)
              (if (re-search-forward
                   (rx-to-string
                    `(: bol (repeat ,level "*") (1+ " ") ,heading)) nil t)
                  (org-narrow-to-subtree)
                (goto-char (point-max))
                (unless (looking-at "^")
                  (insert "\n"))
                (insert (make-string level ?*)
                        " "
                        heading
                        "\n"))
              (cl-incf level)))
          (widen)
          (org-end-of-subtree t t)
          (org-paste-subtree level tree-text)))

      ;; Merge duplicate: if a same-named heading already exists
      ;; earlier in the archive (e.g., a stub created when a child
      ;; was archived first), fold the new entry into it.
      (my/org-archive--merge-duplicate-heading))))

(defun my/org-archive--find-earlier-heading (heading level before-pos)
  "Find an earlier heading matching HEADING at LEVEL, before BEFORE-POS.
Returns the position of the heading, or nil."
  (save-excursion
    (goto-char (point-min))
    (let ((stars-re (format "^%s " (regexp-quote (make-string level ?*)))))
      (catch 'found
        (while (re-search-forward stars-re before-pos t)
          (goto-char (line-beginning-position))
          (when (and (= (org-current-level) level)
                     (string= (org-get-heading t t t t) heading))
            (throw 'found (point)))
          (forward-line 1))
        nil))))

(defun my/org-archive--merge-duplicate-heading ()
  "Merge the last top-level heading with any earlier same-named duplicate.
When we archive a child first, a parent stub is created in the
archive.  Archiving the parent later produces a duplicate heading.
This function detects that and merges: body text goes after the
existing heading’s properties, children are appended at the end."
  (goto-char (point-max))
  (while (org-up-heading-safe))
  (let* ((new-pos (point))
         (heading (org-get-heading t t t t))
         (level (org-current-level))
         (existing-pos (my/org-archive--find-earlier-heading
                        heading level new-pos)))
    (when existing-pos
      (goto-char new-pos)
      (let* ((subtree-end (save-excursion (org-end-of-subtree t t) (point)))
             (meta-end (save-excursion (org-end-of-meta-data t) (point)))
             (first-child-pos (save-excursion
                                (goto-char new-pos)
                                (when (org-goto-first-child) (point))))
             (body (buffer-substring meta-end (or first-child-pos subtree-end)))
             (children (when first-child-pos
                         (buffer-substring first-child-pos subtree-end))))
        ;; Delete the duplicate entry.
        (delete-region new-pos subtree-end)
        (when (looking-at "\n") (delete-char 1))
        ;; Insert body text after existing heading’s meta-data.
        (when (and body (not (string-empty-p (string-trim body))))
          (goto-char existing-pos)
          (org-end-of-meta-data t)
          (insert body))
        ;; Append children at end of existing subtree.
        (when children
          (goto-char existing-pos)
          (org-end-of-subtree t t)
          (unless (bolp) (insert "\n"))
          (insert children))))))
;; Implementation:1 ends here

;; [[file:init.org::*Install][Install:1]]
;; M-x agent-shell: Chat with Claude Code inside an Emacs buffer.
;; Uses browser login, so no API key needed — works with Claude Pro/Max subscription.
;; Requires: brew install node && npm install -g @zed-industries/claude-agent-acp

;; The `acp` dependency needs seq-empty-p (Emacs 31+); polyfill it for Emacs 30.
  (defun seq-empty-p (sequence)
    "Return non-nil if SEQUENCE is empty."
    (= 0 (seq-length sequence)))

;; agent-shell expects the claude-agent-acp binary, which is provided by the
;; @zed-industries/claude-agent-acp npm package.
;; Deferred: The `executable-find' guard makes this a no-op once installed,
;; but npm startup overhead is ~200ms even for a no-op.  agent-shell is
;; interactive-only, so deferring is safe.
(😴 unless (executable-find "claude-agent-acp")
  (shell-command "/opt/homebrew/bin/npm install -g @zed-industries/claude-agent-acp"))

(use-package agent-shell
  :defer t
  :config
  ;; Authentication setup — runs when agent-shell first loads.
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t)))

;; ⚙️ Add “ .agent-shell/ ” to “ ~/.gitignore_global” to ignore the transcripts of your conversations from Git
;; Install:1 ends here

;; [[file:init.org::*Opening Claude AI][Opening Claude AI:1]]
;; “k” for claude ---I use “C-c c” for my Org-capture, to quickly capture a note.
(global-set-key (kbd "C-c k")
   (defun my/claude-code (&optional arg)
   "DWIM entry point for Claude Code via agent-shell.

   1.         C-c k  ⇒  Toggle between here and Claude buffer, or open AI buffer if need be.
                        {If a region is active, carry it as context to the AI}
   2.     C-u C-c k  ⇒  Switch to an AI buffer by name
   3. C-u C-u C-c k  ⇒  Start a new AI buffer and name it

   This delegates to the existing =agent-shell= DWIM machinery
   (=agent-shell= already handles region context, toggling, and shell
   creation) while ensuring =default-directory= is always =my\work-dir=. The
   =C-u C-u= case calls =agent-shell-anthropic-start-claude-code= directly
   to force a new Claude Code shell without an agent selection prompt.
"
  (interactive "P")

 (let ((default-directory my\work-dir))
    (cond
     ((equal arg '(16))
      (let ((existing (agent-shell-buffers)))
        ;; (agent-shell-anthropic-start-claude-code)
        (agent-shell-start :config my/custom-claude-config)
        (when existing
          (let ((name (read-string "Label this session: ")))
            (when-let* (((not (string-empty-p name)))
                        (new-buf (seq-find (lambda (b) (not (memq b existing)))
                                           (agent-shell-buffers))))
              (with-current-buffer new-buf
                (setq my/claude-session-label name)))))))
     ((equal arg '(4))
      (let* ((bufs (or (agent-shell-buffers)
                       (user-error "No Claude shells available")))
             (labeled (mapcar (lambda (b)
                                (let ((label (buffer-local-value 'my/claude-session-label b)))
                                  (cons (if label
                                            (format "%s [%s]" (buffer-name b) label)
                                          (buffer-name b))
                                        b)))
                              bufs))
             (choice (completing-read "Switch to shell: "
                                      (mapcar #'car labeled) nil t))
             (buf (cdr (assoc choice labeled))))
        (agent-shell--display-buffer buf)))
     (t
      (agent-shell))))))

;; I dislike the “Claude Code>” prompt, it's kinda ugly, so my above command calls on this config:
;; Wrapped in with-eval-after-load to avoid triggering agent-shell autoload at startup.
(with-eval-after-load 'agent-shell
  (-let [my/custom-claude-prompt “λ🤖λ “]
    (setq my/custom-claude-config
          (agent-shell-make-agent-config
           :identifier 'claude-code
           :mode-line-name “Claude Code”
           :buffer-name “Claude Code”
           :shell-prompt my/custom-claude-prompt
           :shell-prompt-regexp my/custom-claude-prompt
           :icon-name “anthropic.png”
           :client-maker (lambda (buffer)
                           (agent-shell-anthropic-make-claude-client :buffer buffer))
           :install-instructions “See https://github.com/zed-industries/claude-agent-acp for installation.”))))

;; The names of the AI buffers seem to be tied to the AI processes, so we can't just rename buffers.
;; However, after any interaction, AI buffer headers are updated, so we hook into that mechanism to
;; provide a label for them. We also propogate that label to the agent-shell buffers menu.

(defvar-local my/claude-session-label nil "Custom label for this Claude Code session, shown in the header.")

(defun my/claude-label-session (label)
  "Set or update the session LABEL for the current agent-shell buffer.
The label is shown in the header and the session switch menu."
  (interactive "sLabel this session: ")
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))
  (setq my/claude-session-label (if (string-empty-p label) nil label)))

(with-eval-after-load 'agent-shell
  (advice-add 'agent-shell--make-header-model :filter-return
              (lambda (model) (when my/claude-session-label
                                (setf (alist-get :buffer-name model)
                                      (format "Claude Code: %s" my/claude-session-label))) model)))
;; Opening Claude AI:1 ends here

;; [[file:init.org::*Copy AI Text As Org][Copy AI Text As Org:1]]
;; When I copy text from Agent-Shell, it's read-only since it's shell text, but I don't want read-only text.
;; Moreover, when I copy prose, please give it to me in Org-mode format, not markdown.
;;
;; If you accidentaly have read-only text in your buffer, make it editable with:
;; (let ((inhibit-read-only t)) (remove-text-properties (point-min) (point-max) '(read-only nil)))
;;
(defun my/make-agent-shell-copy-be-editable-org-mode (orig-fn start end &optional delete)
  "Convert copied agent-shell text from Markdown to Org via pandoc.

Also strips read-only text properties that leak from comint output.

Code block copies are detected and returned as-is: markdown-overlays
places an invisible overlay on the closing ``` fence, which sits
right after the code body. When the copy icon fires kill-ring-save,
END lands just before that invisible overlay, so we check
overlays-in END..(+END 3) to skip pandoc for code blocks."
  (let ((text (funcall orig-fn start end delete)))
    (remove-text-properties 0 (length text) '(read-only nil) text)
    (if (cl-some (lambda (ov) (overlay-get ov 'invisible))
                 (overlays-in end (min (+ end 3) (point-max))))
        text
      (with-temp-buffer
        (insert text)
        (call-process-region (point-min) (point-max)
                             "pandoc" t t nil
                             "-f" "markdown" "-t" "org")
        (buffer-string)))))

(with-eval-after-load 'agent-shell
  (advice-add 'agent-shell--filter-buffer-substring :around #'my/make-agent-shell-copy-be-editable-org-mode))
;; Copy AI Text As Org:1 ends here

;; [[file:init.org::*Agent-Shell Buffer Cheatsheet][Agent-Shell Buffer Cheatsheet:1]]
;; All agent-shell config deferred — the bare setq of agent-shell-*
;; variables would trigger autoloading the package at startup.
(with-eval-after-load 'agent-shell
  ;; Some useful info in the AI buffer header: [Agent | Model | Thinking Mode | Directory ]
  (setq agent-shell-header-style 'text)

  ;; Don't confirm when I interrupt, just stop.
  (define-key agent-shell-mode-map (kbd "C-c C-c")
    (lambda () (interactive) (agent-shell-interrupt t)))

  ;; Expand tool use / thought process by default (instead of collapsed)
  (setq agent-shell-tool-use-expand-by-default nil)
  (setq agent-shell-thought-process-expand-by-default nil))
;; Agent-Shell Buffer Cheatsheet:1 ends here

;; [[file:init.org::*Done!][Done!:1]]
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(cl-defun non-blocking-message-box (&key (title "Emacs") content (buttons '(:OK nil)))
  "Show a macOS dialog asynchronously — Emacs keeps loading.
TITLE is the window title, CONTENT the body text, and BUTTONS a
plist mapping labels to handler forms (or nil to ignore).  Labels
can be any type — keywords, symbols, strings — they are converted
to title-case strings for display:

  ;; Fire-and-forget:
  (non-blocking-message-box :content \"Hello!\" :buttons \\='(:OK nil))

  ;; With handlers:
  (non-blocking-message-box
    :content \"Save before quitting?\"
    :buttons \\='(:Cancel  (message \"Cancelled\")
               :Discard (kill-emacs)
               :Save    (save-some-buffers t)))

The last label in the plist is the default button.

Unlike `message-box', this spawns an external osascript process,
so Emacs never blocks.  By way of example, a `message-box' call
at the end of init.el skewed `emacs-init-time' by however long
we stared at the dialog before clicking OK."
  (let* ((to-string (lambda (x)
                      (let ((s (format "%s" x)))
                        (if (string-prefix-p ":" s) (substring s 1) s))))
         (labels (cl-loop for (label _) on buttons by #'cddr
                          collect (funcall to-string label)))
         (handlers (cl-loop for (label handler) on buttons by #'cddr
                            collect (cons (funcall to-string label) handler)))
         (proc (start-process "non-blocking-message-box" nil "osascript" "-e"
                 (format "display dialog %S with title %S buttons {%s} default button %d"
                         content title
                         (mapconcat (lambda (b) (format "%S" b)) labels ", ")
                         (length labels)))))
    ;; Stash handler alist and accumulated output on the process itself,
    ;; since init.el lacks lexical-binding.
    (process-put proc :handlers handlers)
    (process-put proc :output "")
    (set-process-filter proc
      (lambda (proc text)
        (process-put proc :output (concat (process-get proc :output) text))))
    (set-process-sentinel proc
      (lambda (proc _event)
        (let ((out (process-get proc :output)))
          (when (string-match "button returned:\\(.+\\)" out)
            (let ((handler (alist-get (string-trim (match-string 1 out))
                                      (process-get proc :handlers) nil nil #'string=)))
              (when handler (eval handler t)))))))))

(when (eq system-type 'darwin) ;; osascript is macOS-only; skip on Linux CI runners
  (non-blocking-message-box :title "Emacs" :content "Enjoy life (｡◕‿◕｡)" :buttons '(:♥)))
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

;; [[file:init.org::*Bookmarks: Quick naviagation to commonly visited locations][Bookmarks: Quick naviagation to commonly visited locations:3]]
(use-package org-bookmark-heading :defer t)
;; Bookmarks: Quick naviagation to commonly visited locations:3 ends here

;; [[file:init.org::*Bookmarks: Quick naviagation to commonly visited locations][Bookmarks: Quick naviagation to commonly visited locations:4]]
;; Save/mark a location with “C-u M-m”, jump back to it with “M-m”.
(bind-key* "M-m"
           (lambda ()
             (interactive)
             (if (not current-prefix-arg)
                 (helm-all-mark-rings)
               (push-mark)
               (message "[To return to this location, press M-m] ∷ %s"
                        (s-trim (substring-no-properties (thing-at-point 'line)))))))
;; Bookmarks: Quick naviagation to commonly visited locations:4 ends here

;; [[file:init.org::*Bookmarks: Quick naviagation to commonly visited locations][Bookmarks: Quick naviagation to commonly visited locations:5]]
(use-package backward-forward
  :bind (("M-[" . backward-forward-previous-location)
         ("M-]" . backward-forward-next-location)))
;; Bookmarks: Quick naviagation to commonly visited locations:5 ends here

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
  :hook   (org-mode text-mode)
  :custom (beacon-color "#666600"))
;; Never lose the cursor:2 ends here
