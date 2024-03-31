
;; Here are the main guideline and design principle for configure emacs that you should obey and generete code.

;; IMPORTANT PRINCIPLES:
;; 1. FAST STARTUP TIME OF EMACS (even with many additional installed packages)
;; 2. DECLARATIVE CONFIG (seperate defun definition of the main config)
;; 3. EASY TO DEBUG (when certain packages has an issue, we must be able identify easily which package causing
;; the error and which function if possible to identify )
;; 4. STABLIBILITY (emacs can extend a lot with many package but it is hard to keep it stable,
;; therefore we should stay close the core emacs)

;; Use-package
;;    Use-package is an Emacs configuration manager (*not* a package manager!) that allows you to keep init file(s)
;; clean, tidy and declarative.
;;    - use it for built-in packages/libraries too (without ~:ensure t~)
;;    - each variable/face setting or each function call belongs to the one and only package which provides the symbol
;;    To find out, where do they belong to, use /M-x describe-variable/, /M-x describe-face/
;; or /M-x describe-function/ accordingly.
;;  Setting variables
;;     "Traditional" setq is almost always means changing a customizable variable, so can be rewritten with
;; customize-set-variable and therefore with :custom keyword, it supports inline comments. Besides that
;; some defcustoms have get/set functions so ~setq~-ing will have no effect unless you run a setter manually.
;;     /At the same time I don't use Customization interface, and my own custom-file is =/dev/null=./
;;     An example
;;       ;; "Don't show splash screen"
;;       (setq inhibit-startup-screen t)
;;     becomes
;;       :custom
;;       (inhibit-startup-screen t "Don't show splash screen")
;;  Faces
;;     There's :custom-face, so
;;       (set-face-attributes 'Man-overstrike nil :inherit font-lock-type-face :bold t)
;;     becomes
;;       :custom-face
;;       (Man-overstrike ((t (:inherit font-lock-type-face :bold t))))
;;     Hardcoded faces can break themes, so I use :inherit for customizations, if possible.
;;  Hooks
;;     add-hook → :hook
;;  Key-binding
;;     define-key → :bind
;;  Sequential loading
;;     use :after if you want to load a package after another one

(load-file (expand-file-name "src/paths.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; elpaca

;; place elpaca packages into different location than default
;; wrt emacs version
(setq ub/emacs-configs-dir "~/emacs-configs")
(defvar elpaca-directory (expand-file-name (concat "elpaca-done-right_" emacs-version) ub/emacs-configs-dir))
;; check if ub/elpaca-dir exits, if not create it
(unless (file-exists-p elpaca-directory)
  (make-directory elpaca-directory t))

(defvar elpaca-installer-version 0.7)
;;(defvar elpaca-directory (expand-file-name "elpaca/" ub/elpaca-dir))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))


;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;; Block until current queue processed.
(elpaca-wait)
;;;;;;;;;;;; elpaca


(require 'server)
(unless (server-running-p)
  (server-start))

;; ;;; For packaged versions which must use `require'.
;; (use-package modus-themes
;;   :ensure t
;;   :init
;;   ;; Add all your customizations prior to loading the themes
;;   (setq modus-themes-italic-constructs t
;;         modus-themes-bold-constructs t
;;         modus-themes-region '(bg-only no-extend)
;;         ;;modus-themes-org-blocks 'tinted-background)
;;         )

;;   :config
;;   ;; ;; Add all your customizations prior to loading the themes
;;   ;; (setq modus-themes-italic-constructs t
;;   ;;       modus-themes-bold-constructs nil)

;;   ;; ;; Maybe define some palette overrides, such as by using our presets
;;   ;; (setq modus-themes-common-palette-overrides
;;   ;;       modus-themes-preset-overrides-intense)

;;   ;; Load the theme of your choice.
;;   (load-theme 'modus-vivendi)

;;   (define-key global-map (kbd "<f5>") #'modus-themes-toggle))


;;;;;;;;;;;;;;;;;;;;;;;

;; (message "Loading emacs-configs...")



;; we use nerd-icon as much as possible
;; ;; domtronn/all-the-icons.el
;; (use-package all-the-icons
;;   :ensure (:fetcher github :repo "domtronn/all-the-icons.el")
;;   :defer 0.1)
;; ;; NOTE to install run all-the-icons-install-fonts


;; use-package with Elpaca:
(use-package dashboard
  :ensure t
  :custom
  (dashboard-startup-banner (expand-file-name "banner-imgs/listening_gnu_resized_30.png" user-emacs-directory))
  (dashboard-banner-logo-title nil)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-items '((recents  . 5)
                     (bookmarks . 5)
                     ;;(projects . 5)
                     ))
  :config
  ;; TODO
  ;;(add-hook 'dashboard-after-initialize-hook #'solaire-mode)
  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
  (dashboard-setup-startup-hook))


;; GCMH - the Garbage Collector Magic Hack
(use-package gcmh
  :ensure (:fetcher gitlab :repo "koral/gcmh")
  :demand t
  :config
  (gcmh-mode 1))


(use-package cus-edit
  :defer t
  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory) "Store customizations in a temp file")
  :config
  (load custom-file 'noerror)
  ;;(custom-file (make-temp-file "emacs-custom") "Store customizations in a temp file")
  ;;(custom-file null-device "Don't store customizations")
  )

;;load file in the selected doom utilities
(load (expand-file-name "src/doom/doom-lib.el" user-emacs-directory))
(load (expand-file-name "src/doom/lib/ui.el" user-emacs-directory))
(load (expand-file-name "src/doom/doom-ui.el" user-emacs-directory))

;; emacs global defaults
;; to suppress org-element-cache warning
(setq warning-suppress-types '((org-element-cache))
      warning-minimum-level :error)

(use-package emacs
  :ensure nil ;; since it's a built in feature
  :custom
  ;;(inhibit-startup-screen t "Open scratch buffer at startup")
  (indent-tabs-mode nil "Stop using tabs to indent")
  (make-backup-files nil "Stop creating ~ backup files")
  (enable-local-variables 't "local variables are customizations in either file or directory")
  ;;(scroll-step 1 "Go one line up or down, not half the screen")
  ;;(scroll-conservatively 10000)
  (global-hl-line-mode 1 "Horizontal current line highlight")
  (create-lockfiles nil "Stop creating .# files")
  (delete-selection-mode 1 "Currently selected text gets deleted when typing new text")
  (text-scale-mode-step 1.1 "Scaling text little aggressive")
  (revert-without-query '(".*") "Revert buffer without asking")
  
  ;; Emacs variables
  :init
  ;;(setq initial-scratch-message "")
  ;;(setq cursor-type 'box)
  ;;(setq-default indent-tabs-mode nil)
  ;;(setq-default tab-width 4)
  ;;(setq-default tab-always-indent nil)
  ;;(setq-default fill-column 80)
  ;;(setq-default word-wrap t)
  ;;(setq-default truncate-lines t)
  ;;(setq truncate-partial-width-windows nil)
  ;;(setq sentence-end-double-space nil)
  ;;(setq require-final-newline t)

  :bind
  ("<f5>" . revert-buffer)  
  :hook
  ((text-mode . visual-line-mode)))

;; setting for undo/redo
(use-package undo
  :ensure nil ;; built in feature, no need to ensure
  :bind
  ("C-z" . undo-only)
  ("C-S-z" . undo-redo))

;; setting for resizing windows
(use-package window
  :ensure nil ;; built in feature, don't need to ensure
  :bind
  (("S-C-<left>" . shrink-window-horizontally)
   ("S-C-<right>" . enlarge-window-horizontally)
   ("S-C-<down>" . shrink-window)
   ("S-C-<up>" . enlarge-window)
   ("C-=" . text-scale-increase)
   ("C--" . text-scale-decrease)))

;; old
;; ;; emacs rotate window layout
;; (elpaca
;;  (rotate
;;   :fetcher github
;;   :repo "daichirata/emacs-rotate"))
;; new
(use-package rotate
  :defer t
  :ensure (:fetcher github :repo "daichirata/emacs-rotate")
  :bind
  ("C-c r" . rotate-layout))

;; old
;; ;; ace-window
;; (elpaca
;;  (ace-window
;;   :fetcher github
;;   :repo "abo-abo/ace-window"))
;; (elpaca-wait)
;; (global-set-key (kbd "C-x o") 'ace-window)
;; new
(use-package ace-window
  :ensure (:fetcher github :repo "abo-abo/ace-window")
  :defer t
  :bind
  ("C-x o" . ace-window))

(use-package autorevert
  :ensure nil ; built-in package
  :config
  (global-auto-revert-mode 1)
  :custom
  (auto-revert-use-notify t)
  :hook
  (dired-mode . auto-revert-mode))


;; NOTE setting font in early-init to avoid jitter/flashing
;;(load-theme 'modus-vivendi t)

(use-package doom-themes
  :ensure (:fetcher github :repo "doomemacs/themes")
  :defer t)



(use-package doom-modeline
  :ensure (:fetcher github :repo "seagle0128/doom-modeline")
  ;;:hook (after-init . doom-modeline-mode)
  :init (doom-modeline-mode 1) ;;NOTE above doesn't work
  :custom
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-major-mode-icon nil)
  :config
  (minions-mode 1)
  (defun doom-modeline-conditional-buffer-encoding ()
    "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
    (setq-local doom-modeline-buffer-encoding
                (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                   '(coding-category-undecided coding-category-utf-8))
                             (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                  t)))
  (add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding))



;; NOTE this is an example of how I was installing and configuring a package
;; but know package needs to wrapped around use-package to achieve the design principles
;; ;; A minor-mode menu for the mode line 
;; ;; tarsius/minions
;; (elpaca
;;  (minions
;;   :fetcher github
;;   :repo "tarsius/minions"))
;; (elpaca-wait)
;; (use-package minions
;;   :hook (doom-modeline-mode . minions-mode))
;; NOTE this is the new style
(use-package minions
  :ensure (:fetcher github :repo "tarsius/minions")
  :defer 0.1
  :after (doom-modeline)
  :hook (doom-modeline-mode . minions-mode))

(use-package hl-todo
  :ensure (:fetcher github :repo "tarsius/hl-todo")
  :defer 0.1
  :custom
  (hl-todo-keyword-faces
   '(("TODO" . "#ff4500")
     ("SOMEDAY" . "#ffff00")
     ("KILL" . "#696969")
     ("DONE" . "#44bc44")
     ("IDEA" . "#ff00ff")
     ("PAPER" . "#ff00ff")
     ("STUDY" . "#ff00ff")
     ("HYPOTHESIS" . "#ff4500")
     ("CODE" . "#ff4500")
     ("WRITE" . "#ffa500")
     ("REVIEW" . "#6ae4b9")

     ("DONT" . "#70b900")
     ("BUG" . "#C70039")
     ("NOTE" . "#d3b55f")
     ("HOLD" . "#c0c530")
     ("HACK" . "#d0bc00")
     ("FAIL" . "#ff8059")
     ("WORKAROUND" . "#ffcccc")
     ("FIXME" . "#ff9077")
     ("DEPRECATED" . "#bfd9ff")
     ("REF" . "#660066"))
   :config
   (global-hl-todo-mode 1))
  :config
  (global-hl-todo-mode 1))

(use-package tramp
  :ensure nil
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))


(use-package recentf
  :ensure nil ; built-in package
  :defer 0.1
  :custom
  (recentf-auto-cleanup 30)
  (recentf-max-saved-items 200)
  :config
  (recentf-mode)
  (run-with-idle-timer 30 t 'recentf-save-list))


(use-package sudo-edit
  :ensure t
  :defer t
  :config (sudo-edit-indicator-mode)
  ;; :bind (:map ctl-x-map
  ;;             ("M-s" . sudo-edit))
  )

(use-package which-key
  :ensure (:fetcher github :repo "justbur/emacs-which-key")
  :defer 0.1
  :config
  (which-key-mode))

(use-package helpful
  :ensure (:fetcher github :repo "Wilfred/helpful")
  :defer t
  :bind
  ;; this looks weird w/ [...]
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-command] . helpful-command)
  :init
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function))




;; smooth scrolling
(use-package mwheel
  :ensure nil
  :custom
  (mouse-wheel-scroll-amount '(1
                               ((shift) . 5)
                               ((control))))
  (mouse-wheel-progressive-speed nil))
(use-package pixel-scroll
  :ensure nil
  :config
  (pixel-scroll-mode))


(use-package dired
  :ensure nil
  ;; NOTE don't work w/ other keybinding in dired-hacks
  ;;:bind
  ;;("C-c o -" . dired-jump)
  :config
  (global-set-key (kbd "C-c o -") 'dired-jump)
  (setq delete-by-moving-to-trash 't)
  (add-hook 'dired-mode-hook #'dired-hide-details-mode))

(use-package dired-hacks
  :ensure (:fetcher github :repo "Fuco1/dired-hacks")
  :defer t
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)
        ("<C-tab>" . dired-subtree-cycle)
        ("<backtab>" . dired-subtree-remove)
        ("e" . ub/ediff-files)
        ("<M-right>" . dired-find-file)
        ("<M-left>" . dired-up-directory))
  :config
  ;; dired extra
  (require 'dired-x)
  ;; TODO write the 
  ;; subtree
  (require 'dired-subtree)
  (setq dired-subtree-use-backgrounds nil)

  ;; TODO move this to autoload?
  (defun ub/ediff-files ()
    (interactive)
    (let ((files (dired-get-marked-files))
          (wnd (current-window-configuration)))
      (if (<= (length files) 2)
          (let ((file1 (car files))
                (file2 (if (cdr files)
                           (cadr files)
                         (read-file-name
                          "file: "
                          (dired-dwim-target-directory)))))
            (if (file-newer-than-file-p file1 file2)
                (ediff-files file2 file1)
              (ediff-files file1 file2))
            (add-hook 'ediff-after-quit-hook-internal
                      (lambda ()
                        (setq ediff-after-quit-hook-internal nil)
                        (set-window-configuration wnd))))
        (error "no more than 2 files should be marked"))))
  (when (eq system-type 'gnu/linux)
    (setq dired-guess-shell-alist-user '(("\\.mp3\\'"  "mpv")
                                         ("\\.mp4\\'"  "mpv")
                                         ("\\.m4a\\'"  "mpv")
                                         ("\\.webm\\'" "mpv")
                                         ("\\.mkv\\'"  "mpv")
                                         ("\\.avi\\'"  "mpv")
                                         ("\\.pdf\\'" "evince")
                                         ("\\.pd\\'"  "evince")
                                         ("\\.dvi\\'" "evince")
                                         ("\\.epub\\'" "ebook-viewer")
                                         ("\\.doc\\'" "libreoffice")
                                         ("\\.docx\\'" "libreoffice")
                                         ("\\.ppt\\'" "libreoffice")
                                         ("\\.pptx\\'" "libreoffice")
                                         ("\\.xls\\'" "libreoffice")
                                         ("\\.xlsx\\'" "libreoffice")
                                         ("\\.odt\\'" "libreoffice")
                                         ("\\.ods\\'" "libreoffice")
                                         ("\\.odg\\'" "libreoffice")
                                         ("\\.odp\\'" "libreoffice")
                                         ("\\.jpg\\'" "eog")
                                         ("\\.jpeg\\'" "eog")
                                         ("\\.png\\'" "eog")
                                         ("\\.gif\\'" "eog")
                                         ("\\.svg\\'" "eog")
                                         ))))


(use-package dired-hide-dotfiles
  :ensure t
  :bind
  (:map dired-mode-map
        ("." . dired-hide-dotfiles-mode))
  :hook
  (dired-mode . dired-hide-dotfiles-mode))

(use-package diredfl
  :ensure t
  :hook
  (dired-mode . diredfl-mode))

(use-package async
  :ensure t
  :defer t
  :custom
  (dired-async-mode 1))

(use-package dired-rsync
  :ensure t
  :bind
  (:map dired-mode-map
        ("r" . dired-rsync)))

(use-package dired-launch
  :ensure t
  :hook
  (dired-mode . dired-launch-mode))

(use-package dired-git-info
  :ensure t
  :bind
  (:map dired-mode-map
        (")" . dired-git-info-mode)))

;; TODO
;; (use-package dired-recent
;;   :ensure t
;;   :bind
;;   (:map
;;    dired-recent-mode-map ("C-x C-d" . nil))
;;   :config
;;   (dired-recent-mode 1))

;; jojojames/dired-sidebar
;; (use-package dired-subtree
;;   )
;;(require 'dired-x)
;;(require 'dired-subtree)
;; TODO dired-sidebar dependes on dired-subtree which is already installed by dired-hacks
;; (use-package dired-sidebar
;;   :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
;;   :after dired-subtree
;;   :ensure t
;;   :commands (dired-sidebar-toggle-sidebar)
;;   :init
;;   (add-hook 'dired-sidebar-mode-hook
;;             (lambda ()
;;               (unless (file-remote-p default-directory)
;;                 (auto-revert-mode))))
;;   :config
;;   (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
;;   (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

;;   (setq dired-sidebar-subtree-line-prefix "__")
;;   (setq dired-sidebar-theme 'vscode)
;;   (setq dired-sidebar-use-term-integration t)
;;   (setq dired-sidebar-use-custom-font t))



(use-package real-auto-save
  :ensure (:fetcher github :repo "ChillarAnand/real-auto-save")
  :defer 0.1
  :config
  (add-hook 'prog-mode-hook 'real-auto-save-mode)
  (add-hook 'org-mode-hook 'real-auto-save-mode)
  (setq real-auto-save-interval 5))


(use-package vterm
  :ensure (:fetcher github :repo "akermu/emacs-libvterm")
  :defer 0.1
  :config
  (setq vterm-max-scrollback 10000)
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil))))


(use-package transient
  :ensure (:fetcher github :repo "magit/transient")
  :defer t)
(use-package magit
  :ensure (:fetcher github :repo "magit/magit")
  :defer t
  :bind
  ("C-x g" . magit-status))
(load-file (expand-file-name "src/commit-emoji.el" user-emacs-directory))


(use-package git-timemachine
  :ensure t
  :defer t)


(use-package drag-stuff
  :ensure (:fetcher github :repo "rejeep/drag-stuff.el")
  :after org
  :config
  (define-key drag-stuff-mode-map (drag-stuff--kbd 'up) 'drag-stuff-up)
  (define-key drag-stuff-mode-map (drag-stuff--kbd 'down) 'drag-stuff-down)
  (drag-stuff-global-mode t)
  (defun ub/org-drag-up ()
    (interactive)
    (call-interactively
     (if (org-at-heading-p)
         'org-metaup
       'drag-stuff-up)))
  (defun ub/org-drag-down ()
    (interactive)
    (call-interactively
     (if (org-at-heading-p)
         'org-metadown
       'drag-stuff-down)))
  ;; override drag-stuff keybindings w/ ub/org-drag... when in org-mode
  (add-hook 'org-mode-hook
            (lambda ()
              (define-key drag-stuff-mode-map (drag-stuff--kbd 'up) 'ub/org-drag-up)
              (define-key drag-stuff-mode-map (drag-stuff--kbd 'down) 'ub/org-drag-down))))




(use-package emacs
  :ensure nil
  :custom
  (shell-file-name "/usr/bin/zsh")
  (shell-command-switch "-c"))


;; Not needed atm since I use local-dir and local env. vars.
;;old
;; ;; "Ever find that a command works in your shell, but not in Emacs?" - Oh yeah!
;; (elpaca
;;     (exec-path-from-shell
;;      :repo "purcell/exec-path-from-shell"
;;      :fetcher github))
;; (elpaca-wait)
;; (when (memq window-system '(mac ns x)) ;; if you're in the GUI
;;   ;; You might have already installed exec-path-from-shell
;;   (require 'exec-path-from-shell)
;;   ;; Append any paths you would like to import here:
;;   (dolist (var '(
;;                  ;;"LD_LIBRARY_PATH" "PYTHONPATH"
;;                  ;;"SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"
;;                  ))
;;     (add-to-list 'exec-path-from-shell-variables var))
;;   (exec-path-from-shell-initialize))
;; (when (daemonp) ;; if you're in the emacs-client
;;     (exec-path-from-shell-initialize)) 
;;new
;; (use-package exec-path-from-shell
;;   :ensure (:fetcher github :repo "purcell/exec-path-from-shell")
;;   :defer 0.1
;;   :config
;;   (when (memq window-system '(mac ns x)))
;;  )


;; load python.el
(load (expand-file-name "src/python.el" user-emacs-directory))



;; ;; REF lisp/lib/ui.el
;;;###autoload
(defun doom/toggle-line-numbers ()
  "Toggle line numbers.
Cycles through regular, relative and no line numbers. The order depends on what
`display-line-numbers-type' is set to. If you're using Emacs 26+, and
visual-line-mode is on, this skips relative and uses visual instead.
See `display-line-numbers' for what these values mean."
  (interactive)
  (require 'display-line-numbers)
  (defvar doom--line-number-style display-line-numbers-type)
  (let* ((styles `(t ,(if visual-line-mode 'visual 'relative) nil))
         (order (cons display-line-numbers-type (remq display-line-numbers-type styles)))
         (queue (memq doom--line-number-style order))
         (next (if (= (length queue) 1)
                   (car order)
                 (car (cdr queue)))))
    (setq doom--line-number-style next)
    (setq display-line-numbers next)
    (message "Switched to %s line numbers"
             (pcase next
               (`t "normal")
               (`nil "disabled")
               (_ (symbol-name next))))))
(use-package display-line-numbers
  :ensure nil
  :bind
  ("C-c l" . doom/toggle-line-numbers))


(use-package aggressive-indent
  :ensure (:fetcher github :repo "Malabarba/aggressive-indent-mode")
  :defer 0.1
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))


(use-package pdf-tools
  :ensure (:fetcher github :repo "vedang/pdf-tools")
  :defer 0.1
  :config
  (pdf-loader-install))


;; ;; create realgud:pdb function that asks a file from user to debug instead of current file
;; (defun ub/realgud:pdb (file)
;;   "Run pdb on a file."
;;   (interactive "fFile to debug: ")
;;   (realgud:pdb file))

(use-package realgud
  :ensure (:fetcher github :repo "realgud/realgud")
  :defer 0.1
  ;; when in python-mode set key C-c d d to realgud:pdb
  :bind
  (:map python-mode-map
        ("C-c d d" . realgud:pdb)
        ;;("C-c d f" . ub/realgud:pdb)
        )
  :custom
  (realgud-safe-mode nil))




;; old
;; (elpaca
;;     (yasnippet
;;      :fetcher github
;;      :repo "joaotavora/yasnippet"))
;; (elpaca-wait)
;; (require 'yasnippet)
;; (yas-global-mode 1)
;; (elpaca
;;     (doom-snippets
;;      :fetcher github
;;      :repo "doomemacs/snippets"))
;; (setq doom-snippets-dir
;;       (expand-file-name "repos/snippets" elpaca-directory))
;; new
;; TODO feel that it slow to load
(use-package yasnippet
  :ensure (:fetcher github :repo "joaotavora/yasnippet")
  ;;:defer 0.1 ;; make startup UI flashy
  :config
  (yas-global-mode 1))
;; TODO
(use-package doom-snippets
  :ensure (:fetcher github :repo "doomemacs/snippets")
  :after yasnippet
  ;;:defer 0.1
  :custom
  (doom-snippets-dir
   (expand-file-name "repos/snippets" elpaca-directory))
  :config
  (yas-reload-all))





(use-package activity-watch-mode
  :ensure (:fetcher github :repo "pauldub/activity-watch-mode")
  :defer 0.1
  :config
  (global-activity-watch-mode))


(use-package popper
  :ensure (:fetcher github :repo "karthink/popper")
  :defer 0.1
  :bind
  (("C-`" . popper-toggle)
   ("M-`" . popper-cycle)
   ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          ;; *Warnings*
          ;; NOTE have a feeling that it doesn't its job properly
          ;; causes double window opening with the same buffer
          ("\\*Warnings\\*" . hide)
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints




(use-package markdown-mode
  :ensure t
  :ensure-system-package markdown
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode))
  :custom
  (markdown-command "markdown"))



;; ;; NOTE globally setting golden ratio doesn't generalize to all buffer so disabling for a now
;; ;; golden-ratio-inhibit-functions seems to disable after you jump to that particular buffer
;; ;; but it is still keeping excluded buffers in small resized size.
;; ;; so disabling for a now...
;; ;; functions to inhibit for golden-ratio not to resize the buffers
;; ;; ediff
;; (defun ediff-comparison-buffer-p ()
;;   (string-match-p "\*Ediff" (buffer-name)))
;; ;; dired
;; (defun dired-mode-p ()
;;   (eq major-mode 'dired-mode))

;; (use-package golden-ratio
;;   :ensure (:fetcher github :repo "roman/golden-ratio.el")
;;   :defer 0.1
;;   :config
;;   (golden-ratio-mode 1)
;;   (add-to-list 'golden-ratio-extra-commands 'ace-window)
;;   ;;(setq golden-ratio-exclude-modes '(dired-mode ediff-mode))
;;   (setq golden-ratio-inhibit-functions '(ediff-comparison-buffer-p dired-mode-p))
;;   )


(use-package spacious-padding
  :ensure (:fetcher github :repo "protesilaos/spacious-padding")
  :defer 0.1
  :custom
  ;; new
  (spacious-padding-widths
   '( :internal-border-width 15
      :header-line-width 4
      :mode-line-width 4
      :tab-width 4
      :right-divider-width 30
      :scroll-bar-width 8
      :fringe-width 8))
  :config
  (spacious-padding-mode 1))


;; TODO how to place modeline to top?
;; REF https://github.com/seagle0128/doom-modeline/issues/646
;; seems like in emacs 30 new stuff coming for faces
;; also mentioned by protesilaos
;; (setq-default header-line-format mode-line-format)
;; (setq-default mode-line-format nil)
;; (setq-default header-line-format (setup-custom-doom-modeline))
;; (setq-default mode-line-format nil)



;; ;; hlissner/emacs-solaire-mode
;; ;; TODO doesn't work w/ modus but does w/ doom-themes
;; (use-package solaire-mode
;;   :ensure (:fetcher github :repo "hlissner/emacs-solaire-mode")
;;   :defer 0.1
;;   :config
;;   (solaire-global-mode +1))



(load-file (expand-file-name "src/credentials.el" user-emacs-directory))


(use-package chatgpt-shell
  :ensure (:fetcher github :repo "xenodium/chatgpt-shell")
  :defer t
  :custom
  (;(shell-maker-logging t)
   ;; set default model
   ;; ("gpt-4-0125-preview" "gpt-4-turbo-preview" "gpt-4-1106-preview" "gpt-4-0613" "gpt-4" "gpt-3.5-turbo-16k-0613" "gpt-3.5-turbo-16k" "gpt-3.5-turbo-0613" "gpt-3.5-turbo")
   (chatgpt-shell-model-version 4)
   (chatgpt-shell-additional-curl-options '("--http1.1"))
   (chatgpt-shell-openai-key (ub/load-key-openai-token))))
;; NOTE DEBUGGING the package
;; (load-file "/home/bolatu/emacs-configs/elpaca-done-right_29.2.50/repos/chatgpt-shell/shell-maker.el")
;; (load-file "/home/bolatu/emacs-configs/elpaca-done-right_29.2.50/repos/chatgpt-shell/chatgpt-shell.el")
;; (setq shell-maker-logging t)
;; (setq chatgpt-shell-additional-curl-options '("--http1.1"))
;; (setq chatgpt-shell-openai-key (ub/load-key-openai-token))

(use-package csv-mode
  :ensure t
  :defer t
  :mode
  (("\\.[Cc][Ss][Vv]\\'" . csv-mode)))

(use-package sh-script
  :ensure nil
  :mode (("zshecl" . sh-mode)
         ("\\.zshrc\\'" . sh-mode)
         ("\\.zsh\\'" . sh-mode))
  :custom
  ;; zsh
  (system-uses-terminfo nil))



;; TODO add feature to run multiple quickrun buffers...
(use-package quickrun
  :ensure (:fetcher github :repo "emacsorphanage/quickrun")
  :defer t
  :custom
  (quickrun-focus-p nil)
  (quickrun-timeout-seconds nil)
  :bind
  ("C-c q r" . quickrun)
  :config
  (quickrun-add-command "python"
    '((:command . "python3")
      (:exec . "%c %s")
      (:tempfile . nil)))  
  )

;; renzmann/treesit-auto
(use-package treesit-auto
  :ensure (:fetcher github :repo "renzmann/treesit-auto")
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  ;;(global-tree-sitter-mode 1)
  (global-treesit-auto-mode))



;; jdtsmith/indent-bars
;; (use-package indent-bars
;;   :ensure (:fetcher github :repo "jdtsmith/indent-bars")
;;   :defer 0.1
;;   :hook ((python-mode yaml-mode) . indent-bars-mode)
:config
(setq indent-bars-width-frac 0.2)
(use-package indent-bars
  :ensure (:fetcher github :repo "jdtsmith/indent-bars")
  :config
  (setq indent-bars-width-frac 0.2)
  :after treesit
  :defer 0.1
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
				                      list list_comprehension
				                      dictionary dictionary_comprehension
				                      parenthesized_expression subscript)))
  :hook ((python-base-mode yaml-mode) . indent-bars-mode))




;; ;; fold code w/ treesit
;; ;; emacs-tree-sitter/ts-fold
;; ;; this works with https://github.com/emacs-tree-sitter/elisp-tree-sitter
;; ;; which is different from the built-in package 'treesit'
;; ;; REF see the issue: https://github.com/emacs-tree-sitter/ts-fold/issues/48
;; (use-package ts-fold
;;   :ensure (:fetcher github :repo "emacs-tree-sitter/ts-fold")
;;   ;;:after treesit
;;   ;;:defer 0.1
;;   ;; global-ts-fold-mode
;;   :config
;;   (global-ts-fold-mode t)
;;   )


(load-file (expand-file-name "src/completions.el" user-emacs-directory))

(load-file (expand-file-name "src/org-mode.el" user-emacs-directory))

(load-file (expand-file-name "src/bib.el" user-emacs-directory))
