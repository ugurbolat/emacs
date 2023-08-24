(message "This is emacs vanilla config!")

;; loading paths
(let ((file-path (expand-file-name "~/emacs-configs/emacs-vanilla")))
  (when (file-exists-p file-path)
    (setq ub/emacs-dir file-path)))
(setq ub/emacs-dir (file-truename ub/emacs-dir))
(load-file (expand-file-name "paths.el" ub/emacs-dir))

;; https://github.com/radian-software/straight.el#bootstrapping-straightel
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package
(straight-use-package 'use-package)
;;(setq straight-use-package-by-default t)

;; traditional strip-down 
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; good defaults
(delete-selection-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t) ; open scratch buffer at startup
(setq initial-scratch-message "")
(setq indent-tabs-mode nil) ; stop using tabs to indent
(setq make-backup-files nil) ; stop creating ~ backup files
(require 'autorevert)
(global-auto-revert-mode 1) ; load recent changes done outside
(setq auto-revert-use-notify t) ; and notify
(add-hook 'dired-mode-hook 'auto-revert-mode)
(setq enable-local-variables 't) ; local variables are customizations in either file or directory
(setq scroll-step 1
      scroll-conservatively 10000) ;; better scroll: go one line up or down, not half the screen.

;; restart emacs within emacs
(straight-use-package
 '(restart-emacs :type git :host github :repo "iqbalansari/restart-emacs"))

;;;;;; theme
(load-theme 'modus-vivendi t)

;; font
(set-face-attribute 'default (selected-frame) :height 160)


;; margins & fringe
(setq-default left-margin-width 2 right-margin-width 2) ; Define new widths.
(set-window-buffer nil (current-buffer)) ; Use them now.

;; TODO kills the fringes not so perfect after all...
;; (straight-use-package
;;  '(perfect-margin :type git :host github :repo "mpwang/perfect-margin"))
;; (require 'perfect-margin)
;; (perfect-margin-mode 1)
;; (setq perfect-margin-visible-width 140)
;; (setq perfect-margin-hide-fringes nil) ;; it doesn't work..

(setq fringe-mode 'default)
(set-fringe-style (quote (12 . 8)))

;; (straight-use-package
;;  '(emacs-modern-fringes :type git :host github :repo "SpecialBomb/emacs-modern-fringes"))
;; (modern-fringes-mode)
;; (modern-fringes-invert-arrows)

;; window management
(straight-use-package
 '(emacs-rotate :type git :host github :repo "daichirata/emacs-rotate"))

;; modeline
(straight-use-package
 '(nerd-icons :type git :host github :repo "rainstormstudio/nerd-icons.el"))
(require 'nerd-icons)
;; TODO check if not installed
;;(nerd-icons-install-fonts)
(straight-use-package
 '(doom-modeline :type git :host github :repo "seagle0128/doom-modeline"))
(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-project-detection 'auto)
(setq doom-modeline-env-enable-python t)
(setq doom-modeline-env-python-executable "python")
(add-hook 'after-init-hook #'doom-modeline-mode)
(add-hook 'emacs-startup-hook #'(lambda () (size-indication-mode -1)) 'append)


;; doom-modeline-conditional-buffer-encoding
(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                 '(coding-category-undecided coding-category-utf-8))
                           (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                t)))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

;; minions
(straight-use-package
 '(minions :type git :host github :repo "tarsius/minions"))
(setq doom-modeline-minor-modes t)
(minions-mode 1)

;; which-key
(straight-use-package
 '(emacs-which-key :type git :host github :repo "justbur/emacs-which-key"))
(require 'which-key)
(which-key-mode)

;; legendary helpful package!
(straight-use-package
 '(helpful :type git :host github :repo "Wilfred/helpful"))

;; projectile
(straight-use-package
 '(projectile :type git :host github :repo "bbatsov/projectile"))
(require 'projectile)
(projectile-mode +1)
;; TODO
(setq projectile-project-search-path `(,ub/org-root-dir))

;; dired
(require 'dired)
(setq delete-by-moving-to-trash 't)
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; dired extra
(require 'dired-x)
(when (eq system-type 'gnu/linux)
  (setq dired-guess-shell-alist-user '(("\\.mp3\\'"  "mpv")
                                       ("\\.mp4\\'"  "mpv")
                                       ("\\.m4a\\'"  "mpv")
                                       ("\\.webm\\'" "mpv")
                                       ("\\.mkv\\'"  "mpv")
                                       ("\\.avi\\'"  "mpv")

                                       ("\\.pdf\\'" "okular")
                                       ("\\.pd\\'"  "okular")
                                       ("\\.dvi\\'" "okular")

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
                                       )))
;; dired subtree
(straight-use-package
 '(dired-hacks :type git :host github :repo "Fuco1/dired-hacks"))
(require 'dired-subtree)
(setq dired-subtree-use-backgrounds nil)

;; dired ediff marked files
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

;; real-auto-save
(straight-use-package
 '(real-auto-save :type git :host github :repo "ChillarAnand/real-auto-save"))
(require 'real-auto-save)
(add-hook 'prog-mode-hook 'real-auto-save-mode)
(add-hook 'org-mode-hook 'real-auto-save-mode)
(setq real-auto-save-interval 60)

;; ace-window
(straight-use-package
 '(ace-window :type git :host github :repo "abo-abo/ace-window"))

;; aggressive indent
(straight-use-package
 '(aggressive-indent-mode :type git :host github :repo "Malabarba/aggressive-indent-mode"))
(require 'aggressive-indent)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

;; vterm
;; (straight-use-package
;;  '(emacs-libvterm :type git :host github :repo "akermu/emacs-libvterm"))
(straight-use-package
 '(vterm :source melpa))

;; magit
(straight-use-package
 '(magit :type git :host github :repo "magit/magit"))


;; pdf-tools
(straight-use-package
 '(pdf-tools :type git :host github :repo "vedang/pdf-tools"))
(require 'pdf-tools)
(pdf-tools-install)
(setq pdf-annot-default-annotation-properties
      '((t (label . "Ugur Bolat"))
        (text (color . "gold") (icon . "Note"))
        (highlight (color . "orange"))
        (underline (color . "blue"))
        (squiggly (color . "green"))
        (strike-out (color . "red"))))

(add-hook 'pdf-view-mode-hook
          (lambda ()
	    (perfect-margin-mode -1)))


;; note that flycheck is used in consult-flycheck which is search-engines.el
(straight-use-package
 '(flycheck :type git :host github :repo "flycheck/flycheck"))
(use-package flycheck
  ;;:ensure t
  :init (global-flycheck-mode))

;; company completion
(straight-use-package
 '(company-mode :type git :host github :repo "company-mode/company-mode"))
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay nil)
(setq company-minimum-prefix-length 1)

;;;;;;;;;;;;;; search engines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file (expand-file-name "search-engines.el" ub/emacs-dir))

;;;;;;; org mode
(load-file (expand-file-name "org-mode.el" ub/emacs-dir))


;;;;;;; latex
(load-file (expand-file-name "latex.el" ub/emacs-dir))

;;;;;;; lsp and languages
;; TODO not stable, especially dap-debug
;;(load-file (expand-file-name "lsp.el" ub/emacs-dir))
(load-file (expand-file-name "python.el" ub/emacs-dir))

(straight-use-package
 '(eglot :type git :host github :repo "joaotavora/eglot"))

(straight-use-package
 '(consult-eglot :type git :host github :repo "mohkale/consult-eglot"))

(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; setting same face w/ eglot highlight
(custom-set-faces
 '(eglot-highlight-symbol-face ((t (:foreground "#ffffff" :background "#10387c")))))

;; TODO doesn't work
;; (with-eval-after-load "modus-operandi"
;;   (custom-set-faces
;;    '(eglot-highlight-symbol-face ((t (:foreground "red" :background "#d3d3d3")))))) ; colors for light background

;; (with-eval-after-load "modus-vivendi"
;;   (custom-set-faces
;;    '(eglot-highlight-symbol-face ((t (:foreground "#ffffff" :background "#10387c")))))) ; colors for dark background

;; (custom-set-faces
;;  '(embark-target ((t (:foreground "blue" :background "red")))))

(defun ub/enable-eglot ()
  "Enable eglot in the current Python buffer after pyvenv activation."
  (interactive)
  (when (derived-mode-p 'python-mode)
    (eglot-ensure)))

(if (featurep 'pyvenv)
    (add-hook 'pyvenv-post-activate-hooks #'ub/enable-eglot)
  (message "pyvenv is not installed so cannot hook eglot to pyvenv"))

;; debugger
(straight-use-package
 '(realgud :type git :host github :repo "realgud/realgud"))
(require 'realgud)
(setq realgud-safe-mode nil)
;; TODO currently cannot activate tab completion...
;; (straight-use-package
;;  '(realgud-ipdb :type git :host github :repo "realgud/realgud-ipdb"))
;; (require 'realgud-ipdb)
;; (setq realgud--ipdb-command-name "python -m ipdb")
(straight-use-package
 '(emacs-pdb-capf :type git :host github :repo "ugurbolat/emacs-pdb-capf"))
(require 'pdb-capf)
(add-hook 'pdb-track-mode-hook
	  (lambda ()
	    (add-hook 'completion-at-point-functions
                      'pdb-capf nil t)))
(straight-use-package
 '(quickrun :type git :host github :repo "emacsorphanage/quickrun"))
(require 'quickrun)
;; TODO integrate with vterm, only eshell support for terminal


;;;;;;;;;; hydra
(straight-use-package
 '(hydra :type git :host github :repo "abo-abo/hydra"))
(load-file (expand-file-name "hydra.el" ub/emacs-dir))

;;;;;;;;;; doom goodies
(load-file (expand-file-name "doom-goodies.el" ub/emacs-dir))

;;;;;;;;;;; keybindings
(load-file (expand-file-name "keybindings.el" ub/emacs-dir))

