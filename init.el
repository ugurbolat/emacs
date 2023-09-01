(message "This is emacs vanilla config!")

;; loading paths
(let ((file-path (expand-file-name "~/emacs-configs/emacs-vanilla")))
  (when (file-exists-p file-path)
    (setq ub/emacs-dir file-path)))
(setq ub/emacs-dir (file-truename ub/emacs-dir))
(load-file (expand-file-name "paths.el" ub/emacs-dir))
(load-file (expand-file-name "credentials.el" ub/emacs-dir))

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

;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; use-package
(straight-use-package 'use-package)
(use-package straight
  :custom
  (straight-use-package-by-default t))

;; traditional strip-down 
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
:bat:
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
(global-hl-line-mode 1) ;; horizontal current line highlight
(setq cursor-type 'box)

;; restart emacs within emacs

;;;;;; theme
;; prot has additional themes
(straight-use-package
 '(modus-themes :type git :host github :repo "protesilaos/modus-themes"))
(setq modus-themes-headings
      (quote
       ((1 . (variable-pitch bold 1.5))
        (2 . (variable-pitch semibold 1.3))
        (3 . (variable-pitch 1.2))
        (4 . (variable-pitch 1.15))
        (5 . (variable-pitch 1.10))
        (6 . (variable-pitch 1.05))
        (7 . (variable-pitch 1.04))
        (8 . (variable-pitch 1.03))
        (9 . (variable-pitch 1.02))
        (t . (monochrome)))))
(load-theme 'modus-vivendi-tinted t)

(straight-use-package
 '(restart-emacs :type git :host github :repo "iqbalansari/restart-emacs"))

;; font
(set-face-attribute 'default (selected-frame) :height 160)

;; margins & fringe
;; basic
;; (setq-default left-margin-width 10 right-margin-width 10) ; Define new widths.
;; (set-window-buffer nil (current-buffer)) ; Use them now.

;; smart conditional margins
(defun set-window-margins-based-on-size ()
  "Set margins width based on window size."
  (walk-windows (lambda (window)
                  (with-selected-window window
                    (let* ((frame-char-width (frame-width))
			   (window-char-width (window-width))
			   (min-char-width (min window-char-width frame-char-width))
                           (each-side-margin (max 1 (/ (- min-char-width 120) 2)) ))
                      (if (>= each-side-margin 10)
                          ;; For large window width.
                          (setq left-margin-width 10 right-margin-width 10)
                        ;; For small window width.
                        (setq left-margin-width each-side-margin right-margin-width each-side-margin))
                      (set-window-buffer window (current-buffer)))))))

(add-hook 'window-configuration-change-hook #'set-window-margins-based-on-size)

(setq fringe-mode 'default)
(set-fringe-style (quote (12 . 8)))

;; emoji: bring some life/fun into the emacs
;; TODO github emojis are not displayed
(straight-use-package
 '(ht :type git :host github :repo "Wilfred/ht.el")) ;; requirements for emoji
(straight-use-package
 '(emacs-emojify :type git :host github :repo "iqbalansari/emacs-emojify"))
;; search data dir on build folder but it's not copied
(let* ((dst-dir "~/emacs-configs/emacs-vanilla/straight/build/emacs-emojify/data/")
       (src-dir "~/emacs-configs/emacs-vanilla/straight/repos/emacs-emojify/data/"))
  (unless (file-directory-p dst-dir)
    (copy-directory src-dir dst-dir nil nil t)))
;;(straight-use-package 'emacs-emojify)
(require 'emojify)
(add-hook 'after-init-hook #'global-emojify-mode)

(straight-use-package
 '(hl-todo :type git :host github :repo "tarsius/hl-todo"))
(global-hl-todo-mode 1)
(setq hl-todo-keyword-faces
      '(("TODO" . "#ff4500")
	("DONT" . "#70b900")
	("NEXT" . "#b6a0ff")
	("BUG" . "#C70039")
	("DONE" . "#44bc44")
	("NOTE" . "#d3b55f")
	("HOLD" . "#c0c530")
	("HACK" . "#d0bc00")
	("FAIL" . "#ff8059")
	("WORKAROUND" . "#ffcccc")
	("FIXME" . "#ff9077")
	("REVIEW" . "#6ae4b9")
	("DEPRECATED" . "#bfd9ff")
	("REF" . "#660066")))


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
(straight-use-package
 '(emacs-libvterm :type git :host github :repo "akermu/emacs-libvterm"))
;; (straight-use-package
;;  '(vterm :source melpa))

;; magit
(straight-use-package
 '(magit :type git :host github :repo "magit/magit"))
(defun ub/insert-commit-prefix-w-emoji ()
  (interactive)
  (let* ((string-list '(":tada: init: "
			":construction: wip: "
                        ":christmas-tree: christmas tree bill (torba yasa)"
			":bookmark: tag: "
			":sparkles: feat: "
			":bug: fix: "
			":books: docs: "
			":lipstick: style: "
			":hammer: refactor: "
			":rotating_light: test: "
			":smiling-imp: customize:"
			":wrench: chore:"
			":ok_hand: review: "
			":card_index: meta: "
			;;":bulb: source: "
			":racehorse: perf: "
			":white_check_mark: addTest: "
			":heavy_check_mark: passTest: "
			":zap: update: "
			":art: fmt: "
			":fire: remove: "
			":truck: move: "
			":green_heart: ci: "
			":lock: sec: "
			":arrow_up: upDep: "
			":arrow_down: downDep: "
			":shirt: lint: "
			;;":alien: i18n: "
			;;":pencil: txt: "
			":ambulance: hotfix: "
			":rocket: deploy: "
			":apple: fixMac: "
			":penguin: fixLinux: "
			":checkered_flag: fixWin: "
			":construction_worker: ciBuild: "
			":chart_with_upwards_trend: analytics: "
			":heavy_minus_sign: removeDep: "
			":heavy_plus_sign: addDep: "
			":whale: docker: "
			;;":wrench: config: "
			;;":package: pkgJson: "
			":twisted_rightwards_arrows: merge: "
			":hankey: badCode: "
			":rewind: revert: "
			":boom: breaking: "
			;;":wheelchair: a11y: "
			))
         (selected-string (completing-read "Select a string: " string-list)))
    (insert selected-string)))


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

;; M-up M-down for drag up/down line regularly
(straight-use-package
 '(drag-stuff :type git :host github :repo "rejeep/drag-stuff.el"))
(require 'drag-stuff)
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)


;; doom's +popup extracted as a package
;; "Tame sudden yet inevitable temporary windows"
(straight-use-package
 '(emacs-hide-mode-line :type git :host github :repo "hlissner/emacs-hide-mode-line"))
(require 'hide-mode-line)
;; (straight-use-package
;;  '(emacs-popup-mode :type git :host github :repo "aaronjensen/emacs-popup-mode"))
(use-package popup-mode
  ;;:demand t
  :straight (popup-mode :host github :repo "aaronjensen/emacs-popup-mode")
  :hook (after-init . +popup-mode)

  :bind (("C-`" . +popup/toggle))
  :config
  (set-popup-rules! '(("^\\*Process List\\*$"
                       :side bottom :select t :slot -1 :vslot -1 :size +popup-shrink-to-fit)
                      ("^\\*Buffer List\\*$"
                       :side bottom :select t :slot -1 :vslot -1 :size +popup-shrink-to-fit)
		      ("^\\*vterm" :size 0.25 :vslot -4 :select t :quit nil :ttl 0)
		      ("^\\*eshell" :size 0.25 :vslot -4 :select t :quit nil :ttl 0)
		      ("^\\*quickrun" :select nil :slot -1 :vslot -1 :size 0.3 :ttl 0)
		      ("^\\*xref"
		       :side bottom :select t :slot -1 :vslot -1 :size +popup-shrink-to-fit)
		      ("^\\*ielm"
		       :side bottom :select t :slot -1 :vslot -1 :height 0.25 :quit nil :ttl nil)
		      ("^\\*Python"
		       :side right :select t :slot -1 :vslot -1 :width 0.5 :quit nil :ttl nil)
                      ("^\\*Messages\\*$"
                       :side bottom :select t :slot -1 :vslot -1 :height 0.3 :ttl nil)
		      )))

;; TODO fails to re-open last popup after toggling always opens the message...

;;;;;;;;;;;;;; search engines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file (expand-file-name "search-engines.el" ub/emacs-dir))

;;;;;;; org mode
(load-file (expand-file-name "org-mode.el" ub/emacs-dir))


;;;;;;; latex
(load-file (expand-file-name "latex.el" ub/emacs-dir))

;;;;;;; lsp and languages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
(use-package popup-mode
  ;;:demand t
  :straight (realgud :type git :host github :repo "realgud/realgud")
  :config
  (setq realgud-safe-mode nil))
;;(require 'realgud)
;;(setq realgud-safe-mode nil)
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

;; achieving similar to eval-last-sexp for python or other interactive langs
(straight-use-package
 '(eval-in-repl :type git :host github :repo "kaz-yos/eval-in-repl"))
(require 'eval-in-repl)
(setq eir-repl-placement 'right)

(require 'python) ; if not done elsewhere
(require 'eval-in-repl-python)
(add-hook 'python-mode-hook
          '(lambda ()
             (local-set-key (kbd "<C-return>") 'eir-eval-in-python)))

;; yasnippet
(straight-use-package
 '(yasnippet :type git :host github :repo "joaotavora/yasnippet"))
(require 'yasnippet)
(yas-global-mode 1)

;;;;;;;;;; hydra ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(straight-use-package
 '(hydra :type git :host github :repo "abo-abo/hydra"))
(load-file (expand-file-name "hydra.el" ub/emacs-dir))

;;;;;;;;;; doom goodies
(load-file (expand-file-name "doom-goodies.el" ub/emacs-dir))

;;;;;;;;;; gpt, etc.
(load-file (expand-file-name "llm.el" ub/emacs-dir))

;;;;;;;;;;; keybindings
(load-file (expand-file-name "keybindings.el" ub/emacs-dir))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4b026ac68a1aa4d1a91879b64f54c2490b4ecad8b64de5b1865bca0addd053d9" default)))
