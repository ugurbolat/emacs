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



;; traditional strip-down 
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; good defaults
(delete-selection-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t) ; open scratch buffer at startup 
(setq make-backup-files nil) ; stop creating ~ backup files

;;;;;; theme
(load-theme 'modus-vivendi t)

;; font
(set-face-attribute 'default (selected-frame) :height 150)


;; margins
(straight-use-package
 '(perfect-margin :type git :host github :repo "mpwang/perfect-margin"))
(require 'perfect-margin)
(perfect-margin-mode 1)
(setq perfect-margin-visible-width 140)


(straight-use-package
 '(nerd-icons :type git :host github :repo "rainstormstudio/nerd-icons.el"))
(require 'nerd-icons)
;; TODO check if not installed
;;(nerd-icons-install-fonts)
(straight-use-package
 '(doom-modeline :type git :host github :repo "seagle0128/doom-modeline"))
(require 'doom-modeline)
(setq doom-modeline-project-detection 'auto)
(setq doom-modeline-env-enable-python t)
(setq doom-modeline-env-python-executable "python")
(add-hook 'after-init-hook #'doom-modeline-mode)
(add-hook 'emacs-startup-hook #'(lambda () (size-indication-mode -1)) 'append)


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


;;;;;;; search and find packages
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


;;;;;;; org mode
(load-file (expand-file-name "org-mode.el" ub/emacs-dir))


;;;;;;;;;; hydra
(straight-use-package
 '(hydra :type git :host github :repo "abo-abo/hydra"))
(load-file (expand-file-name "hydra.el" ub/emacs-dir))


;;;;;;;;;;; keybindings
(load-file (expand-file-name "keybindings.el" ub/emacs-dir))

