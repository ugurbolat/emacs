
;; undo/redo madness
(global-set-key (kbd "C-z") 'undo-only)
(global-set-key (kbd "C-S-z") 'undo-redo)

;; restart emacs
(global-set-key (kbd "C-c q r") 'ub/restart-emacs)

;; zoom in/out/increase font
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; projectile
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; helpful
;; Note that the built-in `describe-function' includes both functions
;; and macros. `helpful-function' is functions only, so we provide
;; `helpful-callable' as a drop-in replacement.
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)

;; dired
(global-set-key (kbd "C-c o .") 'dired-jump)
(let ((map dired-mode-map))
  (define-key map (kbd "<tab>") #'dired-subtree-toggle)
  (define-key map (kbd "<C-tab>") #'dired-subtree-cycle)
  (define-key map (kbd "<backtab>") #'dired-subtree-remove))
(define-key dired-mode-map "e" 'ub/ediff-files)

;; ace-window
(global-set-key (kbd "C-x o") 'ace-window)


;; pdf-tools
;; TODO doesn't work
(let ((map pdf-view-mode-map)) 
(define-key map (kbd "<s-spc>")  #'pdf-view-scroll-down-or-next-page)
(define-key map (kbd "g"      )  #'pdf-view-first-page)
(define-key map (kbd "G"      )  #'pdf-view-last-page)
(define-key map (kbd "l"      )  #'image-forward-hscroll)
(define-key map (kbd "h"      )  #'image-backward-hscroll)
(define-key map (kbd "j"      )  #'pdf-view-next-page)
(define-key map (kbd "k"      )  #'pdf-view-previous-page)
(define-key map (kbd "e"      )  #'pdf-view-goto-page)
(define-key map (kbd "u"      )  #'pdf-view-revert-buffer)
(define-key map (kbd "al"     )  #'pdf-annot-list-annotations)
(define-key map (kbd "ad"     )  #'pdf-annot-delete)
(define-key map (kbd "aa"     )  #'pdf-annot-attachment-dired)
(define-key map (kbd "am"     )  #'pdf-annot-add-markup-annotation)
(define-key map (kbd "at"     )  #'pdf-annot-add-text-annotation)
(define-key map (kbd "y"      )  #'pdf-view-kill-ring-save)
(define-key map (kbd "i"      )  #'pdf-misc-display-metadata)
(define-key map (kbd "s"      )  #'pdf-occur)
(define-key map (kbd "b"      )  #'pdf-view-set-slice-from-bounding-box)
(define-key map (kbd "r"      )  #'pdf-view-reset-slice))


;; consult
(require 'consult)
(global-set-key (kbd "C-c f r") 'consult-recent-file)
(global-set-key (kbd "C-x C-v") 'consult-buffer)
(global-set-key (kbd "C-x C-'") 'consult-grep)
(global-set-key (kbd "C-s") 'consult-line)


;; python - quickrun
(require 'quickrun)
(with-eval-after-load 'python
  (let ((map-var python-mode-map))
    (define-key map-var (kbd "C-c C-s") #'quickrun-shell)
    (define-key map-var (kbd "<tab>") 'python-indent-shift-right)
    (define-key map-var (kbd "S-<tab>") 'python-indent-shift-left)
    (define-key map-var [S-iso-lefttab] 'python-indent-shift-left)
    (define-key map-var (kbd "C-c C-i") 'pyimport-insert-missing)
    (define-key map-var (kbd "C-c C-b") 'python-black-region)))


(global-set-key (kbd "C-;") #'company-indent-or-complete-common)
