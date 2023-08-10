

;; zoom in/out/increase font
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; projectile
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

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
