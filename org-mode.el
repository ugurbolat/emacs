


;; (package! org
;; 	  :recipe (:host github
;; 			 :repo "emacs-straight/org"
;; 			 :files (:defaults "etc")
;; 			 ;; HACK Org requires a post-install compilation step to generate a
;; 			 ;;      org-version.el with org-release and org-git-version
;; 			 ;;      functions, using a 'git describe ...' call.  This won't work
;; 			 ;;      in a sparse clone and I value smaller network burdens on
;; 			 ;;      users over non-essential variables so we fake it:
;; 			 :build t
;; 			 :pre-build
;; 			 (with-temp-file "org-version.el"
;; 			   (insert "(defun org-release () \"9.5\")\n"
;; 				   (format "(defun org-git-version (&rest _) \"9.5-??-%s\")\n" ;; but 9.6.1
;; 					   (cdr (doom-call-process "git" "rev-parse" "--short" "HEAD")))
;; 				   "(provide 'org-version)\n")))
;; 	  :pin "630f86dfc42472aafd9a4f305e1965cbe92b2891")


;;(use-package org :straight (:type built-in))
(straight-use-package 'org)

;; (straight-use-package
;;  '(org :type git :host github :repo "emacs-straight/org"
;;        ;; :pre-build
;;        ;; (with-temp-file "org-version.el"
;;        ;; 	 (insert "(defun org-release () \"9.5\")\n"
;;        ;; 		 (format "(defun org-git-version (&rest _) \"9.5-??-%s\")\n" ;; but 9.6.1
;;        ;; 			 (cdr (doom-call-process "git" "rev-parse" "--short" "HEAD")))
;;        ;; 		 "(provide 'org-version)\n"))
;;        :commit "630f86dfc42472aafd9a4f305e1965cbe92b2891"))


(setq org-startup-with-inline-images t
      ;;(setq org-startup-with-latex-preview t)
      org-startup-folded nil
      org-checkbox-hierarchical-statistics nil
      org-tags-column 0
      org-enforce-todo-dependencies nil
      org-log-state-notes-into-drawer nil
      ;;+org-startup-with-animated-gifs 't
      org-startup-folded nil
      org-startup-indented t
      org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil
      org-support-shift-select nil
      shift-select-mode nil)

(setq org-list-demote-modify-bullet
      '(("+" . "*") ("*" . "-") ("-" . "+")))

