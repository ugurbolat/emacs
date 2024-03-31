;; org defaults
(use-package org
  :ensure nil
  :config
  (setq
   org-startup-with-inline-images t
   ;;(setq org-startup-with-latex-preview t)=
   org-startup-folded nil
   org-checkbox-hierarchical-statistics nil
   org-tags-column 0
   org-enforce-todo-dependencies nil
   org-log-state-notes-into-drawer nil
   +org-startup-with-animated-gifs 't
   org-startup-folded nil
   org-edit-src-content-indentation 0
   org-startup-indented t
   org-src-tab-acts-natively t
   org-src-fontify-natively t
   org-fontify-quote-and-verse-blocks t
   org-confirm-babel-evaluate nil
   org-support-shift-select nil
   shift-select-mode nil
   org-list-demote-modify-bullet
   '(("+" . "*") ("*" . "-") ("-" . "+"))
   org-hide-emphasis-markers t
   org-image-actual-width '(600)
   org-id-method 'ts
   org-use-speed-commands t
   )

  ;; from doom
  ;; for more lang/org/config.el
  (setq org-special-ctrl-a/e t)
  (setq org-M-RET-may-split-line nil
        ;; insert new headings after current subtree rather than inside it
        org-insert-heading-respect-content t)


  ;;agenda
  (use-package org
    :config
    (setq org-log-reschedule t)
    (setq org-todo-keywords
          '((sequence "PAPER(a!)" "STUDY(s!)" "HYPOTHESIS(h!)" "CODE(c!)" "WRITE(w!)" "REVIEW(r!)" "|" "DONE(d!)" "KILL(k!)")
            (sequence "PROJ(p!)" "J@IN(i!)" "J@HOLD(h!)" "|" "J@DONE(d!)" "J@KILL(k!)")
            (sequence "TODO(t!)" "IDEA(i!)" "SOMEDAY(o!)" "STARTED(s!)" "BLOCKED(b!)" "DELEG(e!)" "|" "DONE(d!)" "KILL(k!)")))
    (setq org-priority-faces '((?A . (:foreground "red" :weight 'bold))
                               (?B . (:foreground "yellow"))
                               (?C . (:foreground "green"))))
    )


  

  
  (defun ub/org-jump-to-heading-beginning ()
    "Jump to the beginning of the line of the closest Org heading."
    (interactive)
    (org-back-to-heading)
    (beginning-of-line))
  (define-key org-mode-map (kbd "C-c *") 'ub/org-jump-to-heading-beginning)
  
  (defun ub/set-created-property ()
    (org-set-property "CREATED" (format-time-string "[%Y-%m-%d %H:%M]" (current-time))))
  (add-hook! 'org-insert-heading-hook #'ub/set-created-property))




;; TODO save generated imgs based on the file similar to org-download
;; ;; org-download-method code snippet taken from
;; ;; https://github.com/jethrokuan/dots/blob/master/.doom.d/config.el
;; (defun +org/org-ai-image-gen-download-method (link)
;;   (let* ((filename
;;           (file-name-nondirectory
;;            (car (url-path-and-query
;;                  (url-generic-parse-url link)))))
;;          ;; Create folder name with current buffer name, and place in root dir
;;          (dirname (concat
;;                    (replace-regexp-in-string " " "_" (downcase (file-name-base buffer-file-name)))
;;                    ".assets/gen-imgs/"))
;;          (filename-with-timestamp (format "%s%s.%s"
;;                                           (format-time-string org-download-timestamp)
;;                                           (file-name-sans-extension filename)
;;                                           (file-name-extension filename))))
;;     (make-directory dirname t)
;;     (expand-file-name filename-with-timestamp dirname)))


(use-package org-ai
  :ensure (:fetcher github :repo "rksm/org-ai")
  :after org
  :defer 0.1
  :config
  (org-ai-global-mode)
  (add-hook 'org-mode-hook #'org-ai-mode)
  (setq org-ai-openai-api-token (ub/load-key-openai-token))
  ;; gpt
  (setq org-ai-default-chat-model "gpt-4") ; if you are on the gpt-4 beta:
  ;;
  (setq org-ai-image-model "dall-e-3")
  (setq org-ai-image-default-size "1024x1024") ; vs. 1024x1792 or 1792x1024
  (setq org-ai-image-default-count 1)
  (setq org-ai-image-default-style 'natural) ; vs. 'vivid
  (setq org-ai-image-default-quality 'standard) ; vs. 'hd
  ;;(setq org-ai-image-directory (+org/org-ai-image-gen-download-method))
  (setq org-ai-image-directory (expand-file-name "tmp/img-gen/" ub/org-root-dir))
  (org-ai-install-yasnippets)) ; if you are using yasnippet and want `ai` snippets


(defcustom org-ai-explain-math-prompt
  (concat "You are an expert in math."
          "The following shows a math description in latex."
          "Explain each element in the equation step-by-step."
          "List the necessary background knowledge needed to understand the given math expression such as theorems."
          "when prompting mathematical equations, you will use latex where the inline math mode equation has prefix "
	      "and suffix as such $...$ and display math mode equations such as"
          "\\begin{equation}"
          "..."
          "\\end{equation}"
          "When providing answers, avoid warnings/disclaimers/extra recommendations!!!"
          )
  "The template to use for `org-ai-explain-math'."
  :type 'string
  :group 'org-ai)

(defun org-ai-explain-math (start end)
  "Ask ChatGPT explain a code snippet.
`START' is the buffer position of the start of the code snippet.
`END' is the buffer position of the end of the code snippet."
  (interactive "r")
  (org-ai-on-region start end org-ai-explain-math-prompt))



(use-package org-cliplink
  :ensure (:fetcher github :repo "rexim/org-cliplink")
  :defer 0.1
  :bind
  ;;("C-c C-l" . org-cliplink)
  )

;;alphapapa/org-sidebar
(use-package org-sidebar
  :ensure (:fetcher github :repo "alphapapa/org-sidebar")
  :defer t
  ;;:bind
  ;;("C-c C-x C-o" . org-sidebar-tree)
  )

;; org-download
(use-package org-download
  :ensure t
  :defer 0.1
  :config
  ;; take an image that is already on the clipboard
  (setq org-download-screenshot-method "export filename=\"%s\"; import png:\"$filename\" ;xclip -selection clipboard -target image/png -filter < \"$filename\" &>/dev/null")
  (setq org-download-timestamp "%y-%m-%d_%H-%M-%S_")
  ;; org-download-method code snippet taken from
  ;; https://github.com/jethrokuan/dots/blob/master/.doom.d/config.el
  (defun +org/org-download-method (link)
    (let* ((filename
            (file-name-nondirectory
             (car (url-path-and-query
                   (url-generic-parse-url link)))))
           ;; Create folder name with current buffer name, and place in root dir
           (dirname (concat
                     (replace-regexp-in-string " " "_" (downcase (file-name-base buffer-file-name)))
                     ".assets/"))
           (filename-with-timestamp (format "%s%s.%s"
                                            (format-time-string org-download-timestamp)
                                            (file-name-sans-extension filename)
                                            (file-name-extension filename))))
      (make-directory dirname t)
      (expand-file-name filename-with-timestamp dirname)))
  (setq org-download-method '+org/org-download-method)
  ;;(setq org-image-actual-width nil) ;; think necessary for 500 to take effect
  ;;(setq org-download-image-org-width 600)
  (setq org-download-annotate-function 'ignore)
  (setq org-download-annotate-function (lambda (_link) ""))
  )

;; TODO current setup disables zmq, which is faster and async?
;; so currently experience freezing sessions
(use-package jupyter
  :ensure (:fetcher github :repo "emacs-jupyter/jupyter")
  :defer 0.1
  :config
  (require 'ob-jupyter)
  (require 'jupyter)
  (setq jupyter-use-zmq nil))

;; REF https://github.com/emacs-jupyter/jupyter/issues/500
;; make sure to install/download the jupyter
;; install jupyter_server=1.23.4


;; babel python
(use-package org
  :config
  (add-to-list 'org-babel-load-languages '(shell . t))
  ;; python
  (setq org-babel-default-header-args:python
        '((:results . "output")
          (:session . "python-default")
          (:python . "python3")
          (:exports . "both")
          (:cache .   "no")
          (:noweb . "no")
          (:hlines . "no")
          (:tangle . "no")
          (:async . "yes")
          (:eval . "never-export")))
  (add-to-list 'org-babel-load-languages '(python . t))
  ;; jupyter-python
  (setq org-babel-default-header-args:jupyter
        '((:results . "both")
          (:session . "jupyter-python-default")
          (:kernel . "python3")
          (:pandoc . "t")
          (:exports . "both")
          (:cache .   "no")
          (:noweb . "no")
          (:hlines . "no")
          (:tangle . "no")
          (:async . "yes")
          (:eval . "never-export")))
  (add-to-list 'org-babel-load-languages '(python . t))
  (add-to-list 'org-src-lang-modes '("jupyter" . python))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))


;; ;; BUG latest Jupyter : REST API error: 404, "Not found" #500
;; ;; mamba install jupyter_server=1.23.4


;; org-latex
;; (setq org-format-latex-options '(:foreground default :background default :scale 2.0
;;                                              :html-foreground "Black" :html-background "Transparent"
;;                                              :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
;; (use-package org
;;   :custom
;;   (org-format-latex-options
;;    (plist-put org-format-latex-options :scale 2.0))
;;   ;; :config
;;   ;; (setq org-format-latex-options
;;   ;;       (plist-put org-format-latex-options :scale 2.0))
;;   ;; (setq org-format-latex-options
;;   ;;       (plist-put org-format-latex-options :html-scale 1.0))
;;   ;; (setq org-format-latex-options
;;   ;;       (plist-put org-format-latex-options :html-foreground "Black"))
;;   ;; (setq org-format-latex-options
;;   ;;       (plist-put org-format-latex-options :html-background "Transparent"))
;;   ;; (setq org-format-latex-options
;;   ;;       (plist-put org-format-latex-options :matchers '("begin" "$1" "$" "$$" "\\(" "\\[")))
;;   )

;; faster rendering for org-latex previews
;; use dvisvgm instead of imagemagick and dvipng
;; NOTE nope it is not faster, but might have some other benefits like auto-scale, trasp. background

(defun my/resize-org-latex-overlays ()
  (cl-loop for o in (car (overlay-lists))
           if (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay)
           do (plist-put (cdr (overlay-get o 'display))
		                 :scale (expt text-scale-mode-step
				                      text-scale-mode-amount))))

(use-package org
  :custom
  (org-preview-latex-default-process 'dvisvgm)
  :config
  (plist-put org-format-latex-options :scale 1.7) ;; default is quite small
  (plist-put org-format-latex-options :foreground nil)
  (plist-put org-format-latex-options :background nil)
  ;; usepackage latex
  (add-to-list 'org-latex-packages-alist '("" "braket" t))
  (add-to-list 'org-latex-packages-alist '("" "bm" t))
  :hook
  (org-mode . (lambda () (add-hook 'text-scale-mode-hook #'my/resize-org-latex-overlays nil t)))
  )


;; io12/org-fragtog
(use-package org-fragtog
  :ensure (:fetcher github :repo "io12/org-fragtog")
  :defer 0.1
  :hook (org-mode . org-fragtog-mode))
;; :config
;; (setq org-fragtog-enable t)
;; (setq org-fragtog-auto-revert-mode t)
;; (setq org-fragtog-global-auto-revert-mode))


;; awth13/org-appear
;;
(use-package org-appear
  :ensure (:fetcher github :repo "awth13/org-appear")
  :defer 0.1
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t)
  (setq org-appear-autosubmarkers t)
  (setq org-appear-autoentities t)
  (setq org-appear-autokeywords t)
  (setq org-appear-autosubmarkers t))


(defun ub/org-roam-dailies-capture-today-respect-my-midnight ()
  "Create an org-roam daily note considering the day ends at 7 am."
  (interactive)
  (let ((current-hour (string-to-number (format-time-string "%H"))))
    (if (< current-hour 7)
        ;; if current hour is between 0 and 7, open 'yesterday' note
        (org-roam-dailies-capture-yesterday)
      ;; else, open 'today' note
      (org-roam-dailies-capture-today))))


(use-package org-roam
  :ensure (:fetcher github :repo "org-roam/org-roam")
  ;;:defer 0.1
  :custom
  (org-roam-directory ub/roam-root-dir)
  (org-roam-db-location (expand-file-name ".roam.db" ub/roam-root-dir))
  ;; TODO migrate roam capture
  ;;(org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      ;;"* %<%H:%M> %?\n:PROPERTIES:\n:CREATED: %U\n:END:"
      ;; cleaner
      "* %<%H:%M> %?"
      :target (file+head
               "%<%Y-%m-%d>.org"
               "#+TITLE: Dailies - %<%Y-%m-%d>
#+CATEGORY: journal
#+FILETAGS: daily daily_file\n"))))
  
  (org-roam-capture-templates
   `(("d" "default" plain
      "%?"
      :target
      (file+head
       ;;"%<%Y%m%d%H%M%S>-${slug}.org"
       ;;"%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/dump/${citar-citekey}.org"
       "%(concat ub/roam-root-dir (format-time-string \"/dump/%y%m%d%H%M%S-\" (current-time) t) (s-left 70 \"${slug}\") \".org\")"
       ,(concat
         ":PROPERTIES:\n"
         ":CREATED: %U\n"
         ":END:\n"
         "#+TITLE: ${title}\n"
         "#+DATE: %<%Y-%m-%d>\n"
         "#+FILETAGS: dump dump_file\n")) 
      :unnarrowed t)
     ("l" "literature note" plain
      "%?"
      :target
      (file+head
       ;;"%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/bib/${citar-citekey}.org"
       "%(concat ub/roam-root-dir \"/bib/${citar-citekey}.org\")"
       ;; TODO ${title} :: ${first-author}
       "#+TITLE: ${note-title}
#+CREATED: %U
#+LAST_MODIFIED: %U
#+SETUPFILE: /home/bolatu/emacs-configs/done-right/latex-include/bib_bolatu_linux.org



* References
#+LATEX: \\printbibliography[heading=none]
")
      :unnarrowed t)

     ;; ("b" "beamer" plain
     ;;  "%?"
     ;;  :target
     ;;  ;; TODO cont.
     ;;  ;; REF see /home/bolatu/main/org/roamable/zk/230725205154-dfki_ric_rl_discussion_paper_presentation_quantum_tensor_networks_for_.org
     ;;  :unnarrowed t)
     
     ))


  ;;#+STARTUP: beamer
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ;;("C-c n j" . org-roam-dailies-capture-today)
         ("C-c n j" . ub/org-roam-dailies-capture-today-respect-my-midnight))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

  (require 'org-roam-protocol)

  ;; org-roam-protocol capture templates
  (setq roam-ref-cap-temp-list `())
  (add-to-list 'roam-ref-cap-temp-list
               `("i" "Dump" plain
                 ,(concat
                   "\n\n- tags ::"
                   "\n\n")
                 :if-new
                 (file+head
                  "%(concat ub/roam-dump-dir (format-time-string \"/%y%m%d%H%M%S-\" (current-time) t) (s-left 70 \"${slug}\") \".org\")"
                  ;;"bib/${citekey}.org"
                  ,(concat
                    ":PROPERTIES:\n"
                    ":CREATED: %U\n"
                    ":ROAM_REFS: ${ref}\n"
                    ":END:\n"
                    "#+TITLE: ${title}\n"
                    "#+DATE: %<%Y-%m-%d>\n"
                    "#+FILETAGS: dump web dump_file\n"))
                 :unnarrowed t)
               t)
  (add-to-list 'roam-ref-cap-temp-list
               `("YY" "Dump Video (bib)" plain
                 ,(concat
                   "\n\n- tags ::"
                   "\n\n")
                 :if-new
                 (file+head
                  "%(concat ub/roam-dump-dir (format-time-string\"/%y%m%d%H%M%S-\" (current-time) t) (s-left 70 \"${slug}\") \".org\")"
                  ;;"bib/${citekey}.org"
                  ,(concat
                    ":PROPERTIES:\n"
                    ":CREATED: %U\n"
                    ":MPV_URL: %i\n"
                    ":ROAM_REFS: ${ref}\n"
                    ":END:\n"
                    "#+TITLE: ${title}\n"
                    "#+DATE: %<%Y-%m-%d>\n"
                    "#+FILETAGS: dump web b@video dump_file\n"))
                 :unnarrowed t)
               t)
  (setq org-roam-capture-ref-templates roam-ref-cap-temp-list)


  (org-roam-db-autosync-mode)
  )




(setq org-agenda-files ub/gtd-main-files-list)


;; ;; REF
;; (require 'org-ql)

;; (defun my-consult-org-ql-agenda-format (o)
;;   (propertize
;;    (org-ql-view--format-element o)
;;    'consult--candidate (org-element-property :org-hd-marker o)))


;; (defun my-consult-org-ql-agenda-match (string)
;;   "Return candidates that match STRING.
;; Sort heading matches first, followed by other matches.
;; Within those groups, sort by date and priority."
;;   (let* ((query (org-ql--query-string-to-sexp string))
;;          (sort '(date reverse priority))
;;          (heading-query (-tree-map (lambda (x) (if (eq x 'rifle) 'heading x)) query))
;;          (matched-heading
;;           (mapcar #'my-consult-org-ql-agenda-format
;;                   (org-ql-select 'org-agenda-files heading-query
;;                     :action 'element-with-markers
;;                     :sort sort)))
;;          (all-matches
;;           (mapcar #'my-consult-org-ql-agenda-format
;;                   (org-ql-select 'org-agenda-files query
;;                     :action 'element-with-markers
;;                     :sort sort))))
;;     (append
;;      matched-heading
;;      (seq-difference all-matches matched-heading))))

;; (defun my-consult-org-ql-agenda-jump ()
;;   "Search agenda files with preview."
;;   (interactive)
;;   (let* ((marker (consult--read
;;                   (consult--dynamic-collection
;;                    #'my-consult-org-ql-agenda-match)
;;                   :state (consult--jump-state)
;;                   :category 'consult-org-heading
;;                   :prompt "Heading: "
;;                   :sort nil
;;                   :lookup #'consult--lookup-candidate))
;;          (buffer (marker-buffer marker))
;;          (pos (marker-position marker)))
;;     ;; based on org-agenda-switch-to
;;     (unless buffer (user-error "Trying to switch to non-existent buffer"))
;;     (pop-to-buffer-same-window buffer)
;;     (goto-char pos)
;;     (when (derived-mode-p 'org-mode)
;;       (org-fold-show-context 'agenda)
;;       (run-hooks 'org-agenda-after-show-hook))))

;; (use-package org-ql
;;   :bind ("M-s a" . my-consult-org-ql-agenda-jump))


;; (defun ub/org-roam-dailies-capture-today-respect-my-midnight ()
;;   "Create an org-roam daily note considering the day ends at 4 am."
;;   (interactive)
;;   (let ((current-hour (string-to-number (format-time-string "%H"))))
;;     (if (< current-hour 4)
;;         ;; if current hour is between 0 and 4, open 'yesterday' note
;;         (org-roam-dailies-capture-yesterday)
;;       ;; else, open 'today' note
;;       (org-roam-dailies-capture-today))))

;; (global-set-key (kbd "C-c n j") 'my/org-roam-capture-custom-daily-note-test)




;; we don't want to dailies relative to the org-roam-directory
;; HACK
(defcustom org-roam-dailies-directory-abs-path nil
  "Absolute path to daily-notes, if non-nil."
  :group 'org-roam
  :type '(choice
          (const :tag "None" nil)
          (directory :tag "Absolute path")))

(defun org-roam--resolve-dailies-dir ()
  (or org-roam-dailies-directory-abs-path
      (expand-file-name org-roam-dailies-directory org-roam-directory)))

(defun org-roam--override-dailies-dir (orig-fun &rest args)
  (let ((org-roam-dailies-directory (org-roam--resolve-dailies-dir)))
    (apply orig-fun args)))

;; below are the functions that need to be overriden since they use org-roam-directory
(dolist (fn '(org-roam-dailies-find-directory
              org-roam-dailies--daily-note-p
              org-roam-dailies--list-files
              org-roam-dailies-calendar-mark-entries
              org-roam-dailies--capture))
  (advice-add fn :around #'org-roam--override-dailies-dir))

(setq org-roam-dailies-directory-abs-path "/home/bolatu/main/org/roam/daily/")




;; set a global key for org-capture to C-c n n
(use-package org
  :ensure nil
  :config
  (global-set-key (kbd "C-c n n") 'org-capture))

;; progfolio/doct
;; install w/ elpaca
(use-package doct
  :ensure (:fetcher github :repo "progfolio/doct")
  :defer 0.1
  :after org
  ;;recommended: defer until calling doct
  :commands (doct)
  :config
  ;; utilities
  (defun ub/capture-id-create (&optional arg)
    (save-excursion)
    (org-id-get-create))

  ;; templates
  (setq doct-cap-temp-list `())
  ;; (setq cap-task `("TODO" :keys "t" :type entry :prepend t
  ;;                  :unnarrowed nil
  ;;                  :empty-lines-before 1
  ;;                  :file ,ub/gtd-inbox-file
  ;;                  :before-finalize ub/capture-id-create
  ;;                  ;;:prepare-finalize ub/capture-newlines-at-end
  ;;                  :template ("* TODO %i%?  "
  ;;                             ":PROPERTIES:"
  ;;                             ":CREATED: %U"
  ;;                             ":REF-LINK: %a"
  ;;                             ":END:")))
  (setq cap-task-work `("TODO - work" :keys "w" :type entry :prepend t
                        :unnarrowed nil
                        :empty-lines-before 1
                        :file ,ub/gtd-work-file
                        :headline "Inbox"
                        :before-finalize ub/capture-id-create
                        ;;:prepare-finalize ub/capture-newlines-at-end
                        :template ("* TODO %i%?  "
                                   ":PROPERTIES:"
                                   ":CREATED: %U"
                                   ":REF-LINK: %a"
                                   ":END:")))
  (add-to-list 'doct-cap-temp-list cap-task-work t)
  (setq cap-task-me `("TODO - me" :keys "m" :type entry :prepend t
                      :unnarrowed nil
                      :empty-lines-before 1
                      :file ,ub/gtd-me-file
                      :headline "Inbox"
                      :before-finalize ub/capture-id-create
                      ;;:prepare-finalize ub/capture-newlines-at-end
                      :template ("* TODO %i%?  "
                                 ":PROPERTIES:"
                                 ":CREATED: %U"
                                 ":REF-LINK: %a"
                                 ":END:")))
  (add-to-list 'doct-cap-temp-list cap-task-me t)
  (setq cap-note `("Note" :keys "n" :type entry
                   :unnarrowed nil
                   :empty-lines-before 1
                   ;; :clock-in t :clock-resume t
                   :file ,ub/daily-today-file
                   ;;:before-finalize ub/capture-id-create
                   :template ("* %<%H:%M> %i%? "
                              ;; ":PROPERTIES:"
                              ;; ":CREATED: %U"
                              ;; ":REF-LINK: %a"
                              ;; ":END:"
                              )))
  (add-to-list 'doct-cap-temp-list cap-note t)
  
  (setq org-capture-templates (doct doct-cap-temp-list))
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; below add to hit to startup 0.0x ms
;; it is too messy atm so we leave to utilize use-package properly :p
;; maybe already


;; FIXME - use regular org capture after doom-emacs#5714 is resolved
;; FIXME - workaround for org capture
;; see https://github.com/hlissner/doom-emacs/issues/5714
;; copying the implementation into a new namespace so it can be referenced without being circular
(defun df/restart-mode-h nil "Restart `org-mode', but only once."
       (if doom-debug-p
           (progn
             (org-mode-restart))
         (let
             ((inhibit-message t)
              (save-silently t))
           (prog1
               (org-mode-restart)
             (message ""))))
       (setq org-agenda-new-buffers
             (delq
              (current-buffer)
              org-agenda-new-buffers))
       (remove-hook 'doom-switch-buffer-hook #'+org--restart-mode-h 'local)
       (run-hooks 'find-file-hook))

(defvar df/restart-mode-fn 'df/restart-mode-h)

(defalias '+org--restart-mode-h (lambda () (funcall df/restart-mode-fn)))

(defun df/org-capture (&optional goto keys)
  (interactive "P")
  (message "ignoring restart-mode")
  (setq df/restart-mode-fn #'ignore)
  (org-capture goto keys))

(defun df/restore-restart-mode ()
  (message "restoring restart-mode")
  (setq df/restart-mode-fn 'df/restart-mode-h))

(add-hook! 'org-capture-before-finalize-hook #'df/restore-restart-mode)




(use-package yequake
  :ensure (:fetcher github :repo "alphapapa/yequake")
  ;;:defer 0.1
  :custom
  (yequake-frames
   '(("org-capture"
      (buffer-fns . (ub/yequake-org-capture))
      (buffer-fns . (org-capture))
      (width . 0.75)
      (height . 0.5)
      (alpha . 0.95)
      (top . 0.10)
      (frame-parameters . (
                           (undecorated . t)
                           ;;(skip-taskbar . t)
                           (sticky . t))))


     ;; ("org-protocol-capture"
     ;;  (buffer-fns . (ub/yequake-org-protocol-capture))
     ;;  (width . 0.75)
     ;;  (height . 0.5)
     ;;  (alpha . 0.95)
     ;;  (top . 0.10)
     ;;  (frame-parameters . ((undecorated . t)
     ;;                       (skip-taskbar . t)
     ;;                       (sticky . t))))
     ;; ("org-protocol-capture-with-menu"
     ;;  (buffer-fns . (ub/yequake-org-protocol-capture))
     ;;  (width . 0.75)
     ;;  (height . 0.5)
     ;;  (alpha . 0.95)
     ;;  (top . 0.10)
     ;;  (frame-parameters . ((undecorated . t)
     ;;                       (skip-taskbar . t)
     ;;                       (sticky . t))))

     ("org-protocol-capture"
      (buffer-fns . (ub/yequake-org-protocol-capture))
      (width . 0.75)
      (height . 0.5)
      (alpha . 0.95)
      (top . 0.10)
      (frame-parameters . ((undecorated . t)
                           (skip-taskbar . t)
                           (sticky . t)
                           )))


     ;; TODO improve eshell such as hook eshell exit and closing windows
     ;; also consider equake package since it designed for shell
     ;; and seems to has more functionality compared to yequake
     ;; DONE using equake instead of yequake for shell
     ;; ("eshell"
     ;;  (buffer-fns . (+eshell/here))
     ;;  (width . 0.75)
     ;;  (height . 0.5)
     ;;  (top . 0.10)
     ;;  (alpha . 0.95)
     ;;  (frame-parameters . ((undecorated . t)
     ;;                       (skip-taskbar . t)
     ;;                       (sticky . t))))

     )))




;; REF: original org-protocol-capture function
;; instead of passing a key for a specific template,
;; we prompt the capture menu for selecting desired template
(defun ub/org-protocol-capture-with-menu (info)
  "
Instead of going passing template key, show org-capture menu"
  (let* ((parts
          (pcase (org-protocol-parse-parameters info)
            ;; New style links are parsed as a plist.
            ((let `(,(pred keywordp) . ,_) info) info)
            ;; Old style links, with or without template key, are
            ;; parsed as a list of strings.
            (p
             (let ((k (if (= 1 (length (car p)))
                          '(:template :url :title :body
                                      '(:url :title :body)))))
               (org-protocol-assign-parameters p k)))))
         (template (or (plist-get parts :template)
                       org-protocol-default-template-key))
         (url (and (plist-get parts :url)
                   (org-protocol-sanitize-uri (plist-get parts :url))))
         (type (and url
                    (string-match "^\\([a-z]+\\):" url)
                    (match-string 1 url)))
         (title (or (plist-get parts :title) ""))
         (region (or (plist-get parts :body) ""))
         (orglink
          (if (null url) title
            (org-link-make-string url (or (org-string-nw-p title) url))))
         ;; Avoid call to `org-store-link'.
         (org-capture-link-is-already-stored t))
    ;; Only store link if there's a URL to insert later on.
    (when url (push (list url title) org-stored-links))
    (org-link-store-props :type type
                          :link url
                          :description title
                          :annotation orglink
                          :initial region
                          :query parts)
    (raise-frame)
    ;; NOTE modified line
    (org-capture nil nil)
    (message "Item captured.")
    ;; Make sure we do not return a string, as `server-visit-files',
    ;; through `server-edit', would interpret it as a file name.
    nil))


;; NOTE ugly hack for fixing ugly hack [[id:93adfc46-653d-44ad-b4b2-626ac39916ef][org-capture-mode doesn't start properly after opening agenda]]
;; org-capture replaced w/ new function
(defun ub/org-protocol-capture (info)
  "Process an org-protocol://capture style url with INFO.

The sub-protocol used to reach this function is set in
`org-protocol-protocol-alist'.

This function detects an URL, title and optional text, separated
by `/'.  The location for a browser's bookmark looks like this:

  javascript:location.href = \\='org-protocol://capture?\\=' +
        new URLSearchParams({
              url: location.href,
              title: document.title,
              body: window.getSelection()})

or to keep compatibility with Org versions from 9.0 to 9.4:

  javascript:location.href = \\='org-protocol://capture?url=\\='+ \\
        encodeURIComponent(location.href) + \\='&title=\\=' + \\
        encodeURIComponent(document.title) + \\='&body=\\=' + \\
        encodeURIComponent(window.getSelection())

By default, it uses the character `org-protocol-default-template-key',
which should be associated with a template in `org-capture-templates'.
You may specify the template with a template= query parameter, like this:

  javascript:location.href = \\='org-protocol://capture?template=b\\='+ ...

Now template ?b will be used."
  (let* ((parts
	      (pcase (org-protocol-parse-parameters info)
	        ;; New style links are parsed as a plist.
	        ((let `(,(pred keywordp) . ,_) info) info)
	        ;; Old style links, with or without template key, are
	        ;; parsed as a list of strings.
	        (p
	         (let ((k (if (= 1 (length (car p)))
			              '(:template :url :title :body)
			            '(:url :title :body))))
	           (org-protocol-assign-parameters p k)))))
	     (template (or (plist-get parts :template)
		               org-protocol-default-template-key))
	     (url (and (plist-get parts :url)
		           (org-protocol-sanitize-uri (plist-get parts :url))))
	     (type (and url
		            (string-match "^\\([a-z]+\\):" url)
		            (match-string 1 url)))
	     (title (or (plist-get parts :title) ""))
	     (region (or (plist-get parts :body) ""))
	     (orglink
	      (if (null url) title
	        (org-link-make-string url (or (org-string-nw-p title) url))))
	     ;; Avoid call to `org-store-link'.
	     (org-capture-link-is-already-stored t))
    ;; Only store link if there's a URL to insert later on.
    (when url (push (list url title) org-stored-links))
    (org-link-store-props :type type
			              :link url
			              :description title
			              :annotation orglink
			              :initial region
			              :query parts)
    (raise-frame)
    (org-capture nil template)
    (message "Item captured.")
    ;; Make sure we do not return a string, as `server-visit-files',
    ;; through `server-edit', would interpret it as a file name.
    nil))


;; REF: ~/.emacs.d/.local/straight/repos/yequake/yequake.el
(defun ub/yequake-org-capture (&optional goto keys)
  "Call `org-capture' in a Yequake frame.
Adds a function to `org-capture-after-finalize-hook' that closes
the recently toggled Yequake frame and removes itself from the
hook."
  (let* ((remove-hook-fn (lambda ()
                           (remove-hook 'org-capture-after-finalize-hook #'org-capture-goto-last-stored))))
    ;;(add-hook 'org-capture-after-finalize-hook remove-hook-fn)
    ;;(add-hook 'org-capture-after-finalize-hook #'yequake-retoggle)
    ;; MAYBE: Propose an `org-capture-switch-buffer-fn' variable that could be rebound here.

    ;; NOTE ub
    ;; instead of retoggling the frame, go to last stored captured item.
    ;; this is done because it gives a possibilty to refile,
    ;; after finalize hook for capture is runned
    ;; even if I hit C-c C-w for refiling.
    ;; so instead, leave the frame open for possible refiling actions
    ;; or quit with extra key hit.
    (add-hook 'org-capture-after-finalize-hook remove-hook-fn)
    (add-hook 'org-capture-after-finalize-hook #'org-capture-goto-last-stored)

    ;; NOTE: We override `org-switch-to-buffer-other-window' because
    ;; it always uses `switch-to-buffer-other-window', and we want to
    ;; display the template menu and capture buffer in the existing
    ;; window rather than splitting the frame.
    (cl-letf* (((symbol-function #'org-switch-to-buffer-other-window)
                (symbol-function #'switch-to-buffer)))
      (condition-case nil
          (progn
            (yequake--toggle-frame
             "org-capture"
             '((buffer-fns nil)
               (width . 0.75)
               (height . 0.5)
               (top . 0.10)
               (alpha . 0.95)
               (frame-parameters
                ;;(undecorated . t)
                ;;(skip-taskbar . t)
                ;;(sticky . t)
                )))
            (df/org-capture nil nil))
        ;; Be sure to return the "CAPTURE-" buffer, which is the current
        ;; buffer at this point.
        ;;(current-buffer)

        ((error quit)
         ;; Capture aborted: remove the hook and hide the capture frame.
         ;;(remove-hook 'org-capture-after-finalize-hook #'yequake-retoggle)

         (message "asd")
         ;; NOTE ub
         (remove-hook 'org-capture-after-finalize-hook #'org-capture-goto-last-stored)
         (yequake-retoggle))))))

;; combining with org-protocol
;; ref: https://www.reddit.com/r/emacs/comments/fjou3c/open_orgprotocol_in_a_standalone_frame/
(defun ub/yequake-org-protocol-capture (info)
  "Call `org-protocol-capture' in a Yequake frame.
Adds a function to `org-capture-after-finalize-hook' that closes
the recently toggled Yequake frame and removes itself from the
hook."
  (let* ((remove-hook-fn (lambda ()
                           (remove-hook 'org-capture-after-finalize-hook #'org-capture-goto-last-stored))))

    (add-hook 'org-capture-after-finalize-hook remove-hook-fn)
    (add-hook 'org-capture-after-finalize-hook #'org-capture-goto-last-stored)

    (require 'org-roam-protocol)

    ;; NOTE: We override `org-switch-to-buffer-other-window' because
    ;; it always uses `switch-to-buffer-other-window', and we want to
    ;; display the template menu and capture buffer in the existing
    ;; window rather than splitting the frame.
    (cl-letf* (((symbol-function #'org-switch-to-buffer-other-window)
                (symbol-function #'switch-to-buffer)))
      (condition-case nil
          (progn
            ;; Hacky solution for opening the drop-down window
            (yequake--toggle-frame
             "org-capture"
             '((buffer-fns nil)
               (width . 0.75)
               (height . 0.5)
               (top . 0.10)
               (alpha . 0.95)
               (frame-parameters
                ;;(undecorated . t)
                ;;(skip-taskbar . t)
                ;;(sticky . t)
                )))
            (cond ((string-match-p "org-protocol-with-menu:" info)
                   (ub/org-protocol-capture-with-menu
                    ;;(message info)
                    (org-protocol-parse-parameters (s-chop-prefix "org-protocol-with-menu://capture\?" info) t)                    
                    ))
                  ((string-match-p "org-protocol:" info)
                   (ub/org-protocol-capture (org-protocol-parse-parameters (s-chop-prefix "org-protocol://capture\?" info) t)))
                  ((string-match-p "org-roam-protocol:" info)
                   ;;(message info)
                   (org-roam-protocol-open-ref (org-protocol-parse-parameters (s-chop-prefix "org-roam-protocol://roam-ref\?" info) t))
                   )
                  (t (message "Unknown protocol")))
            ;; Be sure to return the "CAPTURE-" buffer, which is the current
            ;; buffer at this point.
            (current-buffer))
        ((error quit)
         ;; Capture aborted: remove the hook and hide the capture frame.
         (remove-hook 'org-capture-after-finalize-hook #'org-capture-goto-last-stored)
         (yequake-retoggle))))))

