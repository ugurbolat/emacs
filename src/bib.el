
;; REF:
;; https://gist.github.com/rka97/57779810d3664f41b0ed68a855fcab54
;; https://emacsconf.org/2021/talks/researc/h


;;(setq async-shell-command-buffer 'new-buffer)

(defun ub/async-shell-command-no-switch (orig-fun command &optional output-buffer error-buffer)
  "Run the async-shell-command without switching to the output buffer."
  (let ((display-buffer-alist
         (cons '("*Async Shell Command*" display-buffer-no-window)
               display-buffer-alist)))
    (funcall orig-fun command output-buffer error-buffer)))

;;(advice-add 'async-shell-command :around #'ub/async-shell-command-no-switch)


(defun ub/reformat-bib-library (&optional filename)
  "Formats the bibliography using biber & rebiber and updates the PDF -metadata."
  (interactive "P")
  (let ((cmnd (concat
               (format "cd %s &&" ub/bib-rebiber-dir)
               ;;(format "rebiber -i %s &&" filename) ; Get converence versions of arXiv papers
               ;;(format "rebiber -i %s -o %s &&" filename filename-rebiber)
               (format "rebiber -i ../bolatu/%s -o %s &&" filename filename)
               ;;(format "biber --tool --output_align --output_indent=2 --output_fieldcase=lower --configfile=~/bib-lib/biber-myconf.conf --output_file=%s %s && " filename filename) ; Properly format the bibliography
               ;;(format "biber --tool --output_align --output_indent=2 --output_fieldcase=lower --output_file=%s %s" filename-biber-out filename-rebiber-out) ; Properly
               ;;(format "sed -i -e 's/arxiv/arXiv/gI' -e 's/journaltitle/journal     /' -e 's/date      /year      /' %s &&" filename) ; Some replacements
               ;;(format "git commit -m \"Updating bibliography..\" %s && git push" filename) ; Commit and push the updated bib
               "git commit -m \"Updating bibliography w rebiber..\" ."
               )))
    (async-shell-command cmnd)))


(defun ub/setup-rebiber-async-process ()
  "Setup the async process for rebibering the bib file on change."
  (setq async-shell-command-buffer 'new-buffer)
  (advice-add 'async-shell-command :around #'ub/async-shell-command-no-switch)

  ;; (add-hook 'bib-file-hot-change-hook #'(lambda () (ub/reformat-bib-library ub/bib-file-hot)))
  ;; (add-hook 'bib-file-archive-change-hook #'(lambda () (ub/reformat-bib-library ub/bib-file-archive)))
  ;; (add-hook 'bib-file-ref-change-hook #'(lambda () (ub/reformat-bib-library ub/bib-file-ref)))

  (require 'filenotify)

  (defvar my-file-change-debounce-timer nil
    "Timer for debouncing file change events.")
  (defvar my-file-change-debounce-timer-archive nil
    "Timer for debouncing file change events.")
  (defvar my-file-change-debounce-timer-hot nil
    "Timer for debouncing file change events.")


  (defvar bib-file-hot-change-hook nil
    "Hook run after the specified file is changed.")

  (defun bib-file-hot-change-callback (event)
    "Callback for file change events."
    (message "File Notify `hot-change` Event: %S" event)
    (when (eq (nth 1 event) 'changed)
      (unless my-file-change-debounce-timer-hot
        (setq my-file-change-debounce-timer-hot
              (run-with-timer 3.0 nil ; Set the debounce duration (in seconds) as needed
                              (lambda ()
                                (run-hooks 'bib-file-hot-change-hook)
                                (setq my-file-change-debounce-timer-hot nil)))))))

  (defvar bib-file-hot-monitor-descriptor nil
    "Descriptor for monitoring a specific file.")

  (let ((file-to-watch-hot (expand-file-name ub/bib-file-hot ub/bib-dir)))
    (setq bib-file-hot-monitor-descriptor
          (file-notify-add-watch file-to-watch-hot
                                 '(change)
                                 'bib-file-hot-change-callback)))

  (add-hook 'bib-file-hot-change-hook #'(lambda () (ub/reformat-bib-library ub/bib-file-hot)))

  ;; TODO ugly quick copy-paste of hooks - archive

  (defvar bib-file-archive-change-hook nil
    "Hook run after the specified file is changed.")

  (defun bib-file-archive-change-callback (event)
    "Callback for file change events."
    (message "File Notify `archive-change` Event: %S" event)
    (when (eq (nth 1 event) 'changed)
      (unless my-file-change-debounce-timer-archive
        (setq my-file-change-debounce-timer-archive
              (run-with-timer 3.0 nil ; Set the debounce duration (in seconds) as needed
                              (lambda ()
                                (run-hooks 'bib-file-archive-change-hook)
                                (setq my-file-change-debounce-timer-archive nil)))))))

  (defvar bib-file-archive-monitor-descriptor nil
    "Descriptor for monitoring a specific file.")

  (let ((file-to-watch-archive (expand-file-name ub/bib-file-archive ub/bib-dir)))
    (setq bib-file-archive-monitor-descriptor
          (file-notify-add-watch file-to-watch-archive
                                 '(change)
                                 'bib-file-archive-change-callback)))
                                        ;(lambda (event) (message "Deneme Event %S" event)))))

  (add-hook 'bib-file-archive-change-hook #'(lambda () (ub/reformat-bib-library ub/bib-file-archive)))

  ;; - ref

  (defvar bib-file-ref-change-hook nil
    "Hook run after the specified file is changed.")

  (defun bib-file-ref-change-callback (event)
    "Callback for file change events."
    (message "File Notify `ref-change` Event: %S" event)

    (when (eq (nth 1 event) 'changed)
      (unless my-file-change-debounce-timer
        (setq my-file-change-debounce-timer
              (run-with-timer 3.0 nil ; Set the debounce duration (in seconds) as needed
                              (lambda ()
                                (run-hooks 'bib-file-ref-change-hook)
                                (setq my-file-change-debounce-timer nil)))))))


  (defvar bib-file-ref-monitor-descriptor nil
    "Descriptor for monitoring a specific file.")

  (let ((file-to-watch-ref (expand-file-name ub/bib-file-ref ub/bib-dir)))
    (setq bib-file-ref-monitor-descriptor
          (file-notify-add-watch file-to-watch-ref
                                 '(change)
                                 'bib-file-ref-change-callback)))
                                        ;(lambda (event) (message "Deneme Event %S" event)))))

  (add-hook 'bib-file-ref-change-hook #'(lambda () (ub/reformat-bib-library ub/bib-file-ref)))

  )


(defun ub/run-if-server-active (body-fun)
  "Execute BODY-FUN if an Emacs server is running."
  (when (bound-and-true-p server-process)
    (funcall body-fun)))

;; we only rebiber if emacs is running as a server
;; to avoid doing it multiple times in the other emacs instances
(ub/run-if-server-active #'ub/setup-rebiber-async-process)


(use-package citar
  :ensure (:fetcher github :repo "emacs-citar/citar")
  :defer 0.1
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (citar-notes-paths '("~/main/org/roam/bib"))
  (citar-bibliography '("~/main/library/bib/bolatu/0_Reference_Materials.bib"
                        "~/main/library/bib/bolatu/1_Papers_Archive.bib"
                        "~/main/library/bib/bolatu/2_Hot_Projects.bib"))
  :config
  (setq citar-library-file-extensions
        '("pdf" "jpg" "png" "epub" "mp4" "mp3" "djvu" "txt"
          "doc" "docx" "ppt" "pptx" "xls" "xlsx" "odt" "odp" "ods")
        citar-file-additional-files-separator "_")
  
  (defvar citar-indicator-files-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-file_o"
              :face 'nerd-icons-green
              :v-adjust -0.1)
     :function #'citar-has-files
     :padding "  " ; need this because the default padding is too low for these icons
     :tag "has:files"))
  (defvar citar-indicator-links-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-link"
              :face 'nerd-icons-orange
              :v-adjust 0.01)
     :function #'citar-has-links
     :padding "  "
     :tag "has:links"))
  (defvar citar-indicator-notes-icons
    (citar-indicator-create
     :symbol (nerd-icons-codicon
              "nf-cod-note"
              :face 'nerd-icons-blue
              :v-adjust -0.3)
     :function #'citar-has-notes
     :padding "    "
     :tag "has:notes"))
  (defvar citar-indicator-cited-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-circle_o"
              :face 'nerd-icon-green)
     :function #'citar-is-cited
     :padding "  "
     :tag "is:cited"))
  (setq citar-indicators
        (list ;;citar-indicator-files ; plain text
         citar-indicator-files-icons
         citar-indicator-links-icons
         citar-indicator-notes-icons
         citar-indicator-cited-icons))
  (setq citar-org-roam-note-title-template "${title} :: ${author}")
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))


(use-package citar-org-roam
  :ensure (:fetcher github :repo "emacs-citar/citar-org-roam"))

;; NOTE below config was found heavy search as it seems to not load with :after in use-package
(with-eval-after-load 'citar
  (with-eval-after-load 'org-roam
    (require 'citar-org-roam)
    (setq citar-org-roam-capture-template-key "l")
    (citar-org-roam-mode 1)))



(use-package parsebib
  :ensure (:fetcher github :repo "joostkremers/parsebib")
  ;;:defer 0.1
  )

;; FIXME doesn't load here..
;;(require 'parsebib)
;;(require 'parsebib)

;; (defun extract-bib-entries (entries)
;;   "Extract citekey, title and author as string from a list of bib entries using parsebib."
;;   (let (result)
;;     (dolist (entry entries result)
;;       (with-temp-buffer
;;         (insert entry)
;;         ;; TODO cont. from here. https://github.com/joostkremers/parsebib
;;         ;; below is from gpt..
;;         (let* ((parsed-entry (car (parsebib-parse-bibtex-entry)))
;;                (citekey (car parsed-entry))
;;                (fields (cdr parsed-entry))
;;                (title (cdr (assoc-string "title" fields)))
;;                (author (cdr (assoc-string "author" fields))))
;;           (when (and citekey author title)
;;             (push (format "- %s - %s - %s" citekey title author) result)))))))


(defun read-file-content
    (filepath)
  "Read the content of a file and return as a string."
  (with-temp-buffer
    (insert-file-contents filepath)
    (buffer-string)))

(defun extract-added-lines-from-diff
    (diff-content)
  "Parse given diff content and return lines that start with a plus sign as a list."
  (let
      ((lines
        (split-string diff-content "\n"))
       added-lines)
    (dolist
        (line lines added-lines)
      (when
          (string-prefix-p "+" line)
        (unless
            (string-prefix-p "+++" line)
          (push
           (substring line 1)
           added-lines))))
    (nreverse added-lines)))

;; (extract-added-lines-from-diff
;;  (read-file-content
;;   "/home/bolatu/main/library/bib/bolatu_rebiber_test/my.diff"))

;; (message "yooo")
