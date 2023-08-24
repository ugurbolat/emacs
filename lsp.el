(message "loading lsp.el")

(straight-use-package
 '(lsp-mode :type git :host github :repo "emacs-lsp/lsp-mode"))
(straight-use-package
 '(lsp-ui :type git :host github :repo "emacs-lsp/lsp-ui"))
;; (straight-use-package
;;  '(lsp-treemacs :type git :host github :repo "emacs-lsp/lsp-treemacs"))
(straight-use-package
 '(dap-mode :type git :host github :repo "emacs-lsp/dap-mode"))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;;(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
(require 'dap-python)
(setq dap-python-debugger 'debugpy)

(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))

;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; ;; optional if you want which-key integration
;; (use-package which-key
;;   :config
;;   (which-key-mode))



;;;;;; configs from doom
(setq emacs-cache-dir (expand-file-name "~/emacs-configs/emacs-vanilla/cache/"))
(setq emacs-data-dir (expand-file-name "~/emacs-configs/emacs-vanilla/data/"))

(use-package lsp-mode
  ;;:commands lsp-install-server
  :init
  ;; Don't touch ~/.emacs.d, which could be purged without warning
  (setq lsp-session-file (concat emacs-cache-dir "lsp-session")
	lsp-server-install-dir (concat emacs-data-dir "lsp"))
  ;; Don't auto-kill LSP server after last workspace buffer is killed, because I
  ;; will do it for you, after `+lsp-defer-shutdown' seconds.
  (setq lsp-keep-workspace-alive nil)

  ;; NOTE I tweak LSP's defaults in order to make its more expensive or imposing
  ;;      features opt-in. Some servers implement these poorly and, in most
  ;;      cases, it's safer to rely on Emacs' native mechanisms (eldoc vs
  ;;      lsp-ui-doc, open in popup vs sideline, etc).

  ;; Disable features that have great potential to be slow.
  (setq lsp-enable-folding nil
	lsp-enable-text-document-color nil)
  ;; Reduce unexpected modifications to code
  (setq lsp-enable-on-type-formatting nil)
  ;; Make breadcrumbs opt-in; they're redundant with the modeline and imenu
  (setq lsp-headerline-breadcrumb-enable nil))


(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  ;;:init
  ;;   (defadvice! +lsp--use-hook-instead-a (fn &rest args)
  ;;     "Change `lsp--auto-configure' to not force `lsp-ui-mode' on us. Using a hook
  ;; instead is more sensible."
  ;;     :around #'lsp--auto-configure
  ;;     (letf! ((#'lsp-ui-mode #'ignore))
  ;; 	   (apply fn args)))
  :config
  (setq lsp-ui-peek-enable t
	lsp-ui-doc-max-height 8
	lsp-ui-doc-max-width 72         ; 150 (default) is too wide
	lsp-ui-doc-delay 0.75           ; 0.2 (default) is too naggy
	lsp-ui-doc-show-with-mouse nil  ; don't disappear on mouseover
	lsp-ui-doc-position 'at-point
	lsp-ui-sideline-ignore-duplicate t
	;; Don't show symbol definitions in the sideline. They are pretty noisy,
	;; and there is a bug preventing Flycheck errors from being shown (the
	;; errors flash briefly and then disappear).
	lsp-ui-sideline-show-hover nil
	;; Re-enable icon scaling (it's disabled by default upstream for Emacs
	;; 26.x compatibility; see emacs-lsp/lsp-ui#573)
	lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default)

  ;; (map! :map lsp-ui-peek-mode-map
  ;; 	"j"   #'lsp-ui-peek--select-next
  ;; 	"k"   #'lsp-ui-peek--select-prev
  ;; 	"C-k" #'lsp-ui-peek--select-prev-file
  ;; 	"C-j" #'lsp-ui-peek--select-next-file)
  )
