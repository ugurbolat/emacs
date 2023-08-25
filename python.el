(message "loading python.el")


(straight-use-package
 '(pyvenv :type git :host github :repo "jorgenschaefer/pyvenv"))

(use-package pyvenv
  :demand t
  :config
  (setenv "WORKON_HOME" "/home/bolatu/miniconda3/envs")
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[" pyvenv-virtual-env-name "] ")))
  ;;(pyvenv-tracking-mode 1)
  )


;; stolen from doom
;; modeline
;;;###autoload
(defun +modeline-update-env-in-all-windows-h (&rest _)
  "Update version strings in all buffers."
  (dolist (window (window-list))
    (with-selected-window window
      (when (fboundp 'doom-modeline-update-env)
        (doom-modeline-update-env))
      (force-mode-line-update))))

;;;###autoload
(defun +modeline-clear-env-in-all-windows-h (&rest _)
  "Blank out version strings in all buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (setq doom-modeline-env--version
            (bound-and-true-p doom-modeline-load-string))))
  (force-mode-line-update t))


;; stolen from doom
;; pyvenv
(use-package pyvenv
  :after python
  :init
  (add-hook 'pyvenv-post-activate-hooks #'+modeline-update-env-in-all-windows-h)
  (add-hook 'pyvenv-pre-deactivate-hooks #'+modeline-clear-env-in-all-windows-h)
  :config
  (setenv "WORKON_HOME" "/home/bolatu/miniconda3/envs")
  (add-hook 'python-mode-local-vars-hook #'pyvenv-track-virtualenv)
  (add-to-list 'global-mode-string
	       '(pyvenv-virtual-env-name (" venv:" pyvenv-virtual-env-name " "))
	       'append))

;; flycheck is quite annoying in python-mode
(defun ub/disable-flycheck-in-python-mode ()
  "Disable flycheck in python mode."
  (flycheck-mode -1))
(if (featurep 'flycheck)
    (add-hook 'python-mode-hook #'ub/disable-flycheck-in-python-mode)
  (message "flycheck is not installed so cannot hook disable flycheck to python-mode"))


;; pyimport: remove unused imports and add missing ones
(straight-use-package
 '(pyimport :type git :host github :repo "Wilfred/pyimport"))

;; pytest
(straight-use-package
 '(emacs-python-pytest :type git :host github :repo "wbolster/emacs-python-pytest"))
(require 'python-pytest)

;; python reformat w/ black
(straight-use-package
 '(emacs-reformatter :type git :host github :repo "purcell/emacs-reformatter"))
(require 'reformatter)
(straight-use-package
 '(emacs-python-black :type git :host github :repo "wbolster/emacs-python-black"))
(straight-use-package
 '(black-macchiato :type git :host github :repo "wbolster/black-macchiato"))
