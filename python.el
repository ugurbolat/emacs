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



;;;;;;;;;;;; babel
(straight-use-package
 '(ob-async :type git :host github :repo "astahlman/ob-async"))
(require 'ob-async)
;; Some org-babel languages (e.g., ob-python) define their own :async keyword that conflicts with ob-async.
;; ob-async will ignore any languages in this blacklist, even if the :async keywords is present.
;; REF https://github.com/astahlman/ob-async#configuration
(setq ob-async-no-async-languages-alist '("python" "jupyter-python"))

;;(if (featurep 'org)
(require 'org)
;; babel python
(add-to-list 'org-babel-load-languages '(python . t) t)
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

					;(setq native-comp-deferred-compilation-deny-list '("zmq" "websocket" "jupyter"))

;; babel jupyter-python
;; (straight-use-package
;;  '(emacs-websocket :type git :host github :repo "ahyatt/emacs-websocket"))
;; (straight-use-package
;;  '(emacs-websocket :build nil))
(add-to-list 'load-path (expand-file-name "forks/emacs-websocket" ub/emacs-dir))
(require 'websocket)
(add-to-list 'load-path (expand-file-name "forks/emacs-web-server" ub/emacs-dir))
(require 'simple-httpd)
;; (straight-use-package
;;  '(emacs-zmq :type git :host github :repo "nnicandro/emacs-zmq"))
;; (straight-use-package
;;  '(emacs-zmq :build (:not native-compile)))
(add-to-list 'load-path (expand-file-name "forks/emacs-zmq" ub/emacs-dir))
(require 'zmq)
;; (straight-use-package
;;  '(jupyter :type git :host github :repo "emacs-jupyter/jupyter"))
;; (straight-use-package
;;  '(jupyter :build (:not native-compile)))
(add-to-list 'load-path (expand-file-name "forks/jupyter" ub/emacs-dir))
(require 'jupyter)

(add-to-list 'org-babel-load-languages '(jupyter . t) t)
;; syntax highlighting

(setq org-babel-default-header-args:jupyter-python
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


(add-to-list 'org-src-lang-modes '("jupyter-python" . python))


(defalias 'org-babel-execute:ipython 'org-babel-execute:jupyter-python)
(setq org-babel-default-header-args:ipython org-babel-default-header-args:jupyter-python)
(add-to-list 'org-src-lang-modes '("ipython" . python))


(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

