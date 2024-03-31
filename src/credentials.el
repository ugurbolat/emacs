

(message "loading secret 🤐")

(setq ub/key-openai-token (expand-file-name "key-openai.gpg" user-emacs-directory))

(defun ub/load-encrypted-file (filename)
  "Load an encrypted file."
  ;; emacs add a newlinea to file on save, which I don't know why
  ;; so we check if newline exist in the end of string,
  ;; if so we remove it and return a string
  (with-temp-buffer
    (insert-file-contents filename)
    (setq content (buffer-string)))
    (if (string-suffix-p "\n" content)
        (substring content 0 -1)))



(defun ub/load-key-openai-token ()
  "Load the OpenAI token."
  (interactive)
  (ub/load-encrypted-file ub/key-openai-token))
