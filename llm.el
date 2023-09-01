

(straight-use-package
 '(org-ai :type git :host github :repo "rksm/org-ai"))
(require 'org-ai)
(add-hook 'org-mode-hook #'org-ai-mode)
(setq org-ai-openai-api-token ub/openai-api-key)
;; if you are on the gpt-4 beta:
(setq org-ai-default-chat-model "gpt-4")
(setq chatgpt-temperature 0.0) ;; NOTE set 0.75, etc. if you want creativity/hallicunation
;;(setq org-ai-default-max-tokens 4096)
;; if you are using yasnippet and want `ai` snippets
(org-ai-install-yasnippets)


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
