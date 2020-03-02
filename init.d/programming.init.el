(use-package aggressive-indent
  :hook ((clojure-mode . aggressive-indent-mode)))

(use-package lsp-mode
  :config
  (setq lsp-auto-guess-root t
        lsp-eldoc-render-all nil
        lsp-enable-snippet t
        lsp-enable-file-watchers nil
        lsp-highlight-symbol-at-point nil
        lsp-inhibit-message t
        lsp-prefer-capf t
        lsp-prefer-flymake nil))

(use-package lsp-treemacs
  :after (lsp-mode))

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-use-childframe nil)
  (setq lsp-ui-sideline-enable nil))

(use-package helm-lsp
  :after (lsp-mode))

(use-package prog-mode
  :after (company)
  :ensure nil
  :no-require t
  :hook ((prog-mode . smartparens-mode)
         (prog-mode . subword-mode)
         (prog-mode . flycheck-mode)))

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))
