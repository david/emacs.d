(use-package clojure-mode
  :config
  (put-clojure-indent 'match 1))

(use-package cider
  :after (general)
  :config
  (add-to-list 'display-buffer-alist
               '("^\\*cider-repl"
                 (display-buffer-reuse-window display-buffer-same-window)
                 (reusable-frames . t)))

  (add-hook 'clojure-mode-hook 'cider-mode))

(use-package cider-eval-sexp-fu
  :after (cider))

(use-package clojure-keybindings
  :after (cider general projectile)
  :no-require t
  :ensure nil
  :config
  (add-hook 'projectile-after-switch-project-hook
            (lambda ()
              (global-def
                "cxf" '(cider-eval-defun-at-point :which-key "defun at point")
                "cxu" '(cider-eval-buffer :which-key "buffer")
                "re"  '(:ignore t :which-key "evals")
                "rn"  '(:ignore t :which-key "namespace")
                "rnr" '(cider-ns-reload :which-key "reload")
                "rns" '(cider-repl-set-ns :which-key "set repl namespace")
                "rq"  '(cider-quit :which-key "quit")
                "rs"  '(cider-jack-in-clj :which-key "start")))))

(use-package clj-refactor
  :after (cider clojure-mode yas)
  :config
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              (yas-minor-mode 1))))

(use-package helm-cider
  :after (cider helm)
  :config
  (helm-cider-mode 1))
