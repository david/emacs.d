(use-package edn)

(use-package clojure-mode
  :config
  (put-clojure-indent 'match 1))

(use-package cider
  :after (general)
  :config
  (add-hook 'clojure-mode-hook 'cider-mode))

(use-package cider-eval-sexp-fu
  :after (cider))

(use-package clojure-keybindings
  :after (cider general projectile)
  :no-require t
  :ensure nil
  :hook ((clojure-mode . ior3k/add-clojure-keybindings)
         (cider-repl-mode . ior3k/add-clojure-keybindings)))

(use-package clj-refactor
  :after (cider clojure-mode yas)
  :config
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              (yas-minor-mode 1))))

(defun ior3k/add-clojure-keybindings ()
  (when (equal 'lein-test (projectile-project-type))
    (global-def
      :keymaps 'local
      "cxf" '(cider-eval-defun-at-point :which-key "defun at point")
      "cxu" '(cider-eval-buffer :which-key "buffer")
      "cxx" '(cider-eval-last-sexp :which-key "last sexp")
      "re"  '(:ignore t :which-key "evals")
      "rn"  '(:ignore t :which-key "namespace")
      "rnr" '(cider-ns-reload :which-key "reload")
      "rns" '(cider-repl-set-ns :which-key "set repl namespace")
      "rq"  '(cider-quit :which-key "quit")
      "rs"  '(cider-jack-in-clj :which-key "start"))))
