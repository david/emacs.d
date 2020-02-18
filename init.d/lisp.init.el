(use-package lispyville
  :after (evil-collection)
  :hook ((emacs-lisp-mode . lispyville-mode)
         (lisp-mode . lispyville-mode)
         (clojure-mode . lispyville-mode))
  :config
  (lispyville-set-key-theme
   '(operators
     c-w
     text-objects
     atom-movement
     (additional-movement normal visual motion)
     slurp/barf-lispy
     additional)))
