(use-package site-compilation
  :no-require t
  :ensure nil
  :config
  (setq compilation-environment '("TERM=xterm-256color"))
  (setq compilation-scroll-output 'first-error)
  (setq compilation-buffer-name-function
        (lambda (_) (concat "*compilation: " (projectile-project-name) "*")))

  (defun ior3k/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))

  (advice-add 'compilation-filter :around #'ior3k/advice-compilation-filter))