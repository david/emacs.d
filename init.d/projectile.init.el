(use-package projectile
  :after (helm)
  :config
  (setq projectile-completion-system 'helm)

  (projectile-mode 1))
