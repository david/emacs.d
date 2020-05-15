(use-package projectile
  :after (ivy)
  :config
  (setq projectile-completion-system 'ivy)

  (projectile-mode 1))

(use-package counsel-projectile
  :commands (counsel-projectile))
