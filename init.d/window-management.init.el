(use-package window-management
  :ensure nil
  :no-require t
  :config
  (setq display-buffer-alist '((""
                                (display-buffer-reuse-window display-buffer-same-window)
                                (reusable-frames . t)))))
