(use-package window-management
  :ensure nil
  :no-require t
  :config
  (setq display-buffer-alist
        '((".*\\*transient\\*.*" . ((display-buffer-in-side-window)))
          (".*magit: transient.*" . ((display-buffer-in-side-window)))
          ("^\\*prodigy-.*\\*$"
           (display-buffer-reuse-window display-buffer-pop-up-frame)
           (reusable-frames . t))
          (""
           (display-buffer-reuse-window display-buffer-same-window)
           (reusable-frames . t)))))
