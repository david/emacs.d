(use-package sql
  :no-require t
  :ensure nil
  :config
  (add-to-list 'display-buffer-alist
               '("^\\*SQL.*"
                 (display-buffer-reuse-window display-buffer-same-window)
                 (reusable-frames . t))))

(use-package sql-indent
  :hook (sql-mode . sqlind-minor-mode))
