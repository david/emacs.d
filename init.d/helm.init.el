(use-package helm
  :no-require t
  :init
  (require 'helm-config)

  :config
  (setq helm-ff-auto-update-initial-value t)
  (setq helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s")
  (setq helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'"))

  (general-define-key
   :states 'normal
   :keymaps '(helm-grep-mode-map)

   "RET" 'helm-grep-mode-jump)

  (helm-mode 1)
  (helm-adaptive-mode 1))

(use-package helm-ls-git)

(use-package helm-projectile
  :after (helm projectile)
  :config
  (setq helm-projectile-set-input-automatically nil)
  (setq helm-projectile-sources-list
        '(helm-source-projectile-buffers-list
          helm-source-projectile-recentf-list
          helm-source-projectile-files-list))

  (helm-projectile-on))

(use-package wgrep-helm
  :after (helm)
  :config
  (add-to-list 'display-buffer-alist
               '("^\\*hgrep"
                 (display-buffer-reuse-window display-buffer-same-window)
                 (reusable-frames . t))))
