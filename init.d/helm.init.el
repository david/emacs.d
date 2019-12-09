(use-package helm
  :no-require t
  :init
  (require 'helm-config)

  :config
  (setq helm-ff-auto-update-initial-value t)

  (helm-mode 1)
  (helm-adaptive-mode 1))

(use-package helm-ls-git)

(use-package helm-rg
  :after (general helm))

(use-package helm-ag
  :after (general helm)

  :config
  (general-define-key
   :states '(normal motion)
   :keymaps 'helm-ag-mode-map)

  "RET" 'helm-ag-mode-jump)
