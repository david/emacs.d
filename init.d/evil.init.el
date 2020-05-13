(use-package evil
  :init
  (setq evil-want-integration nil)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (setq-default evil-shift-width 2)

  (evil-mode 1))

(use-package evil-args
  :after evil)

(use-package evil-collection
  :after (evil helm)
  :init
  (setq evil-collection-setup-minibuffer t)

  (evil-collection-init)

  (evil-collection-define-key 'insert 'helm-map
    (kbd "C-n") nil

    (kbd "C-p") nil
    (kbd "C-k") 'helm-previous-line
    (kbd "C-j") 'helm-next-line)

  (evil-collection-define-key 'normal 'lsp-treemacs-error-list-mode-map
    (kbd "=") 'lsp-treemacs-cycle-severity
    (kbd "x") 'lsp-treemacs-quick-fix))

(use-package evil-easymotion)

(use-package evil-exchange
  :config
  (evil-exchange-install))

(use-package evil-matchit
  :after (evil)
  :config
  (global-evil-matchit-mode 1))

(use-package evil-mc
  :config
  (global-evil-mc-mode 1))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))
