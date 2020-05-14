(use-package git-window-management
  :ensure nil
  :no-require t
  :hook ((git-commit-setup . ior3k/git-commit-setup)))

(use-package magit
  :after general
  :commands (magit-status)
  :config
  (setq magit-commit-show-diff nil)
  (setq magit-log-arguments '("-n128" "--decorate"))
  (setq magit-rebase-arguments '("--autostash"))
  (setq magit-branch-arguments nil)
  (setq magit-commit-show-diff nil))

(use-package evil-magit
  :after (evil general magit)
  :config

  (general-define-key
   :states '(normal visual motion)
   :keymaps '(magit-mode-map magit-revision-mode-map)

   "C-w" 'delete-frame))

(use-package git-gutter-fringe
  :custom
  (git-gutter:update-interval 2)
  (git-gutter-fr:side 'right-fringe)
  (git-gutter:added-sign "+")
  (git-gutter:deleted-sign "-")
  :config
  (global-git-gutter-mode 1))

(defun ior3k/git-commit-setup ()
  (setq-local fill-column 70)
  (auto-fill-mode 1))
