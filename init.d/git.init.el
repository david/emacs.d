(use-package git-keybindings
  :after general
  :ensure nil
  :no-require t
  :config
  (global-def
    "v"   '(:ignore t :which-key "version control")
    "vb"  '(magit-blame-addition :which-key "blame")
    "vc"  '(:ignore t :which-key "conflicts")
    "vca" '(smerge-keep-current :which-key "keep all parts")
    "vcn" '(smerge-next :which-key "go to next conflict")
    "vcN" '(smerge-prev :which-key "go to previous conflict")
    "vcj" '(smerge-keep-lower :which-key "keep lower part")
    "vck" '(smerge-keep-upper :which-key "keep upper part")
    "vcl" '(smerge-keep-current :which-key "keep part under cursor")
    "vlf" '(magit-log-buffer-file :which-key "commits related to file")
    "vv"  '(projectile-vc :which-key "git status")
    "vu"  '(:ignore t :which-key "in buffer")
    "vun" '(git-gutter:next-hunk :which-key "next hunk")
    "vup" '(git-gutter:previous-hunk :which-key "previous hunk")
    "vur" '(git-gutter:revert-hunk :which-key "revert hunk")
    "vus" '(git-gutter:stage-hunk :which-key "stage hunk")))

(use-package git-window-management
  :ensure nil
  :no-require t
  :hook ((git-commit-setup . ior3k/git-commit-setup))
  :config
  (add-to-list 'display-buffer-alist '(".*\\*transient\\*.*" . ((display-buffer-in-side-window))))
  (add-to-list 'display-buffer-alist '(".*magit: transient.*" . ((display-buffer-in-side-window)))))

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
