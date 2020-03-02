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

(use-package general
  :after (evil evil-collection helm)
  :config

  (general-create-definer ior3k-def
    :states '(emacs insert motion normal visual)
    :prefix "M-SPC")

  (general-define-key
   :states '(emacs insert motion normal visual)

   "M-h" 'windmove-left
   "M-j" 'windmove-down
   "M-k" 'windmove-up
   "M-l" 'windmove-right)

  (general-define-key
   "C-s" '(save-buffer :which-key "save buffer"))

  (general-define-key
   :states 'normal

   "SPC" 'evil-scroll-page-down
   "+"   'universal-argument
   "q"   'previous-buffer
   "Q"   'delete-other-windows)

  (general-define-key
   :states '(normal motion visual)
   "f" 'avy-goto-char-in-line
   "F" 'avy-goto-char-timer)

  (defun ior3k-insert-semicolon-at-eol ()
    (interactive)
    (end-of-line)
    (self-insert-command 1))

  (defun ior3k-find-agenda-org-in-project ()
    (interactive)
    (find-file (expand-file-name "agenda.org" (projectile-project-root))))

  (general-define-key
   :states 'insert
   :keymaps '(java-mode-map rjsx-mode-map reason-mode)
   ";" 'ior3k-insert-semicolon-at-eol)

  (general-define-key
   :states '(emacs insert motion normal visual)
   :keymaps '(completion-list-mode-map fundamental-mode-map)

   "C-w" 'delete-frame)

  (general-define-key
   :states '(normal visual motion emacs)

   "C-w" 'delete-frame)

  (general-define-key
   :states '(normal motion visual)

   "L" 'evil-forward-arg
   "H" 'evil-backward-arg)

  (general-define-key
   :keymaps 'evil-inner-text-objects-map

   "a" 'evil-inner-arg)

  (general-define-key
   :keymaps 'evil-outer-text-objects-map

   "a" 'evil-outer-arg)

  (general-create-definer global-def
    :states '(normal visual motion insert emacs)
    :prefix "M-SPC")

  (global-def
    "c"   '(:ignore t :which-key "code")
    "cc"  '(projectile-compile-project :which-key "compile")
    "cC"  '(recompile :which-key "recompile")
    "ca"  '(lsp-execute-code-action :which-key "action")
    "cen" '(flycheck-next-error :which-key "next error in file")
    "ceN" '(flycheck-previous-error :which-key "previous error in file")
    "cet" '(lsp-treemacs-errors-list :which-key "show errors in tree")
    "ch"  '(lsp-describe-thing-at-point :which-key "show symbol documentation")
    "cr"  '(lsp-rename :which-key "rename symbol")
    "cwr" '(lsp-workspace-restart :which-key "restart")
    "cx"  '(:ignore t :which-key "evals")
    "cy"  '(helm-imenu :which-key "symbol in file"))

  (global-def
    "e"  '(:ignore t :which-key "edit")
    "es" '(helm-projectile-ack :which-key "search in project")
    "eS"  '(:ignore t :which-key "snippets")
    "eSn" '(yas-new-snippet :which-key "new snippet")
    "eSS" '(yas-visit-snippet-file :which-key "find snippet file")
    "eSi" '(yas-insert-snippet :which-key "insert snippet"))

  (global-def
    "E"  '(:ignore t :which-key "editor")
    "Ed" '(dired :which-key "dired")
    "Ek" '(package-list-packages :which-key "packages")
    "Ep" '(projectile-switch-project :which-key "switch")
    "Eq" '(save-buffers-kill-terminal :which-key "quit")
    "Es" '(prodigy :which-key "servers")
    "Ex"  '(eval-last-sexp :which-key "eval last expression"))

  (global-def
    "f"  '(:ignore t :which-key "file")
    "fa" '(find-alternate-file :which-key "reload")
    "fd" '(delete-current-file :which-key "delete current")
    "ff" '(helm-find-files :which-key "find in current directory")
    "fi" '(insert-file :which-key "insert contents")
    "fp" '(helm-browse-project :which-key "open in project")
    "fr" '(er-rename-file-and-buffer :which-key "rename")
    "fs" '(write-file :which-key "save as")
    "fw" '(pwd :which-key "pwd"))

  (global-def
    "h"  '(:ignore t :which-key "help")
    "hf" '(describe-function :which-key "function")
    "hk" '(describe-key :which-key "key")
    "hm" '(describe-mode :which-key "mode")
    "hv" '(describe-variable :which-key "variable"))

  (global-def
    "m"  '(:ignore t :which-key "frame")
    "md" '(delete-frame :which-key "delete current")
    "mm" '(make-frame-command :which-key "new"))

  (global-def
    "q"  '(:ignore t :which-key "sql")
    "qq" '(sql-connect :which-key "connect"))

  (global-def
    "r" '(:ignore t :which-key "repl"))

  (global-def
    "u"  '(:ignore t :which-key "buffer")
    "ua" '(helm-mini :which-key "switch buffer")
    "ud" '(evil-delete-buffer :which-key "delete current buffer")
    "uo" '(read-only-mode :which-key "toggle read only mode")
    "us" '(save-buffer :which-key "save this buffer")
    "uS" '(write-file :which-key "write to file")
    "uu" '(helm-projectile :which-key "open in project"))

  (global-def
    "w"  '(:ignore t :which-key "window")
    "wd" '(delete-window :which-key "delete current")
    "wD" '(delete-other-windows :which-key "delete other windows")
    "wj" '(split-window-below :which-key "open below")
    "wl" '(split-window-right :which-key "open right"))

  (global-def
    "p" '(:ignore t :which-key "execute")
    "ph" '(projectile-run-eshell :which-key "shell in project")
    "px" '(projectile-run-async-shell-command-in-root :which-key "command in root"))

  (global-def
    "M-SPC" 'helm-M-x))
