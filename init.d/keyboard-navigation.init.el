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

(use-package evil-magit
  :after (evil general magit)
  :config

  (general-define-key
   :states '(normal visual motion emacs)

   "C-w" 'delete-frame)

  (general-define-key
   :states '(normal visual motion)
   :keymaps '(magit-mode-map magit-revision-mode-map)

   "C-w" 'delete-frame))

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
   "q"   'quit-window
   "'"   'avy-goto-char-timer)

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
    "cx"  '(eval-last-sexp :which-key "eval last expression")
    "cy"  '(helm-imenu :which-key "symbol in file"))

  (global-def
    "e"  '(:ignore t :which-key "editor")
    "ed" '(dired :which-key "dired")
    "ek" '(package-list-packages :which-key "packages")
    "ep" '(projectile-switch-project :which-key "switch")
    "eq" '(save-buffers-kill-terminal :which-key "quit")
    "es" '(prodigy :which-key "servers")
    "eSn" '(yas-new-snippet :which-key "new snippet")
    "eSS" '(yas-visit-snippet-file :which-key "find snippet file")
    "eSi" '(yas-insert-snippet :which-key "insert snippet"))

  (global-def
    "f"  '(:ignore t :which-key "file")
    "fa" '(find-alternate-file :which-key "reload")
    "fd" '(delete-current-file :which-key "delete current")
    "ff" '(helm-find-files :which-key "find in current directory")
    "fi" '(insert-file :which-key "insert contents")
    "fp" '(helm-browse-project :which-key "open in project")
    "fr" '(er-rename-file-and-buffer :which-key "rename")
    "fs" '(write-file :which-key "save as"))

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
    "u"  '(:ignore t :which-key "buffer")
    "ud" '(evil-delete-buffer :which-key "delete current buffer")
    "up" '(helm-browse-project :which-key "open in project")
    "us" '(save-buffer :which-key "save this buffer")
    "uS" '(write-file :which-key "write to file")
    "uu" '(helm-mini :which-key "switch buffer"))

  (global-def
    "v"   '(:ignore t :which-key "version control")
    "va"  '(smerge-keep-current :which-key "keep all parts")
    "vb"  '(magit-blame-addition :which-key "blame")
    "vlf" '(magit-log-buffer-file :which-key "commits related to file")
    "vsn" '(smerge-next :which-key "go to next conflict")
    "vsN" '(smerge-prev :which-key "go to previous conflict")
    "vsj" '(smerge-keep-lower :which-key "keep lower part")
    "vsk" '(smerge-keep-upper :which-key "keep upper part")
    "vsl" '(smerge-keep-current :which-key "keep part under cursor")
    "vv"  '(projectile-vc :which-key "git status"))

  (global-def
    "w"  '(:ignore t :which-key "window")
    "wd" '(delete-window :which-key "delete current")
    "wD" '(delete-other-windows :which-key "delete other windows")
    "wj" '(split-window-below :which-key "open below")
    "wl" '(split-window-right :which-key "open right"))

  (global-def
    "x" '(:ignore x :which-key "execute")
    "xh" '(projectile-run-eshell :which-key "shell in project")
    "xp" '(projectile-run-async-shell-command-in-root :which-key "command in root")
    "xs" '(helm-projectile-rg :which-key "search"))

  (global-def
    "M-SPC" 'helm-M-x))
