(use-package general
  :after (evil evil-collection helm)
  :config

  (general-define-key
   :states '(normal motion visual)
   :keymaps 'local
   "f" 'evilem-motion-find-char
   "F" 'evilem-motion-find-char-backward)

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
    "b"  '(:ignore t :which-key "buffer")
    "ba" '(helm-mini :which-key "switch buffer")
    "bd" '(evil-delete-buffer :which-key "delete current buffer")
    "bo" '(read-only-mode :which-key "toggle read only mode")
    "bs" '(save-buffer :which-key "save this buffer")
    "bS" '(write-file :which-key "write to file")
    "bu" '(helm-projectile :which-key "open in project"))

  (global-def
    "c"   '(:ignore t :which-key "code")
    "cc"  '(projectile-compile-project :which-key "compile")
    "cC"  '(recompile :which-key "recompile")
    "ca"  '(lsp-execute-code-action :which-key "action")
    "cet" '(lsp-treemacs-errors-list :which-key "show errors in tree")
    "ch"  '(lsp-describe-thing-at-point :which-key "show symbol documentation")
    "cr"  '(lsp-rename :which-key "rename symbol")
    "cwr" '(lsp-workspace-restart :which-key "restart")
    "cx"  '(:ignore t :which-key "evals"))

  (global-def
    "e"  '(:ignore t :which-key "edit")
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

  (general-define-key
   :states '(normal insert)
   :prefix "M-SPC"

    "p" '(:ignore t :which-key "project")
    "ph" '(projectile-run-eshell :which-key "shell in project")
    "ps" '(counsel-ack :which-key "search in project")
    "px" '(projectile-run-async-shell-command-in-root :which-key "command in root"))

  (global-def
    "q"  '(:ignore t :which-key "sql")
    "qq" '(sql-connect :which-key "connect"))

  (global-def
    "r" '(:ignore t :which-key "repl"))

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
    "vus" '(git-gutter:stage-hunk :which-key "stage hunk"))

  (global-def
    "w"  '(:ignore t :which-key "window")
    "wd" '(delete-window :which-key "delete current")
    "wD" '(delete-other-windows :which-key "delete other windows")
    "wj" '(split-window-below :which-key "open below")
    "wl" '(split-window-right :which-key "open right"))

  (general-define-key
   :prefix "M-SPC"
   :states '(normal insert)

   "M-SPC" 'counsel-M-x))

(use-package elisp-keybindings
  :ensure nil
  :no-require t
  :config
  (general-define-key
   :keymaps '(emacs-lisp-mode-map)
   :prefix "M-SPC"
   :states '(normal insert)

   "e"  '(:ignore t :which-key "eval")
   "ee" '(eval-last-sexp :which-key "last expression")
   "ef" '(eval-defun :which-key "defun")
   "eb" '(eval-buffer :which-key "buffer")))

(use-package clojure-project-keybindings
  :ensure nil
  :no-require t
  :config
  (general-define-key
   :predicate '(equal 'lein-test (projectile-project-type))
   :prefix "M-SPC"
   :states '(normal insert)

   "e"   '(:ignore t :which-key "eval")
   "er"  '(:ignore t :which-key "repl")))
